use std::error::Error;

use crate::ast::{Branch, Class, Expression, Feature, Formal, Program};
use crate::lexer::Token;
use crate::string_table::SharedString;

pub(crate) struct Parser {
    tokens: Vec<(usize, Token)>,
    pos: usize,
    self_type: SharedString,
    object_type: SharedString,
}

const fn precedence(token: &Token) -> u8 {
    match token {
        Token::Not => 1,
        Token::Less => 2,
        Token::Equal => 2,
        Token::LessEqual => 2,
        Token::Plus => 3,
        Token::Dash => 3,
        Token::Asterisk => 4,
        Token::Slash => 4,
        Token::IsVoid => 5,
        Token::Tilde => 6,
        _ => panic!("Invalid token"),
    }
}

const fn is_binary_op(token: &Token) -> bool {
    match token {
        Token::Plus
        | Token::Dash
        | Token::Asterisk
        | Token::Slash
        | Token::Less
        | Token::LessEqual
        | Token::Equal => true,
        _ => false,
    }
}

const fn is_unary_op(token: &Token) -> bool {
    match token {
        Token::Tilde | Token::Not | Token::IsVoid => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseError {
    Err,
}

impl Error for ParseError {
    fn description(&self) -> &str {
        "ParseError"
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParseError")
    }
}

type Result<T> = std::result::Result<T, ParseError>;

impl Parser {
    pub(crate) fn new<'a>(token_iter: impl Iterator<Item = (usize, Token)> + 'a) -> Parser {
        // if tokens contains SELF_TYPE assign it to the self_type field
        let tokens: Vec<(usize, Token)> = token_iter.collect();
        let self_type = match tokens.iter().find(|(_, token)| match token {
            Token::TypeId(t) if t == "SELF_TYPE" => true,
            _ => false,
        }) {
            Some((_, Token::TypeId(t))) => t.clone(),
            _ => SharedString::new("SELF_TYPE"),
        };
        let object_type = match tokens.iter().find(|(_, token)| match token {
            Token::TypeId(t) if t == "Object" => true,
            _ => false,
        }) {
            Some((_, Token::TypeId(t))) => t.clone(),
            _ => SharedString::new("Object"),
        };
        Parser {
            tokens,
            pos: 0,
            self_type,
            object_type,
        }
    }
    fn peek(&self) -> &Token {
        if self.pos >= self.tokens.len() {
            return &Token::Eof;
        }
        &self.tokens[self.pos].1
    }
    // PROGRAM: [CLASS]+
    pub(crate) fn parse_program(&mut self) -> Result<Program> {
        let mut classes = Vec::new();
        loop {
            match self.parse_class() {
                Ok(c) => classes.push(c),
                Err(_) => break,
            }
        }

        if self.pos != self.tokens.len() {
            return Err(ParseError::Err);
        }

        Ok(Program(classes))
    }
    // CLASS: class TYPEID [inherits TYPEID] { [FEATURE]* } ;
    fn parse_class(&mut self) -> Result<Class> {
        let save = self.pos;

        if let None = self.eat_token(Token::Class) {
            return Err(ParseError::Err);
        }

        let name = match self.eat_type_ident() {
            Some((_, t)) => t,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        let parent = match self.eat_token(Token::Inherits) {
            Some(_) => match self.eat_type_ident() {
                Some((_, t)) => t,
                None => {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
            },
            None => self.object_type.clone(),
        };

        if let None = self.eat_token(Token::LBrace) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let mut features = Vec::new();

        loop {
            match self.parse_feature() {
                Ok(f) => features.push(f),
                Err(_) => break,
            }
        }

        if let None = self.eat_token(Token::RBrace) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        if let None = self.eat_token(Token::SemiColon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        Ok(Class {
            name,
            parent,
            features,
        })
    }
    // FEATURE: METHOD | ATTRIBUTE
    fn parse_feature(&mut self) -> Result<Feature> {
        self.parse_method().or_else(|_| self.parse_attribute())
    }
    // METHOD: OBJECTID ( [formal [, formal]*] ) : TYPEID { EXPRESSION } ;
    fn parse_method(&mut self) -> Result<Feature> {
        let save = self.pos;

        let ident = self.eat_object_ident().ok_or(ParseError::Err)?;

        if let None = self.eat_token(Token::LParen) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let mut formals = Vec::new();

        if let None = self.eat_token(Token::RParen) {
            loop {
                match self.parse_formal() {
                    Ok(f) => formals.push(f),
                    Err(_) => {
                        self.pos = save;
                        return Err(ParseError::Err);
                    }
                }
                if let None = self.eat_token(Token::Comma) {
                    break;
                }
            }
            if let None = self.eat_token(Token::RParen) {
                self.pos = save;
                return Err(ParseError::Err);
            }
        }

        if let None = self.eat_token(Token::Colon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let return_type = match self.eat_type_ident() {
            Some((_, t)) => t,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        if let None = self.eat_token(Token::LBrace) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let body = match self.parse_expression() {
            Ok(e) => e,
            Err(_) => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        if let None = self.eat_token(Token::RBrace) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        if let None = self.eat_token(Token::SemiColon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        Ok(Feature::Method {
            ident,
            formals,
            return_type,
            body,
        })
    }
    // FORMAL: OBJECTID : TYPEID
    fn parse_formal(&mut self) -> Result<Formal> {
        let save = self.pos;

        let ident = self.eat_object_ident().ok_or(ParseError::Err)?;

        if let None = self.eat_token(Token::Colon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let ident_type = match self.eat_type_ident() {
            Some((_, t)) => t,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(Formal { ident, ident_type })
    }
    // ATTRIBUTE: OBJECTID : TYPEID [ <- expression ] ;
    fn parse_attribute(&mut self) -> Result<Feature> {
        let save = self.pos;

        let ident = self.eat_object_ident().ok_or(ParseError::Err)?;

        if let None = self.eat_token(Token::Colon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let ident_type = match self.eat_type_ident() {
            Some((_, t)) => t,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        let init = if let Some(_) = self.eat_token(Token::Assign) {
            match self.parse_expression() {
                Ok(e) => Some(e),
                Err(_) => {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
            }
        } else {
            None
        };

        if let None = self.eat_token(Token::SemiColon) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        Ok(Feature::Attribute {
            ident,
            ident_type,
            init,
        })
    }
    /*
     * A: INTEGER
     *  | STRING
     *  | OBJECTID([EXPRESSION(, EXPRESSION)*])
     *  | OBJECTID
     *  | BOOL
     *  | NEW TYPEID
     *  | ( EXPRESSION )
     *  | IF EXPRESSION THEN EXPRESSION ELSE EXPRESSION FI
     *  | WHILE EXPRESSION LOOP EXPRESSION POOL
     *  | { { EXPRESSION }+ }
     *  | CASE EXPRESSION OF { OBJECTID : TYPEID => EXPRESSION ; }+
     *
     * B: OBJECTID <- EXPRESSION
     *  | LET OBJECTID : TYPEID [ <- expression ] ( , OBJECTID : TYPEID [ <- expression ] )* IN EXPRESSION
     *
     * C: @TYPEID.OBJECTID(EXPRESSION(, EXPRESSION)*) C
     *  | .OBJECTID(EXPRESSION(, EXPRESSION )*) C
     *  | @TYPEID.OBJECTID(EXPRESSION(, EXPRESSION)*)
     *  | .OBJECTID(EXPRESSION(, EXPRESSION )*)
     *
     * PRIMARY: B
     *        | A C
     *        | A
     *
     * EXPRESSION: PRIMARY
     *           | PRIMARY + EXPRESSION
     *           | PRIMARY - EXPRESSION
     *           | PRIMARY * EXPRESSION
     *           | PRIMARY / EXPRESSION
     *           | PRIMARY < EXPRESSION
     *           | PRIMARY <= EXPRESSION
     *           | PRIMARY = EXPRESSION
     *           | ~ EXPRESSION
     *           | not EXPRESSION
     *           | isvoid EXPRESSION
     */
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_associative_expression(0)
    }
    fn parse_primary(&mut self) -> Result<Expression> {
        self.parse_integer()
            .or_else(|_| self.parse_string())
            .or_else(|_| self.parse_self_dispatch())
            .or_else(|_| self.parse_object_ident())
            .or_else(|_| self.parse_bool())
            .or_else(|_| self.parse_new())
            .or_else(|_| self.parse_parenthesized())
            .or_else(|_| self.parse_if())
            .or_else(|_| self.parse_while())
            .or_else(|_| self.parse_block())
            .or_else(|_| self.parse_case())
    }
    fn parse_primary_expression(&mut self) -> Result<Expression> {
        self.parse_assignment()
            .or_else(|_| self.parse_let())
            .or_else(|_| self.parse_dispatch())
            .or_else(|_| self.parse_primary())
    }
    fn parse_associative_expression(&mut self, min_precedence: u8) -> Result<Expression> {
        let save = self.pos;
        let mut lookahead = self.peek().clone();
        let mut rhs;
        let mut lhs = if is_unary_op(&lookahead) {
            self.pos += 1;
            match self.parse_associative_expression(precedence(&lookahead) + 1) {
                Ok(result) => match lookahead {
                    Token::Not => Expression::Not(Box::new(result)),
                    Token::Tilde => Expression::Negate(Box::new(result)),
                    Token::IsVoid => Expression::IsVoid(Box::new(result)),
                    _ => panic!("Invalid token"),
                },
                Err(e) => {
                    self.pos = save;
                    return Err(e);
                }
            }
        } else {
            match self.parse_primary_expression() {
                Ok(result) => result,
                Err(e) => {
                    self.pos = save;
                    return Err(e);
                }
            }
        };
        lookahead = self.peek().clone();
        while is_binary_op(&lookahead) && (precedence(&lookahead) >= min_precedence) {
            self.pos += 1;
            rhs = match self.parse_associative_expression(precedence(&lookahead) + 1) {
                Ok(result) => result,
                Err(e) => {
                    self.pos = save;
                    return Err(e);
                }
            };
            lhs = match lookahead {
                Token::Plus => Expression::Plus(Box::new(lhs), Box::new(rhs)),
                Token::Dash => Expression::Subtract(Box::new(lhs), Box::new(rhs)),
                Token::Asterisk => Expression::Multiply(Box::new(lhs), Box::new(rhs)),
                Token::Slash => Expression::Divide(Box::new(lhs), Box::new(rhs)),
                Token::Less => Expression::Less(Box::new(lhs), Box::new(rhs)),
                Token::LessEqual => Expression::LessEqual(Box::new(lhs), Box::new(rhs)),
                Token::Equal => Expression::Equal(Box::new(lhs), Box::new(rhs)),
                _ => panic!("Invalid token"),
            };
            lookahead = self.peek().clone();
        }
        Ok(lhs)
    }
    // OBJECTID <- EXPRESSION
    fn parse_assignment(&mut self) -> Result<Expression> {
        let save = self.pos;

        let object_ident = self.eat_object_ident().ok_or(ParseError::Err)?;

        if let None = self.eat_token(Token::Assign) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let expr = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        Ok(Expression::Assignment {
            ident: object_ident,
            init: Box::new(expr),
        })
    }
    // PRIMARY: A C
    fn parse_dispatch(&mut self) -> Result<Expression> {
        let save = self.pos;

        let lhs = match self.parse_primary() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        let expr = match self.parse_part_dispatch(lhs) {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(expr)
    }
    /* C: @TYPEID.OBJECTID(EXPRESSION(, EXPRESSION)*) C
     *  | .OBJECTID(EXPRESSION(, EXPRESSION )*) C
     *  | @TYPEID.OBJECTID(EXPRESSION(, EXPRESSION)*)
     *  | .OBJECTID(EXPRESSION(, EXPRESSION )*)
     */
    fn parse_part_dispatch(
        &mut self,
        mut lhs: Expression,
    ) -> std::result::Result<Expression, Expression> {
        let expr = match self.peek() {
            Token::At => {
                lhs = self.parse_static_dispatch(lhs)?;

                match self.parse_part_dispatch(lhs) {
                    Ok(result) => result,
                    Err(e) => e,
                }
            }
            Token::Dot => {
                lhs = self.parse_dynamic_dispatch(lhs)?;

                match self.parse_part_dispatch(lhs) {
                    Ok(result) => result,
                    Err(e) => e,
                }
            }
            _ => return Err(lhs),
        };

        Ok(expr)
    }
    // lhs@TYPE.ID([EXPRESSION[, EXPRESSION]*])
    fn parse_static_dispatch(
        &mut self,
        lhs: Expression,
    ) -> std::result::Result<Expression, Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::At) {
            return Err(lhs);
        }

        let (_, as_type) = match self.eat_type_ident() {
            Some(result) => result,
            None => {
                self.pos = save;
                return Err(lhs);
            }
        };

        if let None = self.eat_token(Token::Dot) {
            self.pos = save;
            return Err(lhs);
        }

        let method_ident = match self.eat_object_ident() {
            Some(result) => result,
            None => {
                self.pos = save;
                return Err(lhs);
            }
        };

        if let None = self.eat_token(Token::LParen) {
            self.pos = save;
            return Err(lhs);
        }

        let mut params = Vec::new();

        if let None = self.eat_token(Token::RParen) {
            loop {
                let expr = match self.parse_expression() {
                    Ok(result) => result,
                    Err(e) => {
                        self.pos = save;
                        return Err(lhs);
                    }
                };
                params.push(expr);
                if let None = self.eat_token(Token::Comma) {
                    break;
                }
            }
            if let None = self.eat_token(Token::RParen) {
                self.pos = save;
                return Err(lhs);
            }
        }

        Ok(Expression::Dispatch {
            class_ident: Some(Box::new(lhs)),
            as_type,
            method_ident,
            params,
        })
    }
    // lhs.ID([EXPRESSION[, EXPRESSION]*])
    fn parse_dynamic_dispatch(
        &mut self,
        lhs: Expression,
    ) -> std::result::Result<Expression, Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::Dot) {
            return Err(lhs);
        }

        let method_ident = match self.eat_object_ident() {
            Some(result) => result,
            None => {
                self.pos = save;
                return Err(lhs);
            }
        };

        if let None = self.eat_token(Token::LParen) {
            self.pos = save;
            return Err(lhs);
        }

        let mut params = Vec::new();

        if let None = self.eat_token(Token::RParen) {
            loop {
                let expr = match self.parse_expression() {
                    Ok(result) => result,
                    Err(e) => {
                        self.pos = save;
                        return Err(lhs);
                    }
                };
                params.push(expr);
                if let None = self.eat_token(Token::Comma) {
                    break;
                }
            }
            if let None = self.eat_token(Token::RParen) {
                self.pos = save;
                return Err(lhs);
            }
        }

        Ok(Expression::Dispatch {
            class_ident: Some(Box::new(lhs)),
            as_type: self.self_type.clone(),
            method_ident,
            params,
        })
    }
    // ID([EXPRESSION[, EXPRESSION]*])
    fn parse_self_dispatch(&mut self) -> Result<Expression> {
        let save = self.pos;

        let method_ident = match self.eat_object_ident() {
            Some(result) => result,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        if let None = self.eat_token(Token::LParen) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let mut params = Vec::new();

        if let None = self.eat_token(Token::RParen) {
            loop {
                let expr = match self.parse_expression() {
                    Ok(result) => result,
                    Err(e) => {
                        self.pos = save;
                        return Err(e);
                    }
                };
                params.push(expr);
                if let None = self.eat_token(Token::Comma) {
                    break;
                }
            }
            if let None = self.eat_token(Token::RParen) {
                self.pos = save;
                return Err(ParseError::Err);
            }
        }

        Ok(Expression::Dispatch {
            class_ident: None,
            as_type: self.self_type.clone(),
            method_ident,
            params,
        })
    }
    fn parse_parenthesized(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::LParen) {
            return Err(ParseError::Err);
        }

        let expr = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::RParen) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        Ok(expr)
    }
    // if EXPRESSION then EXPRESSION else EXPRESSION fi
    fn parse_if(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::If) {
            return Err(ParseError::Err);
        }

        let condition = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Then) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let then_do = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Else) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let else_do = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        let line_number = match self.eat_token(Token::Fi) {
            Some(n) => n,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(Expression::If {
            condition: Box::new(condition),
            then_do: Box::new(then_do),
            else_do: Box::new(else_do),
        })
    }
    // while EXPRESSION loop EXPRESSION pool
    fn parse_while(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::While) {
            return Err(ParseError::Err);
        }

        let condition = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => return Err(e),
        };

        if let None = self.eat_token(Token::Loop) {
            return Err(ParseError::Err);
        }

        let do_expr = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => return Err(e),
        };

        let line_number = match self.eat_token(Token::Pool) {
            Some(n) => n,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(Expression::While {
            condition: Box::new(condition),
            do_expr: Box::new(do_expr),
        })
    }
    // { [EXPRESSION;]+}
    fn parse_block(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::LBrace) {
            return Err(ParseError::Err);
        }

        let mut expressions = Vec::new();

        loop {
            let loop_save = self.pos;
            let expr = match self.parse_expression() {
                Ok(result) => result,
                Err(_) => {
                    self.pos = loop_save;
                    break;
                }
            };
            expressions.push(expr);

            if let None = self.eat_token(Token::SemiColon) {
                break;
            }
        }

        if expressions.len() == 0 {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let line_number = match self.eat_token(Token::RBrace) {
            Some(n) => n,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(Expression::Block(expressions))
    }
    // let OBJECTID : TYPE [<- EXPRESSION] [, OBJECTID:TYPE [<- EXPRESSION]]* in EXPRESSION
    fn parse_let(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::Let) {
            return Err(ParseError::Err);
        }

        let mut bindings = Vec::new();

        loop {
            let object_ident = match self.eat_object_ident() {
                Some(ident) => ident,
                None => {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
            };
            if let None = self.eat_token(Token::Colon) {
                self.pos = save;
                return Err(ParseError::Err);
            }

            let object_type = match self.eat_type_ident() {
                Some(result) => result,
                None => {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
            };

            let init = match self.eat_token(Token::Assign) {
                Some(_) => match self.parse_expression() {
                    Ok(result) => Some(Box::new(result)),
                    Err(e) => {
                        self.pos = save;
                        return Err(e);
                    }
                },
                None => None,
            };

            bindings.push((object_ident, object_type, init));

            if let None = self.eat_token(Token::Comma) {
                if let None = self.eat_token(Token::In) {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
                break;
            }
        }

        let mut expr = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        for (ident, (_, ident_type), init) in bindings.into_iter().rev() {
            expr = Expression::Let {
                ident,
                ident_type,
                init,
                do_expr: Box::new(expr),
            };
        }

        Ok(expr)
    }
    // case EXPRESSION of [OBJECTID:TYPE => EXPRESSION;]+ esac
    fn parse_case(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::Case) {
            return Err(ParseError::Err);
        }

        let condition = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.pos = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Of) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        let mut branches = Vec::new();

        loop {
            let ident = match self.eat_object_ident() {
                Some(ident) => ident,
                None => break,
            };

            if let None = self.eat_token(Token::Colon) {
                self.pos = save;
                return Err(ParseError::Err);
            }

            let (_, ident_type) = match self.eat_type_ident() {
                Some(result) => result,
                None => {
                    self.pos = save;
                    return Err(ParseError::Err);
                }
            };

            if let None = self.eat_token(Token::DArrow) {
                self.pos = save;
                return Err(ParseError::Err);
            }

            let do_expr = match self.parse_expression() {
                Ok(result) => result,
                Err(e) => {
                    self.pos = save;
                    return Err(e);
                }
            };

            branches.push(Branch {
                ident,
                ident_type,
                do_expr,
            });

            if let None = self.eat_token(Token::SemiColon) {
                return Err(ParseError::Err);
            }
        }

        if let None = self.eat_token(Token::Esac) {
            self.pos = save;
            return Err(ParseError::Err);
        }

        Ok(Expression::Case {
            condition: Box::new(condition),
            branches,
        })
    }
    // new TYPE
    fn parse_new(&mut self) -> Result<Expression> {
        let save = self.pos;

        if let None = self.eat_token(Token::New) {
            return Err(ParseError::Err);
        }

        let (line_number, ident) = match self.eat_type_ident() {
            Some(ident) => ident,
            None => {
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

        Ok(Expression::New(ident))
    }
    // INTEGER
    fn parse_integer(&mut self) -> Result<Expression> {
        if let Some((_, t)) = self.tokens.get(self.pos) {
            if let Token::IntConst(n) = t {
                self.pos += 1;
                return Ok(Expression::IntLiteral(n.clone()));
            }
        }
        Err(ParseError::Err)
    }
    fn parse_string(&mut self) -> Result<Expression> {
        if let Some((_, t)) = self.tokens.get(self.pos) {
            if let Token::StrConst(n) = t {
                self.pos += 1;
                return Ok(Expression::StringLiteral(n.clone()));
            }
        }
        Err(ParseError::Err)
    }
    fn parse_bool(&mut self) -> Result<Expression> {
        if let Some((_, t)) = self.tokens.get(self.pos) {
            if let Token::BoolConst(n) = t {
                self.pos += 1;
                return Ok(Expression::BoolLiteral(n.clone()));
            }
        }
        Err(ParseError::Err)
    }
    fn parse_object_ident(&mut self) -> Result<Expression> {
        self.eat_object_ident()
            .map(|ident| Expression::ObjectIdent(ident))
            .ok_or(ParseError::Err)
    }
    fn eat_token(&mut self, token: Token) -> Option<usize> {
        if let Some((line_number, t)) = self.tokens.get(self.pos) {
            if *t == token {
                self.pos += 1;
                return Some(*line_number);
            }
        }
        None
    }
    fn try_eat_token(&mut self, token: Token) -> bool {
        if let Some((_, t)) = self.tokens.get(self.pos) {
            if *t == token {
                return true;
            }
        }
        false
    }
    fn eat_object_ident(&mut self) -> Option<SharedString> {
        if let Some((_, token)) = self.tokens.get(self.pos) {
            if let Token::ObjectId(s) = token {
                self.pos += 1;
                return Some(s.clone());
            }
        }
        None
    }
    fn eat_type_ident(&mut self) -> Option<(usize, SharedString)> {
        if let Some((line_number, token)) = self.tokens.get(self.pos) {
            if let Token::TypeId(s) = token {
                self.pos += 1;
                return Some((*line_number, s.clone()));
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use std::{
        fs::{read_to_string, File},
        io::{BufWriter, Write},
    };

    const PATH: &str = "tests/parser";

    //#[test]
    #[allow(dead_code)]
    fn gen_correct_parsed_result() {
        let srcs = vec![
            "program",
            "if",
            "block",
            "method",
            "let",
            "while",
            "case",
            "attribute",
            "class",
            "feature",
            "formal",
        ];
        for src in srcs {
            let text = read_to_string(format!("{PATH}/src/{src}.cl")).unwrap();
            let iter = Lexer::lex(&text);
            let mut parser = Parser::new(iter);
            let result = match src {
                "program" => parser.parse_program().unwrap().to_string(),
                "if" => parser.parse_if().unwrap().to_string(),
                "block" => parser.parse_block().unwrap().to_string(),
                "method" => parser.parse_method().unwrap().to_string(),
                "let" => parser.parse_let().unwrap().to_string(),
                "while" => parser.parse_while().unwrap().to_string(),
                "case" => parser.parse_case().unwrap().to_string(),
                "attribute" => parser.parse_attribute().unwrap().to_string(),
                "class" => parser.parse_class().unwrap().to_string(),
                "feature" => parser.parse_feature().unwrap().to_string(),
                "formal" => parser.parse_formal().unwrap().to_string(),
                _ => unreachable!(),
            };
            let file = File::create(format!("{PATH}/dst/{src}.txt")).unwrap();
            let mut file = BufWriter::new(file);
            write!(file, "{}", result).unwrap();
        }
    }
    #[test]
    fn test_parse_program() {
        let text = read_to_string(format!("{PATH}/src/program.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_program().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/program.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_if() {
        let text = read_to_string(format!("{PATH}/src/if.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_if().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/if.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_block() {
        let text = read_to_string(format!("{PATH}/src/block.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_block().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/block.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_while() {
        let text = read_to_string(format!("{PATH}/src/while.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_while().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/while.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_let() {
        let text = read_to_string(format!("{PATH}/src/let.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_let().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/let.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_method() {
        let text = read_to_string(format!("{PATH}/src/method.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_method().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/method.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_case() {
        let text = read_to_string(format!("{PATH}/src/case.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_case().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/case.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_attribute() {
        let text = read_to_string(format!("{PATH}/src/attribute.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_attribute().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/attribute.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_class() {
        let text = read_to_string(format!("{PATH}/src/class.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_class().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/class.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_feature() {
        let text = read_to_string(format!("{PATH}/src/feature.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_feature().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/feature.txt")).unwrap();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_parse_formal() {
        let text = read_to_string(format!("{PATH}/src/formal.cl")).unwrap();
        let iter = Lexer::lex(&text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_formal().unwrap().to_string();
        let correct = read_to_string(format!("{PATH}/dst/formal.txt")).unwrap();
        assert_eq!(result, correct);
    }
}
