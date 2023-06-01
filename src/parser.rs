use std::error::Error;

use crate::ast::{Branch, Class, Expression, Feature, Formal, Program};
use crate::lexer::Token;
use crate::string_table::SharedString;

struct Parser {
    tokens: Vec<(usize, Token)>,
    next: usize,
}

const fn precedence(token: &Token) -> u8 {
    match token {
        Token::Assign => 1,
        Token::Not => 2,
        Token::Less => 3,
        Token::Equal => 3,
        Token::LessEqual => 3,
        Token::Plus => 4,
        Token::Dash => 4,
        Token::Asterisk => 5,
        Token::Slash => 5,
        Token::IsVoid => 6,
        Token::Wave => 7,
        Token::At => 8,
        Token::Dot => 9,
        _ => panic!("Invalid token"),
    }
}

const fn is_operator(token: &Token) -> bool {
    match token {
        Token::Plus
        | Token::Dash
        | Token::Asterisk
        | Token::Slash
        | Token::Less
        | Token::LessEqual
        | Token::Equal
        | Token::Dot
        | Token::Assign
        | Token::Not
        | Token::IsVoid
        | Token::Wave
        | Token::At => true,
        _ => false,
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
        | Token::Equal
        | Token::Assign => true,
        _ => false,
    }
}

const fn is_right_associative(token: &Token) -> bool {
    match token {
        Token::Assign => true,
        _ => false,
    }
}

#[derive(Debug)]
enum ParseError {
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

type Result<T> = std::result::Result<(usize, T), ParseError>;

impl Parser {
    fn new<'a>(token_iter: impl Iterator<Item = (usize, Token)> + 'a) -> Parser {
        Parser {
            tokens: token_iter.collect(),
            next: 0,
        }
    }
    fn peek(&self) -> &Token {
        if self.next >= self.tokens.len() {
            return &Token::Eof;
        }
        &self.tokens[self.next].1
    }
    // PROGRAM: [CLASS]+
    fn parse(&mut self) -> Result<Program> {
        todo!()
    }
    // CLASS: class TYPEID [inherits TYPEID] { [FEATURE]* } ;
    fn parse_class(&mut self) -> Result<Class> {
        todo!()
    }
    // FEATURE: METHOD | ATTRIBUTE
    fn parse_feature(&mut self) -> Result<Feature> {
        todo!()
    }
    // METHOD: OBJECTID ( [formal_list] ) : TYPEID { EXPRESSION } ;
    fn parse_method(&mut self) -> Result<Feature> {
        todo!()
    }
    // FORMAL_LIST: [formal ( , [formal] )*]
    fn parse_formal_list(&mut self) -> Result<Vec<Formal>> {
        todo!()
    }
    // FORMAL: OBJECTID : TYPEID
    fn parse_formal(&mut self) -> Result<Formal> {
        todo!()
    }
    // ATTRIBUTE: OBJECTID : TYPEID [ <- expression ] ;
    fn parse_attribute(&mut self) -> Result<Feature> {
        todo!()
    }
    // PRIMARY: OBJECTID <- EXPRESSION
    //        | ID([EXPRESSION[, EXPRESSION]*]])
    //        | if EXPRESSION then EXPRESSION else EXPRESSION fi
    //        | while EXPRESSION loop EXPRESSION pool
    //        | { [EXPRESSION;]+}
    //        | LET_STMT
    //        | case EXPRESSION of [OBJECTID : TYPEID => EXPRESSION;]+ esac
    //        | NEW TYPEID
    //        | (EXPRESSION)
    //        | OBJECTID
    //        | INTEGER
    //        | STRING
    //        | BOOL
    // EXPRESSION: OBJECTID <- EXPRESSION
    //           | EXPRESSION[@TYPE].ID([EXPRESSION[, EXPRESSION]*])
    //           | ID([EXPRESSION[, EXPRESSION]*]])
    //           | if EXPRESSION then EXPRESSION else EXPRESSION fi
    //           | while EXPRESSION loop EXPRESSION pool
    //           | { [EXPRESSION;]+}
    //           | LET OBJECTID : TYPEID [ <- EXPRESSION ] ( , OBJECTID : TYPEID [ <- EXPRESSION ] )* IN EXPRESSION
    //           | case EXPRESSION of [OBJECTID : TYPEID => EXPRESSION;]+ esac
    //           | NEW TYPEID
    //           | isvoid EXPRESSION
    //           | EXPRESSION + EXPRESSION
    //           | EXPRESSION - EXPRESSION
    //           | EXPRESSION * EXPRESSION
    //           | EXPRESSION / EXPRESSION
    //           | ~ EXPRESSION
    //           | EXPRESSION < EXPRESSION
    //           | EXPRESSION <= EXPRESSION
    //           | EXPRESSION = EXPRESSION
    //           | not EXPRESSION
    //           | ( EXPRESSION )
    //           | OBJECTID
    //           | INTEGER
    //           | STRING
    //           | BOOL
    fn parse_expression(&mut self) -> Result<Expression> {
        todo!()
    }
    fn parse_primary_expression(&mut self) -> Result<Expression> {
        todo!()
    }
    fn parse_associative_expression(
        &mut self,
        mut lhs: (usize, Expression),
        min_precedence: u8,
    ) -> Result<Expression> {
        let mut lookahead = self.peek().clone();
        while is_binary_op(&lookahead) && (precedence(&lookahead) >= min_precedence) {
            let op = lookahead.clone();
            self.next += 1;
            let mut rhs = match self.parse_primary_expression() {
                Ok(result) => result,
                Err(e) => return Err(e),
            };
            lookahead = self.peek().clone();
            while (is_binary_op(&lookahead) && (precedence(&lookahead) > precedence(&op)))
                || (is_right_associative(&lookahead) && precedence(&lookahead) == precedence(&op))
            {
                let p = if precedence(&lookahead) > precedence(&op) {
                    precedence(&op) + 1
                } else {
                    precedence(&op)
                };
                rhs = match self.parse_associative_expression(rhs, p) {
                    Ok(result) => result,
                    Err(e) => return Err(e),
                };
                lookahead = self.peek().clone();
            }

            lhs = match op {
                Token::Plus => (rhs.0, Expression::Plus(Box::new(lhs.1), Box::new(rhs.1))),
                Token::Dash => (
                    rhs.0,
                    Expression::Subtract(Box::new(lhs.1), Box::new(rhs.1)),
                ),
                Token::Asterisk => (
                    rhs.0,
                    Expression::Multiply(Box::new(lhs.1), Box::new(rhs.1)),
                ),
                Token::Slash => (rhs.0, Expression::Divide(Box::new(lhs.1), Box::new(rhs.1))),
                Token::Less => (rhs.0, Expression::Less(Box::new(lhs.1), Box::new(rhs.1))),
                Token::LessEqual => (
                    rhs.0,
                    Expression::LessEqual(Box::new(lhs.1), Box::new(rhs.1)),
                ),
                Token::Equal => (rhs.0, Expression::Equal(Box::new(lhs.1), Box::new(rhs.1))),
                Token::Assign => (
                    rhs.0,
                    Expression::Assignment {
                        ident: match lhs.1 {
                            Expression::ObjectIdent(ident) => ident,
                            _ => panic!("Invalid expression"),
                        },
                        init: Box::new(rhs.1),
                    },
                ),
                _ => panic!("Invalid token"),
            };
        }
        Ok(lhs)
    }
    // OBJECTID <- EXPRESSION
    fn parse_assignment(&mut self) -> Result<Expression> {
        let save = self.next;

        let object_ident = self.eat_object_ident().ok_or(ParseError::Err)?;

        if let None = self.eat_token(Token::Assign) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let (line_number, expr) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        Ok((
            line_number,
            Expression::Assignment {
                ident: object_ident,
                init: Box::new(expr),
            },
        ))
    }
    fn parse_dispatch(&mut self) -> Result<Expression> {
        todo!()
    }
    // EXPRESSION@TYPE.ID([EXPRESSION[, EXPRESSION]*])
    fn parse_static_dispatch(&mut self) -> Result<Expression> {
        let save = self.next;

        let (_, expr) = match self.parse_primary_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::At) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let (_, as_type) = match self.eat_type_ident() {
            Some(result) => result,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        if let None = self.eat_token(Token::Dot) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let method_ident = match self.eat_object_ident() {
            Some(ident) => ident,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        if let None = self.eat_token(Token::LeftParenthesis) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let mut params = Vec::new();

        loop {
            let (_, arg) = match self.parse_expression() {
                Ok(result) => result,
                Err(_) => {
                    self.next = save;
                    return Err(ParseError::Err);
                }
            };
            params.push(arg);

            if let None = self.eat_token(Token::Comma) {
                break;
            }
        }

        let Some(line_number) = self.eat_token(Token::RightParenthesis) else {
            self.next = save;
            return Err(ParseError::Err);
        };

        Ok((
            line_number,
            Expression::Dispatch {
                class_ident: Box::new(expr),
                as_type,
                method_ident,
                params,
            },
        ))
    }
    // EXPRESSION.ID([EXPRESSION[, EXPRESSION]*])
    fn parse_dynamic_dispatch(&mut self) -> Result<Expression> {
        let save = self.next;

        todo!()
    }
    // ID([EXPRESSION[, EXPRESSION]*])
    fn parse_self_dispatch(&mut self) -> Result<Expression> {
        todo!()
    }
    // if EXPRESSION then EXPRESSION else EXPRESSION fi
    fn parse_if(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::If) {
            return Err(ParseError::Err);
        }

        let (_, condition) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Then) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let (_, then_do) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Else) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let (_, else_do) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        let line_number = match self.eat_token(Token::Fi) {
            Some(n) => n,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        Ok((
            line_number,
            Expression::If {
                condition: Box::new(condition),
                then_do: Box::new(then_do),
                else_do: Box::new(else_do),
            },
        ))
    }
    // while EXPRESSION loop EXPRESSION pool
    fn parse_while(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::While) {
            return Err(ParseError::Err);
        }

        let (_, condition) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => return Err(e),
        };

        if let None = self.eat_token(Token::Loop) {
            return Err(ParseError::Err);
        }

        let (_, do_expr) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => return Err(e),
        };

        let line_number = match self.eat_token(Token::Fi) {
            Some(n) => n,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        Ok((
            line_number,
            Expression::While {
                condition: Box::new(condition),
                do_expr: Box::new(do_expr),
            },
        ))
    }
    // { [EXPRESSION;]+}
    fn parse_block(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::LeftBrace) {
            return Err(ParseError::Err);
        }

        let mut expressions = Vec::new();

        loop {
            let loop_save = self.next;
            let (_, expr) = match self.parse_expression() {
                Ok(result) => result,
                Err(_) => {
                    self.next = loop_save;
                    break;
                }
            };
            expressions.push(expr);

            if let None = self.eat_token(Token::SemiColon) {
                break;
            }
        }

        if expressions.len() == 0 {
            self.next = save;
            return Err(ParseError::Err);
        }

        let line_number = match self.eat_token(Token::RightBrace) {
            Some(n) => n,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        Ok((line_number, Expression::Block(expressions)))
    }
    // let OBJECTID : TYPE [<- EXPRESSION] [, OBJECTID:TYPE [<- EXPRESSION]]* in EXPRESSION
    fn parse_let(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::Let) {
            return Err(ParseError::Err);
        }

        let mut bindings = Vec::new();

        loop {
            let object_ident = match self.eat_object_ident() {
                Some(ident) => ident,
                None => {
                    self.next = save;
                    return Err(ParseError::Err);
                }
            };

            if let None = self.eat_token(Token::Colon) {
                self.next = save;
                return Err(ParseError::Err);
            }

            let object_type = match self.eat_type_ident() {
                Some(result) => result,
                None => {
                    self.next = save;
                    return Err(ParseError::Err);
                }
            };

            let init = match self.eat_token(Token::Assign) {
                Some(_) => match self.parse_expression() {
                    Ok(result) => Some(Box::new(result.1)),
                    Err(e) => {
                        self.next = save;
                        return Err(e);
                    }
                },
                None => None,
            };

            bindings.push((object_ident, object_type, init));

            if let None = self.eat_token(Token::Comma) {
                if let None = self.eat_token(Token::In) {
                    self.next = save;
                    return Err(ParseError::Err);
                }
                break;
            }
        }

        let (line_number, mut expr) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
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

        Ok((line_number, expr))
    }
    // case EXPRESSION of [OBJECTID:TYPE => EXPRESSION;]+ esac
    fn parse_case(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::Case) {
            return Err(ParseError::Err);
        }

        let (_, condition) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        if let None = self.eat_token(Token::Of) {
            self.next = save;
            return Err(ParseError::Err);
        }

        let mut branches = Vec::new();

        let line_number = loop {
            let ident = match self.eat_object_ident() {
                Some(ident) => ident,
                None => {
                    self.next = save;
                    return Err(ParseError::Err);
                }
            };

            if let None = self.eat_token(Token::Colon) {
                self.next = save;
                return Err(ParseError::Err);
            }

            let (_, ident_type) = match self.eat_type_ident() {
                Some(result) => result,
                None => {
                    self.next = save;
                    return Err(ParseError::Err);
                }
            };

            if let None = self.eat_token(Token::DArrow) {
                self.next = save;
                return Err(ParseError::Err);
            }

            let (_, do_expr) = match self.parse_expression() {
                Ok(result) => result,
                Err(e) => {
                    self.next = save;
                    return Err(e);
                }
            };

            branches.push(Branch {
                ident,
                ident_type,
                do_expr,
            });

            if let None = self.eat_token(Token::SemiColon) {
                if let Some(line_number) = self.eat_token(Token::Esac) {
                    break line_number;
                }
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        Ok((
            line_number,
            Expression::Case {
                condition: Box::new(condition),
                branches,
            },
        ))
    }
    // new TYPE
    fn parse_new(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::New) {
            return Err(ParseError::Err);
        }

        let (line_number, ident) = match self.eat_type_ident() {
            Some(ident) => ident,
            None => {
                self.next = save;
                return Err(ParseError::Err);
            }
        };

        Ok((line_number, Expression::New(ident)))
    }
    // isvoid EXPRESSION
    fn parse_isvoid(&mut self) -> Result<Expression> {
        let save = self.next;

        if let None = self.eat_token(Token::IsVoid) {
            return Err(ParseError::Err);
        }

        let (line_number, expr) = match self.parse_expression() {
            Ok(result) => result,
            Err(e) => {
                self.next = save;
                return Err(e);
            }
        };

        Ok((line_number, Expression::IsVoid(Box::new(expr))))
    }

    // INTEGER
    fn parse_integer(&mut self) -> Result<Expression> {
        todo!()
    }
    fn parse_string(&mut self) -> Result<Expression> {
        todo!()
    }
    fn parse_bool(&mut self) -> Result<Expression> {
        todo!()
    }
    fn eat_token(&mut self, token: Token) -> Option<usize> {
        if let Some((line_number, t)) = self.tokens.get(self.next) {
            if *t == token {
                self.next += 1;
                return Some(*line_number);
            }
        }
        None
    }
    fn try_eat_token(&mut self, token: Token) -> bool {
        if let Some((_, t)) = self.tokens.get(self.next) {
            if *t == token {
                return true;
            }
        }
        false
    }
    fn eat_object_ident(&mut self) -> Option<SharedString> {
        if let Some((_, token)) = self.tokens.get(self.next) {
            if let Token::ObjectId(s) = token {
                self.next += 1;
                return Some(s.clone());
            }
        }
        None
    }
    fn eat_type_ident(&mut self) -> Option<(usize, SharedString)> {
        if let Some((line_number, token)) = self.tokens.get(self.next) {
            if let Token::TypeId(s) = token {
                self.next += 1;
                return Some((*line_number, s.clone()));
            }
        }
        None
    }
}
