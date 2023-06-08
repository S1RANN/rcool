use std::error::Error;

use crate::ast::{Branch, Class, Expression, Feature, Formal, Program};
use crate::lexer::Token;
use crate::string_table::SharedString;

struct Parser {
    tokens: Vec<(usize, Token)>,
    pos: usize,
    self_type: SharedString,
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

type Result<T> = std::result::Result<T, ParseError>;

impl Parser {
    fn new<'a>(token_iter: impl Iterator<Item = (usize, Token)> + 'a) -> Parser {
        // if tokens contains SELF_TYPE assign it to the self_type field
        let tokens: Vec<(usize, Token)> = token_iter.collect();
        let self_type = match tokens.iter().find(|(_, token)| match token {
            Token::TypeId(t) if t == "SELF_TYPE" => true,
            _ => false,
        }) {
            Some((_, Token::TypeId(t))) => t.clone(),
            _ => SharedString::new("SELF_TYPE"),
        };
        Parser {
            tokens,
            pos: 0,
            self_type,
        }
    }
    fn peek(&self) -> &Token {
        if self.pos >= self.tokens.len() {
            return &Token::Eof;
        }
        &self.tokens[self.pos].1
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
            rhs = match self.parse_associative_expression(precedence(&lookahead)) {
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

        let line_number = match self.eat_token(Token::Fi) {
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

        let line_number = loop {
            let ident = match self.eat_object_ident() {
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
                if let Some(line_number) = self.eat_token(Token::Esac) {
                    break line_number;
                }
                self.pos = save;
                return Err(ParseError::Err);
            }
        };

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
mod tests{
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_expression(){
        let text = "Let a: String <- 1 + isvoid 2 in not abc * c <- 5 - ~aa / 4 - 1;sdf";
        let iter = Lexer::lex(text);
        let mut parser = Parser::new(iter);
        let result = parser.parse_expression();
        assert_ne!(result, Err(ParseError::Err));
    }
}