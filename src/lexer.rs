use crate::string_table::StringTable;

use super::string_table::StrTable;

#[derive(PartialEq, Debug)]
pub enum Token {
    Class,
    Else,
    Fi,
    If,
    In,
    Inherits,
    Let,
    Loop,
    Pool,
    Then,
    While,
    Case,
    Esac,
    Of,
    DArrow,
    New,
    IsVoid,
    StrConst(usize),
    IntConst(usize),
    BoolConst(bool),
    TypeId(usize),
    ObjectId(usize),
    Assign,
    Not,
    Le,
    Error(String),
    Plus,
    Minus,
    Slash,
    Dash,
    Asterisk,
    Equal,
    Less,
    Dot,
    Wave,
    Comma,
    SemiColon,
    Colon,
    LeftParenthesis,
    RightParenthesis,
    At,
    LeftBrace,
    RightBrace,
}
impl Token {
    fn from_char(c: char) -> Token {
        match c {
            '.' => Token::Dot,
            '*' => Token::Asterisk,
            '@' => Token::At,
            '~' => Token::Wave,
            '/' => Token::Slash,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '<' => Token::Less,
            '=' => Token::Equal,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            _ => Token::Error(format!("Invalid character {c}")),
        }
    }
}
pub struct Lexer {
    text: String,
    pos: usize,
    line_number: usize,
    str_table: StrTable,
    int_table: StrTable,
    id_table: StrTable,
}

impl Lexer {
    pub fn new(text: &str) -> Lexer {
        Lexer {
            text: text.to_string(),
            pos: 0,
            line_number: 1,
            str_table: StrTable::new(),
            int_table: StrTable::new(),
            id_table: StrTable::new(),
        }
    }
    fn set_text(&mut self, text: String) -> &Self {
        self.text = text;
        self.pos = 0;
        self.line_number = 1;
        self
    }
    // return (line_number, token) if succeed
    pub fn lex(&mut self) -> Option<(usize, Token)> {
        self.filter_white_space_and_comment()
            .or_else(|| self.match_all_keywords())
            .or_else(|| self.match_bool_const())
            .or_else(|| self.match_int_const())
            .or_else(|| self.match_string_const())
            .or_else(|| self.match_operator())
            .map(|token| (self.line_number, token))
    }
    fn filter_white_space_and_comment(&mut self) -> Option<Token> {
        loop {
            let mut no_space = true;
            let mut no_comment = true;
            if let Some((pos, line_number)) = self.match_white_space() {
                self.pos = pos;
                self.line_number = line_number;
                no_space = false;
            }
            if let Some((pos, line_number, result)) = self.match_comment() {
                self.pos = pos;
                self.line_number = line_number;
                if let Some(token) = result {
                    return Some(token);
                }
                no_comment = false;
            }
            if no_space && no_comment {
                break None;
            }
        }
    }
    fn match_all_keywords(&mut self) -> Option<Token> {
        self.match_class()
            .or_else(|| self.match_else())
            .or_else(|| self.match_fi())
            .or_else(|| self.match_if())
            .or_else(|| self.match_in())
            .or_else(|| self.match_inherits())
            .or_else(|| self.match_let())
            .or_else(|| self.match_loop())
            .or_else(|| self.match_pool())
            .or_else(|| self.match_then())
            .or_else(|| self.match_while())
            .or_else(|| self.match_case())
            .or_else(|| self.match_esac())
            .or_else(|| self.match_of())
            .or_else(|| self.match_new())
            .or_else(|| self.match_isvoid())
    }
    // return (pos, line_number, Token) if succeed
    fn match_keyword(&mut self, word: &str, token: Token) -> Option<Token> {
        self.try_match_word_ignore_case(word).map(|pos| {
            self.pos = pos;
            token
        })
    }
    fn match_operator(&mut self) -> Option<Token> {
        self.match_multi_char_operator()
            .or_else(|| self.match_single_char_operator())
    }
    fn match_int_const(&mut self) -> Option<Token> {
        if self.pos >= self.text.len() {
            return None;
        }
        let text_char_indices = self.text.char_indices().skip(self.pos);
        for (idx, c) in text_char_indices {
            if idx == self.pos && (c < '0' || c > '9') {
                return None;
            }
            if c < '0' || c > '9' {
                let id = self.int_table.insert(&self.text[self.pos..idx]);
                self.pos = idx;
                return Some(Token::IntConst(id));
            }
        }
        let id = self.int_table.insert(&self.text[self.pos..]);
        self.pos = self.text.len();
        Some(Token::IntConst(id))
    }
    fn match_type_identifier(&mut self) -> Option<Token> {
        let mut text_char_indices = self.text.char_indices().skip(self.pos);
        if let Some((_, c)) = text_char_indices.next() {
            if c < 'A' || c > 'Z' {
                return None;
            }
        } else {
            return None;
        }
        for (idx, c) in text_char_indices {
            if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                let id = self.id_table.insert(&self.text[self.pos..idx]);
                self.pos = idx;
                return Some(Token::TypeId(id));
            }
        }
        let id = self.id_table.insert(&self.text[self.pos..]);
        self.pos = self.text.len();
        Some(Token::TypeId(id))
    }
    fn match_object_identifier(&self) -> Option<Token> {
        todo!()
    }
    fn match_white_space(&self) -> Option<(usize, usize)> {
        todo!()
    }
    fn match_string_const(&mut self) -> Option<Token> {
        todo!()
    }
    fn match_comment(&self) -> Option<(usize, usize, Option<Token>)> {
        todo!()
    }
    fn match_single_char_operator(&mut self) -> Option<Token> {
        let mut text_chars = self.text.chars().skip(self.pos);
        match text_chars.next() {
            Some(
                c @ ('.' | '*' | '@' | '~' | '/' | '+' | '-' | '<' | '=' | '{' | '}' | '(' | ')'
                | ':' | ';' | ','),
            ) => {
                self.pos += 1;
                Some(Token::from_char(c))
            }
            _ => None,
        }
    }
    fn match_multi_char_operator(&mut self) -> Option<Token> {
        self.match_keyword("=>", Token::DArrow)
            .or_else(|| self.match_keyword("<-", Token::Assign))
            .or_else(|| self.match_keyword("<=", Token::Le))
    }
    fn match_class(&mut self) -> Option<Token> {
        self.match_keyword("class", Token::Class)
    }
    fn match_else(&mut self) -> Option<Token> {
        self.match_keyword("else", Token::Else)
    }
    fn match_fi(&mut self) -> Option<Token> {
        self.match_keyword("fi", Token::Fi)
    }
    fn match_if(&mut self) -> Option<Token> {
        self.match_keyword("if", Token::If)
    }
    fn match_in(&mut self) -> Option<Token> {
        self.match_keyword("in", Token::In)
    }
    fn match_inherits(&mut self) -> Option<Token> {
        self.match_keyword("inherits", Token::Inherits)
    }
    fn match_let(&mut self) -> Option<Token> {
        self.match_keyword("let", Token::Let)
    }
    fn match_loop(&mut self) -> Option<Token> {
        self.match_keyword("loop", Token::Loop)
    }
    fn match_pool(&mut self) -> Option<Token> {
        self.match_keyword("pool", Token::Pool)
    }
    fn match_then(&mut self) -> Option<Token> {
        self.match_keyword("then", Token::Then)
    }
    fn match_while(&mut self) -> Option<Token> {
        self.match_keyword("while", Token::While)
    }
    fn match_case(&mut self) -> Option<Token> {
        self.match_keyword("case", Token::Case)
    }
    fn match_esac(&mut self) -> Option<Token> {
        self.match_keyword("esac", Token::Esac)
    }
    fn match_of(&mut self) -> Option<Token> {
        self.match_keyword("of", Token::Of)
    }
    fn match_new(&mut self) -> Option<Token> {
        self.match_keyword("new", Token::New)
    }
    fn match_isvoid(&mut self) -> Option<Token> {
        self.match_keyword("isvoid", Token::IsVoid)
    }
    fn match_bool_const(&mut self) -> Option<Token> {
        self.match_keyword("true", Token::BoolConst(true))
            .or_else(|| self.match_keyword("false", Token::BoolConst(false)))
    }
    fn match_not(&mut self) -> Option<Token> {
        self.match_keyword("not", Token::Not)
    }
    fn try_match_word_ignore_case(&self, word: &str) -> Option<usize> {
        let mut word_chars = word.chars().peekable();
        let mut text_chars = self.text.char_indices().skip(self.pos);
        for (pos, text_char) in text_chars {
            if let Some(&word_char) = word_chars.peek() {
                if word_char.eq_ignore_ascii_case(&text_char) {
                    word_chars.next();
                } else {
                    return None;
                }
            } else {
                return Some(pos);
            }
        }
        if word_chars.peek().is_none() {
            Some(self.pos + word.len())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_match_int_const() {
        let mut lexer = Lexer::new("14335 098342 34214");
        // match 14335
        let mut token = lexer.match_int_const().unwrap();
        if let Token::IntConst(id) = token {
            assert_eq!(lexer.int_table.get(id), Some("14335"));
        } else {
            panic!("Failed to lex int constant");
        }

        // match 098342
        lexer.pos += 1;
        token = lexer.match_int_const().unwrap();
        if let Token::IntConst(id) = token {
            assert_eq!(lexer.int_table.get(id), Some("098342"));
        } else {
            panic!("Failed to lex int constant");
        }

        // match 34214
        lexer.pos += 1;
        token = lexer.match_int_const().unwrap();
        if let Token::IntConst(id) = token {
            assert_eq!(lexer.int_table.get(id), Some("34214"));
        } else {
            panic!("Failed to lex int constant");
        }

        // match EOF
        let result = lexer.match_int_const();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_type_identifier() {
        let mut lexer = Lexer::new("Student apple Fruit");
        // match Student
        let mut token = lexer.match_type_identifier().unwrap();
        if let Token::TypeId(id) = token {
            assert_eq!(lexer.id_table.get(id), Some("Student"));
        } else {
            panic!("Failed to lex type identifier");
        }

        // match apple
        lexer.pos += 1;
        let mut result = lexer.match_type_identifier();
        assert_eq!(result, None);

        // match Fruit
        lexer.pos += 6;
        token = lexer.match_type_identifier().unwrap();
        if let Token::TypeId(id) = token {
            assert_eq!(lexer.id_table.get(id), Some("Fruit"));
        } else {
            panic!("Failed to lex type identifier");
        }

        // match EOF
        result = lexer.match_type_identifier();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_single_operator() {
        let mut lexer = Lexer::new(".*@~/+-<={}():;,aA%");
        // match .
        let mut token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Dot);

        // match *
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Asterisk);

        // match @
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::At);

        // match ~
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Wave);

        // match /
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Slash);

        // match +
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Plus);

        // match -
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Minus);

        // match <
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Less);

        // match =
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Equal);

        // match {
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::LeftBrace);

        // match }
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::RightBrace);

        // match (
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::LeftParenthesis);

        // match )
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::RightParenthesis);

        // match :
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Colon);

        // match ;
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::SemiColon);

        // match ,
        token = lexer.match_single_char_operator().unwrap();
        assert_eq!(token, Token::Comma);

        // match a
        let result = lexer.match_single_char_operator();
        assert_eq!(result, None);

        // match A
        lexer.pos += 1;
        let result = lexer.match_single_char_operator();
        assert_eq!(result, None);

        // match %
        lexer.pos += 1;
        let result = lexer.match_single_char_operator();
        assert_eq!(result, None);

        // match EOF
        lexer.pos += 1;
        let result = lexer.match_single_char_operator();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_multi_char_operator() {
        let mut lexer = Lexer::new("=> <= <-");
        // match =>
        let mut token = lexer.match_multi_char_operator().unwrap();
        assert_eq!(token, Token::DArrow);

        // match <=
        lexer.pos += 1;
        token = lexer.match_multi_char_operator().unwrap();
        assert_eq!(token, Token::Le);

        // match <-
        lexer.pos += 1;
        token = lexer.match_multi_char_operator().unwrap();
        assert_eq!(token, Token::Assign);

        // match EOF
        let result = lexer.match_multi_char_operator();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_operator() {
        let mut lexer = Lexer::new(". * => <-");
        // match .
        let mut token = lexer.match_operator().unwrap();
        assert_eq!(token, Token::Dot);

        // match *
        lexer.pos += 1;
        token = lexer.match_operator().unwrap();
        assert_eq!(token, Token::Asterisk);

        // match =>
        lexer.pos += 1;
        token = lexer.match_operator().unwrap();
        assert_eq!(token, Token::DArrow);

        // match <-
        lexer.pos += 1;
        token = lexer.match_operator().unwrap();
        assert_eq!(token, Token::Assign);

        // match EOF
        let result = lexer.match_operator();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_all_keyword() {
        let mut lexer = Lexer::new("class");
        // match class
        let token = lexer.match_all_keywords().unwrap();
        assert_eq!(token, Token::Class);

        // match EOF
        let result = lexer.match_all_keywords();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_keyword() {
        let mut lexer = Lexer::new("class");
        // match class
        let token = lexer.match_keyword("class", Token::Class).unwrap();
        assert_eq!(token, Token::Class);

        // match EOF
        let result = lexer.match_class();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_class() {
        let mut lexer = Lexer::new("class");
        // match class
        let token = lexer.match_class().unwrap();
        assert_eq!(token, Token::Class);

        // match EOF
        let result = lexer.match_class();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_else() {
        let mut lexer = Lexer::new("else");
        // match else
        let token = lexer.match_else().unwrap();
        assert_eq!(token, Token::Else);

        // match EOF
        let result = lexer.match_else();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_fi() {
        let mut lexer = Lexer::new("fi");
        // match fi
        let token = lexer.match_fi().unwrap();
        assert_eq!(token, Token::Fi);

        // match EOF
        let result = lexer.match_fi();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_if() {
        let mut lexer = Lexer::new("if");
        // match if
        let token = lexer.match_if().unwrap();
        assert_eq!(token, Token::If);

        // match EOF
        let result = lexer.match_if();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_in() {
        let mut lexer = Lexer::new("in");
        // match in
        let token = lexer.match_in().unwrap();
        assert_eq!(token, Token::In);

        // match EOF
        let result = lexer.match_in();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_inherits() {
        let mut lexer = Lexer::new("inherits");
        // match inherits
        let token = lexer.match_inherits().unwrap();
        assert_eq!(token, Token::Inherits);

        // match EOF
        let result = lexer.match_inherits();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_let() {
        let mut lexer = Lexer::new("let");
        // match let
        let token = lexer.match_let().unwrap();
        assert_eq!(token, Token::Let);

        // match EOF
        let result = lexer.match_let();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_loop() {
        let mut lexer = Lexer::new("loop");
        // match loop
        let token = lexer.match_loop().unwrap();
        assert_eq!(token, Token::Loop);

        // match EOF
        let result = lexer.match_loop();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_pool() {
        let mut lexer = Lexer::new("pool");
        // match pool
        let token = lexer.match_pool().unwrap();
        assert_eq!(token, Token::Pool);

        // match EOF
        let result = lexer.match_pool();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_then() {
        let mut lexer = Lexer::new("then");
        // match then
        let token = lexer.match_then().unwrap();
        assert_eq!(token, Token::Then);

        // match EOF
        let result = lexer.match_then();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_while() {
        let mut lexer = Lexer::new("while");
        // match while
        let token = lexer.match_while().unwrap();
        assert_eq!(token, Token::While);

        // match EOF
        let result = lexer.match_while();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_case() {
        let mut lexer = Lexer::new("case");
        // match case
        let token = lexer.match_case().unwrap();
        assert_eq!(token, Token::Case);

        // match EOF
        let result = lexer.match_case();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_esac() {
        let mut lexer = Lexer::new("esac");
        // match esac
        let token = lexer.match_esac().unwrap();
        assert_eq!(token, Token::Esac);

        // match EOF
        let result = lexer.match_esac();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_of() {
        let mut lexer = Lexer::new("of");
        // match of
        let token = lexer.match_of().unwrap();
        assert_eq!(token, Token::Of);

        // match EOF
        let result = lexer.match_of();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_new() {
        let mut lexer = Lexer::new("new");
        // match new
        let token = lexer.match_new().unwrap();
        assert_eq!(token, Token::New);

        // match EOF
        let result = lexer.match_new();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_isvoid() {
        let mut lexer = Lexer::new("isvoid");
        // match isvoid
        let token = lexer.match_isvoid().unwrap();
        assert_eq!(token, Token::IsVoid);

        // match EOF
        let result = lexer.match_isvoid();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_bool_const() {
        let mut lexer = Lexer::new("false true");
        // match false
        let mut token = lexer.match_bool_const().unwrap();
        assert_eq!(token, Token::BoolConst(false));

        // match true
        lexer.pos += 1;
        token = lexer.match_bool_const().unwrap();
        assert_eq!(token, Token::BoolConst(true));

        // match EOF
        let result = lexer.match_bool_const();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_not() {
        let mut lexer = Lexer::new("not");
        // match not
        let token = lexer.match_not().unwrap();
        assert_eq!(token, Token::Not);

        // match EOF
        let result = lexer.match_not();
        assert_eq!(result, None);
    }
    #[test]
    fn test_try_match_word_ignore_case() {
        let mut lexer = Lexer::new("woRd LaTex sOme");
        // match word
        let pos = lexer.try_match_word_ignore_case("word").unwrap();
        assert_eq!(pos, 4);

        // match latex
        lexer.pos += 5;
        let pos = lexer.try_match_word_ignore_case("latex").unwrap();
        assert_eq!(pos, 10);

        // match same
        lexer.pos += 6;
        let result = lexer.try_match_word_ignore_case("same");
        assert_eq!(result, None);
    }
}
