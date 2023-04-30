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
    Error,
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
    Unknown(char),
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
            _ => Token::Unknown(c),
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
    pub fn new(text: String) -> Lexer {
        Lexer {
            text,
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
                    return Some((line_number, token));
                }
                no_comment = false;
            }
            if no_space && no_comment {
                break;
            }
        }
        self.match_all_keywords()
            .or_else(|| self.match_bool())
            .or_else(|| self.match_int())
            .or_else(|| self.match_string())
            .or_else(|| self.match_operator())
            .map(|(pos, line_number, token)| {
                self.pos = pos;
                self.line_number = line_number;
                (line_number, token)
            })
    }
    fn match_all_keywords(&self) -> Option<(usize, usize, Token)> {
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
    fn match_keyword(&self, word: &str, token: Token) -> Option<(usize, usize, Token)> {
        self.match_word_ignore_case(word)
            .map(|pos| (pos, self.line_number, token))
    }
    fn match_operator(&self) -> Option<(usize, usize, Token)> {
        self.match_multi_char_operator()
            .or_else(|| self.match_single_char_operator())
    }
    fn match_int(&self) -> Option<(usize, usize, Token)> {
        todo!()
    }
    fn match_type_identifier(&self) -> Option<(usize, usize, Token)> {
        todo!()
    }
    fn match_object_identifier(&self) -> Option<(usize, usize, Token)> {
        todo!()
    }
    fn match_white_space(&self) -> Option<(usize, usize)> {
        todo!()
    }
    fn match_string(&self) -> Option<(usize, usize, Token)> {
        todo!()
    }
    fn match_comment(&self) -> Option<(usize, usize, Option<Token>)> {
        todo!()
    }
    fn match_single_char_operator(&self) -> Option<(usize, usize, Token)> {
        let mut text_chars = self.text.chars().skip(self.pos);
        match text_chars.next() {
            Some(
                c @ ('.' | '*' | '@' | '~' | '/' | '+' | '-' | '<' | '=' | '{' | '}' | '(' | ')'
                | ':' | ';' | ','),
            ) => Some((self.pos + 1, self.line_number, Token::from_char(c))),
            _ => None,
        }
    }
    fn match_multi_char_operator(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("=>", Token::DArrow)
            .or_else(|| self.match_keyword("<-", Token::Assign))
            .or_else(|| self.match_keyword("<=", Token::Le))
    }
    fn match_class(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("class", Token::Class)
    }
    fn match_else(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("else", Token::Else)
    }
    fn match_fi(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("fi", Token::Fi)
    }
    fn match_if(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("if", Token::If)
    }
    fn match_in(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("in", Token::In)
    }
    fn match_inherits(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("inherits", Token::Inherits)
    }
    fn match_let(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("let", Token::Let)
    }
    fn match_loop(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("loop", Token::Loop)
    }
    fn match_pool(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("pool", Token::Pool)
    }
    fn match_then(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("then", Token::Then)
    }
    fn match_while(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("while", Token::While)
    }
    fn match_case(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("case", Token::Case)
    }
    fn match_esac(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("esac", Token::Esac)
    }
    fn match_of(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("of", Token::Of)
    }
    fn match_new(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("new", Token::New)
    }
    fn match_isvoid(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("isvoid", Token::IsVoid)
    }
    fn match_bool(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("true", Token::BoolConst(true))
            .or_else(|| self.match_keyword("false", Token::BoolConst(false)))
    }
    fn match_not(&self) -> Option<(usize, usize, Token)> {
        self.match_keyword("not", Token::Not)
    }
    fn match_word_ignore_case(&self, word: &str) -> Option<usize> {
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
    fn test_match_single_operator() {
        let mut lexer = Lexer::new(".".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Dot);

        lexer.set_text("*".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Asterisk);

        lexer.set_text("@".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::At);

        lexer.set_text("~".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Wave);

        lexer.set_text("/".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Slash);

        lexer.set_text("+".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Plus);

        lexer.set_text("-".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Minus);

        lexer.set_text("<".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Less);

        lexer.set_text("=".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Equal);

        lexer.set_text("{".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::LeftBrace);

        lexer.set_text("}".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::RightBrace);

        lexer.set_text("(".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::LeftParenthesis);

        lexer.set_text(")".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::RightParenthesis);

        lexer.set_text(":".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Colon);

        lexer.set_text(";".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::SemiColon);

        lexer.set_text(",".to_string());
        let (pos, line_number, token) = lexer.match_single_char_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Comma);

        lexer.set_text("aA%".to_string());
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
        let mut lexer = Lexer::new("=> <= <-".to_string());
        // match =>
        let (pos, line_number, token) = lexer.match_multi_char_operator().unwrap();
        assert_eq!(pos, 2);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::DArrow);

        // match <=
        lexer.pos += 3;
        let (pos, line_number, token) = lexer.match_multi_char_operator().unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Le);

        // match <-
        lexer.pos += 3;
        let (pos, line_number, token) = lexer.match_multi_char_operator().unwrap();
        assert_eq!(pos, 8);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Assign);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_multi_char_operator();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_operator() {
        let mut lexer = Lexer::new(". * => <-".to_string());
        // match .
        let (pos, line_number, token) = lexer.match_operator().unwrap();
        assert_eq!(pos, 1);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Dot);

        // match *
        lexer.pos += 2;
        let (pos, line_number, token) = lexer.match_operator().unwrap();
        assert_eq!(pos, 3);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Asterisk);

        // match =>
        lexer.pos += 2;
        let (pos, line_number, token) = lexer.match_operator().unwrap();
        assert_eq!(pos, 6);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::DArrow);

        // match <-
        lexer.pos += 3;
        let (pos, line_number, token) = lexer.match_operator().unwrap();
        assert_eq!(pos, 9);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Assign);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_operator();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_all_keyword() {
        let mut lexer = Lexer::new("class".to_string());
        // match class
        let (pos, line_number, token) = lexer.match_all_keywords().unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Class);

        // match EOF
        lexer.pos += 5;
        let result = lexer.match_all_keywords();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_keyword() {
        let mut lexer = Lexer::new("class".to_string());
        // match class
        let (pos, line_number, token) = lexer.match_keyword("class", Token::Class).unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Class);

        // match EOF
        lexer.pos += 5;
        let result = lexer.match_class();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_class() {
        let mut lexer = Lexer::new("class".to_string());
        // match class
        let (pos, line_number, token) = lexer.match_class().unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Class);

        // match EOF
        lexer.pos += 5;
        let result = lexer.match_class();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_else() {
        let mut lexer = Lexer::new("else".to_string());
        // match else
        let (pos, line_number, token) = lexer.match_else().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Else);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_else();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_fi() {
        let mut lexer = Lexer::new("fi".to_string());
        // match fi
        let (pos, line_number, token) = lexer.match_fi().unwrap();
        assert_eq!(pos, 2);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Fi);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_fi();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_if() {
        let mut lexer = Lexer::new("if".to_string());
        // match if
        let (pos, line_number, token) = lexer.match_if().unwrap();
        assert_eq!(pos, 2);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::If);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_if();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_in() {
        let mut lexer = Lexer::new("in".to_string());
        // match in
        let (pos, line_number, token) = lexer.match_in().unwrap();
        assert_eq!(pos, 2);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::In);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_in();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_inherits() {
        let mut lexer = Lexer::new("inherits".to_string());
        // match inherits
        let (pos, line_number, token) = lexer.match_inherits().unwrap();
        assert_eq!(pos, 8);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Inherits);

        // match EOF
        lexer.pos += 8;
        let result = lexer.match_inherits();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_let() {
        let mut lexer = Lexer::new("let".to_string());
        // match let
        let (pos, line_number, token) = lexer.match_let().unwrap();
        assert_eq!(pos, 3);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Let);

        // match EOF
        lexer.pos += 3;
        let result = lexer.match_let();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_loop() {
        let mut lexer = Lexer::new("loop".to_string());
        // match loop
        let (pos, line_number, token) = lexer.match_loop().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Loop);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_loop();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_pool() {
        let mut lexer = Lexer::new("pool".to_string());
        // match pool
        let (pos, line_number, token) = lexer.match_pool().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Pool);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_pool();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_then() {
        let mut lexer = Lexer::new("then".to_string());
        // match then
        let (pos, line_number, token) = lexer.match_then().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Then);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_then();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_while() {
        let mut lexer = Lexer::new("while".to_string());
        // match while
        let (pos, line_number, token) = lexer.match_while().unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::While);

        // match EOF
        lexer.pos += 5;
        let result = lexer.match_while();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_case() {
        let mut lexer = Lexer::new("case".to_string());
        // match case
        let (pos, line_number, token) = lexer.match_case().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Case);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_case();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_esac() {
        let mut lexer = Lexer::new("esac".to_string());
        // match esac
        let (pos, line_number, token) = lexer.match_esac().unwrap();
        assert_eq!(pos, 4);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Esac);

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_esac();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_of() {
        let mut lexer = Lexer::new("of".to_string());
        // match of
        let (pos, line_number, token) = lexer.match_of().unwrap();
        assert_eq!(pos, 2);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Of);

        // match EOF
        lexer.pos += 2;
        let result = lexer.match_of();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_new() {
        let mut lexer = Lexer::new("new".to_string());
        // match new
        let (pos, line_number, token) = lexer.match_new().unwrap();
        assert_eq!(pos, 3);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::New);

        // match EOF
        lexer.pos += 3;
        let result = lexer.match_new();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_isvoid() {
        let mut lexer = Lexer::new("isvoid".to_string());
        // match isvoid
        let (pos, line_number, token) = lexer.match_isvoid().unwrap();
        assert_eq!(pos, 6);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::IsVoid);

        // match EOF
        lexer.pos += 6;
        let result = lexer.match_isvoid();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_bool() {
        let mut lexer = Lexer::new("false true".to_string());
        // match false
        let (pos, line_number, token) = lexer.match_bool().unwrap();
        assert_eq!(pos, 5);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::BoolConst(false));

        // match true
        lexer.pos += 6;
        let (pos, line_number, token) = lexer.match_bool().unwrap();
        assert_eq!(pos, 10);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::BoolConst(true));

        // match EOF
        lexer.pos += 4;
        let result = lexer.match_bool();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_not() {
        let mut lexer = Lexer::new("not".to_string());
        // match not
        let (pos, line_number, token) = lexer.match_not().unwrap();
        assert_eq!(pos, 3);
        assert_eq!(line_number, 1);
        assert_eq!(token, Token::Not);

        // match EOF
        lexer.pos += 3;
        let result = lexer.match_not();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_word_ignore_case() {
        let mut lexer = Lexer::new("woRd LaTex sOme".to_string());
        // match word
        let pos = lexer.match_word_ignore_case("word").unwrap();
        assert_eq!(pos, 4);

        // match latex
        lexer.pos += 5;
        let pos = lexer.match_word_ignore_case("latex").unwrap();
        assert_eq!(pos, 10);

        // match same
        lexer.pos += 6;
        let result = lexer.match_word_ignore_case("same");
        assert_eq!(result, None);
    }
}
