use std::str::Chars;

use super::string_table::StrTable;

enum Token {
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
            '(' => Token::LeftBrace,
            ')' => Token::RightBrace,
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            _ => Token::Unknown(c),
        }
    }
}
struct Lexer {
    text: String,
    state: LexState,
    pos: usize,
    line_number: usize,
    str_table: StrTable,
    int_table: StrTable,
    id_table: StrTable,
}

enum LexState {
    Initial,
    InString,
    InComment,
}
impl Lexer {
    fn new(text: String) -> Lexer {
        Lexer {
            text,
            state: LexState::Initial,
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
    fn lex(&mut self) -> Option<(usize, Token)> {
        for c in self.text.chars().skip(self.pos) {
            self.pos += 1;
            match self.state {
                LexState::Initial => {
                    match c {
                        '\n' => self.line_number += 1,
                        '\t' | ' ' | '\r' => {}
                        // '.'|'*'|'@'|'~'|'/'|'+'|'-'|'<'|'='|'{'|'}'|'('|')'|':'|';'|',' => return Some((self.line_number, Token::from_char(c)))
                        _ => return Some((self.line_number, Token::from_char(c))),
                    }
                }
                LexState::InString => todo!(),
                LexState::InComment => todo!(),
            }
        }
        None
    }
    // return (pos, line_number, Token) if succeed
    fn match_keyword(&self, word: &str, token: Token) -> Option<(usize, usize, Token)> {
        self.match_word_ignore_case(word)
            .map(|pos| (pos, self.line_number, token))
    }
    fn match_operator(&self) -> Option<(usize, usize, Token)> {
        self.match_single_char_operator()
            .or_else(|| self.match_multi_char_operator())
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
