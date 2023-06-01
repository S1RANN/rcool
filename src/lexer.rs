use std::fmt::Display;

use crate::string_table::{SharedString, StrTable};

#[derive(PartialEq, Debug, Clone)]
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
    StrConst(SharedString),
    IntConst(SharedString),
    BoolConst(bool),
    TypeId(SharedString),
    ObjectId(SharedString),
    Assign,
    Not,
    LessEqual,
    Error(String),
    Plus,
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
    Eof
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::Class => "CLASS".to_string(),
            Token::Else => "ELSE".to_string(),
            Token::Fi => "FI".to_string(),
            Token::If => "IF".to_string(),
            Token::In => "IN".to_string(),
            Token::Inherits => "INHERITS".to_string(),
            Token::Let => "LET".to_string(),
            Token::Loop => "LOOP".to_string(),
            Token::Pool => "POOL".to_string(),
            Token::Then => "THEN".to_string(),
            Token::While => "WHILE".to_string(),
            Token::Case => "CASE".to_string(),
            Token::Esac => "ESAC".to_string(),
            Token::Of => "OF".to_string(),
            Token::DArrow => "DARROW".to_string(),
            Token::New => "NEW".to_string(),
            Token::IsVoid => "ISVOID".to_string(),
            Token::StrConst(s) => format!(
                "STR_CONST \"{}\"",
                s.replace('\\', "\\\\").replace('\n', "\\n")
            ),
            Token::IntConst(s) => format!("INT_CONST {}", **s),
            Token::BoolConst(value) => format!("BOOL_CONST {value}"),
            Token::TypeId(s) => format!("TYPEID {}", **s),
            Token::ObjectId(s) => format!("OBJECTID {}", **s),
            Token::Assign => "ASSIGN".to_string(),
            Token::Not => "NOT".to_string(),
            Token::LessEqual => "LE".to_string(),
            Token::Error(e) => format!("ERROR \"{e}\""),
            Token::Plus => '+'.to_string(),
            Token::Slash => '/'.to_string(),
            Token::Dash => '-'.to_string(),
            Token::Asterisk => '*'.to_string(),
            Token::Equal => '='.to_string(),
            Token::Less => '<'.to_string(),
            Token::Dot => '.'.to_string(),
            Token::Wave => '~'.to_string(),
            Token::Comma => ','.to_string(),
            Token::SemiColon => ';'.to_string(),
            Token::Colon => ':'.to_string(),
            Token::LeftParenthesis => '('.to_string(),
            Token::RightParenthesis => ')'.to_string(),
            Token::At => '@'.to_string(),
            Token::LeftBrace => '{'.to_string(),
            Token::RightBrace => '}'.to_string(),
            Token::Eof => "<EOF>".to_string(),
        };
        write!(f, "{s}")
    }
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
            '-' => Token::Dash,
            '<' => Token::Less,
            '=' => Token::Equal,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            _ => Token::Error(c.to_string()),
        }
    }

}

pub struct Lexer<'a> {
    text: &'a str,
    pos: usize,
    line_number: usize,
    str_table: StrTable,
    int_table: StrTable,
    id_table: StrTable,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Lexer<'a> {
        Lexer {
            text,
            pos: 0,
            line_number: 1,
            str_table: StrTable::new(),
            int_table: StrTable::new(),
            id_table: StrTable::new(),
        }
    }

    // return (line_number, token) if succeed
    pub fn lex(s: &str) -> impl Iterator<Item = (usize, Token)> + '_ {
        let mut lexer = Lexer::new(s);
        std::iter::from_fn(move || {
            lexer
                .filter_white_space_and_comment()
                .or_else(|| lexer.match_all_keywords())
                .or_else(|| lexer.match_type_identifier())
                .or_else(|| lexer.match_object_identifier())
                .or_else(|| lexer.match_bool_const())
                .or_else(|| lexer.match_int_const())
                .or_else(|| lexer.match_string_const())
                .or_else(|| lexer.match_operator())
                .or_else(|| lexer.catch_unknown_char())
                .map(|token| (lexer.line_number, token))
        })
    }
    fn filter_white_space_and_comment(&mut self) -> Option<Token> {
        loop {
            let no_space = !self.match_white_space();
            let no_comment = match self.match_comment() {
                Ok(matched) => !matched,
                Err(token) => return Some(token),
            };

            if no_space && no_comment {
                break None;
            }
        }
    }
    fn catch_unknown_char(&mut self) -> Option<Token> {
        self.text.chars().nth(self.pos).map(|c| {
            self.pos += 1;
            Token::from_char(c)
        })
    }
    fn match_all_keywords(&mut self) -> Option<Token> {
        self.match_class()
            .or_else(|| self.match_inherits())
            .or_else(|| self.match_isvoid())
            .or_else(|| self.match_while())
            .or_else(|| self.match_else())
            .or_else(|| self.match_loop())
            .or_else(|| self.match_pool())
            .or_else(|| self.match_then())
            .or_else(|| self.match_case())
            .or_else(|| self.match_esac())
            .or_else(|| self.match_new())
            .or_else(|| self.match_not())
            .or_else(|| self.match_let())
            .or_else(|| self.match_fi())
            .or_else(|| self.match_if())
            .or_else(|| self.match_in())
            .or_else(|| self.match_of())
    }
    // return (pos, line_number, Token) if succeed
    fn match_keyword(&mut self, word: &str, token: Token) -> Option<Token> {
        if let Some(pos) = self.try_match_word_ignore_case(word) {
            if let Some(c) = self.text.chars().nth(pos) {
                if c.is_ascii_alphabetic() || c == '_' {
                    None
                } else {
                    self.pos = pos;
                    Some(token)
                }
            } else {
                self.pos = pos;
                Some(token)
            }
        } else {
            None
        }
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
            if idx == self.pos && !c.is_ascii_digit() {
                return None;
            }
            if !c.is_ascii_digit() {
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
            if !c.is_ascii_uppercase() {
                return None;
            }
        } else {
            return None;
        }
        for (idx, c) in text_char_indices {
            if !c.is_ascii_alphabetic() && c != '_' {
                let id = self.id_table.insert(&self.text[self.pos..idx]);
                self.pos = idx;
                return Some(Token::TypeId(id));
            }
        }
        let id = self.id_table.insert(&self.text[self.pos..]);
        self.pos = self.text.len();
        Some(Token::TypeId(id))
    }
    fn match_object_identifier(&mut self) -> Option<Token> {
        let mut text_char_indices = self.text.char_indices().skip(self.pos);
        if let Some((_, c)) = text_char_indices.next() {
            if !c.is_ascii_lowercase() {
                return None;
            }
        } else {
            return None;
        }
        for (idx, c) in text_char_indices {
            if !c.is_ascii_alphabetic() && c != '_' {
                let id = self.id_table.insert(&self.text[self.pos..idx]);
                self.pos = idx;
                return Some(Token::ObjectId(id));
            }
        }
        let id = self.id_table.insert(&self.text[self.pos..]);
        self.pos = self.text.len();
        Some(Token::ObjectId(id))
    }
    // return true when succeed
    fn match_white_space(&mut self) -> bool {
        let old_pos = self.pos;
        let text_chars = self.text.chars().skip(self.pos);
        for c in text_chars {
            match c {
                ' ' | '\t' | '\r' => self.pos += 1,
                '\n' => {
                    self.pos += 1;
                    self.line_number += 1;
                }
                _ => break,
            }
        }
        old_pos != self.pos
    }
    fn match_string_const(&mut self) -> Option<Token> {
        let mut text_char_indices = self.text.char_indices().skip(self.pos).peekable();

        if let Some((_, c)) = text_char_indices.next() {
            if c != '"' {
                return None;
            }
        } else {
            return None;
        }

        let mut chars_to_merge: Vec<(usize, char)> = vec![];

        while let Some((idx, c)) = text_char_indices.next() {
            match c {
                '\n' => {
                    self.pos = idx + 1;
                    self.line_number += 1;
                    return Some(Token::Error("Unterminated string constant".to_string()));
                }
                '\0' => {
                    self.pos = idx + 1;
                    return Some(Token::Error("String contains null character".to_string()));
                }
                '\\' => {
                    if let Some((_, c_next)) = text_char_indices.peek() {
                        match c_next {
                            '\n' => {
                                self.line_number += 1;
                                text_char_indices.next();
                                chars_to_merge.push((idx - self.pos - 1, '\n'));
                            }
                            'n' => {
                                chars_to_merge.push((idx - self.pos - 1, '\n'));
                                text_char_indices.next();
                            }
                            'r' => {
                                chars_to_merge.push((idx - self.pos - 1, '\r'));
                                text_char_indices.next();
                            }
                            't' => {
                                chars_to_merge.push((idx - self.pos - 1, '\t'));
                                text_char_indices.next();
                            }
                            _ => {
                                chars_to_merge.push((idx - self.pos - 1, *c_next));
                                text_char_indices.next();
                            }
                        }
                    }
                }
                '"' => {
                    let matched_string = match self.text.get((self.pos + 1)..idx) {
                        Some(matched) => {
                            let mut matched = matched.to_string();
                            chars_to_merge.reverse();
                            for (pos, char) in chars_to_merge.iter() {
                                matched.replace_range(*pos..=(*pos + 1), char.to_string().as_str());
                            }
                            matched
                        }
                        None => String::from(""),
                    };
                    let id = self.str_table.insert(&matched_string);
                    self.pos = idx + 1;
                    return Some(Token::StrConst(id));
                }
                _ => {}
            }
        }
        self.pos = self.text.len();
        Some(Token::Error("EOF in string constant".to_string()))
    }
    // return Ok(true) when matched; Ok(false) when not matched;
    // Err(token) when comment format error encountered
    fn match_comment(&mut self) -> Result<bool, Token> {
        let first_two = match self.text.get(self.pos..(self.pos + 2)) {
            Some(c) => c,
            None => return Ok(false),
        };

        if first_two == "*)" {
            self.pos += 2;
            return Err(Token::Error("Unmatched *)".to_string()));
        }
        if first_two == "--" {
            if let Some(idx) = self.text.chars().skip(self.pos + 2).position(|c| c == '\n') {
                self.line_number += 1;
                self.pos += idx + 3;
            } else {
                self.pos = self.text.len();
            }
            return Ok(true);
        }
        if first_two != "(*" {
            return Ok(false);
        }

        let mut text_chars_indices = self.text.char_indices().skip(self.pos + 2).peekable();
        let mut comment_depth = 1;
        while let Some((idx, c)) = text_chars_indices.next() {
            if c == '\n' {
                self.line_number += 1;
            } else if c == '*' {
                let c_next = match text_chars_indices.peek() {
                    Some((_, c1)) => *c1,
                    None => break,
                };
                if c_next == ')' {
                    comment_depth -= 1;
                    if comment_depth == 0 {
                        self.pos = idx + 2;
                        return Ok(true);
                    }
                }
            } else if c == '(' {
                let c_next = match text_chars_indices.peek() {
                    Some((_, c1)) => *c1,
                    None => break,
                };
                if c_next == '*' {
                    comment_depth += 1;
                    text_chars_indices.next();
                }
            }
        }
        self.pos = self.text.len();
        Err(Token::Error("EOF in comment".to_string()))
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
            .or_else(|| self.match_keyword("<=", Token::LessEqual))
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
        let text_chars = self.text.char_indices().skip(self.pos);
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
    fn test_lex() {
        let text = r#"(* models one-dimensional cellular automaton on a circle of finite radius
   arrays are faked as Strings,
   X's respresent live cells, dots represent dead cells,
   no error checking is done *)
class CellularAutomaton inherits IO {
    population_map : String;
   
    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
            self;
        }
    };
   
    print() : SELF_TYPE {
        {
            out_string(population_map.concat("\n"));
            self;
        }
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
        population_map.substr(position, 1)
    };
   
    cell_left_neighbor(position : Int) : String {
        if position = 0 then
            cell(num_cells() - 1)
        else
            cell(position - 1)
        fi
    };
   
    cell_right_neighbor(position : Int) : String {
        if position = num_cells() - 1 then
            cell(0)
        else
            cell(position + 1)
        fi
    };
   
    (* a cell will live if exactly 1 of itself and it's immediate
       neighbors are alive *)
    cell_at_next_evolution(position : Int) : String {
        if (if cell(position) = "X" then 1 else 0 fi
            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
            = 1)
        then
            "X"
        else
            '.'
        fi
    };
   
    evolve() : SELF_TYPE {
        (let position : Int in
        (let num : Int <- num_cells[] in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };
};

class Main {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
            cells <- (new CellularAutomaton).init("         X         ");
            cells.print();
            (let countdown : Int <- 20 in
                while countdown > 0 loop
                    {
                        cells.evolve();
                        cells.print();
                        countdown <- countdown - 1;
                    
                pool
            );  (* end let countdown
            self;
        }
    };
};
"#;
        let correct = r#"#5 CLASS
#5 TYPEID CellularAutomaton
#5 INHERITS
#5 TYPEID IO
#5 '{'
#6 OBJECTID population_map
#6 ':'
#6 TYPEID String
#6 ';'
#8 OBJECTID init
#8 '('
#8 OBJECTID map
#8 ':'
#8 TYPEID String
#8 ')'
#8 ':'
#8 TYPEID SELF_TYPE
#8 '{'
#9 '{'
#10 OBJECTID population_map
#10 ASSIGN
#10 OBJECTID map
#10 ';'
#11 OBJECTID self
#11 ';'
#12 '}'
#13 '}'
#13 ';'
#15 OBJECTID print
#15 '('
#15 ')'
#15 ':'
#15 TYPEID SELF_TYPE
#15 '{'
#16 '{'
#17 OBJECTID out_string
#17 '('
#17 OBJECTID population_map
#17 '.'
#17 OBJECTID concat
#17 '('
#17 STR_CONST "\n"
#17 ')'
#17 ')'
#17 ';'
#18 OBJECTID self
#18 ';'
#19 '}'
#20 '}'
#20 ';'
#22 OBJECTID num_cells
#22 '('
#22 ')'
#22 ':'
#22 TYPEID Int
#22 '{'
#23 OBJECTID population_map
#23 '.'
#23 OBJECTID length
#23 '('
#23 ')'
#24 '}'
#24 ';'
#26 OBJECTID cell
#26 '('
#26 OBJECTID position
#26 ':'
#26 TYPEID Int
#26 ')'
#26 ':'
#26 TYPEID String
#26 '{'
#27 OBJECTID population_map
#27 '.'
#27 OBJECTID substr
#27 '('
#27 OBJECTID position
#27 ','
#27 INT_CONST 1
#27 ')'
#28 '}'
#28 ';'
#30 OBJECTID cell_left_neighbor
#30 '('
#30 OBJECTID position
#30 ':'
#30 TYPEID Int
#30 ')'
#30 ':'
#30 TYPEID String
#30 '{'
#31 IF
#31 OBJECTID position
#31 '='
#31 INT_CONST 0
#31 THEN
#32 OBJECTID cell
#32 '('
#32 OBJECTID num_cells
#32 '('
#32 ')'
#32 '-'
#32 INT_CONST 1
#32 ')'
#33 ELSE
#34 OBJECTID cell
#34 '('
#34 OBJECTID position
#34 '-'
#34 INT_CONST 1
#34 ')'
#35 FI
#36 '}'
#36 ';'
#38 OBJECTID cell_right_neighbor
#38 '('
#38 OBJECTID position
#38 ':'
#38 TYPEID Int
#38 ')'
#38 ':'
#38 TYPEID String
#38 '{'
#39 IF
#39 OBJECTID position
#39 '='
#39 OBJECTID num_cells
#39 '('
#39 ')'
#39 '-'
#39 INT_CONST 1
#39 THEN
#40 OBJECTID cell
#40 '('
#40 INT_CONST 0
#40 ')'
#41 ELSE
#42 OBJECTID cell
#42 '('
#42 OBJECTID position
#42 '+'
#42 INT_CONST 1
#42 ')'
#43 FI
#44 '}'
#44 ';'
#48 OBJECTID cell_at_next_evolution
#48 '('
#48 OBJECTID position
#48 ':'
#48 TYPEID Int
#48 ')'
#48 ':'
#48 TYPEID String
#48 '{'
#49 IF
#49 '('
#49 IF
#49 OBJECTID cell
#49 '('
#49 OBJECTID position
#49 ')'
#49 '='
#49 STR_CONST "X"
#49 THEN
#49 INT_CONST 1
#49 ELSE
#49 INT_CONST 0
#49 FI
#50 '+'
#50 IF
#50 OBJECTID cell_left_neighbor
#50 '('
#50 OBJECTID position
#50 ')'
#50 '='
#50 STR_CONST "X"
#50 THEN
#50 INT_CONST 1
#50 ELSE
#50 INT_CONST 0
#50 FI
#51 '+'
#51 IF
#51 OBJECTID cell_right_neighbor
#51 '('
#51 OBJECTID position
#51 ')'
#51 '='
#51 STR_CONST "X"
#51 THEN
#51 INT_CONST 1
#51 ELSE
#51 INT_CONST 0
#51 FI
#52 '='
#52 INT_CONST 1
#52 ')'
#53 THEN
#54 STR_CONST "X"
#55 ELSE
#56 ERROR "'"
#56 '.'
#56 ERROR "'"
#57 FI
#58 '}'
#58 ';'
#60 OBJECTID evolve
#60 '('
#60 ')'
#60 ':'
#60 TYPEID SELF_TYPE
#60 '{'
#61 '('
#61 LET
#61 OBJECTID position
#61 ':'
#61 TYPEID Int
#61 IN
#62 '('
#62 LET
#62 OBJECTID num
#62 ':'
#62 TYPEID Int
#62 ASSIGN
#62 OBJECTID num_cells
#62 ERROR "["
#62 ERROR "]"
#62 IN
#63 '('
#63 LET
#63 OBJECTID temp
#63 ':'
#63 TYPEID String
#63 IN
#64 '{'
#65 WHILE
#65 OBJECTID position
#65 '<'
#65 OBJECTID num
#65 LOOP
#66 '{'
#67 OBJECTID temp
#67 ASSIGN
#67 OBJECTID temp
#67 '.'
#67 OBJECTID concat
#67 '('
#67 OBJECTID cell_at_next_evolution
#67 '('
#67 OBJECTID position
#67 ')'
#67 ')'
#67 ';'
#68 OBJECTID position
#68 ASSIGN
#68 OBJECTID position
#68 '+'
#68 INT_CONST 1
#68 ';'
#69 '}'
#70 POOL
#70 ';'
#71 OBJECTID population_map
#71 ASSIGN
#71 OBJECTID temp
#71 ';'
#72 OBJECTID self
#72 ';'
#73 '}'
#74 ')'
#74 ')'
#74 ')'
#75 '}'
#75 ';'
#76 '}'
#76 ';'
#78 CLASS
#78 TYPEID Main
#78 '{'
#79 OBJECTID cells
#79 ':'
#79 TYPEID CellularAutomaton
#79 ';'
#81 OBJECTID main
#81 '('
#81 ')'
#81 ':'
#81 TYPEID SELF_TYPE
#81 '{'
#82 '{'
#83 OBJECTID cells
#83 ASSIGN
#83 '('
#83 NEW
#83 TYPEID CellularAutomaton
#83 ')'
#83 '.'
#83 OBJECTID init
#83 '('
#83 STR_CONST "         X         "
#83 ')'
#83 ';'
#84 OBJECTID cells
#84 '.'
#84 OBJECTID print
#84 '('
#84 ')'
#84 ';'
#85 '('
#85 LET
#85 OBJECTID countdown
#85 ':'
#85 TYPEID Int
#85 ASSIGN
#85 INT_CONST 20
#85 IN
#86 WHILE
#86 OBJECTID countdown
#86 ERROR ">"
#86 INT_CONST 0
#86 LOOP
#87 '{'
#88 OBJECTID cells
#88 '.'
#88 OBJECTID evolve
#88 '('
#88 ')'
#88 ';'
#89 OBJECTID cells
#89 '.'
#89 OBJECTID print
#89 '('
#89 ')'
#89 ';'
#90 OBJECTID countdown
#90 ASSIGN
#90 OBJECTID countdown
#90 '-'
#90 INT_CONST 1
#90 ';'
#92 POOL
#93 ')'
#93 ';'
#98 ERROR "EOF in comment"
"#;
        let mut result = vec![];
        for (line_number, token) in Lexer::lex(text) {
            result.push(format!("#{line_number} {token}\n"));
        }
        let result = result.concat();
        assert_eq!(result, correct);
    }
    #[test]
    fn test_match_string_const() {
        let mut lexer = Lexer::new("\"some string\"\"\\nthis is \\a string\"\"fasdf");
        if let Token::StrConst(s) = lexer.match_string_const().unwrap() {
            assert_eq!(*s, "some string");
            assert_eq!(lexer.pos, 13);
        } else {
            panic!("Failed to match string const");
        }

        if let Token::StrConst(s) = lexer.match_string_const().unwrap() {
            assert_eq!(*s, "\nthis is a string");
            assert_eq!(lexer.pos, 34);
        } else {
            panic!("Failed to match string const");
        }

        assert_eq!(
            lexer.match_string_const(),
            Some(Token::Error("EOF in string constant".to_string()))
        );

        let mut lexer = Lexer::new("\"asdfsd \\\n\"");
        if let Token::StrConst(s) = lexer.match_string_const().unwrap() {
            assert_eq!(*s, "asdfsd \n");
            assert_eq!(lexer.line_number, 2);
        }

        let mut lexer = Lexer::new("\"asdf\nasdfs");
        assert_eq!(
            lexer.match_string_const(),
            Some(Token::Error("Unterminated string constant".to_string()))
        );
    }
    #[test]
    fn test_filter_white_space_and_comment() {
        let mut lexer =
            Lexer::new("     (*dsfasdf\n\r\tsdf(*(*(*dsfad*)dfasf*)\n\n*)*)      \r\n \r\t");
        assert_eq!(lexer.filter_white_space_and_comment(), None);
        assert_eq!(lexer.pos, lexer.text.len());
        assert_eq!(lexer.line_number, 5);
    }
    #[test]
    fn test_match_comment() {
        let mut lexer = Lexer::new("(*asfddsf*) (*dsaf(*fdaw*)dfsa\n*)");
        assert_eq!(lexer.match_comment(), Ok(true));
        assert_eq!(lexer.pos, 11);

        lexer.pos += 1;
        assert_eq!(lexer.match_comment(), Ok(true));
        assert_eq!(lexer.pos, lexer.text.len());
        assert_eq!(lexer.line_number, 2);

        let mut lexer = Lexer::new("(*dsafsdf");
        assert_eq!(
            lexer.match_comment(),
            Err(Token::Error("EOF in comment".to_string()))
        );
        assert_eq!(lexer.pos, lexer.text.len());

        let mut lexer = Lexer::new("(*ds(*af(*sdf*)*)");
        assert_eq!(
            lexer.match_comment(),
            Err(Token::Error("EOF in comment".to_string()))
        );
        assert_eq!(lexer.pos, lexer.text.len());

        let mut lexer = Lexer::new("*)1234");
        assert_eq!(
            lexer.match_comment(),
            Err(Token::Error("Unmatched *)".to_string()))
        );
        assert_eq!(lexer.pos, 2);

        let mut lexer = Lexer::new("--sdfsdgdgg\n --asdfsdfsfds");
        assert_eq!(lexer.match_comment(), Ok(true));
        assert_eq!(lexer.pos, 12);

        lexer.pos += 1;
        assert_eq!(lexer.match_comment(), Ok(true));
        assert_eq!(lexer.pos, lexer.text.len());
    }
    #[test]
    fn test_match_white_space() {
        let mut lexer = Lexer::new(" \t   \r \n");
        assert_eq!(lexer.match_white_space(), true);
        assert_eq!(lexer.pos, lexer.text.len());
    }
    #[test]
    fn test_match_int_const() {
        let mut lexer = Lexer::new("14335 098342 34214");
        // match 14335
        let mut token = lexer.match_int_const().unwrap();
        if let Token::IntConst(s) = token {
            assert_eq!(*s, "14335");
        } else {
            panic!("Failed to lex int constant");
        }

        // match 098342
        lexer.pos += 1;
        token = lexer.match_int_const().unwrap();
        if let Token::IntConst(s) = token {
            assert_eq!(*s, "098342");
        } else {
            panic!("Failed to lex int constant");
        }

        // match 34214
        lexer.pos += 1;
        token = lexer.match_int_const().unwrap();
        if let Token::IntConst(s) = token {
            assert_eq!(*s, "34214");
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
        if let Token::TypeId(s) = token {
            assert_eq!(*s, "Student");
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
        if let Token::TypeId(s) = token {
            assert_eq!(*s, "Fruit");
        } else {
            panic!("Failed to lex type identifier");
        }

        // match EOF
        result = lexer.match_type_identifier();
        assert_eq!(result, None);
    }
    #[test]
    fn test_match_object_identifier() {
        let mut lexer = Lexer::new("Student apple Fruit test");
        // match Student
        let mut result = lexer.match_object_identifier();
        assert_eq!(result, None);

        // match apple
        lexer.pos += 8;
        let mut token = lexer.match_object_identifier().unwrap();
        if let Token::ObjectId(s) = token {
            assert_eq!(*s, "apple");
        } else {
            panic!("Failed to match object identifier");
        }

        // match Fruit
        lexer.pos += 1;
        result = lexer.match_object_identifier();
        assert_eq!(result, None);

        // match test
        lexer.pos += 6;
        token = lexer.match_object_identifier().unwrap();
        if let Token::ObjectId(s) = token {
            assert_eq!(*s, "test");
        } else {
            panic!("Failed to match object identifier");
        }

        // match EOF
        result = lexer.match_object_identifier();
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
        assert_eq!(token, Token::Dash);

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
        assert_eq!(token, Token::LessEqual);

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
