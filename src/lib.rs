use std::{borrow::Cow, iter::Peekable};

use bstr::{BStr, ByteSlice, Chars};

pub struct Lexer<'a> {
    buffer: Peekable<Chars<'a>>,
    original: &'a BStr,
    pos: usize,
}

pub struct SpannedLexer<'a> {
    inner: Lexer<'a>,
}

impl<'a> SpannedLexer<'a> {
    pub fn new(s: &'a [u8]) -> Self {
        Self {
            inner:Lexer::new(s)
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a [u8]) -> Self {
        let bs: &BStr = s.into();
        Self {
            buffer: bs.chars().peekable(),
            original: bs,
            pos: 0,
        }
    }

    pub fn current_pos(&self) -> usize {
        self.pos
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.buffer.next()?;
        if c as u32 == 0xFFFD {
            self.pos += 1;
        } else {
            self.pos += c.len_utf8();
        }
        Some(c)
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        let next = if self.pos == 0 && self.eat('#') {
            if self.eat('!') {
                return Some(self.comment());
            } else {
                '#'
            }
        } else {
            self.skip_whitespace();
            let next = self.next_char()?;
            next
        };
        if next.is_digit(10) {
            return Some(self.numeral(next));
        }
        if next == '"' || next == '\'' {
            return Some(self.literal_string(next));
        }

        if next.is_ascii_alphabetic() || next == '_' {
            return Some(self.name(next));
        }

        Some(self.punct(next))
    }

    
    fn next_spanned(&mut self) -> Option<Item<'a>> {
        let (next, start) = if self.pos == 0 && self.eat('#') {
            if self.eat('!') {
                let token = self.comment();
                return Some(Item::new(token, 0, self.pos));
            } else {
                ('#', 0)
            }
        } else {
            self.skip_whitespace();
            let start = self.pos;
            let next = self.next_char()?;
            (next, start)
        };
        if next.is_digit(10) {
            let token = self.numeral(next);
            return Some(Item::new(token, start, self.pos));
        }
        if next == '"' || next == '\'' {
            let token = self.literal_string(next);
            return Some(Item::new(token, start, self.pos));
        }

        if next.is_ascii_alphabetic() || next == '_' {
            let token = self.name(next);
            return Some(Item::new(token, start, self.pos));
        }
        let token = self.punct(next);
        Some(Item::new(token, start, self.pos))
    }

    fn numeral(&mut self, c: char) -> Token<'a> {
        let start = self.pos - 1;
        if c == '0' && self.eat_ascii_ignore_case('x') {
            while self.at_digit(16) {
                let _ = self.next_char();
            }
            if self.eat('.') {
                while self.at_digit(16) {
                    let _ = self.next_char();
                }
            }
            if self.eat_ascii_ignore_case('e') {
                let _ = self.eat('-') || self.eat('+');
                while self.at_digit(16) {
                    let _ = self.next_char();
                }
            }
            if self.eat_ascii_ignore_case('p') {
                let _ = self.eat('-') || self.eat('+');
                while self.at_digit(16) {
                    let _ = self.next_char();
                }
            }
        } else {
            while self.at_digit(10) {
                let _ = self.next_char();
            }
            if self.eat('.') {
                while self.at_digit(10) {
                    let _ = self.next_char();
                }
            }
            if self.eat_ascii_ignore_case('e') {
                let _ = self.eat('-') || self.eat('+');
                while self.at_digit(10) {
                    let _ = self.next_char();
                }
            }
        }
        Token::numeral(&self.original[start..self.pos])
    }

    fn literal_string(&mut self, c: char) -> Token<'a> {
        let start = self.pos - 1;

        if c == '=' || c == '[' {
            let mut eq_ct = 0;
            while self.eat('=') {
                eq_ct += 1;
            }
            self.seek_long_string_end(eq_ct);
        } else if c == '"' || c == '\'' {
            let mut escaped = false;
            while !self.at(c) || escaped {
                if let Some(ch) = self.next_char() {
                    escaped = ch == '\\' && !escaped;
                } else {
                    return Token::Unknown(Cow::Borrowed(&self.original[start..self.pos]));
                }
            }
            self.eat(c);
        }
        Token::LiteralString(Cow::Borrowed(&self.original[start..self.pos]))
    }

    fn seek_long_string_end(&mut self, eq_ct: usize) {
        'retry: loop {
            while !self.eat(']') && !self.at_end() {
                let _ = self.next_char();
            }
            let mut found_eq = 0;
            while found_eq < eq_ct {
                if self.eat('=') {
                    found_eq += 1;
                } else {
                    continue 'retry;
                }
            }
            if self.eat(']') {
                return;
            }
        }
    }

    fn name(&mut self, c: char) -> Token<'a> {
        let start = self.pos - 1;
        match c {
            'a' => {
                if self.eat('n') {
                    if self.eat('d') {
                        if self.at_whitespace() {
                            return Token::Keyword(Keyword::And);
                        }
                    }
                }
            }
            'b' => {
                if self.eat('r') {
                    if self.eat('e') {
                        if self.eat('a') {
                            if self.eat('k') {
                                if self.at_whitespace() {
                                    return Token::Keyword(Keyword::Break);
                                }
                            }
                        }
                    }
                }
            }
            'd' => {
                if self.eat('o') {
                    if self.at_whitespace() {
                        return Token::Keyword(Keyword::Do);
                    }
                }
            }
            'e' => {
                if self.eat('l') {
                    if self.eat('s') {
                        if self.eat('e') {
                            if self.at_whitespace() {
                                return Token::Keyword(Keyword::Else);
                            } else if self.eat('i') {
                                if self.eat('f') {
                                    return Token::Keyword(Keyword::ElseIf);
                                }
                            }
                        }
                    }
                } else if self.eat('n') {
                    if self.eat('d') {
                        if self.at_whitespace() {
                            return Token::Keyword(Keyword::End);
                        }
                    }
                }
            }
            'f' => {
                if self.eat('a') {
                    if self.eat('l') {
                        if self.eat('s') {
                            if self.eat('e') {
                                if self.at_whitespace() {
                                    return Token::Keyword(Keyword::False);
                                }
                            }
                        }
                    }
                } else if self.eat('o') {
                    if self.eat('r') {
                        if self.at_whitespace() {
                            return Token::Keyword(Keyword::For);
                        }
                    }
                } else if self.eat('u') {
                    if self.eat('n') {
                        if self.eat('c') {
                            if self.eat('t') {
                                if self.eat('i') {
                                    if self.eat('o') {
                                        if self.eat('n') {
                                            if self.at_whitespace() {
                                                return Token::Keyword(Keyword::Function);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            'g' => {
                if self.eat('o') {
                    if self.eat('t') {
                        if self.eat('o') {
                            if self.at_whitespace() {
                                return Token::Keyword(Keyword::GoTo);
                            }
                        }
                    }
                }
            }
            'i' => {
                if self.eat('n') {
                    if self.at_whitespace() {
                        return Token::Keyword(Keyword::In);
                    }
                } else if self.eat('f') {
                    if self.at_whitespace() {
                        return Token::Keyword(Keyword::If);
                    }
                }
            }
            'l' => {
                if self.eat('o') {
                    if self.eat('c') {
                        if self.eat('a') {
                            if self.eat('l') {
                                if self.at_whitespace() {
                                    return Token::Keyword(Keyword::Local);
                                }
                            }
                        }
                    }
                }
            }
            'n' => {
                if self.eat('i') {
                    if self.eat('l') {
                        if self.at_whitespace() {
                            return Token::Keyword(Keyword::Nil);
                        }
                    }
                } else if self.eat('o') {
                    if self.eat('t') {
                        if self.at_whitespace() {
                            return Token::Keyword(Keyword::Not);
                        }
                    }
                }
            }
            'o' => {
                if self.eat('r') {
                    if self.at_whitespace() {
                        return Token::Keyword(Keyword::Or);
                    }
                }
            }
            'r' => {
                if self.eat('e') {
                    if self.eat('p') {
                        if self.eat('e') {
                            if self.eat('a') {
                                if self.eat('t') {
                                    if self.at_whitespace() {
                                        return Token::Keyword(Keyword::Repeat);
                                    }
                                }
                            }
                        }
                    } else if self.eat('t') {
                        if self.eat('u') {
                            if self.eat('r') {
                                if self.eat('n') {
                                    if self.at_whitespace() {
                                        return Token::Keyword(Keyword::Return);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            't' => {
                if self.eat('h') {
                    if self.eat('e') {
                        if self.eat('n') {
                            if self.at_whitespace() {
                                return Token::Keyword(Keyword::Then);
                            }
                        }
                    }
                } else if self.eat('r') {
                    if self.eat('u') {
                        if self.eat('e') {
                            if self.at_whitespace() {
                                return Token::Keyword(Keyword::True);
                            }
                        }
                    }
                }
            }
            'u' => {
                if self.eat('n') {
                    if self.eat('t') {
                        if self.eat('i') {
                            if self.eat('l') {
                                if self.at_whitespace() {
                                    return Token::Keyword(Keyword::Until);
                                }
                            }
                        }
                    }
                }
            }
            'w' => {
                if self.eat('h') {
                    if self.eat('i') {
                        if self.eat('l') {
                            if self.eat('e') {
                                if self.at_whitespace() {
                                    return Token::Keyword(Keyword::While);
                                }
                            }
                        }
                    }
                }
            }
            _ => {
                if !c.is_ascii_alphanumeric() && c != '_' {
                    return Token::Unknown(Cow::Borrowed(&self.original[start..self.pos]));
                }
            }
        }
        while self.at_name_cont() {
            let _ = self.next_char();
        }
        Token::name(&self.original[start..self.pos])
    }

    fn punct(&mut self, c: char) -> Token<'a> {
        let p = match c {
            '+' => Punct::Plus,
            '-' => {
                if self.eat('-') {
                    return self.comment();
                } else {
                    Punct::Minus
                }
            }
            '*' => Punct::Asterisk,
            '%' => Punct::Percent,
            '^' => Punct::Caret,
            '#' => Punct::Hash,
            '&' => Punct::Ampersand,
            '~' => {
                if self.eat('=') {
                    Punct::TildeEqual
                } else {
                    Punct::Tilde
                }
            }
            '|' => Punct::Pipe,
            '(' => Punct::OpenParen,
            ')' => Punct::CloseParen,
            '{' => Punct::OpenBrace,
            '}' => Punct::CloseBrace,
            ';' => Punct::SemiColon,
            ',' => Punct::Comma,
            ']' => Punct::CloseBracket,
            '.' => {
                if self.eat('.') {
                    if self.eat('.') {
                        Punct::Ellipsis
                    } else {
                        Punct::DoubleDot
                    }
                } else {
                    Punct::Dot
                }
            }
            ':' => {
                if self.eat(':') {
                    Punct::DoubleColon
                } else {
                    Punct::Colon
                }
            }
            '/' => {
                if self.eat('/') {
                    Punct::DoubleForwardSlash
                } else {
                    Punct::ForwardSlash
                }
            }
            '=' => {
                if self.eat('=') {
                    Punct::DoubleEqual
                } else {
                    Punct::Equal
                }
            }
            '<' => {
                if self.eat('<') {
                    Punct::DoubleLessThan
                } else if self.eat('=') {
                    Punct::LessThanEqual
                } else {
                    Punct::LessThan
                }
            }
            '>' => {
                if self.eat('>') {
                    Punct::DoubleGreaterThan
                } else if self.eat('=') {
                    Punct::GreaterThanEqual
                } else {
                    Punct::GreaterThan
                }
            }
            '[' => {
                if self.at('[') {
                    return self.literal_string('[');
                } else if self.at('=') {
                    return self.literal_string('=');
                } else {
                    Punct::OpenBracket
                }
            }
            _ => return Token::Unknown(Cow::Borrowed(&self.original[self.pos - 1..self.pos])),
        };
        Token::Punct(p)
    }

    fn comment(&mut self) -> Token<'a> {
        let start = self.pos - 2;
        if self.eat('[') && self.eat('[') {
            while !self.at_end() {
                if self.eat(']') {
                    if self.eat(']') {
                        break;
                    }
                } else {
                    let _ = self.next_char();
                }
            }
            let _ = self.next_char();
        } else {
            while !self.at('\n') && !self.at_end() {
                let _ = self.next_char();
            }
        }
        Token::comment(&self.original[start..self.pos])
    }

    fn at(&mut self, c: char) -> bool {
        if let Some(ch) = self.buffer.peek() {
            *ch == c
        } else {
            false
        }
    }

    fn eat(&mut self, c: char) -> bool {
        if let Some(ch) = self.buffer.peek() {
            if *ch == c {
                let _ = self.next_char();
                return true;
            }
        }
        false
    }

    fn eat_ascii_ignore_case(&mut self, c: char) -> bool {
        if let Some(ch) = self.buffer.peek() {
            if ch.eq_ignore_ascii_case(&c) {
                let _ = self.next_char();
                return true;
            }
        }
        false
    }

    fn at_whitespace(&mut self) -> bool {
        if let Some(ch) = self.buffer.peek() {
            if ch.is_ascii_whitespace() {
                return true;
            }
        }
        false
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.buffer.peek() {
            if ch.is_ascii_whitespace() {
                let _ = self.next_char();
            } else {
                break;
            }
        }
    }

    fn at_name_cont(&mut self) -> bool {
        if let Some(ch) = self.buffer.peek() {
            ch.is_ascii_alphanumeric() || *ch == '_'
        } else {
            false
        }
    }

    fn at_digit(&mut self, radix: u32) -> bool {
        if let Some(ch) = self.buffer.peek() {
            ch.is_digit(radix)
        } else {
            false
        }
    }

    fn at_end(&mut self) -> bool {
        self.buffer.peek().is_none()
    }
}

impl<'a> std::iter::Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> std::iter::Iterator for SpannedLexer<'a> {
    type Item = Item<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next_spanned()
    }
}

pub struct Item<'a> {
    pub token: Token<'a>,
    pub span: Span,
}

pub struct Span {
    pub start: usize,
    pub end: usize,
}


impl<'a> Item<'a> {
    pub fn new(token: Token<'a>, start: usize, end: usize) -> Self {
        Self {
            token,
            span: Span  {
                start,
                end,
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token<'a> {
    Name(Cow<'a, str>),
    Numeral(Cow<'a, str>),
    LiteralString(Cow<'a, BStr>),
    Punct(Punct),
    Keyword(Keyword),
    Comment(Cow<'a, BStr>),
    Unknown(Cow<'a, BStr>),
}

impl<'a> Token<'a> {
    pub fn name(s: &'a BStr) -> Self {
        Self::Name(s.to_str_lossy())
    }
    pub fn numeral(s: &'a BStr) -> Self {
        Self::Numeral(s.to_str_lossy())
    }
    pub fn literal_string(s: &'a BStr) -> Self {
        Self::LiteralString(Cow::Borrowed(s))
    }
    pub fn comment(s: &'a BStr) -> Self {
        Self::Comment(Cow::Borrowed(s))
    }
}
#[derive(Debug, Eq, PartialEq)]
pub enum Punct {
    Ampersand,
    Asterisk,
    Caret,
    CloseBrace,
    CloseBracket,
    CloseParen,
    Colon,
    Comma,
    Dot,
    DoubleColon,
    DoubleDot,
    DoubleEqual,
    DoubleForwardSlash,
    DoubleGreaterThan,
    DoubleLessThan,
    Ellipsis,
    Equal,
    ForwardSlash,
    GreaterThan,
    GreaterThanEqual,
    Hash,
    LessThan,
    LessThanEqual,
    Minus,
    OpenBrace,
    OpenBracket,
    OpenParen,
    Percent,
    Pipe,
    Plus,
    SemiColon,
    Tilde,
    TildeEqual,
}

impl std::str::FromStr for Punct {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let p = match s {
            "&" => Punct::Ampersand,
            "*" => Punct::Asterisk,
            "^" => Punct::Caret,
            "}" => Punct::CloseBrace,
            "]" => Punct::CloseBracket,
            ")" => Punct::CloseParen,
            ":" => Punct::Colon,
            "," => Punct::Comma,
            "." => Punct::Dot,
            "::" => Punct::DoubleColon,
            ".." => Punct::DoubleDot,
            "==" => Punct::DoubleEqual,
            "//" => Punct::DoubleForwardSlash,
            ">>" => Punct::DoubleGreaterThan,
            "<<" => Punct::DoubleLessThan,
            "..." => Punct::Ellipsis,
            "=" => Punct::Equal,
            "/" => Punct::ForwardSlash,
            ">" => Punct::GreaterThan,
            ">=" => Punct::GreaterThanEqual,
            "#" => Punct::Hash,
            "<" => Punct::LessThan,
            "<=" => Punct::LessThanEqual,
            "-" => Punct::Minus,
            "{" => Punct::OpenBrace,
            "[" => Punct::OpenBracket,
            "(" => Punct::OpenParen,
            "%" => Punct::Percent,
            "|" => Punct::Pipe,
            "+" => Punct::Plus,
            ";" => Punct::SemiColon,
            "~" => Punct::Tilde,
            "~=" => Punct::TildeEqual,
            _ => return Err(format!("Invalid punct: {:?}", s)),
        };
        Ok(p)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    And,
    Or,
    Function,
    Local,
    For,
    Do,
    End,
    Nil,
    True,
    False,
    Not,
    Return,
    In,
    While,
    GoTo,
    Break,
    Repeat,
    Then,
    ElseIf,
    Else,
    Until,
    If,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn numerals() {
        let strings = &[
            "3",
            "345",
            "0xff",
            "0xBEBADA",
            "3.0",
            "3.1416",
            "314.16e-2",
            "0.31416E1",
            "34e1",
            "0x0.1E",
            "0xA23p-4",
            "0X1.921FB54442D18P+1",
        ];
        for &string in strings {
            let mut t = Lexer::new(string.as_bytes());
            assert_eq!(
                t.next_token().unwrap(),
                Token::Numeral(Cow::Borrowed(string.into()))
            )
        }
    }

    #[test]
    fn puncts() {
        let strings = &[
            "+", "-", "*", "/", "//", "^", "%", "&", "~", "|", ">>", "<<", ".", "..", "...", "=",
            "<", "<=", ">", ">=", "==", "~=", "#", "~", "(", ")", "[", "]", "{", "}", ",", ":",
            "::", ";",
        ];
        for &string in strings {
            println!("testing {:?}", string);
            let mut t = Lexer::new(string.as_bytes());
            assert_eq!(
                t.next_token().unwrap(),
                Token::Punct(string.parse().unwrap())
            )
        }
        let mut t = Lexer::new(b"$");
        assert_eq!(
            t.next_token().unwrap(),
            Token::Unknown(Cow::Borrowed("$".into())),
        )
    }

    #[test]
    fn literal_string() {
        let strings = &[
            r#"'alo\n123"'"#,
            r#""alo\n123\"""#,
            r#"[[alo
123"]]"#,
            r#"[==[
alo
123"]==]"#,
            r#"[==[
alo
123"]=]==]"#,
        ];
        for (i, &s) in strings.iter().enumerate() {
            println!("{} testing {:?}", i, s);
            let mut t = Lexer::new(s.as_bytes());
            assert_eq!(
                t.next_token().unwrap(),
                Token::LiteralString(Cow::Borrowed(s.into()))
            );
        }
    }

    #[test]
    fn comments() {
        let strings = &[
            "#!hashbang",
            "--single line comment",
            "--[[multi
            line comment]]",
        ];
        for &s in strings {
            let mut t = Lexer::new(s.as_bytes());
            assert_eq!(
                t.next_token().unwrap(),
                Token::Comment(Cow::Borrowed(s.into()))
            )
        }
    }
}
