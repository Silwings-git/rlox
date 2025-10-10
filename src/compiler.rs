use std::{iter::Peekable, str::CharIndices};

pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'static str) -> Self {
        Parser {
            scanner: Scanner::init_scanner(source),
        }
    }

    pub fn compile(&mut self) {
        let mut line = 0;
        loop {
            let token = self.scanner.scan_token();
            if token.line != line {
                print!("{:<4}", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }
            println!("{:2?} '{}'\n", token.token_type, token.lexeme);
            if token.token_type == TokenType::Eof {
                break;
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    // Single-character tokens. 单字符词法
    // (
    LeftParen,
    // )
    RightParen,
    // {
    LeftBrace,
    // }
    RightBrace,
    // ,
    Comma,
    // .
    Dot,
    // -
    Minus,
    // +
    Plus,
    // ;
    Semicolon,
    // /
    Slash,
    // *
    Star,
    // One or two character tokens. 一或两字符词法
    // !
    Bang,
    // !=
    BangEqual,
    // =
    Equal,
    // ==
    EqualEqual,
    // >
    Greater,
    // >=
    GreaterEqual,
    // <
    Less,
    // <=
    LessEqual,
    // Literals. 字面量
    Identifier,
    String,
    Number,
    // Keywords. 关键字
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    line: usize,
}

pub struct Scanner<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    // 当前扫描的词素的首位置
    start: usize,
    // 当前扫描的词素的尾位置
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn init_scanner(source: &str) -> Scanner {
        Scanner {
            source,
            iter: source.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Token<'a> {
        // 跳过空白字符, 下一次advance得到的就是非空白字符
        self.skip_whitespace();

        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        match c {
            Some('(') => self.make_token(TokenType::LeftParen),
            Some(')') => self.make_token(TokenType::RightParen),
            Some('{') => self.make_token(TokenType::LeftBrace),
            Some('}') => self.make_token(TokenType::RightBrace),
            Some(';') => self.make_token(TokenType::Semicolon),
            Some(',') => self.make_token(TokenType::Comma),
            Some('.') => self.make_token(TokenType::Dot),
            Some('-') => self.make_token(TokenType::Minus),
            Some('+') => self.make_token(TokenType::Plus),
            Some('/') => self.make_token(TokenType::Slash),
            Some('*') => self.make_token(TokenType::Star),
            Some('!') => {
                if self.match_token('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            Some('=') => {
                if self.match_token('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            Some('<') => {
                if self.match_token('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            Some('>') => {
                if self.match_token('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            Some('"') => self.string(),
            Some(digit) if digit.is_ascii_digit() => self.number(),
            Some(alpha) if Self::is_alpha(alpha) => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    /// 消费标识符并返回标识符类型的token
    fn identifier(&mut self) -> Token<'a> {
        while matches!(self.peek(),Some(&c) if Self::is_alpha(c))
            || matches!(self.peek(),Some(c) if c.is_ascii_digit())
        {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    /// 根据当前词素获得对应的token类型
    fn identifier_type(&self) -> TokenType {
        let s = &self.source[self.start..self.current];
        match s.len() {
            2 => match s {
                "or" => TokenType::Or,
                "if" => TokenType::If,
                _ => TokenType::Identifier,
            },
            3 => match s {
                "and" => TokenType::And,
                "for" => TokenType::For,
                "fun" => TokenType::Fun,
                "nil" => TokenType::Nil,
                "var" => TokenType::Var,
                _ => TokenType::Identifier,
            },
            4 => match s {
                "else" => TokenType::Else,
                "true" => TokenType::True,
                "this" => TokenType::This,
                _ => TokenType::Identifier,
            },
            5 => match s {
                "class" => TokenType::Class,
                "false" => TokenType::False,
                "print" => TokenType::Print,
                "super" => TokenType::Super,
                "while" => TokenType::While,
                _ => TokenType::Identifier,
            },
            6 => match s {
                "return" => TokenType::Return,
                _ => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        }
    }

    fn is_alpha(c: char) -> bool {
        ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
    }

    /// 消费数值并返回数值类型的token
    fn number(&mut self) -> Token<'a> {
        while matches!(self.peek(),Some(c) if c.is_ascii_digit()) {
            self.advance();
        }

        if matches!(self.peek(),Some(d) if *d == '.')
            && matches!(self.peek_next(),Some(d) if d.is_ascii_digit())
        {
            self.advance();
            while matches!(self.peek(),Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    /// 消费字符串并返回字符串类型的token
    fn string(&mut self) -> Token<'a> {
        while let Some(&c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.peek().is_none() {
            return self.error_token("Unterminated string.");
        }

        // 消费末尾的双引号
        self.advance();

        self.make_token(TokenType::String)
    }

    /// 消费所有前置空白字符
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if matches!(self.peek_next(),Some(c) if c =='/') {
                        // 消费注释(//后的内容)直到遇到换行符,但不消费换行符
                        while matches!(self.peek(),Some(pn) if *pn != '\n') && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => {
                    return;
                }
            }
        }
    }

    /// 如果下一个字符和expected匹配,则消费并返回true,否则返回false
    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if matches!(self.peek(),Some(p) if *p == expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// 预览下一个字符
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek().map(|(_, c)| c)
    }

    #[inline]
    fn peek_next(&mut self) -> Option<char> {
        self.iter
            .peek()
            .and_then(|&(pos, _)| self.source[pos..].chars().nth(1))
    }

    /// 消费下一个字符,并返回被消费的字符
    fn advance(&mut self) -> Option<char> {
        let (_, c) = self.iter.next()?;
        self.current += c.len_utf8();
        Some(c)
    }

    /// 判断是否到达源代码末尾
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            token_type: TokenType::Error,
            lexeme: message,
            line: self.line,
        }
    }
}
