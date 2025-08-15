use crate::ast::Pos;
use crate::err;
use crate::err::Source;
use crate::err::syntax_error;

pub fn lex(source: &Source) -> Result<Vec<Token>, err::E> {
    match Lexer::new(&source.text).lex() {
        Ok(tokens) => Ok(tokens),
        Err(mut errors) => Err(errors.remove(0)),
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token {
    pub ty: TokenType,
    pub pos: Pos,
}

impl Token {
    pub fn new(ty: TokenType, l: usize, r: usize) -> Token {
        Token {
            ty,
            pos: Pos::new(l, r),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Func,
    Ampersand, // &
    Bang,
    BangEq,
    Char,
    Colon,
    ColonEq,
    Comma,
    Dedent,
    Dot,
    EOF,
    Else,
    Eq,
    EqEq,
    For,
    Geq,
    Gt,
    Identifier,
    If,
    Indent,
    Lbrace,
    Leq,
    Lparen,
    Lt,
    Minus,
    MinusEq,
    Newline,
    Number,
    Percent,
    Plus,
    PlusEq,
    Rbrace,
    Rbracket,
    Return,
    Rparen,
    Semicolon,
    Slash,
    SlashComment,
    SlashEq,
    Star,
    StarEq,
    Str,
    Struct,
    Tab,
    While,
    Ellipsis,

    True, // true
    False, // false
}

struct Lexer<'src> {
    src: &'src str,
    cur: std::str::CharIndices<'src>,
    tokens: Vec<Token>,
    errors: Vec<err::E>,
    /// Unbalanced parentheses are detected in the lexer.
    parens: Vec<Pos>,
}

impl Lexer<'_> {
    fn new(src: &str) -> Lexer<'_> {
        Lexer {
            src,
            cur: src.char_indices(),
            tokens: Vec::new(),
            parens: Vec::new(),
            errors: Vec::new(),
        }
    }
    fn peek(&mut self) -> Option<char> {
        self.cur.clone().next().map(|(_, c)| c)
    }
    fn peek_next(&mut self) -> Option<char> {
        self.cur.clone().nth(1).map(|(_, c)| c)
    }
    fn advance(&mut self, n: usize) -> usize {
        self.cur.nth(n - 1).unwrap().0
    }
    fn pos(&mut self) -> usize {
        self.cur.clone().next().unwrap().0
    }
    fn add_tok(&mut self, kind: TokenType, n: usize) {
        let prev = self.pos();
        let now = self.advance(n);
        self.tokens.push(Token::new(kind, prev, now + 1));
    }
    fn lex(mut self) -> Result<Vec<Token>, Vec<err::E>> {
        while self.peek().is_some() && self.get_tok() {}
        if self.errors.is_empty() {
            let eof_pos = 0;
            self.tokens
                .push(Token::new(TokenType::EOF, eof_pos, eof_pos));
            return Ok(self.tokens);
        }
        Err(self.errors)
    }
    fn get_tok(&mut self) -> bool {
        let ch = match self.peek() {
            None => return false,
            Some(ch) => ch,
        };
        match ch {
            ',' => self.add_tok(TokenType::Comma, 1),
            '.' => {
                let length = 1 + self
                    .cur
                    .clone()
                    .skip(1)
                    .take_while(|(_, c)| *c == '.')
                    .count();
                if length == 3 {
                    self.add_tok(TokenType::Ellipsis, 3);
                } else {
                    self.add_tok(TokenType::Dot, 1);
                }
            }
            '\t' => {
                self.advance(1);
            }
            '(' => {
                let p = self.pos();
                self.parens.push(Pos::new(p, p + 1));
                self.add_tok(TokenType::Lparen, 1)
            }
            ')' => {
                if self.parens.is_empty() {
                    let pos = self.pos();
                    self.errors.push(syntax_error!(
                        "Unmatched ')'".to_string(),
                        Pos::new(pos, pos + 1)
                    ));
                } else {
                    self.parens.pop();
                }
                self.add_tok(TokenType::Rparen, 1)
            }
            ';' => self.add_tok(TokenType::Semicolon, 1),
            '&' => self.add_tok(TokenType::Ampersand, 1), // TODO
            '{' => self.add_tok(TokenType::Lbrace, 1),
            '}' => self.add_tok(TokenType::Rbrace, 1),
            ']' => self.add_tok(TokenType::Rbracket, 1),
            '%' => self.add_tok(TokenType::Percent, 1),
            '!' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::BangEq, 2),
                _ => self.add_tok(TokenType::Bang, 1),
            },
            '<' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::Leq, 2),
                _ => self.add_tok(TokenType::Lt, 1),
            },
            '>' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::Geq, 2),
                _ => self.add_tok(TokenType::Gt, 1),
            },
            '=' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::EqEq, 2),
                _ => self.add_tok(TokenType::Eq, 1),
            },
            '+' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::PlusEq, 2),
                _ => self.add_tok(TokenType::Plus, 1),
            },
            ':' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::ColonEq, 2),
                _ => self.add_tok(TokenType::Colon, 1),
            },
            '-' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::MinusEq, 2),
                _ => self.add_tok(TokenType::Minus, 1),
            },
            '*' => match self.peek_next() {
                Some('=') => self.add_tok(TokenType::StarEq, 2),
                _ => self.add_tok(TokenType::Star, 1),
            },
            '/' => match self.peek_next() {
                Some('/') => {
                    self.advance(self.cur.clone().take_while(|(_, c)| *c != '\n').count());
                }
                Some('=') => self.add_tok(TokenType::SlashEq, 2),
                _ => self.add_tok(TokenType::Slash, 1),
            },
            '0'..='9' => self.add_tok(
                TokenType::Number,
                self.cur
                    .clone()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .count(),
            ),
            '"' => {
                let length = 1 + self
                    .cur
                    .clone()
                    .skip(1)
                    .take_while(|(_, c)| !matches!(c, '"' | '\n'))
                    .count();
                if self.cur.clone().nth(length).map(|(_, c)| c) == Some('\n') {
                    let err_pos = Pos::new(self.pos(), self.pos() + length);
                    self.errors.push(syntax_error!(
                        "unterminated string literal".to_string(),
                        err_pos
                    ));
                }
                self.add_tok(TokenType::Str, length + 1)
            }
            '\'' => {
                let length = 1 + self
                    .cur
                    .clone()
                    .skip(1)
                    .take_while(|(_, c)| !matches!(c, '\'' | '\n'))
                    .count();
                if self.cur.clone().nth(length).map(|(_, c)| c) == Some('\n') {
                    let err_pos = Pos::new(self.pos(), self.pos() + length);
                    self.errors.push(syntax_error!(
                        "unterminated char literal".to_string(),
                        err_pos
                    ));
                }
                self.add_tok(TokenType::Char, length + 1)
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let start = self.pos();
                let end = 1 + self
                    .cur
                    .clone()
                    .take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                    .last()
                    .unwrap()
                    .0;
                let kind = match &self.src[start..end] {
                    "struct" => TokenType::Struct,
                    "func" => TokenType::Func,
                    "else" => TokenType::Else,
                    "for" => TokenType::For,
                    "if" => TokenType::If,
                    "return" => TokenType::Return,
                    "while" => TokenType::While,
		    "true" => TokenType::True,
		    "false" => TokenType::False,
                    _ => TokenType::Identifier,
                };
                self.add_tok(kind, end - start)
            }
            ' ' => {
                self.advance(1);
            }
            '\n' => {
                self.advance(1);
                // self.add_tok(TokenType::Newline, 1);
            }
            r => {
                let err_pos = Pos::new(self.pos(), self.pos() + 1);
                self.errors.push(syntax_error!(
                    format!("unrecognised character {r}"),
                    err_pos
                ));
                return false;
            }
        };
        true
    }
}
