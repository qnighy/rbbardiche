use crate::ast::{Expr, ExprKind, Range};
use bstr::BString;

pub fn parse(source: &[u8]) -> Expr {
    let mut parser = Parser::new(source);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident(BString),
    InvalidPunct(u8),
    Eof,
}

#[derive(Debug, Clone)]
pub struct ParseError {}

#[derive(Debug)]
struct Parser {
    source: BString,
    pos: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(source: &[u8]) -> Parser {
        Parser {
            source: source.into(),
            pos: 0,
            errors: vec![],
        }
    }

    fn parse(&mut self) -> Expr {
        let token = self.next_token();
        match &token.kind {
            TokenKind::Ident(name) => {
                if name == "nil" {
                    Expr {
                        kind: ExprKind::Nil,
                        range: token.range,
                    }
                } else {
                    Expr {
                        kind: ExprKind::Ident {
                            name: name.to_string(),
                        },
                        range: token.range,
                    }
                }
            }
            TokenKind::InvalidPunct(_) => {
                self.errors.push(ParseError {});
                Expr {
                    kind: ExprKind::Errored,
                    range: token.range,
                }
            }
            TokenKind::Eof => {
                self.errors.push(ParseError {});
                Expr {
                    kind: ExprKind::Errored,
                    range: token.range,
                }
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let start = self.pos;
        if self.pos >= self.source.len() {
            return Token {
                kind: TokenKind::Eof,
                range: Range(start, self.pos),
            };
        }
        let first = self.source[self.pos];
        if first.is_ascii_alphabetic() || first == b'_' || first >= 0x80 {
            let start = self.pos;
            while self.pos < self.source.len() && {
                let ch = self.source[self.pos];
                ch.is_ascii_alphanumeric() || first == b'_' || first >= 0x80
            } {
                self.pos += 1;
            }
            return Token {
                kind: TokenKind::Ident(BString::from(&self.source[start..self.pos])),
                range: Range(start, self.pos),
            };
        } else {
            self.pos += 1;
            return Token {
                kind: TokenKind::InvalidPunct(first),
                range: Range(start, self.pos),
            };
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.source.len() {
            let ch = self.source[self.pos];
            if ch.is_ascii_whitespace() {
                self.pos += 1;
            } else {
                return;
            }
        }
    }
}