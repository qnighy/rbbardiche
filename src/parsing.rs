use crate::ast::{Expr, ExprKind, Range};
use bstr::BString;

pub fn parse(source: &[u8]) -> Expr {
    let mut parser = Parser::new(source);
    parser.parse_stmt()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident(BString),
    Equal,
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

    fn parse_stmt(&mut self) -> Expr {
        let mut e = self.parse_expr();
        loop {
            let token = self.next_token();
            match &token.kind {
                TokenKind::Eof => break,
                TokenKind::Equal => {
                    let rhs = self.parse_expr();
                    let range = e.range | rhs.range;
                    e = Expr {
                        kind: ExprKind::Assign {
                            lhs: Box::new(e),
                            rhs: Box::new(rhs),
                        },
                        range,
                        node_id: 0,
                    };
                }
                _ => {
                    // rollback
                    self.pos = token.range.0;
                }
            }
        }
        e
    }

    fn parse_expr(&mut self) -> Expr {
        let token = self.next_token();
        match &token.kind {
            TokenKind::Ident(name) => {
                if name == "nil" {
                    Expr {
                        kind: ExprKind::Nil,
                        range: token.range,
                        node_id: 0,
                    }
                } else {
                    Expr {
                        kind: ExprKind::Ident {
                            name: name.to_string(),
                        },
                        range: token.range,
                        node_id: 0,
                    }
                }
            }
            TokenKind::Equal | TokenKind::InvalidPunct(_) => {
                self.errors.push(ParseError {});
                Expr {
                    kind: ExprKind::Errored,
                    range: token.range,
                    node_id: 0,
                }
            }
            TokenKind::Eof => {
                self.errors.push(ParseError {});
                Expr {
                    kind: ExprKind::Errored,
                    range: token.range,
                    node_id: 0,
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
                kind: match first {
                    b'=' => TokenKind::Equal,
                    _ => TokenKind::InvalidPunct(first),
                },
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
