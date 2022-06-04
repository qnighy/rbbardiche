use std::borrow::Cow;

use crate::ast::{Expr, ExprKind, Range};
use crate::util::OptionPredExt;
use bstr::{BString, ByteSlice};

pub fn parse(source: &[u8]) -> Expr {
    let mut parser = Parser::new(source);
    parser.parse_program()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident(BString),
    // TODO: bigint, float, etc.
    Numeric(i32),
    /// `=`
    Equal,
    /// `;`
    Semi,
    NewLine,
    InvalidPunct(u8),
    Eof,
}

#[derive(Debug, Clone)]
pub struct ParseError {}

#[derive(Debug)]
struct Parser {
    source: BString,
    pos: usize,
    next_token: Token,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(source: &[u8]) -> Parser {
        let mut parser = Parser {
            source: source.into(),
            pos: 0,
            // dummy value
            next_token: Token {
                kind: TokenKind::Eof,
                range: Range(0, 0),
            },
            errors: vec![],
        };
        parser.bump(true);
        parser
    }

    fn parse_program(&mut self) -> Expr {
        let stmts = self.parse_compstmt();
        if stmts.len() == 1 {
            let mut stmts = stmts;
            stmts.pop().unwrap()
        } else {
            Expr {
                kind: ExprKind::Compound { stmts },
                range: Range(0, self.source.len()),
                node_id: 0,
            }
        }
    }

    fn parse_compstmt(&mut self) -> Vec<Expr> {
        while matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
            self.bump(true);
        }
        let mut stmts = Vec::new();
        while match self.next_token.kind {
            TokenKind::Semi | TokenKind::Eof => false,
            _ => true,
        } {
            stmts.push(self.parse_stmt());
            while matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                self.bump(true);
            }
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Expr {
        let mut e = self.parse_expr();
        loop {
            match &self.next_token.kind {
                TokenKind::Equal => {
                    self.bump(true);
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
                _ => break,
            }
        }
        e
    }

    fn parse_expr(&mut self) -> Expr {
        match &self.next_token.kind {
            TokenKind::Ident(name) => {
                let name = name.to_string();
                let token = self.bump(false);
                if name == "nil" {
                    Expr {
                        kind: ExprKind::Nil,
                        range: token.range,
                        node_id: 0,
                    }
                } else {
                    Expr {
                        kind: ExprKind::Ident { name },
                        range: token.range,
                        node_id: 0,
                    }
                }
            }
            TokenKind::Numeric(numval) => {
                let numval = *numval;
                let token = self.bump(false);
                Expr {
                    kind: ExprKind::Numeric { numval },
                    range: token.range,
                    node_id: 0,
                }
            }
            TokenKind::Equal
            | TokenKind::Semi
            | TokenKind::NewLine
            | TokenKind::InvalidPunct(_) => {
                let token = self.bump(false);
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
                    range: self.next_token.range,
                    node_id: 0,
                }
            }
        }
    }

    fn bump(&mut self, beg: bool) -> Token {
        let next_token = self.lex_token(beg);
        std::mem::replace(&mut self.next_token, next_token)
    }

    fn lex_token(&mut self, beg: bool) -> Token {
        self.skip_whitespace(beg);
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
        } else if first.is_ascii_digit() {
            self.lex_numeric()
        } else {
            self.pos += 1;
            return Token {
                kind: match first {
                    b'=' => TokenKind::Equal,
                    b';' => TokenKind::Semi,
                    b'\n' => TokenKind::NewLine,
                    _ => TokenKind::InvalidPunct(first),
                },
                range: Range(start, self.pos),
            };
        }
    }

    fn lex_numeric(&mut self) -> Token {
        let start = self.pos;
        if self.next() == Some(b'-') || self.next() == Some(b'+') {
            todo!("signed number literals");
        }
        if self.next() == Some(b'0') {
            self.pos += 1;
            match self.next() {
                Some(b'x') | Some(b'X') => todo!("hexadecimal number"),
                Some(b'b') | Some(b'B') => todo!("binary number"),
                Some(b'd') | Some(b'D') => todo!("prefixed decimal nuber"),
                Some(b'o') | Some(b'O') | Some(b'_') => todo!("octal number"),
                Some(ch) if ch.is_ascii_digit() => todo!("octal number"),
                Some(b'.') | Some(b'e') | Some(b'E') => todo!("scientific notation"),
                _ => {
                    return Token {
                        kind: TokenKind::Numeric(0),
                        range: Range(start, self.pos),
                    };
                }
            }
        } else if self.next().is_some_and_(|ch| ch.is_ascii_digit()) {
            while self
                .next()
                .is_some_and_(|&ch| ch.is_ascii_digit() || ch == b'_')
            {
                self.pos += 1;
            }
            match self.next() {
                Some(b'.') | Some(b'e') | Some(b'E') => todo!("floating-point number"),
                _ => {}
            }
            let s = self.source[start..self.pos].to_str().unwrap();
            let s = if s.contains("_") {
                Cow::Owned(s.replace("_", ""))
            } else {
                Cow::Borrowed(s)
            };
            let numval = s.parse::<i32>().unwrap_or_else(|_| todo!("large integers"));
            return Token {
                kind: TokenKind::Numeric(numval),
                range: Range(start, self.pos),
            };
        }
        todo!();
    }

    fn next(&self) -> Option<u8> {
        self.source.get(self.pos).copied()
    }

    // fn peek(&self, off: usize) -> Option<u8> {
    //     self.source.get(self.pos + off).copied()
    // }

    fn skip_whitespace(&mut self, beg: bool) {
        while self.pos < self.source.len() {
            let ch = self.source[self.pos];
            if ch == b'\n' && !beg {
                return;
            } else if ch.is_ascii_whitespace() {
                self.pos += 1;
            } else if ch == b'#' {
                self.pos += 1;
                while self.pos < self.source.len() && self.source[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                return;
            }
        }
    }
}
