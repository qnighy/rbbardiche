use bstr::BString;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum Node {
    Ident { name: String },
    Nil,
    Errored,
}

pub fn parse(source: &[u8]) -> Node {
    let mut parser = Parser::new(source);
    parser.parse()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
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

    fn parse(&mut self) -> Node {
        let token = self.next_token();
        match &token.kind {
            TokenKind::Ident(name) => {
                if name == "nil" {
                    Node::Nil
                } else {
                    Node::Ident {
                        name: name.to_string(),
                    }
                }
            }
            TokenKind::InvalidPunct(_) => {
                self.errors.push(ParseError {});
                Node::Errored
            }
            TokenKind::Eof => {
                self.errors.push(ParseError {});
                Node::Errored
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.pos >= self.source.len() {
            return Token {
                kind: TokenKind::Eof,
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
            };
        } else {
            self.pos += 1;
            return Token {
                kind: TokenKind::InvalidPunct(first),
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
