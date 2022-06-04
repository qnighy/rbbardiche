use crate::ast::Range;
use crate::parser::Parser;
use crate::parser_diagnostics::ParseError;
use crate::util::OptionPredExt;
use bstr::{BStr, BString, ByteSlice};
use once_cell::sync::Lazy;
use std::{borrow::Cow, collections::HashMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TokenKind {
    Ident(BString),
    UnderscoreEncodingKeyword,
    UnderscoreLineKeyword,
    UnderscoreFileKeyword,
    CapitalBeginKeyword,
    CapitalEndKeyword,
    AliasKeyword,
    AndKeyword,
    BeginKeyword,
    BreakKeyword,
    CaseKeyword,
    ClassKeyword,
    DefKeyword,
    DefinedQKeyword,
    DoKeyword,
    ElseKeyword,
    ElsifKeyword,
    EndKeyword,
    EnsureKeyword,
    FalseKeyword,
    ForKeyword,
    IfKeyword,
    InKeyword,
    ModuleKeyword,
    NextKeyword,
    NilKeyword,
    NotKeyword,
    OrKeyword,
    RedoKeyword,
    RescueKeyword,
    RetryKeyword,
    ReturnKeyword,
    SelfKeyword,
    SuperKeyword,
    ThenKeyword,
    TrueKeyword,
    UndefKeyword,
    UnlessKeyword,
    UntilKeyword,
    WhenKeyword,
    WhileKeyword,
    YieldKeyword,
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

static KEYWORDS: Lazy<HashMap<&BStr, TokenKind>> = Lazy::new(|| {
    vec![
        ("__ENCODING__", TokenKind::UnderscoreEncodingKeyword),
        ("__LINE__", TokenKind::UnderscoreLineKeyword),
        ("__FILE__", TokenKind::UnderscoreFileKeyword),
        ("BEGIN", TokenKind::CapitalBeginKeyword),
        ("END", TokenKind::CapitalEndKeyword),
        ("alias", TokenKind::AliasKeyword),
        ("and", TokenKind::AndKeyword),
        ("begin", TokenKind::BeginKeyword),
        ("break", TokenKind::BreakKeyword),
        ("case", TokenKind::CaseKeyword),
        ("class", TokenKind::ClassKeyword),
        ("def", TokenKind::DefKeyword),
        ("defined?", TokenKind::DefinedQKeyword),
        ("do", TokenKind::DoKeyword),
        ("else", TokenKind::ElseKeyword),
        ("elsif", TokenKind::ElsifKeyword),
        ("end", TokenKind::EndKeyword),
        ("ensure", TokenKind::EnsureKeyword),
        ("false", TokenKind::FalseKeyword),
        ("for", TokenKind::ForKeyword),
        ("if", TokenKind::IfKeyword),
        ("in", TokenKind::InKeyword),
        ("module", TokenKind::ModuleKeyword),
        ("next", TokenKind::NextKeyword),
        ("nil", TokenKind::NilKeyword),
        ("not", TokenKind::NotKeyword),
        ("or", TokenKind::OrKeyword),
        ("redo", TokenKind::RedoKeyword),
        ("rescue", TokenKind::RescueKeyword),
        ("retry", TokenKind::RetryKeyword),
        ("return", TokenKind::ReturnKeyword),
        ("self", TokenKind::SelfKeyword),
        ("super", TokenKind::SuperKeyword),
        ("then", TokenKind::ThenKeyword),
        ("true", TokenKind::TrueKeyword),
        ("undef", TokenKind::UndefKeyword),
        ("unless", TokenKind::UnlessKeyword),
        ("until", TokenKind::UntilKeyword),
        ("when", TokenKind::WhenKeyword),
        ("while", TokenKind::WhileKeyword),
        ("yield", TokenKind::YieldKeyword),
    ]
    .into_iter()
    .map(|(k, v)| (k.as_bytes().as_bstr(), v))
    .collect::<HashMap<_, _>>()
});

impl Parser {
    pub(crate) fn bump(&mut self, beg: bool) -> Token {
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
            let ident = self.source[start..self.pos].as_bstr();
            let kind = if let Some(kind) = KEYWORDS.get(ident) {
                kind.clone()
            } else {
                TokenKind::Ident(ident.to_owned())
            };
            return Token {
                kind,
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
                Some(b'i') | Some(b'r') => todo!("number suffixes"),
                _ => {}
            }
            let range = Range(start, self.pos);
            let s = self.source[start..self.pos].to_str().unwrap();
            if s.ends_with("_") {
                self.errors.push(ParseError::TrailingUnderscore { range });
            } else if s.contains("__") {
                self.errors.push(ParseError::DoubleUnderscore { range });
            }
            let s = if s.contains("_") {
                Cow::Owned(s.replace("_", ""))
            } else {
                Cow::Borrowed(s)
            };
            let numval = s.parse::<i32>().unwrap_or_else(|_| todo!("large integers"));
            return Token {
                kind: TokenKind::Numeric(numval),
                range,
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
