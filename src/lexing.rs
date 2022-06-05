use crate::ast::{BinaryOp, Range, UnaryOp};
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

impl Token {
    pub(crate) fn to_binop<F>(&self, cond: F) -> Option<BinaryOp>
    where
        F: FnOnce(BinaryOp) -> bool,
    {
        match &self.kind {
            TokenKind::BinOp(op) if cond(*op) => Some(*op),
            _ => None,
        }
    }

    pub(crate) fn to_unop<F>(&self, cond: F) -> Option<UnaryOp>
    where
        F: FnOnce(UnaryOp) -> bool,
    {
        match &self.kind {
            TokenKind::UnOp(op) if cond(*op) => Some(*op),
            _ => None,
        }
    }
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
    /// Binary operator
    /// Note: some operators have overloaded meanings.
    BinOp(BinaryOp),
    /// Unary operator
    /// Note: some operators have overloaded meanings.
    UnOp(UnaryOp),
    /// `?`
    Question,
    /// `:`
    Colon,
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
        let kind = match first {
            _ if first.is_ascii_alphabetic() || first == b'_' || first >= 0x80 => {
                // TODO: support __END__
                let start = self.pos;
                while self.pos < self.source.len() && {
                    let ch = self.source[self.pos];
                    ch.is_ascii_alphanumeric() || first == b'_' || first >= 0x80
                } {
                    self.pos += 1;
                }
                let ident = self.source[start..self.pos].as_bstr();
                if let Some(kind) = KEYWORDS.get(ident) {
                    kind.clone()
                } else {
                    TokenKind::Ident(ident.to_owned())
                }
            }
            b'*' => {
                self.pos += 1;
                if self.next() == Some(b'*') {
                    self.pos += 1;
                    if beg {
                        todo!("** as tDStar");
                    } else {
                        TokenKind::BinOp(BinaryOp::Pow)
                    }
                } else if self.next() == Some(b'=') {
                    todo!("*=");
                } else {
                    // TODO: spcarg condition
                    if beg {
                        todo!("* as tStar");
                    } else {
                        TokenKind::BinOp(BinaryOp::Mul)
                    }
                }
            }
            b'!' => {
                self.pos += 1;
                // TODO: after_operator condition
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::NEq)
                } else if self.next() == Some(b'~') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::NMatch)
                } else {
                    TokenKind::UnOp(UnaryOp::Not)
                }
            }
            b'=' => {
                self.pos += 1;
                // TODO: "=begin" comment
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        self.pos += 1;
                        TokenKind::BinOp(BinaryOp::Eqq)
                    } else {
                        TokenKind::BinOp(BinaryOp::Eq)
                    }
                } else if self.next() == Some(b'~') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::Match)
                } else if self.next() == Some(b'>') {
                    todo!("=>");
                } else {
                    TokenKind::Equal
                }
            }
            b'<' => {
                self.pos += 1;
                // TODO: heredoc
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    if self.next() == Some(b'>') {
                        self.pos += 1;
                        TokenKind::BinOp(BinaryOp::Cmp)
                    } else {
                        TokenKind::BinOp(BinaryOp::LtEq)
                    }
                } else if self.next() == Some(b'<') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        todo!("<<=");
                    } else {
                        TokenKind::BinOp(BinaryOp::LShift)
                    }
                } else {
                    TokenKind::BinOp(BinaryOp::Lt)
                }
            }
            b'>' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::GtEq)
                } else if self.next() == Some(b'>') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::RShift)
                } else {
                    TokenKind::BinOp(BinaryOp::Gt)
                }
            }
            b'?' => {
                self.pos += 1;
                // TODO: end_any condition
                match self.next() {
                    None => {
                        // On EOF, this is reported as "incomplete character syntax" in CRuby
                        // Here we emit '?' instead, and it will lead to syntax error anyway.
                        TokenKind::Question
                    }
                    Some(ch) if ch.is_ascii_whitespace() || ch == b'\x0B' => TokenKind::Question,
                    Some(b'\\') => {
                        todo!("character syntax with escapes")
                    }
                    Some(_ch) => {
                        // TODO: fallback to Question token in case like `cond ?foo : bar`
                        todo!("character syntax")
                    }
                }
            }
            b'&' => {
                self.pos += 1;
                if self.next() == Some(b'&') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        todo!("&&=");
                    } else {
                        TokenKind::BinOp(BinaryOp::LogicalAnd)
                    }
                } else if self.next() == Some(b'=') {
                    self.pos += 1;
                    todo!("&=");
                } else if self.next() == Some(b'.') {
                    self.pos += 1;
                    todo!("&.");
                // TODO: spcarg condition
                } else if beg {
                    todo!("& as tAMPER");
                } else {
                    TokenKind::BinOp(BinaryOp::BitwiseAnd)
                }
            }
            b'|' => {
                self.pos += 1;
                if self.next() == Some(b'|') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        self.pos += 1;
                        todo!("||=");
                    } else if beg {
                        // Split `||` into two `|`s
                        self.pos = start + 1;
                        TokenKind::BinOp(BinaryOp::BitwiseOr)
                    } else {
                        TokenKind::BinOp(BinaryOp::LogicalOr)
                    }
                } else if self.next() == Some(b'=') {
                    self.pos += 1;
                    todo!("|=");
                } else {
                    TokenKind::BinOp(BinaryOp::BitwiseOr)
                }
            }
            b'+' => {
                self.pos += 1;
                // TODO: after_operator condition
                if self.next() == Some(b'=') {
                    todo!("+=");
                }
                // TODO: spcarg condition
                if beg {
                    if self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                        self.pos = start;
                        self.lex_numeric()
                    } else {
                        TokenKind::UnOp(UnaryOp::Plus)
                    }
                } else {
                    TokenKind::BinOp(BinaryOp::Add)
                }
            }
            b'-' => {
                self.pos += 1;
                // TODO: after_operator condition
                if self.next() == Some(b'=') {
                    todo!("-=");
                }
                if self.next() == Some(b'>') {
                    todo!("->");
                }
                // TODO: spcarg condition
                if beg {
                    if self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                        self.pos = start;
                        self.lex_numeric()
                    } else {
                        TokenKind::UnOp(UnaryOp::Neg)
                    }
                } else {
                    TokenKind::BinOp(BinaryOp::Sub)
                }
            }
            b'.' => {
                self.pos += 1;
                if self.next() == Some(b'.') {
                    self.pos += 1;
                    if self.next() == Some(b'.') {
                        self.pos += 1;
                        // TODO: in_argdef condition
                        // TODO: EOF warning
                        // TODO: lpar_beg condition
                        if beg {
                            TokenKind::UnOp(UnaryOp::RangeExcl)
                        } else {
                            TokenKind::BinOp(BinaryOp::RangeExcl)
                        }
                    } else if beg {
                        TokenKind::UnOp(UnaryOp::RangeIncl)
                    } else {
                        TokenKind::BinOp(BinaryOp::RangeIncl)
                    }
                } else {
                    // TODO: digit case
                    todo!(".");
                }
            }
            _ if first.is_ascii_digit() => self.lex_numeric(),
            b':' => {
                self.pos += 1;
                if self.next() == Some(b':') {
                    self.pos += 1;
                    todo!("::");
                    // TODO: end_any condition
                } else if self
                    .next()
                    .is_some_and_(|&ch| ch.is_ascii_whitespace() || ch == b'\x0B' || ch == b'#')
                {
                    TokenKind::Colon
                } else {
                    todo!("tSYMBEG");
                }
            }
            b'/' => {
                self.pos += 1;
                if beg {
                    todo!("regexp");
                }
                if self.next() == Some(b'=') {
                    todo!("/=");
                }
                // TODO: spcarg condition
                TokenKind::BinOp(BinaryOp::Div)
            }
            b'^' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    todo!("^=");
                } else {
                    TokenKind::BinOp(BinaryOp::BitwiseXor)
                }
            }
            b'~' => {
                self.pos += 1;
                // TODO: after_operator condition
                TokenKind::UnOp(UnaryOp::BitwiseNot)
            }
            b'%' => {
                self.pos += 1;
                if beg {
                    todo!("%q()");
                }
                if self.next() == Some(b'=') {
                    todo!("%=");
                }
                // TODO: spcarg condition
                // TODO: fitem condition
                TokenKind::BinOp(BinaryOp::Mod)
            }
            _ => {
                self.pos += 1;
                match first {
                    b';' => TokenKind::Semi,
                    b'\n' => TokenKind::NewLine,
                    _ => TokenKind::InvalidPunct(first),
                }
            }
        };
        Token {
            kind,
            range: Range(start, self.pos),
        }
    }

    fn lex_numeric(&mut self) -> TokenKind {
        let start = self.pos;
        if self.next() == Some(b'-') || self.next() == Some(b'+') {
            self.pos += 1;
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
                _ => TokenKind::Numeric(0),
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
            TokenKind::Numeric(numval)
        } else {
            todo!();
        }
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
            } else if ch.is_ascii_whitespace() || ch == b'\x0B' {
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
