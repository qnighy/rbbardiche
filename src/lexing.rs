use crate::ast::{BinaryOp, Range, UnaryOp};
use crate::parser::Parser;
use crate::parser_diagnostics::ParseError;
use crate::token::{IdentType, StringType, Token, TokenKind};
use crate::util::OptionPredExt;
use bstr::{BStr, ByteSlice};
use once_cell::sync::Lazy;
use std::{borrow::Cow, collections::HashMap};

static KEYWORDS: Lazy<HashMap<&BStr, Option<TokenKind>>> = Lazy::new(|| {
    vec![
        ("__ENCODING__", Some(TokenKind::UnderscoreEncodingKeyword)),
        ("__END__", None),
        ("__LINE__", Some(TokenKind::UnderscoreLineKeyword)),
        ("__FILE__", Some(TokenKind::UnderscoreFileKeyword)),
        ("BEGIN", Some(TokenKind::CapitalBeginKeyword)),
        ("END", Some(TokenKind::CapitalEndKeyword)),
        ("alias", Some(TokenKind::AliasKeyword)),
        ("and", Some(TokenKind::AndKeyword)),
        ("begin", Some(TokenKind::BeginKeyword)),
        ("break", Some(TokenKind::BreakKeyword)),
        ("case", Some(TokenKind::CaseKeyword)),
        ("class", Some(TokenKind::ClassKeyword)),
        ("def", Some(TokenKind::DefKeyword)),
        ("defined?", Some(TokenKind::DefinedQKeyword)),
        ("do", None),
        ("else", Some(TokenKind::ElseKeyword)),
        ("elsif", Some(TokenKind::ElsifKeyword)),
        ("end", Some(TokenKind::EndKeyword)),
        ("ensure", Some(TokenKind::EnsureKeyword)),
        ("false", Some(TokenKind::FalseKeyword)),
        ("for", Some(TokenKind::ForKeyword)),
        ("if", None),
        ("in", Some(TokenKind::InKeyword)),
        ("module", Some(TokenKind::ModuleKeyword)),
        ("next", Some(TokenKind::NextKeyword)),
        ("nil", Some(TokenKind::NilKeyword)),
        ("not", Some(TokenKind::NotKeyword)),
        ("or", Some(TokenKind::OrKeyword)),
        ("redo", Some(TokenKind::RedoKeyword)),
        ("rescue", None),
        ("retry", Some(TokenKind::RetryKeyword)),
        ("return", Some(TokenKind::ReturnKeyword)),
        ("self", Some(TokenKind::SelfKeyword)),
        ("super", Some(TokenKind::SuperKeyword)),
        ("then", Some(TokenKind::ThenKeyword)),
        ("true", Some(TokenKind::TrueKeyword)),
        ("undef", Some(TokenKind::UndefKeyword)),
        ("unless", None),
        ("until", None),
        ("when", Some(TokenKind::WhenKeyword)),
        ("while", None),
        ("yield", Some(TokenKind::YieldKeyword)),
    ]
    .into_iter()
    .map(|(k, v)| (k.as_bytes().as_bstr(), v))
    .collect::<HashMap<_, _>>()
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LexerMode {
    Normal(NormalLexerMode),
    String(StringLexerMode),
}

impl LexerMode {
    pub(crate) const BEG: LexerMode = LexerMode::Normal(NormalLexerMode { beg: true });
    pub(crate) const MID: LexerMode = LexerMode::Normal(NormalLexerMode { beg: false });
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NormalLexerMode {
    pub(crate) beg: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StringLexerMode {
    SingleQuoted,
    DoubleQuoted,
}

impl Parser {
    pub(crate) fn bump(&mut self, mode: LexerMode) -> Token {
        let next_token = self.lex_token(mode);
        std::mem::replace(&mut self.next_token, next_token)
    }

    fn lex_token(&mut self, mode: LexerMode) -> Token {
        match mode {
            LexerMode::Normal(mode) => self.lex_token_normal(mode),
            LexerMode::String(mode) => self.lex_token_string(mode),
        }
    }

    fn lex_token_normal(&mut self, mode: NormalLexerMode) -> Token {
        let space_seen = self.skip_whitespace(mode.beg);
        let start = self.pos;
        if self.pos >= self.source.len() {
            return Token {
                kind: TokenKind::Eof,
                range: Range(start, self.pos),
            };
        }
        let first = self.source[self.pos];
        let kind = match first {
            b'\n' => TokenKind::NewLine,
            b'*' => {
                self.pos += 1;
                if self.next() == Some(b'*') {
                    self.pos += 1;
                    if mode.beg {
                        todo!("** as tDStar");
                    } else {
                        TokenKind::BinOp(BinaryOp::Pow)
                    }
                } else if self.next() == Some(b'=') {
                    todo!("*=");
                } else {
                    // TODO: spcarg condition
                    if mode.beg {
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
            b'\'' => {
                self.pos += 1;
                // TODO: check label condition
                TokenKind::StringBeg(StringType::SQuote)
            }
            b'"' => {
                self.pos += 1;
                // TODO: check label condition
                TokenKind::StringBeg(StringType::DQuote)
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
                    TokenKind::AndDot
                // TODO: spcarg condition
                } else if mode.beg {
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
                    } else if mode.beg {
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
                if mode.beg {
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
                if mode.beg {
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
                        if mode.beg {
                            TokenKind::Dot3Beg
                        } else {
                            TokenKind::Dot3Mid
                        }
                    } else if mode.beg {
                        TokenKind::Dot2Beg
                    } else {
                        TokenKind::Dot2Mid
                    }
                } else if self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                    todo!("error handling for digit after dot");
                } else {
                    TokenKind::Dot
                }
            }
            _ if first.is_ascii_digit() => self.lex_numeric(),
            b')' => {
                self.pos += 1;
                TokenKind::RParen
            }
            b':' => {
                self.pos += 1;
                if self.next() == Some(b':') {
                    self.pos += 1;
                    // TODO: EXPR_CLASS and spcarg conditions
                    if mode.beg {
                        TokenKind::DColonBeg
                    } else {
                        TokenKind::DColon
                    }
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
                if mode.beg {
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
            b';' => {
                self.pos += 1;
                TokenKind::Semi
            }
            b',' => {
                self.pos += 1;
                TokenKind::Comma
            }
            b'~' => {
                self.pos += 1;
                // TODO: after_operator condition
                TokenKind::UnOp(UnaryOp::BitwiseNot)
            }
            b'(' => {
                self.pos += 1;
                if mode.beg {
                    TokenKind::LParenBeg
                } else if !space_seen {
                    TokenKind::LParenCall
                } else {
                    todo!("(")
                }
            }
            b'%' => {
                self.pos += 1;
                if mode.beg {
                    todo!("%q()");
                }
                if self.next() == Some(b'=') {
                    todo!("%=");
                }
                // TODO: spcarg condition
                // TODO: fitem condition
                TokenKind::BinOp(BinaryOp::Mod)
            }
            _ if first.is_ascii_alphabetic() || first == b'_' || first >= 0x80 => {
                let start = self.pos;
                while self.pos < self.source.len() && {
                    let ch = self.source[self.pos];
                    ch.is_ascii_alphanumeric() || ch == b'_' || ch >= 0x80
                } {
                    self.pos += 1;
                }
                let ident = self.source[start..self.pos].as_bstr();
                if let Some(kind) = KEYWORDS.get(ident) {
                    if let Some(kind) = kind {
                        kind.clone()
                    } else {
                        match ident.as_bytes() {
                            b"__END__" => todo!("__END__"),
                            b"do" => todo!("do keyword"),
                            b"if" => {
                                if mode.beg {
                                    TokenKind::IfKeyword
                                } else {
                                    TokenKind::IfModifier
                                }
                            }
                            b"rescue" => {
                                if mode.beg {
                                    TokenKind::RescueKeyword
                                } else {
                                    TokenKind::RescueModifier
                                }
                            }
                            b"unless" => {
                                if mode.beg {
                                    TokenKind::UnlessKeyword
                                } else {
                                    TokenKind::UnlessModifier
                                }
                            }
                            b"until" => {
                                if mode.beg {
                                    TokenKind::UntilKeyword
                                } else {
                                    TokenKind::UntilModifier
                                }
                            }
                            b"while" => {
                                if mode.beg {
                                    TokenKind::WhileKeyword
                                } else {
                                    TokenKind::WhileModifier
                                }
                            }
                            _ => unreachable!("invalid special keyword: {:?}", ident),
                        }
                    }
                } else if ident.chars().next().is_some_and_(|ch| ch.is_uppercase()) {
                    // TODO: handle titlecase letters
                    TokenKind::Ident(IdentType::Const, ident.to_owned())
                } else {
                    TokenKind::Ident(IdentType::Ident, ident.to_owned())
                }
            }
            _ => todo!("character {:?}", first as char),
        };
        Token {
            kind,
            range: Range(start, self.pos),
        }
    }

    fn lex_token_string(&mut self, mode: StringLexerMode) -> Token {
        let term = match mode {
            StringLexerMode::SingleQuoted => b'\'',
            StringLexerMode::DoubleQuoted => b'"',
        };
        let start = self.pos;
        if self.next() == Some(term) {
            self.pos += 1;
            return Token {
                kind: TokenKind::StringEnd,
                range: Range(start, self.pos),
            };
        }
        while self.next().is_some_and_(|&ch| ch != term) {
            if self.next() == Some(b'\\') {
                todo!("escapes in string");
            } else if self.next() == Some(b'#') && matches!(mode, StringLexerMode::DoubleQuoted) {
                todo!("# in string");
            }
            self.pos += 1;
        }
        let content = self.source[start..self.pos].as_bstr();
        Token {
            kind: TokenKind::StringContent(content.to_string()),
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

    fn skip_whitespace(&mut self, beg: bool) -> bool {
        let start = self.pos;
        while self.pos < self.source.len() {
            let ch = self.source[self.pos];
            if ch == b'\n' && !beg {
                break;
            } else if ch.is_ascii_whitespace() || ch == b'\x0B' {
                self.pos += 1;
            } else if ch == b'#' {
                self.pos += 1;
                while self.pos < self.source.len() && self.source[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
        start < self.pos
    }
}
