use crate::ast::{BinaryOp, Range, UnaryOp};
use crate::parser::token::{IdentType, StringType, Token, TokenKind};
use crate::parser::Parser;
use crate::parser_diagnostics::ParseError;
use crate::util::{CharExt, OptionPredExt};
use bstr::{BStr, ByteSlice};
use once_cell::sync::Lazy;
use std::{borrow::Cow, collections::HashMap};

static KEYWORDS: Lazy<HashMap<&BStr, Option<TokenKind>>> = Lazy::new(|| {
    vec![
        ("__ENCODING__", Some(TokenKind::KeywordUnderscoreEncoding)),
        ("__END__", None),
        ("__LINE__", Some(TokenKind::KeywordUnderscoreLine)),
        ("__FILE__", Some(TokenKind::KeywordUnderscoreFile)),
        ("BEGIN", Some(TokenKind::KeywordCapitalBegin)),
        ("END", Some(TokenKind::KeywordCapitalEnd)),
        ("alias", Some(TokenKind::KeywordAlias)),
        ("and", Some(TokenKind::KeywordAnd)),
        ("begin", Some(TokenKind::KeywordBegin)),
        ("break", Some(TokenKind::KeywordBreak)),
        ("case", Some(TokenKind::KeywordCase)),
        ("class", Some(TokenKind::KeywordClass)),
        ("def", Some(TokenKind::KeywordDef)),
        ("defined?", Some(TokenKind::KeywordDefinedQ)),
        ("do", None),
        ("else", Some(TokenKind::KeywordElse)),
        ("elsif", Some(TokenKind::KeywordElsif)),
        ("end", Some(TokenKind::KeywordEnd)),
        ("ensure", Some(TokenKind::KeywordEnsure)),
        ("false", Some(TokenKind::KeywordFalse)),
        ("for", Some(TokenKind::KeywordFor)),
        ("if", None),
        ("in", Some(TokenKind::KeywordIn)),
        ("module", Some(TokenKind::KeywordModule)),
        ("next", Some(TokenKind::KeywordNext)),
        ("nil", Some(TokenKind::KeywordNil)),
        ("not", Some(TokenKind::KeywordNot)),
        ("or", Some(TokenKind::KeywordOr)),
        ("redo", Some(TokenKind::KeywordRedo)),
        ("rescue", None),
        ("retry", Some(TokenKind::KeywordRetry)),
        ("return", Some(TokenKind::KeywordReturn)),
        ("self", Some(TokenKind::KeywordSelf)),
        ("super", Some(TokenKind::KeywordSuper)),
        ("then", Some(TokenKind::KeywordThen)),
        ("true", Some(TokenKind::KeywordTrue)),
        ("undef", Some(TokenKind::KeywordUndef)),
        ("unless", None),
        ("until", None),
        ("when", Some(TokenKind::KeywordWhen)),
        ("while", None),
        ("yield", Some(TokenKind::KeywordYield)),
    ]
    .into_iter()
    .map(|(k, v)| (k.as_bytes().as_bstr(), v))
    .collect::<HashMap<_, _>>()
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) struct LexerParams {
    pub(in crate::parser) mode: LexerMode,
    pub(in crate::parser) in_condition: bool,
    pub(in crate::parser) in_command_args: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum LexerMode {
    /// Beginning of an expression; IS_BEG().
    ///
    /// ## Behavior (example)
    ///
    /// - Newline insignificant
    /// - `+`/`-` are unary
    /// - `*`/`**`/`&` are argument prefixes
    /// - `%` starts a percent literal
    /// - `()` is a parenthesized expression
    /// - `[]` is an array expression
    /// - `{}` is a hash expression
    Begin(LexerBeginMode),
    /// Beginning of the first command argument, if any; IS_ARG()
    /// (excluding those satisfying IS_BEG()).
    ///
    /// ## Conditions
    ///
    /// - After `defined?`, `not`, `super`, `yield`
    /// - After method name
    ///
    /// ## Behavior
    ///
    /// - Newline significant
    /// - When preceded by spaces, the following token parses
    ///   as if it is in the Begin state:
    ///   - `[`, `::`
    ///   - `(` (but the contents will be more restricted)
    ///   - `%`, `**`, `*`, `&`, `+`, `-` (if not followed by spaces)
    ///   - `/=` (if not followed by spaces or `=`)
    /// - Label is allowed
    Arg,
    /// Where identifier is preferred; EXPR_DOT, EXPR_FNAME, or EXPR_FNAME|EXPR_FITEM
    ///
    /// ## Conditions
    ///
    /// - After `.`, `&.`,
    /// - After `::` as an infix operator
    /// - After `:` as a symbol prefix
    /// - Before an argument of `alias` and `undef`
    /// - After `def`
    ///
    /// ## Behavior
    ///
    /// - Behaves similarly to `Begin` (like being newline insignificant)
    /// - `foo=` is allowed (only if EXPR_FNAME)
    /// - Keywords are not recognized (unless between `def` and `.`)
    /// - Operators are treated as identifiers
    /// - `` ` `` represents an operator (= an identifier)
    /// - `+@`, `-@`, `!@`, `~@`, `[]`, `[]=` are parsed
    /// - No heredocs (only if EXPR_DOT)
    SpecialIdent,
    End,
    String(StringLexerMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum LexerBeginMode {
    /// Ordinary beginning state; EXPR_BEG or EXPR_VALUE.
    ///
    /// ## Conditions
    ///
    /// - At the beginning of the program
    /// - After `and`, `begin`, `case`, `do`, `else`, `elsif`,
    ///   `ensure`, `for`, `if`, `in`, `module`, `or`, `then`,
    ///   `unless`, `until`, `when`, or `while`
    /// - After `#` in a string before ivar, cvar, or gvar
    /// - After `#{` as a string interpolation delimiter
    /// - After `<` as a superclass clause
    /// - After `)`, `;`, or `\n` at the end of parameter list
    /// - After `(` after `def`
    /// - After operators `!`, `~`, `**`, `*`, `/`, `%`, `+`, `-`,
    ///   `<<`, `>>`, `&`, `^`, `>`, `>=`, `<`, `<=`, `<=>`, `==`,
    ///   `===`, `!=`, `=~`, `!~`, `&&`, `||`, `..`, `...`, `?`,
    ///   or `:`, but see the following exceptions:
    ///   - EXPR_LABEL is given after `|`
    ///   - `...` is treated differently in arguments
    /// - After `=`, `**=`, `*=`, `/=`, `%=`, `+=`, `-=`, `<<=`,
    ///   `>>=`, `&=`, `^=`, `&&=`, or `||=`
    /// - After `\n` or `;`
    /// - After `=>` except before pattern
    /// - After `::` at the beginning of the expression
    /// - After `{` as a block/lambda delimiter
    Normal,
    /// Beginning of an expression or a labelled argument; EXPR_BEG|EXPR_LABEL or EXPR_ARG|EXPR_LABELED.
    ///
    /// ## Conditions
    ///
    /// - After `if`, `rescue`, `unless`, `until`, or `while` as a modifier
    /// - After `=>`/`in` before pattern
    /// - After another label (`foo:` or `"foo":`)
    /// - After `{` as a hash delimiter
    /// - After `|`, `,`, `(`, or `[`
    ///
    /// ## Behavior
    ///
    /// - Label is allowed
    ///
    Labelable,
    /// Beginning of an optional expression; EXPR_MID
    ///
    /// ## Conditions
    ///
    /// After `break`, `next`, `rescue` or `return`
    ///
    /// ## Behavior
    ///
    /// - Newline significant
    /// - `||` is parsed normally (but the resulting code is usually useless)
    Omittable,
    /// After the class keyword; EXPR_CLASS
    ///
    /// ## Conditions
    ///
    /// After `class`
    ///
    /// ## Behavior
    ///
    /// - no heredocs
    AfterClass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum StringLexerMode {
    SingleQuoted,
    DoubleQuoted,
}

impl Parser {
    pub(in crate::parser) fn bump(&mut self, params: LexerParams) -> Token {
        let next_token = self.lex_token(params);
        std::mem::replace(&mut self.next_token, next_token)
    }

    fn lex_token(&mut self, params: LexerParams) -> Token {
        match params.mode {
            LexerMode::Begin(_) | LexerMode::Arg | LexerMode::End => self.lex_token_normal(params),
            LexerMode::SpecialIdent => self.lex_token_special_ident(params),
            LexerMode::String(mode) => self.lex_token_string(mode),
        }
    }

    fn lex_token_normal(&mut self, params: LexerParams) -> Token {
        let (beg, arg) = match params.mode {
            LexerMode::Begin(_) => (true, false),
            LexerMode::Arg => (false, true),
            LexerMode::SpecialIdent | LexerMode::End => (false, false),
            LexerMode::String(_) => unreachable!(),
        };
        let space_seen = self.skip_whitespace(params.mode);
        // `a.b + c` -> false
        // `a.b +c` -> true
        // `a.b+c` -> false
        let is_begin_like = |next_ch: Option<u8>| {
            beg || (space_seen && arg && next_ch.is_none_or_(|&ch| !ch.isspace()))
        };

        let start = self.pos;

        let is_special_ident = matches!(params.mode, LexerMode::SpecialIdent);

        if self.pos >= self.source.len() {
            return Token {
                kind: TokenKind::Eof,
                range: Range(start, self.pos),
            };
        }
        let first = self.source[self.pos];
        let kind = match first {
            b'\0' | b'\x04' | b'\x1A' => {
                // NUL, ^D, or ^Z
                self.pos = self.source.len();
                return Token {
                    kind: TokenKind::Eof,
                    range: Range(start, start + 1),
                };
            }
            b'\n' => TokenKind::NewLine,
            _ if first.isspace() => unreachable!("unconsumed whitespace"),
            b'#' => unreachable!("unconsumed comment"),
            b'*' => {
                self.pos += 1;
                if self.next() == Some(b'*') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        self.pos += 1;
                        TokenKind::OpAssign(BinaryOp::Pow)
                    } else if is_begin_like(self.next()) {
                        TokenKind::Star2
                    } else {
                        TokenKind::BinOp(BinaryOp::Pow)
                    }
                } else if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::Mul)
                } else {
                    if is_begin_like(self.next()) {
                        TokenKind::Star
                    } else {
                        TokenKind::BinOp(BinaryOp::Mul)
                    }
                }
            }
            b'!' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::NEq)
                } else if self.next() == Some(b'~') {
                    self.pos += 1;
                    TokenKind::BinOp(BinaryOp::NMatch)
                } else {
                    if is_special_ident && self.next() == Some(b'@') {
                        self.pos += 1;
                    }
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
                    self.pos += 1;
                    TokenKind::Assoc
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
                        self.pos += 1;
                        TokenKind::OpAssign(BinaryOp::LShift)
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
                    if self.next() == Some(b'=') {
                        self.pos += 1;
                        TokenKind::OpAssign(BinaryOp::RShift)
                    } else {
                        TokenKind::BinOp(BinaryOp::RShift)
                    }
                } else {
                    TokenKind::BinOp(BinaryOp::Gt)
                }
            }
            b'`' => {
                self.pos += 1;
                if is_special_ident {
                    TokenKind::Ident(IdentType::Op, b"`".as_bstr().to_owned())
                } else {
                    todo!("`")
                }
            }
            b'\'' => {
                self.pos += 1;
                // TODO: check label condition
                TokenKind::StringBegin(StringType::SQuote)
            }
            b'"' => {
                self.pos += 1;
                // TODO: check label condition
                TokenKind::StringBegin(StringType::DQuote)
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
                    Some(ch) if ch.isspace() => TokenKind::Question,
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
                        self.pos += 1;
                        TokenKind::OpAssign(BinaryOp::LogicalAnd)
                    } else {
                        TokenKind::BinOp(BinaryOp::LogicalAnd)
                    }
                } else if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::BitwiseAnd)
                } else if self.next() == Some(b'.') {
                    self.pos += 1;
                    TokenKind::AndDot
                } else if is_begin_like(self.next()) {
                    TokenKind::Amper
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
                        TokenKind::OpAssign(BinaryOp::LogicalOr)
                    } else if beg {
                        // Note: the condition above differs from parse.y
                        // in some broken cases
                        //
                        // - EXPR_CLASS ... `class ||`
                        // - EXPR_ARG|EXPR_LABELED ... `{ foo: || }`
                        // - EXPR_MID ... `return || true`

                        // Split `||` into two `|`s
                        self.pos = start + 1;
                        TokenKind::BinOp(BinaryOp::BitwiseOr)
                    } else {
                        TokenKind::BinOp(BinaryOp::LogicalOr)
                    }
                } else if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::BitwiseOr)
                } else {
                    TokenKind::BinOp(BinaryOp::BitwiseOr)
                }
            }
            b'+' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::Add)
                } else if is_begin_like(self.next()) {
                    if self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                        self.pos = start;
                        self.lex_numeric()
                    } else {
                        TokenKind::UnOp(UnaryOp::Plus)
                    }
                } else {
                    if is_special_ident && self.next() == Some(b'@') {
                        self.pos += 1;
                    }
                    TokenKind::BinOp(BinaryOp::Add)
                }
            }
            b'-' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::Sub)
                } else if self.next() == Some(b'>') {
                    self.pos += 1;
                    TokenKind::Lambda
                } else if is_begin_like(self.next()) {
                    if self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                        self.pos = start;
                        self.lex_numeric()
                    } else {
                        TokenKind::UnOp(UnaryOp::Neg)
                    }
                } else {
                    if is_special_ident && self.next() == Some(b'@') {
                        self.pos += 1;
                    }
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
                            TokenKind::Dot3Prefix
                        } else {
                            TokenKind::Dot3Infix
                        }
                    } else if beg {
                        TokenKind::Dot2Prefix
                    } else {
                        TokenKind::Dot2Infix
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
            b']' => {
                self.pos += 1;
                TokenKind::RBrack
            }
            b'}' => {
                self.pos += 1;
                // TODO: string interpolation
                TokenKind::RBrace
            }
            b':' => {
                self.pos += 1;
                if self.next() == Some(b':') {
                    self.pos += 1;
                    if is_begin_like(None) {
                        TokenKind::Colon2Prefix
                    } else {
                        TokenKind::Colon2Infix
                    }
                    // TODO: end_any condition
                } else if self.next().is_some_and_(|&ch| ch.isspace() || ch == b'#') {
                    TokenKind::Colon
                } else if self.next() == Some(b'"') || self.next() == Some(b'\'') {
                    todo!("tSYMBEG string");
                } else {
                    TokenKind::SymbolBegin
                }
            }
            b'/' => {
                self.pos += 1;
                // Note the complex rule for `/=` vs. regexp:
                // - `/y / +1` ... regexp
                // - `/ y / +1` ... regexp
                // - `/=y / +1` ... regexp
                // - `/= y / +1` ... regexp
                // - `x /y / +1` ... regexp
                // - `x / y / +1` ... operator
                // - `x /=y / +1` ... operator
                // - `x /= y / +1` ... operator
                if self.next() == Some(b'=') && !beg {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::Div)
                } else if is_begin_like(self.next()) {
                    TokenKind::RegExpBegin
                } else {
                    TokenKind::BinOp(BinaryOp::Div)
                }
            }
            b'^' => {
                self.pos += 1;
                if self.next() == Some(b'=') {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::BitwiseXor)
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
                if is_special_ident && self.next() == Some(b'@') {
                    self.pos += 1;
                }
                TokenKind::UnOp(UnaryOp::BitwiseNot)
            }
            b'(' => {
                self.pos += 1;
                if beg {
                    TokenKind::LParenBeg
                } else if !space_seen {
                    TokenKind::LParenCall
                } else if arg {
                    // TODO: include EXPR_END|EXPR_LABEL too
                    TokenKind::LParenArg
                } else {
                    TokenKind::LParenCall
                }
            }
            b'[' => {
                self.pos += 1;
                if is_special_ident && self.next() == Some(b']') {
                    self.pos += 1;
                    if self.next() == Some(b'=') {
                        self.pos += 1;
                    }
                    TokenKind::Ident(
                        IdentType::Op,
                        self.source[start..self.pos].as_bstr().to_owned(),
                    )
                } else if is_begin_like(None) {
                    TokenKind::LBrackBeg
                } else {
                    TokenKind::LBrackARef
                }
            }
            b'{' => {
                self.pos += 1;
                // TODO: various other conditions
                TokenKind::LBraceHash
            }
            b'%' => {
                self.pos += 1;
                if self.next() == Some(b'=') && !beg {
                    self.pos += 1;
                    TokenKind::OpAssign(BinaryOp::Mod)
                } else if is_begin_like(self.next()) {
                    // TODO: include EXPR_FITEM condition
                    todo!("%q()");
                } else {
                    // TODO: fitem condition
                    TokenKind::BinOp(BinaryOp::Mod)
                }
            }
            b'$' => {
                let start = self.pos;
                let is_ok = self.advance_gvar();
                if !is_ok {
                    todo!("error recovery from invalid gvar");
                }
                let ident = self.source[start..self.pos].as_bstr();
                TokenKind::Ident(IdentType::GVar, ident.to_owned())
            }
            b'@' => {
                let start = self.pos;
                let is_ok = self.advance_ivar_or_cvar();
                if !is_ok {
                    todo!("error recovery from invalid ivar/cvar");
                }
                let ident = self.source[start..self.pos].as_bstr();
                TokenKind::Ident(
                    if ident.starts_with(b"@@") {
                        IdentType::CVar
                    } else {
                        IdentType::IVar
                    },
                    ident.to_owned(),
                )
            }
            _ if first.is_ascii_alphabetic() || first == b'_' || first >= 0x80 => {
                let start = self.pos;
                self.advance_ident();
                if (self.next() == Some(b'!') || self.next() == Some(b'?'))
                    && self.next_n(1) != Some(b'=')
                {
                    self.pos += 1;
                }
                let ident = self.source[start..self.pos].as_bstr();
                if let Some(kind) = if is_special_ident {
                    None
                } else {
                    KEYWORDS.get(ident)
                } {
                    if let Some(kind) = kind {
                        kind.clone()
                    } else {
                        match ident.as_bytes() {
                            b"__END__" => todo!("__END__"),
                            b"do" => {
                                // TODO: lambda_beginning_p
                                if params.in_condition {
                                    TokenKind::KeywordDoAfterCondition
                                    // TODO: take EXPR_CMDARG into account
                                } else if params.in_command_args {
                                    TokenKind::KeywordDoAfterCommandCall
                                } else {
                                    TokenKind::KeywordDoAfterMethodCall
                                }
                            }
                            b"if" => {
                                if beg {
                                    TokenKind::KeywordIf
                                } else {
                                    TokenKind::ModifierIf
                                }
                            }
                            b"rescue" => {
                                if beg {
                                    TokenKind::KeywordRescue
                                } else {
                                    TokenKind::ModifierRescue
                                }
                            }
                            b"unless" => {
                                if beg {
                                    TokenKind::KeywordUnless
                                } else {
                                    TokenKind::ModifierUnless
                                }
                            }
                            b"until" => {
                                if beg {
                                    TokenKind::KeywordUntil
                                } else {
                                    TokenKind::ModifierUntil
                                }
                            }
                            b"while" => {
                                if beg {
                                    TokenKind::KeywordWhile
                                } else {
                                    TokenKind::ModifierWhile
                                }
                            }
                            _ => unreachable!("invalid special keyword: {:?}", ident),
                        }
                    }
                } else if ident.ends_with(b"!") || ident.ends_with(b"?") {
                    TokenKind::Ident(IdentType::FIdent, ident.to_owned())
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

    fn lex_token_special_ident(&mut self, params: LexerParams) -> Token {
        let Token { kind, range } = self.lex_token_normal(params);
        let kind = match kind {
            TokenKind::BinOp(_) | TokenKind::UnOp(_) => TokenKind::Ident(
                IdentType::Op,
                self.source[range.0..range.1].as_bstr().to_owned(),
            ),
            TokenKind::Ident(..) => kind,
            _ => todo!("Error recovery from invalid ident"),
        };
        Token { kind, range }
    }

    fn advance_gvar(&mut self) -> bool {
        assert_eq!(self.next(), Some(b'$'));
        self.pos += 1;
        match self.next() {
            Some(
                b'!' | b'$' | b'&' | b'*' | b'+' | b',' | b'.' | b'/' | b':' | b';' | b'<' | b'='
                | b'>' | b'?' | b'@' | b'\"' | b'\'' | b'\\' | b'`' | b'~',
            ) => {
                self.pos += 1;
                true
            }
            Some(b'-') => {
                self.pos += 1;
                if self
                    .next()
                    .is_some_and_(|&ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch >= 0x80)
                {
                    if self.next().unwrap() >= 0x80 {
                        self.advance_utf8_char();
                    } else {
                        self.pos += 1;
                    }
                    true
                } else {
                    if self.next().is_some_and_(|&ch| !ch.isspace()) {
                        self.advance_utf8_char();
                    }
                    false
                }
            }
            Some(b'1'..=b'9') => {
                self.pos += 1;
                // Allow $10, $11, ... too
                while self.next().is_some_and_(|&ch| ch.is_ascii_digit()) {
                    self.pos += 1;
                }
                true
            }
            Some(first) if first.is_ascii_alphanumeric() || first == b'_' || first >= 0x80 => {
                let start = self.pos;
                self.advance_ident();
                if first == b'0' {
                    // Disallow $0abc
                    self.pos - start == 1
                } else {
                    true
                }
            }
            _ => {
                if self.next().is_some_and_(|&ch| !ch.isspace()) {
                    self.advance_utf8_char();
                }
                false
            }
        }
    }

    fn advance_ivar_or_cvar(&mut self) -> bool {
        assert_eq!(self.next(), Some(b'@'));
        self.pos += 1;
        if self.next() == Some(b'@') {
            self.pos += 1;
        }
        let pos = self.pos;
        self.advance_ident();
        pos < self.pos
    }

    fn advance_ident(&mut self) {
        while self
            .next()
            .is_some_and_(|&ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch >= 0x80)
        {
            self.pos += 1;
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

    fn advance_utf8_char(&mut self) {
        assert!(self.pos < self.source.len());
        self.pos += 1;
        while self.next().is_some_and_(|&ch| (0x80..0xC0).contains(&ch)) {
            self.pos += 1;
        }
    }

    fn next_n(&self, off: usize) -> Option<u8> {
        self.source.get(self.pos + off).copied()
    }

    fn next(&self) -> Option<u8> {
        self.next_n(0)
    }

    // fn peek(&self, off: usize) -> Option<u8> {
    //     self.source.get(self.pos + off).copied()
    // }

    fn skip_whitespace(&mut self, mode: LexerMode) -> bool {
        // TODO: these cases are also newline-insensitive
        // - `def f foo:`
        // - `42 in foo:`
        let newline_sensitive = match mode {
            LexerMode::Begin(LexerBeginMode::Omittable) => true,
            LexerMode::Begin(_) => false,
            LexerMode::Arg => true,
            LexerMode::SpecialIdent => false,
            LexerMode::End => true,
            LexerMode::String(_) => unreachable!(),
        };
        let start = self.pos;
        while self.pos < self.source.len() {
            let ch = self.source[self.pos];
            if ch == b'\n' && newline_sensitive {
                let newline_pos = self.pos;
                // lookahead
                self.pos += 1;
                while self.next().is_some_and_(|&ch| ch.isspace()) {
                    self.pos += 1;
                }
                match self.next() {
                    Some(b'#') => {
                        // continue and check the next newline.
                        continue;
                    }
                    Some(b'&') => {
                        if self.next_n(1) == Some(b'.') {
                            // This is `&.`.
                            // Ignore the previous newline.
                            continue;
                        }
                    }
                    Some(b'.') => {
                        if self.next_n(1) != Some(b'.') {
                            // This is `.` other than `..` or `...`.
                            // Ignore the previous newline.
                            continue;
                        }
                    }
                    _ => {}
                }
                // Lookahead failed. Rewind the position to emit newline
                self.pos = newline_pos;
                break;
            } else if ch.isspace() {
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
