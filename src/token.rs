use crate::ast::{BinaryOp, Range, UnaryOp};
use bstr::BString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
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
pub enum TokenKind {
    /// - `foo` (a.k.a. tIDENTIFIER)
    /// - `foo!` (a.k.a. tFID)
    /// - `Foo` (a.k.a. tCONSTANT)
    Ident(IdentType, BString),
    /// `__ENCODING__` (a.k.a. keyword__ENCODING__)
    UnderscoreEncodingKeyword,
    /// `__LINE__` (a.k.a. keyword__LINE__)
    UnderscoreLineKeyword,
    /// `__FILE__` (a.k.a. keyword__FILE__)
    UnderscoreFileKeyword,
    /// `BEGIN` (a.k.a. keyword_BEGIN)
    CapitalBeginKeyword,
    /// `END` (a.k.a. keyword_END)
    CapitalEndKeyword,
    /// `alias` (a.k.a. keyword_alias)
    AliasKeyword,
    /// `and` (a.k.a. keyword_and)
    AndKeyword,
    /// `begin` (a.k.a. keyword_begin)
    BeginKeyword,
    /// `break` (a.k.a. keyword_break)
    BreakKeyword,
    /// `case` (a.k.a. keyword_case)
    CaseKeyword,
    /// `class` (a.k.a. keyword_class)
    ClassKeyword,
    /// `def` (a.k.a. keyword_def)
    DefKeyword,
    /// `defined?` (a.k.a. keyword_defined)
    DefinedQKeyword,
    /// `do` (a.k.a. keyword_do)
    DoKeyword,
    /// `else` (a.k.a. keyword_else)
    ElseKeyword,
    /// `elsif` (a.k.a. keyword_elsif)
    ElsifKeyword,
    /// `end` (a.k.a. keyword_end)
    EndKeyword,
    /// `ensure` (a.k.a. keyword_ensure)
    EnsureKeyword,
    /// `false` (a.k.a. keyword_false)
    FalseKeyword,
    /// `for` (a.k.a. keyword_for)
    ForKeyword,
    /// `if` (a.k.a. keyword_if)
    IfKeyword,
    /// `if` (a.k.a. modifier_if)
    IfModifier,
    /// `in` (a.k.a. keyword_in)
    InKeyword,
    /// `module` (a.k.a. keyword_module)
    ModuleKeyword,
    /// `next` (a.k.a. keyword_next)
    NextKeyword,
    /// `nil` (a.k.a. keyword_nil)
    NilKeyword,
    /// `not` (a.k.a. keyword_not)
    NotKeyword,
    /// `or` (a.k.a. keyword_or)
    OrKeyword,
    /// `redo` (a.k.a. keyword_redo)
    RedoKeyword,
    /// `rescue` (a.k.a. keyword_rescue)
    RescueKeyword,
    /// `rescue` (a.k.a. modifier_rescue)
    RescueModifier,
    /// `retry` (a.k.a. keyword_retry)
    RetryKeyword,
    /// `return` (a.k.a. keyword_return)
    ReturnKeyword,
    /// `self` (a.k.a. keyword_self)
    SelfKeyword,
    /// `super` (a.k.a. keyword_super)
    SuperKeyword,
    /// `then` (a.k.a. keyword_then)
    ThenKeyword,
    /// `true` (a.k.a. keyword_true)
    TrueKeyword,
    /// `undef` (a.k.a. keyword_undef)
    UndefKeyword,
    /// `unless` (a.k.a. keyword_unless)
    UnlessKeyword,
    /// `unless` (a.k.a. modifier_unless)
    UnlessModifier,
    /// `until` (a.k.a. keyword_until)
    UntilKeyword,
    /// `until` (a.k.a. modifier_until)
    UntilModifier,
    /// `when` (a.k.a. keyword_when)
    WhenKeyword,
    /// `while` (a.k.a. keyword_while)
    WhileKeyword,
    /// `while` (a.k.a. modifier_while)
    WhileModifier,
    /// `yield` (a.k.a. keyword_yield)
    YieldKeyword,
    /// `42` (a.k.a. tINTEGER, tFLOAT, tRATIONAL or tIMAGINARY)
    // TODO: bigint, float, etc.
    Numeric(i32),
    /// `'`, `"`, or <code>`</code> (a.k.a. tSTRING or tXSTRING)
    StringBeg(StringType),
    /// Text in a string literal
    StringContent(String),
    /// `'`, `"`, or <code>`</code> (a.k.a. tSTRING_END)
    StringEnd,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `&.`
    AndDot,
    /// `..` at the beginning of the expression (a.k.a. tBDOT2)
    Dot2Beg,
    /// `..` after the expression (a.k.a. tDOT2)
    Dot2Mid,
    /// `...` at the beginning of the expression (a.k.a. tBDOT3)
    Dot3Beg,
    /// `...` after the expression (a.k.a. tBDOT3)
    Dot3Mid,
    /// Binary operator
    /// Note: some operators have overloaded meanings.
    ///
    /// In `parse.y` it corresponds to one of the following:
    ///
    /// - tDOT2 (`..`)
    /// - tDOT3 (`...`)
    /// - tOROP (`||`)
    /// - tANDOP (`&&`)
    /// - tCMP (`<=>`)
    /// - tEQ (`==`)
    /// - tEQQ (`===`)
    /// - tNEQ (`!=`)
    /// - tMATCH (`=~`)
    /// - tNMATCH (`!~`)
    /// - '>'
    /// - tGEQ (`>=`)
    /// - '<'
    /// - tLEQ (`<`)
    /// - '|'
    /// - '^'
    /// - '&'
    /// - tLSHFT (`<<`)
    /// - tRSHFT (`>>`)
    /// - '+'
    /// - '-'
    /// - '*'
    /// - '/'
    /// - '%'
    /// - tPOW (`**`)
    BinOp(BinaryOp),
    /// Unary operator
    /// Note: some operators have overloaded meanings.
    ///
    /// In `parse.y` it corresponds to one of the following:
    ///
    /// - tBDOT2 (`..`)
    /// - tBDOT3 (`...`)
    /// - tUMINUS (`-`)
    ///   - Note that tUMINUS_NUM is part of the numeric literal
    /// - '!'
    /// - '~'
    /// - tUPLUS (`+`)
    UnOp(UnaryOp),
    /// `(` at the beginning of the expression (a.k.a. tLPAREN)
    LParenBeg,
    /// `(` after the expression, usually without preceding spaces (a.k.a. '(')
    LParenCall,
    /// `)`
    RParen,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `=`
    Equal,
    /// `;`
    Semi,
    // `::` at the beginning of the expression (a.k.a. tCOLON3)
    DColonBeg,
    // `::` (a.k.a. tCOLON2)
    DColon,
    NewLine,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentType {
    /// - `foo` (a.k.a. tIDENTIFIER)
    Ident,
    /// - `foo!` (a.k.a. tFID)
    FIdent,
    /// - `Foo` (a.k.a. tCONSTANT)
    Const,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringType {
    /// `'`
    SQuote,
    /// `"`
    DQuote,
    // /// <code>`</code>
    // BQuote,
}
