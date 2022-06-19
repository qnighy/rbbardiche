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
    KeywordUnderscoreEncoding,
    /// `__LINE__` (a.k.a. keyword__LINE__)
    KeywordUnderscoreLine,
    /// `__FILE__` (a.k.a. keyword__FILE__)
    KeywordUnderscoreFile,
    /// `BEGIN` (a.k.a. keyword_BEGIN)
    KeywordCapitalBegin,
    /// `END` (a.k.a. keyword_END)
    KeywordCapitalEnd,
    /// `alias` (a.k.a. keyword_alias)
    KeywordAlias,
    /// `and` (a.k.a. keyword_and)
    KeywordAnd,
    /// `begin` (a.k.a. keyword_begin)
    KeywordBegin,
    /// `break` (a.k.a. keyword_break)
    KeywordBreak,
    /// `case` (a.k.a. keyword_case)
    KeywordCase,
    /// `class` (a.k.a. keyword_class)
    KeywordClass,
    /// `def` (a.k.a. keyword_def)
    KeywordDef,
    /// `defined?` (a.k.a. keyword_defined)
    KeywordDefinedQ,
    /// `do` (a.k.a. keyword_do_block)
    KeywordDoAfterCommandCall,
    /// `do` (a.k.a. keyword_do)
    KeywordDoAfterMethodCall,
    /// `do` (a.k.a. keyword_do_cond)
    KeywordDoAfterCondition,
    /// `do` (a.k.a. keyword_do_LAMBDA)
    KeywordDoAfterLambda,
    /// `else` (a.k.a. keyword_else)
    KeywordElse,
    /// `elsif` (a.k.a. keyword_elsif)
    KeywordElsif,
    /// `end` (a.k.a. keyword_end)
    KeywordEnd,
    /// `ensure` (a.k.a. keyword_ensure)
    KeywordEnsure,
    /// `false` (a.k.a. keyword_false)
    KeywordFalse,
    /// `for` (a.k.a. keyword_for)
    KeywordFor,
    /// `if` (a.k.a. keyword_if)
    KeywordIf,
    /// `if` (a.k.a. modifier_if)
    ModifierIf,
    /// `in` (a.k.a. keyword_in)
    KeywordIn,
    /// `module` (a.k.a. keyword_module)
    KeywordModule,
    /// `next` (a.k.a. keyword_next)
    KeywordNext,
    /// `nil` (a.k.a. keyword_nil)
    KeywordNil,
    /// `not` (a.k.a. keyword_not)
    KeywordNot,
    /// `or` (a.k.a. keyword_or)
    KeywordOr,
    /// `redo` (a.k.a. keyword_redo)
    KeywordRedo,
    /// `rescue` (a.k.a. keyword_rescue)
    KeywordRescue,
    /// `rescue` (a.k.a. modifier_rescue)
    ModifierRescue,
    /// `retry` (a.k.a. keyword_retry)
    KeywordRetry,
    /// `return` (a.k.a. keyword_return)
    KeywordReturn,
    /// `self` (a.k.a. keyword_self)
    KeywordSelf,
    /// `super` (a.k.a. keyword_super)
    KeywordSuper,
    /// `then` (a.k.a. keyword_then)
    KeywordThen,
    /// `true` (a.k.a. keyword_true)
    KeywordTrue,
    /// `undef` (a.k.a. keyword_undef)
    KeywordUndef,
    /// `unless` (a.k.a. keyword_unless)
    KeywordUnless,
    /// `unless` (a.k.a. modifier_unless)
    ModifierUnless,
    /// `until` (a.k.a. keyword_until)
    KeywordUntil,
    /// `until` (a.k.a. modifier_until)
    ModifierUntil,
    /// `when` (a.k.a. keyword_when)
    KeywordWhen,
    /// `while` (a.k.a. keyword_while)
    KeywordWhile,
    /// `while` (a.k.a. modifier_while)
    ModifierWhile,
    /// `yield` (a.k.a. keyword_yield)
    KeywordYield,
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
