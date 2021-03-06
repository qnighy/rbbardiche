use crate::ast::{BinaryOp, Range, Token as PublicToken, UnaryOp};
use bstr::BString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::parser) struct Token {
    pub(in crate::parser) kind: TokenKind,
    pub(in crate::parser) range: Range,
}

impl Token {
    pub(in crate::parser) fn to_binop<F>(&self, cond: F) -> Option<BinaryOp>
    where
        F: FnOnce(BinaryOp) -> bool,
    {
        match &self.kind {
            TokenKind::BinOp(op) if cond(*op) => Some(*op),
            _ => None,
        }
    }

    pub(in crate::parser) fn to_unop<F>(&self, cond: F) -> Option<UnaryOp>
    where
        F: FnOnce(UnaryOp) -> bool,
    {
        match &self.kind {
            TokenKind::UnOp(op) if cond(*op) => Some(*op),
            _ => None,
        }
    }

    pub(in crate::parser) fn p(&self) -> PublicToken {
        PublicToken { range: self.range }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::parser) enum TokenKind {
    // Identifiers and keywords
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

    /// `foo:` (a.k.a. tLABEL)
    Label(BString),

    // Literals
    /// `42` (a.k.a. tINTEGER, tFLOAT, tRATIONAL or tIMAGINARY)
    // TODO: bigint, float, etc.
    Numeric(i32),
    /// `?a` (a.k.a. tCHAR)
    Char,
    /// `:` as a symbol (a.k.a. tSYMBEG but excluding dsym case)
    SymbolBegin,
    /// `'`, `"`, or <code>`</code> (a.k.a. tSTRING, tSYMBEG, or tXSTRING)
    // TODO: take tWORDS_BEG, tQWORDS_BEG, tSYMBOLS_BEG, tQSYMBOLS_BEG into account
    StringBegin(StringType),
    /// Text in a string literal
    StringContent(String),
    /// `'`, `"`, or <code>`</code> (a.k.a. tSTRING_END)
    StringEnd,
    /// `':`, `":`, or <code>`:</code> (a.k.a. tLABEL_END)
    StringLabelEnd,
    /// `#@foo` (a.k.a. tSTRING_DVAR + tIVAR etc.)
    StringEmbedIdent(IdentType),
    /// `/` (a.k.a. tREGEXP_BEG)
    RegExpBegin,
    /// `/` (a.k.a. tREGEXP_END)
    RegExpEnd,

    // Punctuators - parentheses
    /// `(` at the beginning of the expression (a.k.a. tLPAREN)
    LParenBeg,
    /// `(` at the first command argument (a.k.a. tLPAREN_ARG)
    LParenArg,
    /// `(` after the expression, usually without preceding spaces (a.k.a. '(')
    LParenCall,
    /// `)`
    RParen,
    /// `[` at the beginning of the expression (a.k.a. tLBRACK)
    LBrackBeg,
    /// `[` as an array reference (a.k.a. '[')
    LBrackARef,
    /// `]`
    RBrack,
    /// `{` at the beginning of the expression (a.k.a. tBRACE)
    LBraceHash,
    /// `{` after a paren call (a.k.a. '{')
    LBraceBlockNarrow,
    /// `{` after a command call (a.k.a. tBRACE_ARG)
    LBraceBlockWide,
    /// `{` corresponding `->` (a.k.a. tLAMBEG)
    LBraceLambda,
    /// `#{` in a string (a.k.a. tSTRING_DBEG)
    StringEmbedBegin,
    /// `}` (a.k.a. '}' or tSTRING_DEND)
    RBrace,

    // Punctuators - separators / connectors
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `&.`
    AndDot,
    /// `;`
    Semi,
    NewLine,
    /// `::` (a.k.a. tCOLON2)
    Colon2Infix,
    /// `=>` (a.k.a. tASSOC)
    Assoc,

    // Punctuators - prefixes
    /// `::` at the beginning of the expression (a.k.a. tCOLON3)
    Colon2Prefix,
    /// `->` (a.k.a. tLAMBDA)
    Lambda,
    /// `*` at the beginning of the expression (a.k.a. tSTAR)
    Star,
    /// `**` at the beginning of the expression (a.k.a. tDSTAR)
    Star2,
    /// `&` at the beginning of the expression (a.k.a. tAMPER)
    Amper,

    // Operators
    /// `..` at the beginning of the expression (a.k.a. tBDOT2)
    Dot2Prefix,
    /// `..` after the expression (a.k.a. tDOT2)
    Dot2Infix,
    /// `...` at the beginning of the expression (a.k.a. tBDOT3)
    Dot3Prefix,
    /// `...` after the expression (a.k.a. tBDOT3)
    Dot3Infix,
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
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `=`
    Equal,
    /// Compound assignment like `+=`
    OpAssign(BinaryOp),

    // Others
    Eof,
}

impl TokenKind {
    pub(in crate::parser) fn token_class(&self) -> TokenClass {
        match self {
            // `foo`
            TokenKind::Ident(_, _)
            // `__ENCODING__`
            | TokenKind::KeywordUnderscoreEncoding
            // `__LINE__`
            | TokenKind::KeywordUnderscoreLine
            // `__FILE__`
            | TokenKind::KeywordUnderscoreFile
            // `BEGIN {}` (resembles `f {}`)
            | TokenKind::KeywordCapitalBegin
            // `END {}` (resembles `f {}`)
            | TokenKind::KeywordCapitalEnd
            // `false`
            | TokenKind::KeywordFalse
            // `nil`
            | TokenKind::KeywordNil
            // `redo`
            | TokenKind::KeywordRedo
            // `retry`
            | TokenKind::KeywordRetry
            // `self`
            | TokenKind::KeywordSelf
            // `super` / `super e` (behaves like a method)
            | TokenKind::KeywordSuper
            // `true`
            | TokenKind::KeywordTrue
            // `yield` / `yield e` (behaves like a method)
            | TokenKind::KeywordYield
            // `123`
            | TokenKind::Numeric(_)
            // `?a`
            | TokenKind::Char
            // `"foo"` (though the contents are not expressions)
            | TokenKind::StringContent(_)
            // `"#@foo"` (though the contents are not expressions)
            | TokenKind::StringEmbedIdent(_)
            // `-> x {}`
            | TokenKind::Lambda
            // `f(*e)`
            | TokenKind::Star
            // `f(**e)`
            | TokenKind::Star2
            // `f(&e)`
            | TokenKind::Amper => TokenClass::SelfContained,

            // `break` / `break e`
            TokenKind::KeywordBreak
            // `next` / `next e`
            | TokenKind::KeywordNext
            // `return` / `return e`
            | TokenKind::KeywordReturn => TokenClass::MaybePrefix,

            // `alias foo bar`
            TokenKind::KeywordAlias
            // `begin e end`
            | TokenKind::KeywordBegin
            // `case e when e; end`
            | TokenKind::KeywordCase
            // `class C end`
            | TokenKind::KeywordClass
            // `def m; end`
            | TokenKind::KeywordDef
            // `defined? e`
            | TokenKind::KeywordDefinedQ
            // `for x in e; end`
            | TokenKind::KeywordFor
            // `if e; end`
            | TokenKind::KeywordIf
            // `module C end`
            | TokenKind::KeywordModule
            // `not e`
            | TokenKind::KeywordNot
            // `undef foo`
            | TokenKind::KeywordUndef
            // `unless e; end`
            | TokenKind::KeywordUnless
            // `until e; end`
            | TokenKind::KeywordUntil
            // `while e; end`
            | TokenKind::KeywordWhile
            // `[foo: 42]` (though it does not begin expression itself)
            | TokenKind::Label(_)
            // `:foo`
            | TokenKind::SymbolBegin
            // `"foo"` (though the contents are not expressions)
            | TokenKind::StringBegin(_)
            // `/foo/` (though the contents are not expressions)
            | TokenKind::RegExpBegin
            // `(e)`
            | TokenKind::LParenBeg
            // `f (e)`
            | TokenKind::LParenArg
            // `[e]`
            | TokenKind::LBrackBeg
            // `{ x: e }`
            | TokenKind::LBraceHash
            // `"#{foo}"` (though the contents are not expressions)
            | TokenKind::StringEmbedBegin
            // `::C`
            | TokenKind::Colon2Prefix
            // `..e`
            | TokenKind::Dot2Prefix
            // `...e`
            | TokenKind::Dot3Prefix
            // `+e`
            | TokenKind::UnOp(_)
            => TokenClass::Prefix,

            // `begin e end`
            TokenKind::KeywordEnd
            // `"foo"` (though the contents are not expressions)
            | TokenKind::StringEnd
            // `/foo/` (though the contents are not expressions)
            | TokenKind::RegExpEnd
            // `(e)`
            | TokenKind::RParen
            // `[e]`
            | TokenKind::RBrack
            // `{ x: e }`
            | TokenKind::RBrace
            // `e (eof)`
            | TokenKind::Eof => TokenClass::Postfix,

            // `begin rescue; end` / `begin rescue e; end`
            TokenKind::KeywordRescue => TokenClass::MaybeInfix,

            // `e and e`
            TokenKind::KeywordAnd
            // `f e do e end`
            | TokenKind::KeywordDoAfterCommandCall
            // `f() do e end`
            | TokenKind::KeywordDoAfterMethodCall
            // `while e do e end`
            | TokenKind::KeywordDoAfterCondition
            // `-> do e end`
            | TokenKind::KeywordDoAfterLambda
            // `if e; else e; end`
            | TokenKind::KeywordElse
            // `if e; elsif e; end`
            | TokenKind::KeywordElsif
            // `begin ensure e end`
            | TokenKind::KeywordEnsure
            // `e if e`
            | TokenKind::ModifierIf
            // `for x in e; end`
            | TokenKind::KeywordIn
            // `e or e`
            | TokenKind::KeywordOr
            // `e rescue e`
            | TokenKind::ModifierRescue
            // `if e then e end`
            | TokenKind::KeywordThen
            // `e unless e`
            | TokenKind::ModifierUnless
            // `e until e`
            | TokenKind::ModifierUntil
            // `case e when e; end`
            | TokenKind::KeywordWhen
            // `e while e`
            | TokenKind::ModifierWhile
            // `"foo": e`
            | TokenKind::StringLabelEnd
            // `f(e)`
            | TokenKind::LParenCall
            // `e[e]`
            | TokenKind::LBrackARef
            // `f() { e }`
            | TokenKind::LBraceBlockNarrow
            // `f e { e }`
            | TokenKind::LBraceBlockWide
            // `-> { e }`
            | TokenKind::LBraceLambda
            // `f(e, e)`
            | TokenKind::Comma
            // `f.f`
            | TokenKind::Dot
            // `f&.f`
            | TokenKind::AndDot
            // `e; e`
            | TokenKind::Semi
            // `[e => e]`
            | TokenKind::Assoc
            // `e..e` (`e..` is considered a special case)
            | TokenKind::Dot2Infix
            // `e...e` (`e...` is considered a special case)
            | TokenKind::Dot3Infix
            // `e+e`
            | TokenKind::BinOp(_)
            // `e ? e : e`
            | TokenKind::Question
            // `e ? e : e`
            | TokenKind::Colon
            // `x = e`
            | TokenKind::Equal
            // `x += e`
            | TokenKind::OpAssign(_)
            // `C::C`
            | TokenKind::Colon2Infix
            // `e (newline) e`
            | TokenKind::NewLine => TokenClass::Infix,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum IdentType {
    /// `foo` (a.k.a. tIDENTIFIER)
    Ident,
    /// `foo!` (a.k.a. tFID)
    FIdent,
    /// `Foo` (a.k.a. tCONSTANT)
    Const,
    /// `$foo` (a.k.a. tGVAR, tNTH_REF, or tBACK_REF)
    GVar,
    /// `@foo` (a.k.a. tIVAR)
    IVar,
    /// `@@foo` (a.k.a. tCVAR)
    CVar,
    /// Operators like `+` after `def`, `.`, `&.`, `::`, or `:`
    ///
    /// Includes tAREF and tASET.
    Op,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum StringType {
    /// `'`
    SQuote,
    /// `"`
    DQuote,
    // /// <code>`</code>
    // BQuote,
}

/// Describes how the token relates to expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::parser) enum TokenClass {
    /// Starts an expression and immediately ends the expression.
    SelfContained,
    /// Starts an expression and possibly is followed by an expression.
    MaybePrefix,
    /// Starts an expression and is followed by an expression.
    Prefix,
    /// Follows an expression and ends an expression.
    Postfix,
    /// Follows an expression and possibly is followed by an expression.
    MaybeInfix,
    /// Follows an expression and is followed by an expression.
    Infix,
}
