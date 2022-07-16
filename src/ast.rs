use crate::token::Token;
use derive_more::{AsMut, AsRef, From};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range(pub usize, pub usize);

impl std::ops::BitOr for Range {
    type Output = Range;

    fn bitor(self, rhs: Self) -> Self::Output {
        Range(std::cmp::min(self.0, rhs.0), std::cmp::max(self.1, rhs.1))
    }
}

pub trait Node {
    fn meta(&self) -> &NodeMeta;
    fn meta_mut(&mut self) -> &mut NodeMeta;
    fn range(&self) -> Range {
        self.meta().range
    }
}

impl<T> Node for T
where
    T: AsRef<NodeMeta> + AsMut<NodeMeta> + ?Sized,
{
    fn meta(&self) -> &NodeMeta {
        self.as_ref()
    }

    fn meta_mut(&mut self) -> &mut NodeMeta {
        self.as_mut()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeMeta {
    pub range: Range,
    pub node_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Expr {
    /// Parenthesized expression, `(1 + 2)`
    Parenthesized(ParenthesizedExpr),
    /// Container for multiple expressions
    Compound(CompoundExpr),
    /// Numeric, `42`, `123.4`
    // TODO: bigint, float, etc.
    Numeric(NumericExpr),
    /// Symbol, `:foo`
    Symbol(SymbolExpr),
    /// String, `"foo"`
    StringLiteral(StringLiteralExpr),
    /// `foo`, `$foo`, `@foo`, `@@foo`
    Var(VarExpr),
    /// `nil`
    Nil(NilExpr),
    /// `self`
    Self_(SelfExpr),
    /// `true` or `false`
    BooleanLiteral(BooleanLiteralExpr),
    /// One of `__FILE__`, `__LINE__`, or `__ENCODING__`
    FileMeta(FileMetaExpr),
    /// `cond ? e1 : e2`
    TernaryCond(TernaryCondExpr),
    /// `0..10`
    Range(RangeExpr),
    /// Binary operators like `1 + 1`
    Binary(BinaryExpr),
    /// Unary operators like `-x`
    Unary(UnaryExpr),
    /// Assignment, `x = 42`
    Assign(AssignExpr),
    /// Method call, `obj.foo()`
    Send(SendExpr),
    /// Constant reference, `Foo::Bar`
    Const(ConstExpr),
    /// Array expression, `[1, 2, 3]`
    Array(ArrayExpr),
    /// Hash expression, `{ foo: 1, bar: 2 }`
    Hash(HashExpr),
    /// Class expression, `class C; end`
    Class(ClassExpr),
    /// Module expression, `module M; end`
    Module(ModuleExpr),
    /// Instance method definition, `def f; end`
    Defn(DefnExpr),
    /// A dummy expression generated on a parse error
    Errored(ErroredExpr),
}

#[macro_export]
macro_rules! delegate_expr {
    ($e:expr, $x:ident => $arm:expr) => {
        match $e {
            $crate::ast::Expr::Parenthesized($x) => $arm,
            $crate::ast::Expr::Compound($x) => $arm,
            $crate::ast::Expr::Numeric($x) => $arm,
            $crate::ast::Expr::Symbol($x) => $arm,
            $crate::ast::Expr::StringLiteral($x) => $arm,
            $crate::ast::Expr::TernaryCond($x) => $arm,
            $crate::ast::Expr::Range($x) => $arm,
            $crate::ast::Expr::Binary($x) => $arm,
            $crate::ast::Expr::Unary($x) => $arm,
            $crate::ast::Expr::Var($x) => $arm,
            $crate::ast::Expr::Nil($x) => $arm,
            $crate::ast::Expr::Self_($x) => $arm,
            $crate::ast::Expr::BooleanLiteral($x) => $arm,
            $crate::ast::Expr::FileMeta($x) => $arm,
            $crate::ast::Expr::Assign($x) => $arm,
            $crate::ast::Expr::Send($x) => $arm,
            $crate::ast::Expr::Const($x) => $arm,
            $crate::ast::Expr::Array($x) => $arm,
            $crate::ast::Expr::Hash($x) => $arm,
            $crate::ast::Expr::Class($x) => $arm,
            $crate::ast::Expr::Module($x) => $arm,
            $crate::ast::Expr::Defn($x) => $arm,
            $crate::ast::Expr::Errored($x) => $arm,
        }
    };
}

impl AsRef<NodeMeta> for Expr {
    fn as_ref(&self) -> &NodeMeta {
        delegate_expr!(self, x => &x.meta)
    }
}

impl AsMut<NodeMeta> for Expr {
    fn as_mut(&mut self) -> &mut NodeMeta {
        delegate_expr!(self, x => &mut x.meta)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ParenthesizedExpr {
    pub stmts: Vec<Stmt>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct CompoundExpr {
    pub stmts: Vec<Stmt>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

// TODO: bigint, float, etc.
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct NumericExpr {
    pub numval: i32,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct SymbolExpr {
    pub open_token: Token,
    pub ident_token: Token,
    pub value: String,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct StringLiteralExpr {
    pub strval: String,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// `foo`, `$foo`, `@foo`, `@@foo`
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct VarExpr {
    pub name: String,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// `nil`
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct NilExpr {
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// `self`
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct SelfExpr {
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// `true` or `false`
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct BooleanLiteralExpr {
    pub value: bool,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// One of `__FILE__`, `__LINE__`, or `__ENCODING__`
#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct FileMetaExpr {
    pub name: FileMetaName,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

/// One of `__FILE__`, `__LINE__`, or `__ENCODING__`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileMetaName {
    /// `__FILE__`
    File,
    /// `__LINE__`
    Line,
    /// `__ENCODING__`
    Encoding,
}

impl FileMetaName {
    pub fn name(self) -> &'static str {
        match self {
            FileMetaName::File => "__FILE__",
            FileMetaName::Line => "__LINE__",
            FileMetaName::Encoding => "__ENCODING__",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct TernaryCondExpr {
    pub cond: Box<Expr>,
    pub consequence: Box<Expr>,
    pub alternate: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct RangeExpr {
    pub begin: Option<Box<Expr>>,
    pub range_type: RangeType,
    pub end: Option<Box<Expr>>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct SendExpr {
    pub optional: bool,
    pub recv: Option<Box<Expr>>,
    pub name: String,
    pub args: Option<Args>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

impl SendExpr {
    pub fn set_args(&mut self, args: Args) {
        assert!(self.args.is_none());
        self.meta.range = self.meta.range | args.range();
        self.args = Some(args);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Args {
    Paren(ParenArgs),
    Command(CommandArgs),
}

impl Args {
    pub fn list(&self) -> &Vec<DelimitedArg> {
        match self {
            Args::Paren(e) => &e.list,
            Args::Command(e) => &e.list,
        }
    }

    pub fn list_mut(&mut self) -> &mut Vec<DelimitedArg> {
        match self {
            Args::Paren(e) => &mut e.list,
            Args::Command(e) => &mut e.list,
        }
    }
}

impl AsRef<NodeMeta> for Args {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            Args::Paren(e) => &e.meta,
            Args::Command(e) => &e.meta,
        }
    }
}

impl AsMut<NodeMeta> for Args {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            Args::Paren(e) => &mut e.meta,
            Args::Command(e) => &mut e.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ParenArgs {
    pub open_token: Token,
    pub list: Vec<DelimitedArg>,
    pub close_token: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct CommandArgs {
    pub list: Vec<DelimitedArg>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct DelimitedArg {
    pub arg: Arg,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

impl DelimitedArg {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Arg {
    Simple(Expr),
    // Splat(SplatArg),
    // KeywordSplat(KeywordSplatArg),
    // Assoc(AssocArg),
    // Labeled(LabeledArg),
    // Block(BlockArg),
}

impl AsRef<NodeMeta> for Arg {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            Arg::Simple(e) => e.meta(),
        }
    }
}

impl AsMut<NodeMeta> for Arg {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            Arg::Simple(e) => e.meta_mut(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ConstExpr {
    /// If true, this expression starts with `::`.
    /// Cannot coexist with recv.
    pub toplevel: bool,
    /// Expression before the token `::`.
    pub recv: Option<Box<Expr>>,
    pub name: String,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

impl ConstExpr {
    pub fn convertible_to_send(&self) -> bool {
        !self.toplevel
    }

    pub fn convert_to_send(self) -> SendExpr {
        assert!(self.convertible_to_send());
        SendExpr {
            optional: false,
            recv: self.recv,
            name: self.name,
            args: None,
            meta: self.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ArrayExpr {
    pub open_token: Token,
    pub list: Vec<DelimitedArg>,
    pub close_token: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct HashExpr {
    pub open_token: Token,
    pub list: Vec<DelimitedArg>,
    pub close_token: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ClassExpr {
    pub cpath: Box<Expr>,
    pub superclass: Option<SuperclassClause>,
    pub body: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct SuperclassClause {
    pub expr: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

impl SuperclassClause {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ModuleExpr {
    pub cpath: Box<Expr>,
    pub body: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct DefnExpr {
    pub name: String,
    pub args: Option<FArgs>,
    pub body: Box<Expr>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum FArgs {
    Paren(ParenFArgs),
    Command(CommandFArgs),
}

impl FArgs {
    pub fn list(&self) -> &Vec<DelimitedFArg> {
        match self {
            FArgs::Paren(e) => &e.list,
            FArgs::Command(e) => &e.list,
        }
    }

    pub fn list_mut(&mut self) -> &mut Vec<DelimitedFArg> {
        match self {
            FArgs::Paren(e) => &mut e.list,
            FArgs::Command(e) => &mut e.list,
        }
    }
}

impl AsRef<NodeMeta> for FArgs {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            FArgs::Paren(e) => &e.meta,
            FArgs::Command(e) => &e.meta,
        }
    }
}

impl AsMut<NodeMeta> for FArgs {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            FArgs::Paren(e) => &mut e.meta,
            FArgs::Command(e) => &mut e.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ParenFArgs {
    pub open_token: Token,
    pub list: Vec<DelimitedFArg>,
    pub close_token: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct CommandFArgs {
    pub list: Vec<DelimitedFArg>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct DelimitedFArg {
    pub arg: FArg,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

impl DelimitedFArg {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum FArg {
    Simple(Expr),
    // Splat(SplatFArg),
    // KeywordSplat(KeywordSplatFArg),
    // Assoc(AssocFArg),
    // Labeled(LabeledFArg),
    // Block(BlockFArg),
}

impl AsRef<NodeMeta> for FArg {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            FArg::Simple(e) => e.meta(),
        }
    }
}

impl AsMut<NodeMeta> for FArg {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            FArg::Simple(e) => e.meta_mut(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ErroredExpr {
    pub debris: Vec<Debri>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Stmt {
    Expr(ExprStmt),
    Empty(EmptyStmt),
}

impl AsRef<NodeMeta> for Stmt {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            Stmt::Expr(e) => e.meta(),
            Stmt::Empty(e) => e.meta(),
        }
    }
}

impl AsMut<NodeMeta> for Stmt {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            Stmt::Expr(e) => e.meta_mut(),
            Stmt::Empty(e) => e.meta_mut(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct ExprStmt {
    pub expr: Expr,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, AsRef, AsMut)]
pub struct EmptyStmt {
    pub delim: Token,
    #[as_ref]
    #[as_mut]
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeType {
    /// `..`
    Inclusive,
    /// `...`
    Exclusive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,
    /// `<=>`
    Cmp,
    /// `==`
    Eq,
    /// `===`
    Eqq,
    /// `!=`
    NEq,
    /// `=~`
    Match,
    /// `!~`
    NMatch,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `|`
    BitwiseOr,
    /// `^`
    BitwiseXor,
    /// `&`
    BitwiseAnd,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `**`
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `+`
    Plus,
    /// `-`
    Neg,
    /// `!`
    Not,
    /// `~`
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Debri {
    Token(Token),
    ExprLike(Expr),
}

impl Debri {
    pub fn range(&self) -> Range {
        match self {
            Debri::Token(token) => token.range,
            Debri::ExprLike(expr) => expr.range(),
        }
    }
}
