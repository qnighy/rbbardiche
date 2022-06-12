use crate::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range(pub usize, pub usize);

impl std::ops::BitOr for Range {
    type Output = Range;

    fn bitor(self, rhs: Self) -> Self::Output {
        Range(std::cmp::min(self.0, rhs.0), std::cmp::max(self.1, rhs.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeMeta {
    pub range: Range,
    pub node_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub stmts: Vec<Expr>,
    pub meta: NodeMeta,
}

impl AsRef<NodeMeta> for Program {
    fn as_ref(&self) -> &NodeMeta {
        &self.meta
    }
}

impl AsMut<NodeMeta> for Program {
    fn as_mut(&mut self) -> &mut NodeMeta {
        &mut self.meta
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Parenthesized(ParenthesizedExpr),
    Compound(CompoundExpr),
    // `foo`
    Ident(IdentExpr),
    // `Foo`
    CIdent(CIdentExpr),
    // `::Foo`
    RootIdent(RootIdentExpr),
    // `Foo::Bar`
    RelativeConstant(RelativeConstantExpr),
    // TODO: bigint, float, etc.
    Numeric(NumericExpr),
    StringLiteral(StringLiteralExpr),
    TernaryCond(TernaryCondExpr),
    Range(RangeExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Nil(NilExpr),
    Assign(AssignExpr),
    Send(SendExpr),
    Module(ModuleExpr),
    Errored(ErroredExpr),
}

#[macro_export]
macro_rules! delegate_expr {
    ($e:expr, $x:ident => $arm:expr) => {
        match $e {
            $crate::ast::Expr::Parenthesized($x) => $arm,
            $crate::ast::Expr::Compound($x) => $arm,
            $crate::ast::Expr::Ident($x) => $arm,
            $crate::ast::Expr::CIdent($x) => $arm,
            $crate::ast::Expr::RootIdent($x) => $arm,
            $crate::ast::Expr::RelativeConstant($x) => $arm,
            $crate::ast::Expr::Numeric($x) => $arm,
            $crate::ast::Expr::StringLiteral($x) => $arm,
            $crate::ast::Expr::TernaryCond($x) => $arm,
            $crate::ast::Expr::Range($x) => $arm,
            $crate::ast::Expr::Binary($x) => $arm,
            $crate::ast::Expr::Unary($x) => $arm,
            $crate::ast::Expr::Nil($x) => $arm,
            $crate::ast::Expr::Assign($x) => $arm,
            $crate::ast::Expr::Send($x) => $arm,
            $crate::ast::Expr::Module($x) => $arm,
            $crate::ast::Expr::Errored($x) => $arm,
        }
    };
}

impl Expr {
    pub fn meta(&self) -> &NodeMeta {
        self.as_ref()
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        self.as_mut()
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
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

impl From<ParenthesizedExpr> for Expr {
    fn from(e: ParenthesizedExpr) -> Self {
        Expr::Parenthesized(e)
    }
}

impl From<CompoundExpr> for Expr {
    fn from(e: CompoundExpr) -> Self {
        Expr::Compound(e)
    }
}

impl From<IdentExpr> for Expr {
    fn from(e: IdentExpr) -> Self {
        Expr::Ident(e)
    }
}

impl From<CIdentExpr> for Expr {
    fn from(e: CIdentExpr) -> Self {
        Expr::CIdent(e)
    }
}

impl From<RootIdentExpr> for Expr {
    fn from(e: RootIdentExpr) -> Self {
        Expr::RootIdent(e)
    }
}

impl From<RelativeConstantExpr> for Expr {
    fn from(e: RelativeConstantExpr) -> Self {
        Expr::RelativeConstant(e)
    }
}

impl From<NumericExpr> for Expr {
    fn from(e: NumericExpr) -> Self {
        Expr::Numeric(e)
    }
}

impl From<StringLiteralExpr> for Expr {
    fn from(e: StringLiteralExpr) -> Self {
        Expr::StringLiteral(e)
    }
}

impl From<TernaryCondExpr> for Expr {
    fn from(e: TernaryCondExpr) -> Self {
        Expr::TernaryCond(e)
    }
}

impl From<RangeExpr> for Expr {
    fn from(e: RangeExpr) -> Self {
        Expr::Range(e)
    }
}

impl From<BinaryExpr> for Expr {
    fn from(e: BinaryExpr) -> Self {
        Expr::Binary(e)
    }
}

impl From<UnaryExpr> for Expr {
    fn from(e: UnaryExpr) -> Self {
        Expr::Unary(e)
    }
}

impl From<NilExpr> for Expr {
    fn from(e: NilExpr) -> Self {
        Expr::Nil(e)
    }
}

impl From<AssignExpr> for Expr {
    fn from(e: AssignExpr) -> Self {
        Expr::Assign(e)
    }
}

impl From<SendExpr> for Expr {
    fn from(e: SendExpr) -> Self {
        Expr::Send(e)
    }
}

impl From<ModuleExpr> for Expr {
    fn from(e: ModuleExpr) -> Self {
        Expr::Module(e)
    }
}

impl From<ErroredExpr> for Expr {
    fn from(e: ErroredExpr) -> Self {
        Expr::Errored(e)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedExpr {
    pub stmts: Vec<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundExpr {
    pub stmts: Vec<Expr>,
    pub meta: NodeMeta,
}

// `foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentExpr {
    pub name: String,
    pub meta: NodeMeta,
}

// `Foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CIdentExpr {
    pub name: String,
    pub meta: NodeMeta,
}

// `::Foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootIdentExpr {
    pub name: String,
    pub meta: NodeMeta,
}

// `Foo::Bar`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelativeConstantExpr {
    pub base: Box<Expr>,
    pub name: String,
    pub meta: NodeMeta,
}

// TODO: bigint, float, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumericExpr {
    pub numval: i32,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteralExpr {
    pub strval: String,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TernaryCondExpr {
    pub cond: Box<Expr>,
    pub consequence: Box<Expr>,
    pub alternate: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeExpr {
    pub begin: Option<Box<Expr>>,
    pub range_type: RangeType,
    pub end: Option<Box<Expr>>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilExpr {
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendExpr {
    pub optional: bool,
    pub recv: Option<Box<Expr>>,
    pub name: String,
    pub args: Vec<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExpr {
    pub cpath: Box<Expr>,
    pub body: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErroredExpr {
    pub debris: Vec<Debri>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
