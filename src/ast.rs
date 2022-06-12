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
pub enum Expr {
    Parenthesized(Parenthesized),
    Compound(Compound),
    // `foo`
    Ident(Ident),
    // `Foo`
    CIdent(CIdent),
    // `::Foo`
    RootIdent(RootIdent),
    // `Foo::Bar`
    RelativeConstant(RelativeConstant),
    // TODO: bigint, float, etc.
    Numeric(Numeric),
    TernaryCond(TernaryCond),
    Binary(Binary),
    Unary(Unary),
    PostfixUnary(PostfixUnary),
    Nil(Nil),
    Assign(Assign),
    Send(Send),
    Module(Module),
    Errored(Errored),
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
            $crate::ast::Expr::TernaryCond($x) => $arm,
            $crate::ast::Expr::Binary($x) => $arm,
            $crate::ast::Expr::Unary($x) => $arm,
            $crate::ast::Expr::PostfixUnary($x) => $arm,
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

impl From<Parenthesized> for Expr {
    fn from(e: Parenthesized) -> Self {
        Expr::Parenthesized(e)
    }
}

impl From<Compound> for Expr {
    fn from(e: Compound) -> Self {
        Expr::Compound(e)
    }
}

impl From<Ident> for Expr {
    fn from(e: Ident) -> Self {
        Expr::Ident(e)
    }
}

impl From<CIdent> for Expr {
    fn from(e: CIdent) -> Self {
        Expr::CIdent(e)
    }
}

impl From<RootIdent> for Expr {
    fn from(e: RootIdent) -> Self {
        Expr::RootIdent(e)
    }
}

impl From<RelativeConstant> for Expr {
    fn from(e: RelativeConstant) -> Self {
        Expr::RelativeConstant(e)
    }
}

impl From<Numeric> for Expr {
    fn from(e: Numeric) -> Self {
        Expr::Numeric(e)
    }
}

impl From<TernaryCond> for Expr {
    fn from(e: TernaryCond) -> Self {
        Expr::TernaryCond(e)
    }
}

impl From<Binary> for Expr {
    fn from(e: Binary) -> Self {
        Expr::Binary(e)
    }
}

impl From<Unary> for Expr {
    fn from(e: Unary) -> Self {
        Expr::Unary(e)
    }
}

impl From<PostfixUnary> for Expr {
    fn from(e: PostfixUnary) -> Self {
        Expr::PostfixUnary(e)
    }
}

impl From<Nil> for Expr {
    fn from(e: Nil) -> Self {
        Expr::Nil(e)
    }
}

impl From<Assign> for Expr {
    fn from(e: Assign) -> Self {
        Expr::Assign(e)
    }
}

impl From<Send> for Expr {
    fn from(e: Send) -> Self {
        Expr::Send(e)
    }
}

impl From<Module> for Expr {
    fn from(e: Module) -> Self {
        Expr::Module(e)
    }
}

impl From<Errored> for Expr {
    fn from(e: Errored) -> Self {
        Expr::Errored(e)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parenthesized {
    pub stmts: Vec<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compound {
    pub stmts: Vec<Expr>,
    pub meta: NodeMeta,
}

// `foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub meta: NodeMeta,
}

// `Foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CIdent {
    pub name: String,
    pub meta: NodeMeta,
}

// `::Foo`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootIdent {
    pub name: String,
    pub meta: NodeMeta,
}

// `Foo::Bar`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelativeConstant {
    pub base: Box<Expr>,
    pub name: String,
    pub meta: NodeMeta,
}

// TODO: bigint, float, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Numeric {
    pub numval: i32,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TernaryCond {
    pub cond: Box<Expr>,
    pub consequence: Box<Expr>,
    pub alternate: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PostfixUnary {
    pub expr: Box<Expr>,
    pub op: PostfixUnaryOp,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nil {
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Send {
    pub optional: bool,
    pub recv: Option<Box<Expr>>,
    pub name: String,
    pub args: Vec<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub cpath: Box<Expr>,
    pub body: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Errored {
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `..`
    RangeIncl,
    /// `...`
    RangeExcl,
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
    /// `..`
    RangeIncl,
    /// `...`
    RangeExcl,
    /// `+`
    Plus,
    /// `-`
    Neg,
    /// `!`
    Not,
    /// `~`
    BitwiseNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixUnaryOp {
    /// `..`
    RangeIncl,
    /// `...`
    RangeExcl,
}
