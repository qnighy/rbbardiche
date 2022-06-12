#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range(pub usize, pub usize);

impl std::ops::BitOr for Range {
    type Output = Range;

    fn bitor(self, rhs: Self) -> Self::Output {
        Range(std::cmp::min(self.0, rhs.0), std::cmp::max(self.1, rhs.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub range: Range,
    pub node_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Parenthesized {
        stmts: Vec<Expr>,
    },
    Compound {
        stmts: Vec<Expr>,
    },
    // `foo`
    Ident {
        name: String,
    },
    // `Foo`
    CIdent {
        name: String,
    },
    // `::Foo`
    RootIdent {
        name: String,
    },
    // `Foo::Bar`
    RelativeConstant {
        base: Box<Expr>,
        name: String,
    },
    // TODO: bigint, float, etc.
    Numeric {
        numval: i32,
    },
    TernaryCond {
        cond: Box<Expr>,
        consequence: Box<Expr>,
        alternate: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    PostfixUnary {
        expr: Box<Expr>,
        op: PostfixUnaryOp,
    },
    Nil,
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Send {
        optional: bool,
        recv: Option<Box<Expr>>,
        name: String,
        args: Vec<Expr>,
    },
    Module {
        cpath: Box<Expr>,
        body: Box<Expr>,
    },
    Errored,
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
