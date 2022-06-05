use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Range(pub usize, pub usize);

impl std::ops::BitOr for Range {
    type Output = Range;

    fn bitor(self, rhs: Self) -> Self::Output {
        Range(std::cmp::min(self.0, rhs.0), std::cmp::max(self.1, rhs.1))
    }
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct Expr {
    #[serde(flatten)]
    pub kind: ExprKind,
    pub range: Range,
    #[serde(skip_serializing_if = "is_zero")]
    pub node_id: usize,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum ExprKind {
    Compound {
        stmts: Vec<Expr>,
    },
    Ident {
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
    Errored,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
pub enum PostfixUnaryOp {
    /// `..`
    RangeIncl,
    /// `...`
    RangeExcl,
}

fn is_zero<T: PartialEq + Default>(x: &T) -> bool {
    *x == T::default()
}
