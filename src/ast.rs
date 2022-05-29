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
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum ExprKind {
    Ident { name: String },
    Nil,
    Assign { lhs: Box<Expr>, rhs: Box<Expr> },
    Errored,
}
