use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Range(pub usize, pub usize);

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
    Errored,
}
