use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Range(pub usize, pub usize);

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct Node {
    #[serde(flatten)]
    pub kind: NodeKind,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum NodeKind {
    Ident { name: String },
    Nil,
    Errored,
}
