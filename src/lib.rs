use bstr::BString;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum Node {
    Nil,
}

pub fn parse(source: &[u8]) -> Node {
    let parser = Parser::new(source);
    Node::Nil
}

#[derive(Debug)]
struct Parser {
    source: BString,
    pos: usize,
}

impl Parser {
    fn new(source: &[u8]) -> Parser {
        Parser {
            source: source.into(),
            pos: 0,
        }
    }
}
