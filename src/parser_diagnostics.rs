use thiserror::Error;

use crate::ast::Range;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token")]
    UnexpectedToken { range: Range },
    #[error("Unexpected end of file")]
    UnexpectedEof { range: Range },
    #[error("Trailing `_' in number")]
    TrailingUnderscore { range: Range },
    #[error("Consecutive `__' in number")]
    DoubleUnderscore { range: Range },
}

impl ParseError {
    pub fn range(&self) -> Range {
        use ParseError::*;
        match self {
            UnexpectedToken { range } => *range,
            UnexpectedEof { range } => *range,
            TrailingUnderscore { range } => *range,
            DoubleUnderscore { range } => *range,
        }
    }
}
