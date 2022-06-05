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
    #[error("Unexpected chained range operator")]
    ChainedRange { range: Range },
    #[error("Unexpected chained comparison")]
    ChainedEquality { range: Range },
    #[error("comparison after comparison")]
    ChainedInequality { range: Range },
}

impl ParseError {
    pub fn range(&self) -> Range {
        use ParseError::*;
        match self {
            UnexpectedToken { range } => *range,
            UnexpectedEof { range } => *range,
            TrailingUnderscore { range } => *range,
            DoubleUnderscore { range } => *range,
            ChainedRange { range } => *range,
            ChainedEquality { range } => *range,
            ChainedInequality { range } => *range,
        }
    }

    pub fn is_error(&self) -> bool {
        use ParseError::*;
        match self {
            UnexpectedToken { .. }
            | UnexpectedEof { .. }
            | TrailingUnderscore { .. }
            | DoubleUnderscore { .. }
            | ChainedRange { .. }
            | ChainedEquality { .. } => true,
            ChainedInequality { .. } => false,
        }
    }
}
