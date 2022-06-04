use thiserror::Error;

use crate::ast::Range;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token")]
    UnexpectedToken { range: Range },
    #[error("Unexpected end of file")]
    UnexpectedEof { range: Range },
}
