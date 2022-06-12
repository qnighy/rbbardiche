use crate::parser_diagnostics::ParseError;
use crate::token::Token;

use bstr::BString;

#[derive(Debug)]
pub(crate) struct Parser {
    pub(crate) source: BString,
    pub(crate) pos: usize,
    pub(crate) next_token: Token,
    pub(crate) errors: Vec<ParseError>,
}
