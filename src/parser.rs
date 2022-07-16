use crate::ast::Program;
use crate::parser::token::Token;
use crate::parser_diagnostics::ParseError;

use bstr::BString;

mod lexer;
mod parser_impl;
mod token;

pub fn parse(source: &[u8]) -> (Program, Vec<ParseError>) {
    let mut parser = Parser::new(source);
    let program = parser.parse_program();
    (program, parser.errors)
}

#[derive(Debug)]
pub(self) struct Parser {
    pub(self) source: BString,
    pub(self) pos: usize,
    pub(self) next_token: Token,
    pub(self) errors: Vec<ParseError>,
}
