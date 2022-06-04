use crate::ast::{Expr, ExprKind, Range, UnaryOp};
use crate::lexing::{Token, TokenKind};
use crate::parser::Parser;
use crate::parser_diagnostics::ParseError;

pub fn parse(source: &[u8]) -> (Expr, Vec<ParseError>) {
    let mut parser = Parser::new(source);
    let program = parser.parse_program();
    (program, parser.errors)
}

impl Parser {
    fn new(source: &[u8]) -> Parser {
        let mut parser = Parser {
            source: source.into(),
            pos: 0,
            // dummy value
            next_token: Token {
                kind: TokenKind::Eof,
                range: Range(0, 0),
            },
            errors: vec![],
        };
        parser.bump(true);
        parser
    }

    fn parse_program(&mut self) -> Expr {
        let stmts = self.parse_compstmt();
        if stmts.len() == 1 {
            let mut stmts = stmts;
            stmts.pop().unwrap()
        } else {
            Expr {
                kind: ExprKind::Compound { stmts },
                range: Range(0, self.source.len()),
                node_id: 0,
            }
        }
    }

    fn parse_compstmt(&mut self) -> Vec<Expr> {
        while matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
            self.bump(true);
        }
        let mut stmts = Vec::new();
        while match self.next_token.kind {
            TokenKind::Semi | TokenKind::Eof => false,
            _ => true,
        } {
            stmts.push(self.parse_stmt());
            while matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                self.bump(true);
            }
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Expr {
        let mut e = self.parse_expr();
        loop {
            match &self.next_token.kind {
                TokenKind::Equal => {
                    self.bump(true);
                    let rhs = self.parse_expr();
                    let range = e.range | rhs.range;
                    e = Expr {
                        kind: ExprKind::Assign {
                            lhs: Box::new(e),
                            rhs: Box::new(rhs),
                        },
                        range,
                        node_id: 0,
                    };
                }
                _ => break,
            }
        }
        e
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_unary()
    }

    // %right tPOW
    // %right '!' '~' tUPLUS /* here */
    // %token tLAST_TOKEN
    //
    // arg : tUPLUS arg
    //     | '!' arg
    //     | '~' arg
    fn parse_unary(&mut self) -> Expr {
        let op = match &self.next_token.kind {
            TokenKind::UPlus => UnaryOp::Plus,
            TokenKind::Excl => UnaryOp::Not,
            TokenKind::Tilde => UnaryOp::BitwiseNot,
            _ => return self.parse_primary(),
        };
        let op_token = self.bump(true);
        let expr = self.parse_unary();
        let range = op_token.range | expr.range;
        Expr {
            kind: ExprKind::Unary {
                op,
                expr: Box::new(expr),
            },
            range,
            node_id: 0,
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match &self.next_token.kind {
            TokenKind::Ident(name) => {
                let name = name.to_string();
                let token = self.bump(false);
                Expr {
                    kind: ExprKind::Ident { name },
                    range: token.range,
                    node_id: 0,
                }
            }
            TokenKind::NilKeyword => {
                let token = self.bump(false);
                Expr {
                    kind: ExprKind::Nil,
                    range: token.range,
                    node_id: 0,
                }
            }
            TokenKind::Numeric(numval) => {
                let numval = *numval;
                let token = self.bump(false);
                Expr {
                    kind: ExprKind::Numeric { numval },
                    range: token.range,
                    node_id: 0,
                }
            }
            TokenKind::Eof => {
                self.errors.push(ParseError::UnexpectedEof {
                    range: self.next_token.range,
                });
                Expr {
                    kind: ExprKind::Errored,
                    range: self.next_token.range,
                    node_id: 0,
                }
            }
            _ => {
                let token = self.bump(false);
                self.errors
                    .push(ParseError::UnexpectedToken { range: token.range });
                Expr {
                    kind: ExprKind::Errored,
                    range: token.range,
                    node_id: 0,
                }
            }
        }
    }
}
