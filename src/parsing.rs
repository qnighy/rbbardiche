use crate::ast::{BinaryOp, Expr, ExprKind, Range, UnaryOp};
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
        while !matches!(self.next_token.kind, TokenKind::Eof | TokenKind::RParen) {
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
        self.parse_arg()
    }

    fn parse_arg(&mut self) -> Expr {
        self.parse_ternary_cond()
    }

    // %left modifier_rescue
    // %right '?' ':' /* <------ here */
    // %nonassoc tDOT2 tDOT3 tBDOT2 tBDOT3
    //
    // arg : arg '?' arg opt_nl ':' arg
    //
    /// Parses `e ? e : e`, and higher.
    fn parse_ternary_cond(&mut self) -> Expr {
        let expr = self.parse_range(false);
        if matches!(self.next_token.kind, TokenKind::Question) {
            self.bump(true);
            // This is delimited by `?` and `:` so the full `arg` can come here
            let consequence = self.parse_arg();
            self.parse_opt_nl();
            if matches!(self.next_token.kind, TokenKind::Colon) {
                self.bump(true);
            } else {
                todo!("error handling for missing colon");
            }
            let alternate = self.parse_ternary_cond();
            let range = expr.range | alternate.range;
            Expr {
                kind: ExprKind::TernaryCond {
                    cond: Box::new(expr),
                    consequence: Box::new(consequence),
                    alternate: Box::new(alternate),
                },
                range,
                node_id: 0,
            }
        } else {
            expr
        }
    }

    // %right '?' ':'
    // %nonassoc tDOT2 tDOT3 tBDOT2 tBDOT3 /* <------ here */
    // %left tOROP
    //
    // arg : arg tDOT2 arg
    //     | arg tDOT3 arg
    //     | arg tDOT2
    //     | arg tDOT3
    //     | tBDOT2 arg /* <------ handled in parse_unary */
    //     | tBDOT3 arg /* <------ handled in parse_unary */
    //
    /// Parses `e .. e`, `e ... e`, `e ..`, `e ...`, and higher.
    fn parse_range(&mut self, mut chaining: bool) -> Expr {
        let mut expr = self.parse_logical_or();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::RangeIncl | BinaryOp::RangeExcl))
            {
                op
            } else {
                break;
            };
            let op_token = self.bump(true);
            if chaining {
                self.errors.push(ParseError::ChainedRange {
                    range: op_token.range,
                });
            }
            // TODO: parse endless ranges (like `42..`)
            let rhs = self.parse_logical_or();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
            chaining = true;
        }
        expr
    }

    // %nonassoc tDOT2 tDOT3 tBDOT2 tBDOT3
    // %left tOROP /* <------ here */
    // %left tANDOP
    //
    // arg : arg tOROP arg
    //
    /// Parses `e || e`, and higher.
    fn parse_logical_or(&mut self) -> Expr {
        let mut expr = self.parse_logical_and();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LogicalOr))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_logical_and();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left tOROP
    // %left tANDOP /* <------ here */
    // %nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
    //
    // arg : arg tANDOP arg
    //
    /// Parses `e && e`, and higher.
    fn parse_logical_and(&mut self) -> Expr {
        let mut expr = self.parse_equality();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LogicalAnd))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_equality();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left tANDOP
    // %nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH /* <------ here */
    // %left '>' tGEQ '<' tLEQ
    //
    // arg : arg tCMP arg
    //     | arg tEQ arg
    //     | arg tEQQ arg
    //     | arg tNEQ arg
    //     | arg tMATCH arg
    //     | arg tNMATCH arg
    //
    /// Parses `e <=> e`, `e == e`, `e === e`, `e != e`, `e =~ e`, `e !~ e`, and higher.
    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_inequality();
        let mut chaining = false;
        loop {
            let op = if let Some(op) = self.next_token.to_binop(|op| {
                matches!(
                    op,
                    BinaryOp::Cmp
                        | BinaryOp::Eq
                        | BinaryOp::Eqq
                        | BinaryOp::NEq
                        | BinaryOp::Match
                        | BinaryOp::NMatch
                )
            }) {
                op
            } else {
                break;
            };
            let op_token = self.bump(true);
            if chaining {
                self.errors.push(ParseError::ChainedEquality {
                    range: op_token.range,
                });
            }
            let rhs = self.parse_inequality();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
            chaining = true;
        }
        expr
    }

    // %nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
    // %left '>' tGEQ '<' tLEQ /* <------ here */
    // %left '|' '^'
    //
    // arg : rel_expr
    // relop : '>'
    //       | '<'
    //       | tGEQ
    //       | tLEQ
    // rel_expr : arg relop arg  %prec '>'
    //          | rel_expr relop arg  %prec '>' /* warned against */
    //
    /// Parses `e > e`, `e >= e`, `e < e`, `e <= e`, and higher.
    fn parse_inequality(&mut self) -> Expr {
        let mut expr = self.parse_bitwise_or();
        let mut chaining = false;
        loop {
            let op = if let Some(op) = self.next_token.to_binop(|op| {
                matches!(
                    op,
                    BinaryOp::Gt | BinaryOp::GtEq | BinaryOp::Lt | BinaryOp::LtEq
                )
            }) {
                op
            } else {
                break;
            };
            let op_token = self.bump(true);
            if chaining {
                self.errors.push(ParseError::ChainedInequality {
                    range: op_token.range,
                });
            }
            let rhs = self.parse_bitwise_or();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
            chaining = true;
        }
        expr
    }

    // %left '>' tGEQ '<' tLEQ
    // %left '|' '^' /* <------ here */
    // %left '&'
    //
    // arg : arg '|' arg
    //     | arg '^' arg
    //
    /// Parses `e | e`, `e ^ e`, and higher.
    fn parse_bitwise_or(&mut self) -> Expr {
        let mut expr = self.parse_bitwise_and();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::BitwiseOr | BinaryOp::BitwiseXor))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_bitwise_and();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left '|' '^'
    // %left '&' /* <------ here */
    // %left tLSHFT tRSHFT
    //
    // arg : arg '&' arg
    //
    /// Parses `e & e` and higher.
    fn parse_bitwise_and(&mut self) -> Expr {
        let mut expr = self.parse_shift();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::BitwiseAnd))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_shift();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left '&'
    // %left tLSHFT tRSHFT /* <------ here */
    // %left '+' '-'
    //
    // arg : arg tLSHFT arg
    //     | arg tRSHFT arg
    //
    /// Parses `e << e`, `e >> e`, and higher.
    fn parse_shift(&mut self) -> Expr {
        let mut expr = self.parse_additive();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LShift | BinaryOp::RShift))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_additive();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left tLSHFT tRSHFT
    // %left '+' '-' /* <------ here */
    // %left '*' '/' '%'
    //
    // arg : arg '+' arg
    //     | arg '-' arg
    //
    /// Parses `e + e`, `e - e`, and higher.
    fn parse_additive(&mut self) -> Expr {
        let mut expr = self.parse_multiplicative();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::Add | BinaryOp::Sub))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_multiplicative();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %left '+' '-'
    // %left '*' '/' '%' /* <------ here */
    // %right tUMINUS_NUM tUMINUS
    //
    // arg : arg '*' arg
    //     | arg '/' arg
    //     | arg '%' arg
    //
    /// Parses `e * e`, `e / e`, `e % e`, and higher.
    fn parse_multiplicative(&mut self) -> Expr {
        let mut expr = self.parse_pow();
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod))
            {
                op
            } else {
                break;
            };
            self.bump(true);
            let rhs = self.parse_pow();
            let range = expr.range | rhs.range;
            expr = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                },
                range,
                node_id: 0,
            };
        }
        expr
    }

    // %right tUMINUS_NUM tUMINUS
    // %right tPOW /* <------ here */
    // %right '!' '~' tUPLUS
    //
    // arg : arg tPOW arg
    //
    /// Parses `e ** e` and higher.
    fn parse_pow(&mut self) -> Expr {
        let expr = self.parse_unary();
        let _op = if let Some(op) = self.next_token.to_binop(|op| matches!(op, BinaryOp::Pow)) {
            op
        } else {
            return expr;
        };
        self.bump(true);
        let rhs = self.parse_pow();
        match self.decompose_uminus(expr) {
            Ok((uminus_range, expr)) => {
                // Special case for `-e ** e`
                // In this case, we reinterpret `(-e) ** e` as `-(e ** e)`.
                let pow_range = expr.range | rhs.range;
                let range = uminus_range | expr.range;
                Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr {
                            kind: ExprKind::Binary {
                                lhs: Box::new(expr),
                                op: BinaryOp::Pow,
                                rhs: Box::new(rhs),
                            },
                            range: pow_range,
                            node_id: 0,
                        }),
                    },
                    range,
                    node_id: 0,
                }
            }
            Err(expr) => {
                let range = expr.range | rhs.range;
                Expr {
                    kind: ExprKind::Binary {
                        lhs: Box::new(expr),
                        op: BinaryOp::Pow,
                        rhs: Box::new(rhs),
                    },
                    range,
                    node_id: 0,
                }
            }
        }
    }

    /// Decomposes `-x` into `-` and `x`.
    /// Handles negative literals like `-123` as well.
    fn decompose_uminus(&self, expr: Expr) -> Result<(Range, Expr), Expr> {
        match expr {
            Expr {
                kind:
                    ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr,
                    },
                range,
                ..
            } => Ok((Range(range.0, range.0 + 1), *expr)),
            Expr {
                kind: ExprKind::Numeric { numval },
                range,
                ..
            } if self.source.get(range.0).copied() == Some(b'-') => Ok((
                Range(range.0, range.0 + 1),
                Expr {
                    kind: ExprKind::Numeric { numval: -numval },
                    range: Range(range.0 + 1, range.1),
                    node_id: 0,
                },
            )),
            _ => Err(expr),
        }
    }

    // %left '?' ':'
    // %nonassoc tDOT2 tDOT3 tBDOT2 tBDOT3 /* <------ here as well */
    // %left tOROP
    //
    // %left '*' '/' '%'
    // %right tUMINUS_NUM tUMINUS /* <------ here as well */
    // %right tPOW
    // %right '!' '~' tUPLUS /* <------ here */
    // %token tLAST_TOKEN
    //
    // arg : tBDOT2 arg
    //     | tBDOT3 arg
    //     | tUPLUS arg
    //     | tUMINUS arg
    //     | '!' arg
    //     | '~' arg
    //
    /// Parses `+e`, `-e`, `!e`, `~e`, and higher.
    fn parse_unary(&mut self) -> Expr {
        let op = if let Some(op) = self.next_token.to_unop(|op| {
            matches!(
                op,
                UnaryOp::RangeIncl
                    | UnaryOp::RangeExcl
                    | UnaryOp::Plus
                    | UnaryOp::Neg
                    | UnaryOp::Not
                    | UnaryOp::BitwiseNot
            )
        }) {
            op
        } else {
            return self.parse_primary();
        };
        let op_token = self.bump(true);
        let expr = if matches!(op, UnaryOp::RangeIncl | UnaryOp::RangeExcl) {
            // Within these rules, shift takes precedence over reduce
            // E.g. `..1 + 2` is interpreted as being `..(1 + 2)`
            self.parse_range(true)
        } else {
            self.parse_unary()
        };
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
            TokenKind::LParenBeg => {
                let lparen_token = self.bump(true);
                let stmts = self.parse_compstmt();
                if !matches!(self.next_token.kind, TokenKind::RParen) {
                    todo!(
                        "error recovery on unmatched parentheses: {:?}",
                        self.next_token
                    );
                }
                let rparen_token = self.bump(false);
                let range = lparen_token.range | rparen_token.range;
                Expr {
                    kind: ExprKind::Parenthesized { stmts },
                    range,
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

    fn parse_opt_nl(&mut self) {
        if matches!(self.next_token.kind, TokenKind::NewLine) {
            self.bump(true);
        }
    }
}
