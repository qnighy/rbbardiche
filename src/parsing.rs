use bstr::ByteSlice;

use crate::ast::{
    self, Arg, Args, ArrayExpr, BinaryOp, CommandArgs, Debri, DefnExpr, DelimitedArg,
    DelimitedFArg, EmptyStmt, Expr, ExprStmt, FArg, FArgs, FileMetaName, NilExpr, NodeMeta,
    ParenArgs, ParenFArgs, Program, Range, RangeType, Stmt, SuperclassClause, UnaryOp,
};
use crate::lexing::{LexerBeginMode, LexerMode, LexerParams, StringLexerMode};
use crate::parser::Parser;
use crate::parser_diagnostics::ParseError;
use crate::token::{IdentType, StringType, Token, TokenClass, TokenKind};

pub fn parse(source: &[u8]) -> (Program, Vec<ParseError>) {
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
        parser.bump(LexCtx::default().beg());
        parser
    }

    fn parse_program(&mut self) -> Program {
        let stmts = self.parse_compstmt(|token| matches!(token.kind, TokenKind::Eof));
        Program {
            stmts,
            meta: NodeMeta {
                range: Range(0, self.source.len()),
                node_id: 0,
            },
        }
    }

    fn parse_compstmt_<F>(&mut self, is_end_token: F) -> Expr
    where
        F: Fn(&Token) -> bool,
    {
        let stmts = self.parse_compstmt(is_end_token);
        let range = if stmts.is_empty() {
            // TODO: empty range?
            Range(self.next_token.range.0, self.next_token.range.0)
        } else {
            stmts.first().unwrap().range() | stmts.last().unwrap().range()
        };
        ast::CompoundExpr {
            stmts,
            meta: NodeMeta { range, node_id: 0 },
        }
        .into()
    }

    fn parse_compstmt<F>(&mut self, is_end_token: F) -> Vec<Stmt>
    where
        F: Fn(&Token) -> bool,
    {
        let mut stmts = Vec::new();
        while !is_end_token(&self.next_token) {
            if matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                let delim = self.bump(LexCtx::default().beg());
                let range = delim.range;
                stmts.push(Stmt::Empty(EmptyStmt {
                    delim,
                    meta: NodeMeta { range, node_id: 0 },
                }));
                continue;
            }
            let expr = self.parse_stmt();
            let debris = self.skip_debris(|token| {
                matches!(token.kind, TokenKind::Semi | TokenKind::NewLine) || is_end_token(token)
            });
            let delim = if matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                Some(self.bump(LexCtx::default().beg()))
            } else {
                None
            };
            let range = if let Some(delim) = &delim {
                expr.range() | delim.range
            } else {
                expr.range()
            };
            stmts.push(Stmt::Expr(ExprStmt {
                expr,
                debris,
                delim,
                meta: NodeMeta { range, node_id: 0 },
            }));
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Expr {
        let mut e = self.parse_expr(LexCtx::default());
        loop {
            match &self.next_token.kind {
                TokenKind::Equal => {
                    self.bump(LexCtx::default().beg());
                    let rhs = self.parse_expr(LexCtx::default());
                    let range = e.range() | rhs.range();
                    e = ast::AssignExpr {
                        lhs: Box::new(e),
                        rhs: Box::new(rhs),
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into();
                }
                _ => break,
            }
        }
        e
    }

    // expr : command_call
    //      | expr keyword_and expr
    //      | expr keyword_or expr
    //      | keyword_not opt_nl expr
    //      | '!' command_call
    //      | arg tASSOC p_top_expr_body
    //      | arg keyword_in p_top_expr_body
    //      | arg  %prec tLBRACE_ARG
    fn parse_expr(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_arg_expr(ctx);

        if starts_arg(&self.next_token) {
            if let Some(args_hole) = command_head(&mut expr) {
                let args = self.parse_command_args(ctx);
                *args_hole = Some(Args::Command(args));
            }
        }
        expr
    }

    fn parse_arg_expr(&mut self, ctx: LexCtx) -> Expr {
        self.parse_ternary_cond(ctx)
    }

    // %left modifier_rescue
    // %right '?' ':' /* <------ here */
    // %nonassoc tDOT2 tDOT3 tBDOT2 tBDOT3
    //
    // arg : arg '?' arg opt_nl ':' arg
    //
    /// Parses `e ? e : e`, and higher.
    fn parse_ternary_cond(&mut self, ctx: LexCtx) -> Expr {
        let expr = self.parse_range(ctx, false);
        if matches!(self.next_token.kind, TokenKind::Question) {
            self.bump(ctx.beg());
            // This is delimited by `?` and `:` so the full `arg` can come here
            let consequence = self.parse_arg_expr(ctx);
            self.parse_opt_nl(ctx);
            if matches!(self.next_token.kind, TokenKind::Colon) {
                self.bump(ctx.beg());
            } else {
                todo!("error handling for missing colon");
            }
            let alternate = self.parse_ternary_cond(ctx);
            let range = expr.range() | alternate.range();
            ast::TernaryCondExpr {
                cond: Box::new(expr),
                consequence: Box::new(consequence),
                alternate: Box::new(alternate),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into()
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
    fn parse_range(&mut self, ctx: LexCtx, mut chaining: bool) -> Expr {
        let mut expr = self.parse_logical_or(ctx);
        loop {
            let range_type = match self.next_token.kind {
                TokenKind::Dot2Infix => RangeType::Inclusive,
                TokenKind::Dot3Infix => RangeType::Exclusive,
                _ => break,
            };
            let op_token = self.bump(ctx.beg());
            if chaining {
                self.errors.push(ParseError::ChainedRange {
                    range: op_token.range,
                });
            }
            // TODO: parse endless ranges (like `42..`)
            let rhs = self.parse_logical_or(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::RangeExpr {
                begin: Some(Box::new(expr)),
                range_type,
                end: Some(Box::new(rhs)),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_logical_or(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_logical_and(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LogicalOr))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_logical_and(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_logical_and(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_equality(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LogicalAnd))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_equality(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_equality(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_inequality(ctx);
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
            let op_token = self.bump(ctx.beg());
            if chaining {
                self.errors.push(ParseError::ChainedEquality {
                    range: op_token.range,
                });
            }
            let rhs = self.parse_inequality(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_inequality(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_bitwise_or(ctx);
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
            let op_token = self.bump(ctx.beg());
            if chaining {
                self.errors.push(ParseError::ChainedInequality {
                    range: op_token.range,
                });
            }
            let rhs = self.parse_bitwise_or(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_bitwise_or(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_bitwise_and(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::BitwiseOr | BinaryOp::BitwiseXor))
            {
                op
            } else {
                break;
            };
            self.bump(if matches!(op, BinaryOp::BitwiseOr) {
                ctx.beg_labelable()
            } else {
                ctx.beg()
            });
            let rhs = self.parse_bitwise_and(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_bitwise_and(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_shift(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::BitwiseAnd))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_shift(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_shift(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_additive(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::LShift | BinaryOp::RShift))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_additive(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_additive(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_multiplicative(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::Add | BinaryOp::Sub))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_multiplicative(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_multiplicative(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_pow(ctx);
        loop {
            let op = if let Some(op) = self
                .next_token
                .to_binop(|op| matches!(op, BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod))
            {
                op
            } else {
                break;
            };
            self.bump(ctx.beg());
            let rhs = self.parse_pow(ctx);
            let range = expr.range() | rhs.range();
            expr = ast::BinaryExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
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
    fn parse_pow(&mut self, ctx: LexCtx) -> Expr {
        let expr = self.parse_unary(ctx);
        let _op = if let Some(op) = self.next_token.to_binop(|op| matches!(op, BinaryOp::Pow)) {
            op
        } else {
            return expr;
        };
        self.bump(ctx.beg());
        let rhs = self.parse_pow(ctx);
        match self.decompose_uminus(expr) {
            Ok((uminus_range, expr)) => {
                // Special case for `-e ** e`
                // In this case, we reinterpret `(-e) ** e` as `-(e ** e)`.
                let pow_range = expr.range() | rhs.range();
                let range = uminus_range | expr.range();
                ast::UnaryExpr {
                    op: UnaryOp::Neg,
                    expr: Box::new(
                        ast::BinaryExpr {
                            lhs: Box::new(expr),
                            op: BinaryOp::Pow,
                            rhs: Box::new(rhs),
                            meta: NodeMeta {
                                range: pow_range,
                                node_id: 0,
                            },
                        }
                        .into(),
                    ),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            Err(expr) => {
                let range = expr.range() | rhs.range();
                ast::BinaryExpr {
                    lhs: Box::new(expr),
                    op: BinaryOp::Pow,
                    rhs: Box::new(rhs),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
        }
    }

    /// Decomposes `-x` into `-` and `x`.
    /// Handles negative literals like `-123` as well.
    fn decompose_uminus(&self, expr: Expr) -> Result<(Range, Expr), Expr> {
        match expr {
            Expr::Unary(ast::UnaryExpr {
                op: UnaryOp::Neg,
                expr,
                meta: NodeMeta { range, .. },
                ..
            }) => Ok((Range(range.0, range.0 + 1), *expr)),
            Expr::Numeric(ast::NumericExpr {
                numval,
                meta: NodeMeta { range, .. },
                ..
            }) if self.source.get(range.0).copied() == Some(b'-') => Ok((
                Range(range.0, range.0 + 1),
                ast::NumericExpr {
                    numval: -numval,
                    meta: NodeMeta {
                        range: Range(range.0 + 1, range.1),
                        node_id: 0,
                    },
                }
                .into(),
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
    fn parse_unary(&mut self, ctx: LexCtx) -> Expr {
        let range_type = match self.next_token.kind {
            TokenKind::Dot2Prefix => Some(RangeType::Inclusive),
            TokenKind::Dot3Prefix => Some(RangeType::Exclusive),
            _ => None,
        };
        if let Some(range_type) = range_type {
            let op_token = self.bump(ctx.beg());
            // Within these rules, shift takes precedence over reduce
            // E.g. `..1 + 2` is interpreted as being `..(1 + 2)`
            let expr = self.parse_range(ctx, true);
            let range = op_token.range | expr.range();
            return ast::RangeExpr {
                begin: None,
                range_type,
                end: Some(Box::new(expr)),
                meta: NodeMeta { range, node_id: 0 },
            }
            .into();
        }

        let op = if let Some(op) = self.next_token.to_unop(|op| {
            matches!(op, |UnaryOp::Plus| UnaryOp::Neg
                | UnaryOp::Not
                | UnaryOp::BitwiseNot)
        }) {
            op
        } else {
            return self.parse_primary(ctx);
        };
        let op_token = self.bump(ctx.beg());
        let expr = self.parse_unary(ctx);
        let range = op_token.range | expr.range();
        ast::UnaryExpr {
            op,
            expr: Box::new(expr),
            meta: NodeMeta { range, node_id: 0 },
        }
        .into()
    }

    // primary : tLBRACK aref_args ']'
    // aref_args : none
    //           | args trailer
    //           | args ',' assocs trailer
    //           | assocs trailer
    /// Parses an array literal like `[]`.
    fn parse_array(&mut self, ctx: LexCtx) -> ArrayExpr {
        assert!(matches!(self.next_token.kind, TokenKind::LBrackBeg));
        let lbrack_token = self.bump(ctx.beg_labelable());
        let mut list = Vec::new();
        while !matches!(
            self.next_token.kind,
            TokenKind::Eof | TokenKind::KeywordEnd | TokenKind::Semi | TokenKind::RBrack
        ) {
            let arg = self.parse_arg_elem(ctx);

            let debris = self.skip_debris(|token| {
                matches!(
                    token.kind,
                    TokenKind::Eof
                        | TokenKind::KeywordEnd
                        | TokenKind::Semi
                        | TokenKind::RBrack
                        | TokenKind::Comma
                        | TokenKind::NewLine
                )
            });

            // TODO: the condition below should exclude block args
            let delim = if matches!(self.next_token.kind, TokenKind::Comma | TokenKind::NewLine) {
                Some(
                    self.bump(if matches!(self.next_token.kind, TokenKind::Comma) {
                        ctx.beg_labelable()
                    } else {
                        ctx.beg()
                    }),
                )
            } else {
                None
            };
            let range = if let Some(delim) = &delim {
                arg.range() | delim.range
            } else {
                arg.range()
            };
            list.push(DelimitedArg {
                arg,
                debris,
                delim,
                meta: NodeMeta { range, node_id: 0 },
            })
        }
        if !matches!(self.next_token.kind, TokenKind::RBrack) {
            todo!("error recovery in aref_args");
        }
        // TODO: handle opt_nl
        let rbrack_token = self.bump(ctx.end());
        let range = lbrack_token.range | rbrack_token.range;
        // TODO: check invalid arguments (like `[&block]`)
        ArrayExpr {
            open_token: lbrack_token,
            list,
            meta: NodeMeta { range, node_id: 0 },
            close_token: Some(rbrack_token),
        }
    }

    // paren_args : '(' opt_call_args rparen
    //            | '(' args ',' args_forward rparen
    //            | '(' args_forward rparen
    /// Parses a parenthesized argument list like `(42, 80)` as in `foo(42, 80)`.
    fn parse_paren_args(&mut self, ctx: LexCtx) -> Args {
        assert!(matches!(self.next_token.kind, TokenKind::LParenCall));
        let lparen_token = self.bump(ctx.beg_labelable());
        let mut list = Vec::new();
        while !matches!(
            self.next_token.kind,
            TokenKind::Eof | TokenKind::KeywordEnd | TokenKind::Semi | TokenKind::RParen
        ) {
            let arg = self.parse_arg_elem(ctx);

            let debris = self.skip_debris(|token| {
                matches!(
                    token.kind,
                    TokenKind::Eof
                        | TokenKind::KeywordEnd
                        | TokenKind::Semi
                        | TokenKind::RParen
                        | TokenKind::Comma
                )
            });

            // TODO: the condition below should exclude block args
            let delim = if matches!(self.next_token.kind, TokenKind::Comma) {
                Some(self.bump(ctx.beg_labelable()))
            } else {
                None
            };
            let range = if let Some(delim) = &delim {
                arg.range() | delim.range
            } else {
                arg.range()
            };
            list.push(DelimitedArg {
                arg,
                debris,
                delim,
                meta: NodeMeta { range, node_id: 0 },
            })
        }
        if !matches!(self.next_token.kind, TokenKind::RParen) {
            todo!("error recovery in paren_args");
        }
        // TODO: handle opt_nl
        let rparen_token = self.bump(ctx.end());
        let range = lparen_token.range | rparen_token.range;
        Args::Paren(ParenArgs {
            open_token: lparen_token,
            list,
            meta: NodeMeta { range, node_id: 0 },
            close_token: Some(rparen_token),
        })
    }

    /// Parses a non-parenthesized argument list.
    fn parse_command_args(&mut self, ctx: LexCtx) -> CommandArgs {
        let ctx = LexCtx {
            in_command_args: true,
            ..ctx
        };
        let mut list = Vec::new();
        loop {
            let arg = self.parse_arg_elem(ctx);

            // TODO: the condition below should exclude block args
            let delim = if matches!(self.next_token.kind, TokenKind::Comma) {
                Some(self.bump(ctx.beg_labelable()))
            } else {
                None
            };
            let has_delim = delim.is_some();
            let range = if let Some(delim) = &delim {
                arg.range() | delim.range
            } else {
                arg.range()
            };
            list.push(DelimitedArg {
                arg,
                debris: vec![],
                delim,
                meta: NodeMeta { range, node_id: 0 },
            });
            if !has_delim {
                break;
            }
        }
        let range = list.first().unwrap().range() | list.last().unwrap().range();
        CommandArgs {
            list,
            meta: NodeMeta { range, node_id: 0 },
        }
    }

    fn parse_arg_elem(&mut self, ctx: LexCtx) -> Arg {
        // TODO: argument splat `f(*a)`
        // if matches!(self.next_token.kind, TokenKind::Star) {}
        // TODO: kwargs splat `f(**options)`
        // if matches!(self.next_token.kind, TokenKind::DStar) {}
        // TODO: label assoc `f(foo: bar)`
        // if matches!(self.next_token.kind, TokenKind::Label) {}
        // TODO: block arg `f(&block)`
        // if matches!(self.next_token.kind, TokenKind::Amper) {}

        // TODO: arg_value check `f(foo => bar)`
        let expr = self.parse_arg_expr(ctx);

        // TODO: assoc arg
        // if matches!(self.next_token.kind, TokenKind::Assoc) {}

        // TODO: command in call_args `f(g h)`
        // if list.is_empty() && is_eligible_for_command(&expr) && is_beginning {}
        Arg::Simple(expr)
    }

    // primary : primary_value tCOLON2 tCONSTANT
    //
    /// Parses a primary expression. Primary expressions are those enclosed with
    /// delimiters from both sides, with a few exceptions.
    ///
    /// These are the exceptions (which are directly parsed in this `parse_primary`):
    ///
    /// - `foo::Bar`
    /// - `foo.bar`
    fn parse_primary(&mut self, ctx: LexCtx) -> Expr {
        let mut expr = self.parse_primary_inner(ctx);
        loop {
            match &self.next_token.kind {
                // primary : primary_value tCOLON2 tCONSTANT
                //         | method_call
                // method_call : primary_value call_op operation2 opt_paren_args
                //             | primary_value tCOLON2 operation2 paren_args
                //             | primary_value tCOLON2 operation3
                //             | primary_value call_op paren_args
                //             | primary_value tCOLON2 paren_args
                TokenKind::Dot | TokenKind::AndDot | TokenKind::Colon2Infix => {
                    // TODO: handle primary_value condition
                    let op_token = self.bump(ctx.beg());
                    match &self.next_token.kind {
                        TokenKind::Ident(ident_type, name) => {
                            let ident_type = *ident_type;
                            let is_dcolon = matches!(op_token.kind, TokenKind::Colon2Infix);
                            let name = name.to_string();
                            let token = self.bump(ctx.with_mode(LexerMode::Arg));
                            let range = expr.range() | token.range;
                            if is_dcolon && ident_type == IdentType::Const {
                                let e = ast::ConstExpr {
                                    toplevel: false,
                                    recv: Some(Box::new(expr)),
                                    name,
                                    meta: NodeMeta { range, node_id: 0 },
                                };
                                expr = if matches!(self.next_token.kind, TokenKind::LParenCall) {
                                    let mut e = e.convert_to_send();
                                    let args = self.parse_paren_args(ctx);
                                    e.set_args(args);
                                    e.into()
                                } else {
                                    e.into()
                                };
                            } else {
                                let mut e = ast::SendExpr {
                                    optional: matches!(op_token.kind, TokenKind::AndDot),
                                    recv: Some(Box::new(expr)),
                                    args: None,
                                    name,
                                    meta: NodeMeta { range, node_id: 0 },
                                };
                                if matches!(self.next_token.kind, TokenKind::LParenCall) {
                                    let args = self.parse_paren_args(ctx);
                                    e.set_args(args);
                                }
                                expr = e.into();
                            }
                        }
                        TokenKind::LParenBeg => todo!("proc call expression"),
                        _ => {
                            self.errors.push(ParseError::UnexpectedToken {
                                range: self.next_token.range,
                            });
                            let range = expr.range() | op_token.range;
                            expr = ast::ConstExpr {
                                toplevel: false,
                                recv: Some(Box::new(expr)),
                                name: "".to_owned(),
                                meta: NodeMeta { range, node_id: 0 },
                            }
                            .into();
                        }
                    }
                }
                _ => break,
            }
        }
        expr
    }

    /// Parses a primary expression, with a few exceptions (which are parsed in `parse_primary`)
    fn parse_primary_inner(&mut self, ctx: LexCtx) -> Expr {
        match &self.next_token.kind {
            // primary : strings
            // strings : string
            // string : ...
            //        | string1
            //        | string string1
            // string1 : tSTRING_BEG string_contents tSTRING_END
            TokenKind::StringBegin(string_type) => {
                let mode = match string_type {
                    StringType::DQuote => StringLexerMode::DoubleQuoted,
                    StringType::SQuote => StringLexerMode::SingleQuoted,
                };
                let beg_token = self.bump(ctx.with_mode(LexerMode::String(mode)));
                let mut contents = Vec::new();
                loop {
                    if matches!(self.next_token.kind, TokenKind::StringEnd) {
                        break;
                    }
                    if let TokenKind::StringContent(content) = &self.next_token.kind {
                        contents.push(content.clone());
                        self.bump(ctx.with_mode(LexerMode::String(mode)));
                    } else {
                        unreachable!();
                    }
                }
                let end_token = self.bump(ctx.end());
                let range = beg_token.range | end_token.range;
                // TODO: multiple quoted parts (like `"foo" "bar"`)
                ast::StringLiteralExpr {
                    strval: contents.join(""),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : var_ref
            //         | method_call
            // method_call : fcall paren_args
            // var_ref : user_variable
            // user_variable : tIDENTIFIER
            TokenKind::Ident(IdentType::Ident | IdentType::FIdent, name) => {
                let name = name.to_string();
                // TODO: check if it is lvar
                let token = self.bump(ctx.with_mode(LexerMode::Arg));
                let mut e = ast::SendExpr {
                    optional: false,
                    recv: None,
                    name,
                    args: None,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                };
                if matches!(self.next_token.kind, TokenKind::LParenCall) {
                    let args = self.parse_paren_args(ctx);
                    e.set_args(args);
                }
                e.into()
            }
            // primary : var_ref
            //         | method_call
            // method_call : fcall paren_args
            // var_ref : user_variable
            // user_variable : tCONSTANT
            TokenKind::Ident(IdentType::Const, name) => {
                let name = name.to_string();
                let token = self.bump(ctx.with_mode(LexerMode::Arg));
                let e = ast::ConstExpr {
                    toplevel: false,
                    recv: None,
                    name,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                };
                if matches!(self.next_token.kind, TokenKind::LParenCall) {
                    let mut e = e.convert_to_send();
                    let args = self.parse_paren_args(ctx);
                    e.set_args(args);
                    e.into()
                } else {
                    e.into()
                }
            }
            // primary : var_ref
            // var_ref : user_variable
            // user_variable : tGVAR
            TokenKind::Ident(IdentType::GVar, name) => {
                let name = name.to_string();
                let token = self.bump(ctx.end());
                ast::VarExpr {
                    name,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : tCOLON3 tCONSTANT
            TokenKind::Colon2Prefix => {
                let dcolon_token = self.bump(ctx.beg());
                if let TokenKind::Ident(IdentType::Const, name) = &self.next_token.kind {
                    let name = name.to_string();
                    let token = self.bump(ctx.with_mode(LexerMode::Arg));
                    let range = dcolon_token.range | token.range;
                    ast::ConstExpr {
                        toplevel: true,
                        recv: None,
                        name,
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into()
                } else {
                    self.errors.push(ParseError::UnexpectedToken {
                        range: self.next_token.range,
                    });
                    let range = dcolon_token.range;
                    ast::ConstExpr {
                        toplevel: true,
                        recv: None,
                        name: "".to_owned(),
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into()
                }
            }
            // primary : literal
            // literal : numeric
            // numeric : simple_numeric
            //         | tMINUS_NUM simple_numeric %prec tLOWEST
            //
            // Note that tMINUS_NUM is handled in lexer in this parer
            TokenKind::Numeric(numval) => {
                let numval = *numval;
                let token = self.bump(ctx.end());
                ast::NumericExpr {
                    numval,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : literal
            // literal : symbol
            // symbol : ssym
            // ssym : tSYMBEG sym
            TokenKind::SymbolBegin => {
                let open_token = self.bump(ctx.beg());
                if !matches!(self.next_token.kind, TokenKind::Ident(_, _)) {
                    todo!("symbol other than identifier");
                }
                let ident_token = self.bump(ctx.end());
                let value = match &ident_token.kind {
                    TokenKind::Ident(_, name) => name.to_string(),
                    _ => unreachable!(),
                };
                let range = open_token.range | ident_token.range;
                ast::SymbolExpr {
                    open_token,
                    ident_token,
                    value,
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : var_ref
            // var_ref : keyword_variable
            // keyword_variable : keyword_nil
            TokenKind::KeywordNil => {
                let token = self.bump(ctx.end());
                ast::NilExpr {
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : var_ref
            // var_ref : keyword_variable
            // keyword_variable : keyword_self
            TokenKind::KeywordSelf => {
                let token = self.bump(ctx.end());
                ast::SelfExpr {
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : var_ref
            // var_ref : keyword_variable
            // keyword_variable : keyword_true
            //                  | keyword_false
            TokenKind::KeywordTrue | TokenKind::KeywordFalse => {
                let token = self.bump(ctx.end());
                ast::BooleanLiteralExpr {
                    value: matches!(token.kind, TokenKind::KeywordTrue),
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : var_ref
            // var_ref : keyword_variable
            // keyword_variable : keyword_true
            //                  | keyword_false
            TokenKind::KeywordUnderscoreEncoding
            | TokenKind::KeywordUnderscoreFile
            | TokenKind::KeywordUnderscoreLine => {
                let token = self.bump(ctx.end());
                ast::FileMetaExpr {
                    name: match token.kind {
                        TokenKind::KeywordUnderscoreEncoding => FileMetaName::Encoding,
                        TokenKind::KeywordUnderscoreFile => FileMetaName::File,
                        TokenKind::KeywordUnderscoreLine => FileMetaName::Line,
                        _ => unreachable!(),
                    },
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : tLPAREN compstmt ')'
            TokenKind::LParenBeg => {
                let lparen_token = self.bump(ctx.beg_labelable());
                let stmts = self.parse_compstmt(|token| {
                    matches!(
                        token.kind,
                        TokenKind::Eof | TokenKind::RParen | TokenKind::KeywordEnd
                    )
                });
                if !matches!(self.next_token.kind, TokenKind::RParen) {
                    todo!(
                        "error recovery on unmatched parentheses: {:?}",
                        self.next_token
                    );
                }
                let rparen_token = self.bump(ctx.end());
                let range = lparen_token.range | rparen_token.range;
                ast::ParenthesizedExpr {
                    stmts,
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : tLBRACK aref_args ']'
            TokenKind::LBrackBeg => self.parse_array(ctx).into(),
            // primary : tLBRACE assoc_list '}'
            TokenKind::LBraceHash => {
                let open_token = self.bump(ctx.beg_labelable());
                if !matches!(self.next_token.kind, TokenKind::RBrace) {
                    todo!("Hash contents");
                }
                let close_token = self.bump(ctx.end());
                let range = open_token.range | close_token.range;
                ast::HashExpr {
                    open_token,
                    list: vec![],
                    close_token: Some(close_token),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : k_class cpath superclass bodystmt k_end
            //         | k_class tLSHFT expr term bodystmt k_end
            TokenKind::KeywordClass => {
                // TODO: class-specific lexer condition (e.g. class<<Foo)
                let class_token =
                    self.bump(ctx.with_mode(LexerMode::Begin(LexerBeginMode::AfterClass)));
                if matches!(self.next_token.kind, TokenKind::BinOp(BinaryOp::LShift)) {
                    todo!("singular class (class << foo)");
                }
                // TODO: check cpath condition
                let cpath = self.parse_primary(ctx);
                let superclass = if matches!(self.next_token.kind, TokenKind::BinOp(BinaryOp::Lt)) {
                    let op_token = self.bump(ctx.beg());
                    // TODO: check expr_value condition
                    let expr = self.parse_expr(ctx);
                    let range = op_token.range | expr.range();
                    if !matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                        todo!("error handling after superclass clause");
                    }
                    self.bump(ctx.beg());
                    Some(SuperclassClause {
                        expr: Box::new(expr),
                        meta: NodeMeta { range, node_id: 0 },
                    })
                } else {
                    None
                };
                // TODO: bodystmt
                let body = self.parse_compstmt_(|token| {
                    matches!(token.kind, TokenKind::Eof | TokenKind::KeywordEnd)
                });
                if !matches!(self.next_token.kind, TokenKind::KeywordEnd) {
                    todo!(
                        "error recovery on unmatched class-end: {:?}",
                        self.next_token
                    );
                }
                let end_token = self.bump(ctx.end());
                let range = class_token.range | end_token.range;
                ast::ClassExpr {
                    cpath: Box::new(cpath),
                    superclass,
                    body: Box::new(body),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : k_module cpath bodystmt k_end
            TokenKind::KeywordModule => {
                let module_token = self.bump(ctx.beg());
                // TODO: check cpath condition
                let cpath = self.parse_primary(ctx);
                // TODO: bodystmt
                let body = self.parse_compstmt_(|token| {
                    matches!(token.kind, TokenKind::Eof | TokenKind::KeywordEnd)
                });
                if !matches!(self.next_token.kind, TokenKind::KeywordEnd) {
                    todo!(
                        "error recovery on unmatched module-end: {:?}",
                        self.next_token
                    );
                }
                let end_token = self.bump(ctx.end());
                let range = module_token.range | end_token.range;
                ast::ModuleExpr {
                    cpath: Box::new(cpath),
                    body: Box::new(body),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : defn_head f_arglist bodystmt k_end
            //         | defs_head f_arglist bodystmt k_end
            TokenKind::KeywordDef => {
                // TODO: EXPR_FNAME
                let def_token = self.bump(ctx.beg());
                // TODO: LParen -> always singleton
                let name = match &self.next_token.kind {
                    TokenKind::Ident(_, name) => &name[..],
                    TokenKind::BinOp(BinaryOp::BitwiseOr) => b"|",
                    TokenKind::BinOp(BinaryOp::BitwiseXor) => b"^",
                    TokenKind::BinOp(BinaryOp::BitwiseAnd) => b"&",
                    TokenKind::BinOp(BinaryOp::Cmp) => b"<=>",
                    TokenKind::BinOp(BinaryOp::Eq) => b"==",
                    TokenKind::BinOp(BinaryOp::Eqq) => b"===",
                    TokenKind::BinOp(BinaryOp::Match) => b"=~",
                    TokenKind::BinOp(BinaryOp::NMatch) => b"!~",
                    TokenKind::BinOp(BinaryOp::Gt) => b">",
                    TokenKind::BinOp(BinaryOp::GtEq) => b">=",
                    TokenKind::BinOp(BinaryOp::Lt) => b"<",
                    TokenKind::BinOp(BinaryOp::LtEq) => b"<=",
                    TokenKind::BinOp(BinaryOp::NEq) => b"!=",
                    TokenKind::BinOp(BinaryOp::LShift) => b"<<",
                    TokenKind::BinOp(BinaryOp::RShift) => b">>",
                    TokenKind::BinOp(BinaryOp::Add) => b"+",
                    TokenKind::BinOp(BinaryOp::Sub) => b"-",
                    TokenKind::BinOp(BinaryOp::Mul) => b"*",
                    // TokenKind::Star => b"*",
                    TokenKind::BinOp(BinaryOp::Div) => b"/",
                    TokenKind::BinOp(BinaryOp::Mod) => b"%",
                    TokenKind::BinOp(BinaryOp::Pow) => b"**",
                    // TokenKind::DStar => b"**",
                    TokenKind::UnOp(UnaryOp::Not) => b"!",
                    TokenKind::UnOp(UnaryOp::BitwiseNot) => b"~",
                    TokenKind::UnOp(UnaryOp::Plus) => b"+@",
                    TokenKind::UnOp(UnaryOp::Neg) => b"-@",
                    // TokenKind::ARef => b"[]",
                    // TokenKind::ASet => b"[]=",
                    // TokenKind::BackQuote => b"`",
                    TokenKind::KeywordUnderscoreEncoding => b"__ENCODING__",
                    TokenKind::KeywordUnderscoreLine => b"__LINE__",
                    TokenKind::KeywordUnderscoreFile => b"__FILE__",
                    TokenKind::KeywordCapitalBegin => b"BEGIN",
                    TokenKind::KeywordCapitalEnd => b"END",
                    TokenKind::KeywordAlias => b"alias",
                    TokenKind::KeywordAnd => b"and",
                    TokenKind::KeywordBegin => b"begin",
                    TokenKind::KeywordBreak => b"break",
                    TokenKind::KeywordCase => b"case",
                    TokenKind::KeywordClass => b"class",
                    TokenKind::KeywordDef => b"def",
                    TokenKind::KeywordDefinedQ => b"defined?",
                    TokenKind::KeywordDoAfterMethodCall => b"do",
                    TokenKind::KeywordElse => b"else",
                    TokenKind::KeywordElsif => b"elsif",
                    TokenKind::KeywordEnd => b"end",
                    TokenKind::KeywordEnsure => b"ensure",
                    TokenKind::KeywordFalse => b"false",
                    TokenKind::KeywordFor => b"for",
                    TokenKind::KeywordIf => b"if",
                    TokenKind::KeywordIn => b"in",
                    TokenKind::KeywordModule => b"module",
                    TokenKind::KeywordNext => b"next",
                    TokenKind::KeywordNil => b"nil",
                    TokenKind::KeywordNot => b"not",
                    TokenKind::KeywordOr => b"or",
                    TokenKind::KeywordRedo => b"redo",
                    TokenKind::KeywordRescue => b"rescue",
                    TokenKind::KeywordRetry => b"retry",
                    TokenKind::KeywordReturn => b"return",
                    TokenKind::KeywordSelf => b"self",
                    TokenKind::KeywordSuper => b"super",
                    TokenKind::KeywordThen => b"then",
                    TokenKind::KeywordTrue => b"true",
                    TokenKind::KeywordUndef => b"undef",
                    TokenKind::KeywordUnless => b"unless",
                    TokenKind::KeywordUntil => b"until",
                    TokenKind::KeywordWhen => b"when",
                    TokenKind::KeywordWhile => b"while",
                    TokenKind::KeywordYield => b"yield",
                    _ => todo!("error recovery after def: {:?}", self.next_token),
                }
                .as_bstr()
                .to_string();

                // TODO: EXPR_ENDFN
                self.bump(ctx.end());
                if matches!(self.next_token.kind, TokenKind::Dot) {
                    todo!("singleton def");
                }
                let args = self.parse_f_arglist(ctx);
                if !matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
                    todo!("def args");
                }
                self.bump(ctx.beg());
                // TODO: bodystmt
                let body = self.parse_compstmt_(|token| {
                    matches!(token.kind, TokenKind::Eof | TokenKind::KeywordEnd)
                });
                if !matches!(self.next_token.kind, TokenKind::KeywordEnd) {
                    todo!("error recovery for def body");
                }
                let end_token = self.bump(ctx.end());
                let range = def_token.range | end_token.range;
                DefnExpr {
                    name,
                    args,
                    body: Box::new(body),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            TokenKind::Eof | TokenKind::RParen | TokenKind::KeywordEnd => {
                self.errors.push(ParseError::UnexpectedEof {
                    range: self.next_token.range,
                });
                ast::ErroredExpr {
                    debris: vec![],
                    meta: NodeMeta {
                        range: self.next_token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            TokenKind::KeywordBegin => todo!("begin .. end"),
            TokenKind::KeywordCapitalBegin => todo!("BEGIN {{ .. }}"),
            TokenKind::KeywordCapitalEnd => todo!("END {{ .. }}"),
            TokenKind::KeywordDoAfterCommandCall
            | TokenKind::KeywordDoAfterMethodCall
            | TokenKind::KeywordDoAfterCondition
            | TokenKind::KeywordDoAfterLambda => todo!("do .. end"),
            TokenKind::KeywordFor => todo!("for .. end"),
            TokenKind::KeywordIf => todo!("if .. end"),
            TokenKind::KeywordWhile => todo!("while .. end"),
            _ => {
                let token = self.bump(ctx.end());
                self.errors
                    .push(ParseError::UnexpectedToken { range: token.range });
                let range = token.range;
                ast::ErroredExpr {
                    debris: vec![Debri::Token(token)],
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
        }
    }

    fn parse_f_paren_args(&mut self, ctx: LexCtx) -> ParenFArgs {
        assert!(matches!(self.next_token.kind, TokenKind::LParenCall));
        let lparen_token = self.bump(ctx.beg_labelable());
        let mut list = Vec::new();
        while !matches!(
            self.next_token.kind,
            TokenKind::Eof | TokenKind::KeywordEnd | TokenKind::Semi | TokenKind::RParen
        ) {
            let arg = self.parse_f_arg(ctx);

            let debris = self.skip_debris(|token| {
                matches!(
                    token.kind,
                    TokenKind::Eof
                        | TokenKind::KeywordEnd
                        | TokenKind::Semi
                        | TokenKind::RParen
                        | TokenKind::Comma
                )
            });

            // TODO: the condition below should exclude block args
            let delim = if matches!(self.next_token.kind, TokenKind::Comma) {
                Some(self.bump(ctx.beg_labelable()))
            } else {
                None
            };
            let range = if let Some(delim) = &delim {
                arg.range() | delim.range
            } else {
                arg.range()
            };
            list.push(DelimitedFArg {
                arg,
                debris,
                delim,
                meta: NodeMeta { range, node_id: 0 },
            })
        }
        if !matches!(self.next_token.kind, TokenKind::RParen) {
            // TODO: actual argument contents
            todo!("arguments in paren_args");
        }
        // TODO: handle opt_nl
        let rparen_token = self.bump(ctx.end());
        let range = lparen_token.range | rparen_token.range;
        // TODO: check ordering constraints
        // TODO: check invalid trailing comma
        ParenFArgs {
            open_token: lparen_token,
            list,
            meta: NodeMeta { range, node_id: 0 },
            close_token: Some(rparen_token),
        }
    }

    fn parse_f_arglist(&mut self, ctx: LexCtx) -> Option<FArgs> {
        if matches!(self.next_token.kind, TokenKind::LParenCall) {
            Some(FArgs::Paren(self.parse_f_paren_args(ctx)))
        } else if !matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
            todo!("def args");
        } else {
            None
        }
    }
    fn parse_f_arg(&mut self, ctx: LexCtx) -> FArg {
        // TODO: argument splat `f(*a)`
        // if matches!(self.next_token.kind, TokenKind::Star) {}
        // TODO: kwargs splat `f(**options)`
        // if matches!(self.next_token.kind, TokenKind::DStar) {}
        // TODO: label assoc `f(foo: bar)`
        // if matches!(self.next_token.kind, TokenKind::Label) {}
        // TODO: block arg `f(&block)`
        // if matches!(self.next_token.kind, TokenKind::Amper) {}

        // TODO: arg_value check `f(foo => bar)`
        let expr = self.parse_arg_expr(ctx);

        // TODO: assoc arg
        // if matches!(self.next_token.kind, TokenKind::Assoc) {}

        // TODO: command in call_args `f(g h)`
        // if list.is_empty() && is_eligible_for_command(&expr) && is_beginning {}
        FArg::Simple(expr)
    }

    fn parse_opt_nl(&mut self, ctx: LexCtx) {
        if matches!(self.next_token.kind, TokenKind::NewLine) {
            self.bump(ctx.beg());
        }
    }

    fn skip_debris<F>(&mut self, is_end_token: F) -> Vec<Debri>
    where
        F: Fn(&Token) -> bool,
    {
        let ctx = LexCtx::default();
        let first_range = self.next_token.range;
        let mut debris = Vec::new();
        while !is_end_token(&self.next_token) {
            match self.next_token.kind.token_class() {
                // Those which likely starts an expression
                TokenClass::SelfContained | TokenClass::MaybePrefix | TokenClass::Prefix => {
                    let expr = self.parse_primary(ctx);
                    debris.push(Debri::ExprLike(expr));
                }

                // Those which is likely followed by an expression
                TokenClass::MaybeInfix | TokenClass::Infix => {
                    let token = self.bump(ctx.beg());
                    debris.push(Debri::Token(token));
                }

                // Those which usually closes an expression
                TokenClass::Postfix => {
                    let token = self.bump(ctx.end());
                    debris.push(Debri::Token(token));
                }
            }
        }
        if !debris.is_empty() {
            self.errors
                .push(ParseError::UnexpectedToken { range: first_range });
        }
        debris
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct LexCtx {
    in_condition: bool,
    in_command_args: bool,
}

impl LexCtx {
    fn with_mode(&self, mode: LexerMode) -> LexerParams {
        let Self {
            in_condition,
            in_command_args,
        } = *self;
        LexerParams {
            mode,
            in_condition,
            in_command_args,
        }
    }

    fn beg(&self) -> LexerParams {
        self.with_mode(LexerMode::Begin(LexerBeginMode::Normal))
    }

    fn beg_labelable(&self) -> LexerParams {
        self.with_mode(LexerMode::Begin(LexerBeginMode::Labelable))
    }

    fn end(&self) -> LexerParams {
        self.with_mode(LexerMode::End)
    }
}

fn starts_arg(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Ident(_, _)
            | TokenKind::KeywordUnderscoreEncoding
            | TokenKind::KeywordUnderscoreLine
            | TokenKind::KeywordUnderscoreFile
            | TokenKind::KeywordCapitalBegin
            | TokenKind::KeywordCapitalEnd
            | TokenKind::KeywordAlias
            | TokenKind::KeywordBegin
            | TokenKind::KeywordBreak
            | TokenKind::KeywordCase
            | TokenKind::KeywordClass
            | TokenKind::KeywordDef
            | TokenKind::KeywordDefinedQ
            | TokenKind::KeywordFalse
            | TokenKind::KeywordFor
            | TokenKind::KeywordIf
            | TokenKind::KeywordModule
            | TokenKind::KeywordNext
            | TokenKind::KeywordNil
            | TokenKind::KeywordRedo
            | TokenKind::KeywordRetry
            | TokenKind::KeywordReturn
            | TokenKind::KeywordSelf
            | TokenKind::KeywordSuper
            | TokenKind::KeywordTrue
            | TokenKind::KeywordUndef
            | TokenKind::KeywordUnless
            | TokenKind::KeywordUntil
            | TokenKind::KeywordWhile
            | TokenKind::KeywordYield
            | TokenKind::Numeric(_)
            | TokenKind::StringBegin(_)
            | TokenKind::Dot2Prefix
            | TokenKind::Dot3Prefix
            | TokenKind::UnOp(_)
            | TokenKind::LParenBeg
            | TokenKind::Colon2Prefix
    )
}

const SENTINEL_EXPR: Expr = Expr::Nil(NilExpr {
    meta: NodeMeta {
        range: Range(0, 0),
        node_id: 0,
    },
});

fn command_head(expr: &mut Expr) -> Option<&mut Option<Args>> {
    // Convert const as send
    if let Expr::Const(e) = expr {
        if e.convertible_to_send() {
            let e = match std::mem::replace(expr, SENTINEL_EXPR) {
                Expr::Const(expr) => expr,
                _ => unreachable!(),
            };
            let e = e.convert_to_send();
            *expr = Expr::Send(e);
        }
    }
    match expr {
        Expr::Send(expr) if expr.args.is_none() => Some(&mut expr.args),
        _ => None,
    }
}
