use crate::ast::{self, BinaryOp, Expr, NodeMeta, Range, RangeType, UnaryOp};
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
            ast::CompoundExpr {
                stmts,
                meta: NodeMeta {
                    range: Range(0, self.source.len()),
                    node_id: 0,
                },
            }
            .into()
        }
    }

    fn parse_compstmt_(&mut self) -> Expr {
        let stmts = self.parse_compstmt();
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

    fn parse_compstmt(&mut self) -> Vec<Expr> {
        while matches!(self.next_token.kind, TokenKind::Semi | TokenKind::NewLine) {
            self.bump(true);
        }
        let mut stmts = Vec::new();
        while !matches!(
            self.next_token.kind,
            TokenKind::Eof | TokenKind::RParen | TokenKind::EndKeyword
        ) {
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
    fn parse_range(&mut self, mut chaining: bool) -> Expr {
        let mut expr = self.parse_logical_or();
        loop {
            let range_type = match self.next_token.kind {
                TokenKind::Dot2Mid => RangeType::Inclusive,
                TokenKind::Dot3Mid => RangeType::Exclusive,
                _ => break,
            };
            let op_token = self.bump(true);
            if chaining {
                self.errors.push(ParseError::ChainedRange {
                    range: op_token.range,
                });
            }
            // TODO: parse endless ranges (like `42..`)
            let rhs = self.parse_logical_or();
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
    fn parse_unary(&mut self) -> Expr {
        let range_type = match self.next_token.kind {
            TokenKind::Dot2Beg => Some(RangeType::Inclusive),
            TokenKind::Dot3Beg => Some(RangeType::Exclusive),
            _ => None,
        };
        if let Some(range_type) = range_type {
            let op_token = self.bump(true);
            // Within these rules, shift takes precedence over reduce
            // E.g. `..1 + 2` is interpreted as being `..(1 + 2)`
            let expr = self.parse_range(true);
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
            return self.parse_primary();
        };
        let op_token = self.bump(true);
        let expr = self.parse_unary();
        let range = op_token.range | expr.range();
        ast::UnaryExpr {
            op,
            expr: Box::new(expr),
            meta: NodeMeta { range, node_id: 0 },
        }
        .into()
    }

    // paren_args : '(' opt_call_args rparen
    //            | '(' args ',' args_forward rparen
    //            | '(' args_forward rparen
    /// Parses a parenthesized argument list like `(42, 80)` as in `foo(42, 80)`.
    fn parse_paren_args(&mut self) -> (Vec<Expr>, Range) {
        assert!(matches!(self.next_token.kind, TokenKind::LParenCall));
        let lparen_token = self.bump(true);
        if !matches!(self.next_token.kind, TokenKind::RParen) {
            // TODO: actual argument contents
            todo!("arguments in paren_args");
        }
        // TODO: handle opt_nl
        let rparen_token = self.bump(false);
        (vec![], lparen_token.range | rparen_token.range)
    }

    // primary : primary_value tCOLON2 tCONSTANT
    //
    /// Parses a primary expression. Primary expressions are those enclosed with
    /// delimiters from both sides, with a few exceptions.
    ///
    /// These are the exceptions (which are directly parsed in this `parse_primary`):
    ///
    /// - `foo::Bar`
    fn parse_primary(&mut self) -> Expr {
        let mut expr = self.parse_primary_inner();
        loop {
            match &self.next_token.kind {
                TokenKind::DColon => {
                    // TODO: handle primary_value condition
                    let dcolon_token = self.bump(true);
                    if let TokenKind::CIdent(name) = &self.next_token.kind {
                        let name = name.to_string();
                        let token = self.bump(false);
                        let range = expr.range() | token.range;
                        expr = ast::RelativeConstantExpr {
                            base: Box::new(expr),
                            name,
                            meta: NodeMeta { range, node_id: 0 },
                        }
                        .into();
                    } else {
                        self.errors.push(ParseError::UnexpectedToken {
                            range: self.next_token.range,
                        });
                        let range = expr.range() | dcolon_token.range;
                        expr = ast::RelativeConstantExpr {
                            base: Box::new(expr),
                            name: "".to_owned(),
                            meta: NodeMeta { range, node_id: 0 },
                        }
                        .into();
                    }
                }
                _ => break,
            }
        }
        expr
    }

    /// Parses a primary expression, with a few exceptions (which are parsed in `parse_primary`)
    fn parse_primary_inner(&mut self) -> Expr {
        match &self.next_token.kind {
            // primary : var_ref
            //         | method_call
            // method_call : fcall paren_args
            // var_ref : user_variable
            // user_variable : tIDENTIFIER
            TokenKind::Ident(name) => {
                let name = name.to_string();
                let token = self.bump(false);
                if matches!(self.next_token.kind, TokenKind::LParenCall) {
                    let (args, args_range) = self.parse_paren_args();
                    let range = token.range | args_range;
                    return ast::SendExpr {
                        optional: false,
                        recv: None,
                        name,
                        args,
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into();
                }
                ast::IdentExpr {
                    name,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : var_ref
            // var_ref : user_variable
            // user_variable : tCONSTANT
            TokenKind::CIdent(name) => {
                let name = name.to_string();
                let token = self.bump(false);
                ast::CIdentExpr {
                    name,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : tCOLON3 tCONSTANT
            TokenKind::DColonBeg => {
                let dcolon_token = self.bump(true);
                if let TokenKind::CIdent(name) = &self.next_token.kind {
                    let name = name.to_string();
                    let token = self.bump(false);
                    let range = dcolon_token.range | token.range;
                    ast::RootIdentExpr {
                        name,
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into()
                } else {
                    self.errors.push(ParseError::UnexpectedToken {
                        range: self.next_token.range,
                    });
                    let range = dcolon_token.range;
                    ast::RootIdentExpr {
                        name: "".to_owned(),
                        meta: NodeMeta { range, node_id: 0 },
                    }
                    .into()
                }
            }
            // primary : var_ref
            // var_ref : keyword_variable
            // keyword_variable : keyword_nil
            TokenKind::NilKeyword => {
                let token = self.bump(false);
                ast::NilExpr {
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : literal
            // literal : numeric
            // numeric : simple_numeric
            //         | tMINUS_NUM simple_numeric %prec tLOWEST
            //
            // Note that tMINUS_NUM is handled in lexer in this parer
            TokenKind::Numeric(numval) => {
                let numval = *numval;
                let token = self.bump(false);
                ast::NumericExpr {
                    numval,
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            // primary : tLPAREN compstmt ')'
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
                ast::ParenthesizedExpr {
                    stmts,
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            // primary : k_module cpath bodystmt k_end
            TokenKind::ModuleKeyword => {
                let module_token = self.bump(true);
                // TODO: check cpath condition
                let cpath = self.parse_primary();
                // TODO: bodystmt
                let body = self.parse_compstmt_();
                if !matches!(self.next_token.kind, TokenKind::EndKeyword) {
                    todo!(
                        "error recovery on unmatched module-end: {:?}",
                        self.next_token
                    );
                }
                let end_token = self.bump(false);
                let range = module_token.range | end_token.range;
                ast::ModuleExpr {
                    cpath: Box::new(cpath),
                    body: Box::new(body),
                    meta: NodeMeta { range, node_id: 0 },
                }
                .into()
            }
            TokenKind::Eof => {
                self.errors.push(ParseError::UnexpectedEof {
                    range: self.next_token.range,
                });
                ast::ErroredExpr {
                    meta: NodeMeta {
                        range: self.next_token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
            _ => {
                let token = self.bump(false);
                self.errors
                    .push(ParseError::UnexpectedToken { range: token.range });
                ast::ErroredExpr {
                    meta: NodeMeta {
                        range: token.range,
                        node_id: 0,
                    },
                }
                .into()
            }
        }
    }

    fn parse_opt_nl(&mut self) {
        if matches!(self.next_token.kind, TokenKind::NewLine) {
            self.bump(true);
        }
    }
}
