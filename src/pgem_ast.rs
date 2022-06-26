use std::fmt::Display;

use crate::ast::{
    self, Arg, BinaryOp, DelimitedArg, EmptyStmt, Expr, ExprStmt, Program, RangeType, Stmt, UnaryOp,
};
use crate::delegate_expr;

#[derive(Debug, Clone)]
pub enum SExp {
    Tagged { tag: String, args: Vec<SExp> },
    Nil,
    Symbol { name: String },
    Number { value: i32 },
    String { value: String },
    Invalid,
}

impl Display for SExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            SExpIndent {
                sexp: self,
                nest: 0,
            }
        )
    }
}

#[derive(Debug)]
pub struct SExpIndent<'a> {
    sexp: &'a SExp,
    nest: u32,
}

impl<'a> Display for SExpIndent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.sexp {
            SExp::Tagged { tag, args } => {
                write!(f, "s(:{}", tag)?;
                for arg in args {
                    if matches!(arg, SExp::Tagged { .. }) {
                        write!(f, ",\n{}", Indent(self.nest + 1))?;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}",
                        SExpIndent {
                            sexp: arg,
                            nest: self.nest + 1,
                        }
                    )?;
                }
                write!(f, ")")?;
            }
            SExp::Nil => {
                f.write_str("nil")?;
            }
            SExp::Symbol { name } => {
                write!(f, ":{}", name)?;
            }
            SExp::Number { value } => {
                write!(f, "{}", value)?;
            }
            SExp::String { value } => {
                // TODO: escaping
                f.write_str("\"")?;
                for ch in value.chars() {
                    if ch.is_control() {
                        let name = match ch {
                            '\x07' => Some("\\a"),
                            '\x08' => Some("\\b"),
                            '\x09' => Some("\\t"),
                            '\x0A' => Some("\\n"),
                            '\x0B' => Some("\\v"),
                            '\x0C' => Some("\\f"),
                            '\x0D' => Some("\\r"),
                            '\x1B' => Some("\\e"),
                            _ => None,
                        };
                        if let Some(name) = name {
                            f.write_str(name)?;
                        } else if (ch as u32) < 0x10000 {
                            write!(f, "\\u{:04X}", ch as u32)?;
                        } else {
                            write!(f, "\\u{{{:X}}}", ch as u32)?;
                        }
                    } else {
                        write!(f, "{}", ch)?;
                    }
                }
                f.write_str("\"")?;
            }
            SExp::Invalid => {
                f.write_str("<invalid>")?;
            }
        }
        Ok(())
    }
}

impl From<&Program> for SExp {
    fn from(expr: &Program) -> Self {
        let Program { stmts, meta: _ } = expr;
        let stmts = stmts
            .iter()
            .filter(|&stmt| !matches!(stmt, Stmt::Empty(_)))
            .map(|stmt| SExp::from(stmt))
            .collect::<Vec<_>>();
        if stmts.is_empty() {
            SExp::Nil
        } else if stmts.len() == 1 {
            // stmts[0]
            stmts.into_iter().next().unwrap()
        } else {
            SExp::Tagged {
                tag: "begin".to_owned(),
                args: stmts,
            }
        }
    }
}

impl From<&ast::ParenthesizedExpr> for SExp {
    fn from(expr: &ast::ParenthesizedExpr) -> Self {
        let ast::ParenthesizedExpr { stmts, meta: _ } = expr;
        let stmts = stmts
            .iter()
            .filter(|&stmt| !matches!(stmt, Stmt::Empty(_)))
            .map(|stmt| SExp::from(stmt))
            .collect::<Vec<_>>();
        SExp::Tagged {
            tag: "begin".to_owned(),
            args: stmts,
        }
    }
}

impl From<&ast::CompoundExpr> for SExp {
    fn from(expr: &ast::CompoundExpr) -> Self {
        let ast::CompoundExpr { stmts, meta: _ } = expr;
        let stmts = stmts
            .iter()
            .filter(|&stmt| !matches!(stmt, Stmt::Empty(_)))
            .map(|stmt| SExp::from(stmt))
            .collect::<Vec<_>>();
        if stmts.is_empty() {
            SExp::Nil
        } else if stmts.len() == 1 {
            // stmts[0]
            stmts.into_iter().next().unwrap()
        } else {
            SExp::Tagged {
                tag: "begin".to_owned(),
                args: stmts,
            }
        }
    }
}

impl From<&ast::NumericExpr> for SExp {
    fn from(expr: &ast::NumericExpr) -> Self {
        let ast::NumericExpr { numval, meta: _ } = expr;
        SExp::Tagged {
            tag: "int".to_string(),
            args: vec![SExp::Number { value: *numval }],
        }
    }
}

impl From<&ast::SymbolExpr> for SExp {
    fn from(expr: &ast::SymbolExpr) -> Self {
        let ast::SymbolExpr {
            open_token: _,
            ident_token: _,
            value,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "sym".to_string(),
            args: vec![SExp::Symbol {
                name: value.clone(),
            }],
        }
    }
}

impl From<&ast::StringLiteralExpr> for SExp {
    fn from(expr: &ast::StringLiteralExpr) -> Self {
        let ast::StringLiteralExpr { strval, meta: _ } = expr;
        SExp::Tagged {
            tag: "str".to_string(),
            args: vec![SExp::String {
                value: strval.clone(),
            }],
        }
    }
}

impl From<&ast::TernaryCondExpr> for SExp {
    fn from(expr: &ast::TernaryCondExpr) -> Self {
        let ast::TernaryCondExpr {
            cond,
            consequence,
            alternate,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "if".to_string(),
            args: vec![to_sexp(cond), to_sexp(consequence), to_sexp(alternate)],
        }
    }
}

impl From<&ast::RangeExpr> for SExp {
    fn from(expr: &ast::RangeExpr) -> Self {
        let ast::RangeExpr {
            begin,
            range_type,
            end,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: match range_type {
                RangeType::Inclusive => "irange",
                RangeType::Exclusive => "erange",
            }
            .to_owned(),
            args: vec![
                if let Some(begin) = begin {
                    to_sexp(begin)
                } else {
                    SExp::Nil
                },
                if let Some(end) = end {
                    to_sexp(end)
                } else {
                    SExp::Nil
                },
            ],
        }
    }
}

impl From<&ast::BinaryExpr> for SExp {
    fn from(expr: &ast::BinaryExpr) -> Self {
        let ast::BinaryExpr {
            lhs,
            op,
            rhs,
            meta: _,
        } = expr;
        match op {
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => SExp::Tagged {
                tag: binop_node_name(*op).to_owned(),
                args: vec![to_sexp(lhs), to_sexp(rhs)],
            },
            _ => SExp::Tagged {
                tag: "send".to_owned(),
                args: vec![
                    to_sexp(lhs),
                    SExp::Symbol {
                        name: binop_send_name(*op).to_owned(),
                    },
                    to_sexp(rhs),
                ],
            },
        }
    }
}

impl From<&ast::UnaryExpr> for SExp {
    fn from(expr: &ast::UnaryExpr) -> Self {
        let ast::UnaryExpr { op, expr, meta: _ } = expr;
        SExp::Tagged {
            tag: "send".to_owned(),
            args: vec![
                to_sexp(expr),
                SExp::Symbol {
                    name: unop_send_name(*op).to_owned(),
                },
            ],
        }
    }
}

impl From<&ast::NilExpr> for SExp {
    fn from(expr: &ast::NilExpr) -> Self {
        let ast::NilExpr { meta: _ } = expr;
        SExp::Tagged {
            tag: "nil".to_owned(),
            args: vec![],
        }
    }
}

impl From<&ast::AssignExpr> for SExp {
    fn from(expr: &ast::AssignExpr) -> Self {
        let ast::AssignExpr { lhs, rhs, meta: _ } = expr;
        match &**lhs {
            Expr::Send(ast::SendExpr {
                optional: false,
                recv: None,
                name,
                args: None,
                meta: _,
            }) => SExp::Tagged {
                tag: "lvasgn".to_owned(),
                args: vec![SExp::Symbol { name: name.clone() }, to_sexp(rhs)],
            },
            _ => SExp::Tagged {
                tag: "lvasgn".to_owned(),
                args: vec![SExp::Invalid, to_sexp(rhs)],
            },
        }
    }
}

impl From<&ast::SendExpr> for SExp {
    fn from(expr: &ast::SendExpr) -> Self {
        let ast::SendExpr {
            optional,
            recv,
            name,
            args,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: if *optional {
                "csend".to_owned()
            } else {
                "send".to_owned()
            },
            args: vec![
                if let Some(recv) = recv {
                    to_sexp(recv)
                } else {
                    SExp::Nil
                },
                SExp::Symbol { name: name.clone() },
            ]
            .into_iter()
            .chain(
                args.as_ref()
                    .map(|x| x.list().as_slice())
                    .unwrap_or_else(|| &[])
                    .iter()
                    .map(|arg| SExp::from(arg)),
            )
            .collect::<Vec<_>>(),
        }
    }
}

impl From<&ast::ConstExpr> for SExp {
    fn from(expr: &ast::ConstExpr) -> Self {
        let ast::ConstExpr {
            toplevel,
            recv,
            name,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![
                if let Some(recv) = recv {
                    to_sexp(recv)
                } else if *toplevel {
                    SExp::Tagged {
                        tag: "cbase".to_owned(),
                        args: vec![],
                    }
                } else {
                    SExp::Nil
                },
                SExp::Symbol { name: name.clone() },
            ],
        }
    }
}

impl From<&DelimitedArg> for SExp {
    fn from(arg: &DelimitedArg) -> Self {
        SExp::from(&arg.arg)
    }
}

impl From<&Arg> for SExp {
    fn from(arg: &Arg) -> Self {
        match arg {
            Arg::Simple(arg) => SExp::from(arg),
        }
    }
}

impl From<&ast::ArrayExpr> for SExp {
    fn from(expr: &ast::ArrayExpr) -> Self {
        let ast::ArrayExpr {
            open_token: _,
            list,
            close_token: _,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "array".to_owned(),
            args: list.iter().map(|elem| SExp::from(elem)).collect::<Vec<_>>(),
        }
    }
}

impl From<&ast::HashExpr> for SExp {
    fn from(expr: &ast::HashExpr) -> Self {
        let ast::HashExpr {
            open_token: _,
            list,
            close_token: _,
            meta: _,
        } = expr;
        if !list.is_empty() {
            todo!("serializing hash contents");
        }
        SExp::Tagged {
            tag: "hash".to_owned(),
            args: vec![],
        }
    }
}

impl From<&ast::ClassExpr> for SExp {
    fn from(expr: &ast::ClassExpr) -> Self {
        let ast::ClassExpr {
            cpath,
            superclass,
            body,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "class".to_owned(),
            args: vec![
                to_sexp(cpath),
                if let Some(superclass) = superclass {
                    to_sexp(&superclass.expr)
                } else {
                    SExp::Nil
                },
                to_sexp(body),
            ],
        }
    }
}

impl From<&ast::ModuleExpr> for SExp {
    fn from(expr: &ast::ModuleExpr) -> Self {
        let ast::ModuleExpr {
            cpath,
            body,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "module".to_owned(),
            args: vec![to_sexp(cpath), to_sexp(body)],
        }
    }
}

impl From<&ast::DefnExpr> for SExp {
    fn from(expr: &ast::DefnExpr) -> Self {
        let ast::DefnExpr {
            name,
            args,
            body,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "def".to_owned(),
            args: vec![
                SExp::Symbol { name: name.clone() },
                SExp::Tagged {
                    tag: "args".to_owned(),
                    args: if let Some(args) = args {
                        args.list()
                            .iter()
                            .map(|arg| SExp::from(arg))
                            .collect::<Vec<_>>()
                    } else {
                        vec![]
                    },
                },
                to_sexp(body),
            ],
        }
    }
}

impl From<&ast::DelimitedFArg> for SExp {
    fn from(expr: &ast::DelimitedFArg) -> Self {
        let ast::DelimitedFArg {
            arg,
            debris: _,
            delim: _,
            meta: _,
        } = expr;
        SExp::from(arg)
    }
}

impl From<&ast::FArg> for SExp {
    fn from(expr: &ast::FArg) -> Self {
        match expr {
            ast::FArg::Simple(expr) => match expr {
                Expr::Send(ast::SendExpr {
                    optional: false,
                    recv: None,
                    name,
                    args: None,
                    meta: _,
                }) => SExp::Tagged {
                    tag: "arg".to_owned(),
                    args: vec![SExp::Symbol { name: name.clone() }],
                },
                _ => SExp::Tagged {
                    tag: "arg".to_owned(),
                    args: vec![SExp::Invalid],
                },
            },
        }
    }
}

impl From<&ast::ErroredExpr> for SExp {
    fn from(expr: &ast::ErroredExpr) -> Self {
        let ast::ErroredExpr { debris: _, meta: _ } = expr;
        SExp::Invalid
    }
}

impl From<&Expr> for SExp {
    fn from(expr: &Expr) -> Self {
        delegate_expr!(expr, expr => SExp::from(expr))
    }
}

impl From<&Stmt> for SExp {
    fn from(stmt: &Stmt) -> Self {
        match stmt {
            Stmt::Expr(stmt) => SExp::from(stmt),
            Stmt::Empty(stmt) => SExp::from(stmt),
        }
    }
}

impl From<&ExprStmt> for SExp {
    fn from(stmt: &ExprStmt) -> Self {
        SExp::from(&stmt.expr)
    }
}

impl From<&EmptyStmt> for SExp {
    fn from(_: &EmptyStmt) -> Self {
        SExp::Nil
    }
}

fn to_sexp(expr: &Expr) -> SExp {
    SExp::from(expr)
}

fn binop_node_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::LogicalOr => "or",
        BinaryOp::LogicalAnd => "and",
        _ => unreachable!(),
    }
}

fn binop_send_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
            unreachable!()
        }
        BinaryOp::Cmp => "<=>",
        BinaryOp::Eq => "==",
        BinaryOp::Eqq => "===",
        BinaryOp::NEq => "!=",
        BinaryOp::Match => "=~",
        BinaryOp::NMatch => "!~",
        BinaryOp::Gt => ">",
        BinaryOp::GtEq => ">=",
        BinaryOp::Lt => "<",
        BinaryOp::LtEq => "<=",
        BinaryOp::BitwiseOr => "|",
        BinaryOp::BitwiseXor => "^",
        BinaryOp::BitwiseAnd => "&",
        BinaryOp::LShift => "<<",
        BinaryOp::RShift => ">>",
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::Pow => "**",
    }
}

fn unop_send_name(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Plus => "+@",
        UnaryOp::Neg => "-@",
        UnaryOp::Not => "!",
        UnaryOp::BitwiseNot => "~",
    }
}

struct Indent(u32);

impl Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            f.write_str("  ")?;
        }
        Ok(())
    }
}
