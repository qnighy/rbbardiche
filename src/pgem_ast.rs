use std::fmt::Display;

use crate::ast::{BinaryOp, Expr, ExprKind, PostfixUnaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum SExp {
    Tagged { tag: String, args: Vec<SExp> },
    Nil,
    Symbol { name: String },
    Number { value: i32 },
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
            SExp::Invalid => {
                f.write_str("<invalid>")?;
            }
        }
        Ok(())
    }
}

fn to_sexp(expr: &Expr) -> SExp {
    match &expr.kind {
        ExprKind::Parenthesized { stmts } => SExp::Tagged {
            tag: "begin".to_owned(),
            args: stmts.iter().map(|stmt| to_sexp(stmt)).collect::<Vec<_>>(),
        },
        ExprKind::Compound { stmts } if stmts.is_empty() => SExp::Nil,
        ExprKind::Compound { stmts } if stmts.len() == 1 => to_sexp(&stmts[0]),
        ExprKind::Compound { stmts } => SExp::Tagged {
            tag: "begin".to_owned(),
            args: stmts.iter().map(|stmt| to_sexp(stmt)).collect::<Vec<_>>(),
        },
        ExprKind::Ident { name } => SExp::Tagged {
            tag: "send".to_owned(),
            args: vec![SExp::Nil, SExp::Symbol { name: name.clone() }],
        },
        ExprKind::CIdent { name } => SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![SExp::Nil, SExp::Symbol { name: name.clone() }],
        },
        ExprKind::RootIdent { name } => SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![
                SExp::Tagged {
                    tag: "cbase".to_owned(),
                    args: vec![],
                },
                SExp::Symbol { name: name.clone() },
            ],
        },
        ExprKind::RelativeConstant { base, name } => SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![to_sexp(base), SExp::Symbol { name: name.clone() }],
        },
        ExprKind::Numeric { numval } => SExp::Tagged {
            tag: "int".to_string(),
            args: vec![SExp::Number { value: *numval }],
        },
        ExprKind::TernaryCond {
            cond,
            consequence,
            alternate,
        } => SExp::Tagged {
            tag: "if".to_string(),
            args: vec![to_sexp(cond), to_sexp(consequence), to_sexp(alternate)],
        },
        ExprKind::Binary { lhs, op, rhs } => match op {
            BinaryOp::RangeIncl
            | BinaryOp::RangeExcl
            | BinaryOp::LogicalOr
            | BinaryOp::LogicalAnd => SExp::Tagged {
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
        },
        ExprKind::Unary { op, expr } => match op {
            UnaryOp::RangeIncl => SExp::Tagged {
                tag: "irange".to_owned(),
                args: vec![SExp::Nil, to_sexp(expr)],
            },
            UnaryOp::RangeExcl => SExp::Tagged {
                tag: "erange".to_owned(),
                args: vec![SExp::Nil, to_sexp(expr)],
            },
            _ => SExp::Tagged {
                tag: "send".to_owned(),
                args: vec![
                    to_sexp(expr),
                    SExp::Symbol {
                        name: unop_send_name(*op).to_owned(),
                    },
                ],
            },
        },
        ExprKind::PostfixUnary { expr, op } => match op {
            PostfixUnaryOp::RangeIncl => SExp::Tagged {
                tag: "irange".to_owned(),
                args: vec![to_sexp(expr), SExp::Nil],
            },
            PostfixUnaryOp::RangeExcl => SExp::Tagged {
                tag: "erange".to_owned(),
                args: vec![to_sexp(expr), SExp::Nil],
            },
        },
        ExprKind::Nil => SExp::Tagged {
            tag: "nil".to_owned(),
            args: vec![],
        },
        ExprKind::Assign { lhs, rhs } => match &lhs.kind {
            ExprKind::Ident { name } => SExp::Tagged {
                tag: "lvasgn".to_owned(),
                args: vec![SExp::Symbol { name: name.clone() }, to_sexp(rhs)],
            },
            _ => SExp::Tagged {
                tag: "lvasgn".to_owned(),
                args: vec![SExp::Invalid, to_sexp(rhs)],
            },
        },
        ExprKind::Module { cpath, body } => SExp::Tagged {
            tag: "module".to_owned(),
            args: vec![to_sexp(cpath), to_sexp(body)],
        },
        ExprKind::Errored => SExp::Invalid,
    }
}

fn binop_node_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::RangeIncl => "irange",
        BinaryOp::RangeExcl => "erange",
        BinaryOp::LogicalOr => "or",
        BinaryOp::LogicalAnd => "and",
        _ => unreachable!(),
    }
}

fn binop_send_name(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::RangeIncl | BinaryOp::RangeExcl | BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
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
        UnaryOp::RangeIncl | UnaryOp::RangeExcl => unreachable!(),
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

pub fn display_pgem(expr: &Expr) -> SExp {
    to_sexp(expr)
}
