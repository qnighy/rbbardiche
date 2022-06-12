use std::fmt::Display;

use crate::ast::{self, BinaryOp, Expr, PostfixUnaryOp, UnaryOp};
use crate::delegate_expr;

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

impl From<&ast::Parenthesized> for SExp {
    fn from(expr: &ast::Parenthesized) -> Self {
        let ast::Parenthesized { stmts, meta: _ } = expr;
        SExp::Tagged {
            tag: "begin".to_owned(),
            args: stmts.iter().map(|stmt| to_sexp(stmt)).collect::<Vec<_>>(),
        }
    }
}

impl From<&ast::Compound> for SExp {
    fn from(expr: &ast::Compound) -> Self {
        let ast::Compound { stmts, meta: _ } = expr;
        if stmts.is_empty() {
            SExp::Nil
        } else if stmts.len() == 1 {
            to_sexp(&stmts[0])
        } else {
            SExp::Tagged {
                tag: "begin".to_owned(),
                args: stmts.iter().map(|stmt| to_sexp(stmt)).collect::<Vec<_>>(),
            }
        }
    }
}

impl From<&ast::Ident> for SExp {
    fn from(expr: &ast::Ident) -> Self {
        let ast::Ident { name, meta: _ } = expr;
        SExp::Tagged {
            tag: "send".to_owned(),
            args: vec![SExp::Nil, SExp::Symbol { name: name.clone() }],
        }
    }
}

impl From<&ast::CIdent> for SExp {
    fn from(expr: &ast::CIdent) -> Self {
        let ast::CIdent { name, meta: _ } = expr;
        SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![SExp::Nil, SExp::Symbol { name: name.clone() }],
        }
    }
}

impl From<&ast::RootIdent> for SExp {
    fn from(expr: &ast::RootIdent) -> Self {
        let ast::RootIdent { name, meta: _ } = expr;
        SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![
                SExp::Tagged {
                    tag: "cbase".to_owned(),
                    args: vec![],
                },
                SExp::Symbol { name: name.clone() },
            ],
        }
    }
}

impl From<&ast::RelativeConstant> for SExp {
    fn from(expr: &ast::RelativeConstant) -> Self {
        let ast::RelativeConstant {
            base,
            name,
            meta: _,
        } = expr;
        SExp::Tagged {
            tag: "const".to_owned(),
            args: vec![to_sexp(base), SExp::Symbol { name: name.clone() }],
        }
    }
}

impl From<&ast::Numeric> for SExp {
    fn from(expr: &ast::Numeric) -> Self {
        let ast::Numeric { numval, meta: _ } = expr;
        SExp::Tagged {
            tag: "int".to_string(),
            args: vec![SExp::Number { value: *numval }],
        }
    }
}

impl From<&ast::TernaryCond> for SExp {
    fn from(expr: &ast::TernaryCond) -> Self {
        let ast::TernaryCond {
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

impl From<&ast::Binary> for SExp {
    fn from(expr: &ast::Binary) -> Self {
        let ast::Binary {
            lhs,
            op,
            rhs,
            meta: _,
        } = expr;
        match op {
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
        }
    }
}

impl From<&ast::Unary> for SExp {
    fn from(expr: &ast::Unary) -> Self {
        let ast::Unary { op, expr, meta: _ } = expr;
        match op {
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
        }
    }
}

impl From<&ast::PostfixUnary> for SExp {
    fn from(expr: &ast::PostfixUnary) -> Self {
        let ast::PostfixUnary { expr, op, meta: _ } = expr;
        match op {
            PostfixUnaryOp::RangeIncl => SExp::Tagged {
                tag: "irange".to_owned(),
                args: vec![to_sexp(expr), SExp::Nil],
            },
            PostfixUnaryOp::RangeExcl => SExp::Tagged {
                tag: "erange".to_owned(),
                args: vec![to_sexp(expr), SExp::Nil],
            },
        }
    }
}

impl From<&ast::Nil> for SExp {
    fn from(expr: &ast::Nil) -> Self {
        let ast::Nil { meta: _ } = expr;
        SExp::Tagged {
            tag: "nil".to_owned(),
            args: vec![],
        }
    }
}

impl From<&ast::Assign> for SExp {
    fn from(expr: &ast::Assign) -> Self {
        let ast::Assign { lhs, rhs, meta: _ } = expr;
        match &**lhs {
            Expr::Ident(ast::Ident { name, meta: _ }) => SExp::Tagged {
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

impl From<&ast::Send> for SExp {
    fn from(expr: &ast::Send) -> Self {
        let ast::Send {
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
            .chain(args.iter().map(|arg| to_sexp(arg)))
            .collect::<Vec<_>>(),
        }
    }
}

impl From<&ast::Module> for SExp {
    fn from(expr: &ast::Module) -> Self {
        let ast::Module {
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

impl From<&ast::Errored> for SExp {
    fn from(expr: &ast::Errored) -> Self {
        let ast::Errored { meta: _ } = expr;
        SExp::Invalid
    }
}

impl From<&Expr> for SExp {
    fn from(expr: &Expr) -> Self {
        delegate_expr!(expr, expr => SExp::from(expr))
    }
}

fn to_sexp(expr: &Expr) -> SExp {
    SExp::from(expr)
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
