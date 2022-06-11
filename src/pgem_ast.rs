use std::fmt::Display;

use crate::ast::{BinaryOp, Expr, ExprKind, PostfixUnaryOp, UnaryOp};

#[derive(Debug)]
pub struct PgemDisplay<'a> {
    expr: &'a Expr,
    nest: u32,
}

impl<'a> PgemDisplay<'a> {
    fn same(&self, expr: &'a Expr) -> Self {
        PgemDisplay {
            expr,
            nest: self.nest,
        }
    }
    fn sub(&self, expr: &'a Expr) -> Self {
        PgemDisplay {
            expr,
            nest: self.nest + 1,
        }
    }
}

impl<'a> Display for PgemDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent = Indent(self.nest + 1);
        match &self.expr.kind {
            ExprKind::Parenthesized { stmts } => {
                write!(f, "s(:begin")?;
                for stmt in stmts {
                    write!(f, ",\n{}{}", indent, self.sub(stmt))?;
                }
                write!(f, ")")?;
            }
            ExprKind::Compound { stmts } if stmts.is_empty() => {
                write!(f, "nil")?;
            }
            ExprKind::Compound { stmts } if stmts.len() == 1 => {
                write!(f, "{}", self.same(&stmts[0]))?;
            }
            ExprKind::Compound { stmts } => {
                write!(f, "s(:begin")?;
                for stmt in stmts {
                    write!(f, ",\n{}{}", indent, self.sub(stmt))?;
                }
                write!(f, ")")?;
            }
            ExprKind::Ident { name } => {
                write!(f, "s(:send, nil, :{})", name)?;
            }
            ExprKind::CIdent { name } => {
                write!(f, "s(:const, nil, :{})", name)?;
            }
            ExprKind::RootIdent { name } => {
                write!(f, "s(:const,\n{}s(:cbase), :{})", indent, name)?;
            }
            ExprKind::RelativeConstant { base, name } => {
                write!(f, "s(:const,\n{}{}, :{})", indent, self.sub(base), name,)?;
            }
            ExprKind::Numeric { numval } => {
                write!(f, "s(:int, {})", numval)?;
            }
            ExprKind::TernaryCond {
                cond,
                consequence,
                alternate,
            } => {
                write!(
                    f,
                    "s(:if,\n{}{},\n{}{},\n{}{})",
                    indent,
                    self.sub(cond),
                    indent,
                    self.sub(consequence),
                    indent,
                    self.sub(alternate),
                )?;
            }
            ExprKind::Binary { lhs, op, rhs } => match op {
                BinaryOp::RangeIncl
                | BinaryOp::RangeExcl
                | BinaryOp::LogicalOr
                | BinaryOp::LogicalAnd => {
                    write!(
                        f,
                        "s(:{},\n{}{},\n{}{})",
                        binop_node_name(*op),
                        indent,
                        self.sub(lhs),
                        indent,
                        self.sub(rhs)
                    )?;
                }
                _ => {
                    write!(
                        f,
                        "s(:send,\n{}{}, :{},\n{}{})",
                        indent,
                        self.sub(lhs),
                        binop_send_name(*op),
                        indent,
                        self.sub(rhs)
                    )?;
                }
            },
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::RangeIncl => {
                    write!(f, "s(:irange, nil,\n{}{})", indent, self.sub(expr))?;
                }
                UnaryOp::RangeExcl => {
                    write!(f, "s(:erange, nil,\n{}{})", indent, self.sub(expr))?;
                }
                _ => {
                    write!(
                        f,
                        "s(:send,\n{}{}, :{})",
                        indent,
                        self.sub(expr),
                        unop_send_name(*op),
                    )?;
                }
            },
            ExprKind::PostfixUnary { expr, op } => match op {
                PostfixUnaryOp::RangeIncl => {
                    write!(f, "s(:irange,\n{}{}, nil)", indent, self.sub(expr))?;
                }
                PostfixUnaryOp::RangeExcl => {
                    write!(f, "s(:erange,\n{}{}, nil)", indent, self.sub(expr))?;
                }
            },
            ExprKind::Nil => {
                write!(f, "s(:nil)")?;
            }
            ExprKind::Assign { lhs, rhs } => match &lhs.kind {
                ExprKind::Ident { name } => {
                    write!(f, "s(:lvasgn, :{},\n{}{})", name, indent, self.sub(rhs))?;
                }
                _ => {
                    write!(f, "s(:lvasgn, <invalid>,\n{}{})", indent, self.sub(rhs))?;
                }
            },
            ExprKind::Module { cpath, body } => {
                write!(
                    f,
                    "s(:module,\n{}{},\n{}{})",
                    indent,
                    self.sub(cpath),
                    indent,
                    self.sub(body)
                )?;
            }
            ExprKind::Errored => {
                write!(f, "<invalid>")?;
            }
        }
        Ok(())
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

pub fn display_pgem<'a>(expr: &'a Expr) -> PgemDisplay<'a> {
    PgemDisplay { expr, nest: 0 }
}
