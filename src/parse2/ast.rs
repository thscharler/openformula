//!
//! AST for OpenFormula
//!

use crate::parse2::refs::CellRef;
use crate::parse2::Span;
use std::fmt::{Display, Formatter};
use std::mem;

/// Defines the AST tree.
#[derive(Debug)]
pub enum AstTree<'a> {
    /// TODO:
    Number(OFNumber<'a>),
    /// TODO:
    String(OFString<'a>),
    /// TODO:
    CellRef(OFCellRef<'a>),
    /// TODO:
    Parenthesis(Box<AstTree<'a>>),
    /// TODO:
    PrefixOp(OFPrefixOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    InfixOp(Box<AstTree<'a>>, OFInfixOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    PostfixOp(Box<AstTree<'a>>, OFPostfixOp<'a>),
}

impl<'a> AstTree<'a> {
    /// Number variant
    pub fn number(v: f64, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::Number(OFNumber(v, s)))
    }

    /// String variant
    pub fn string(v: String, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::String(OFString(v, s)))
    }
}

impl<'a> Display for AstTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstTree::Number(v) => {
                write!(f, "{}", v)
            }
            AstTree::String(v) => {
                write!(f, "{}", v)
            }
            AstTree::Parenthesis(expr) => {
                write!(f, "({})", expr)
            }
            AstTree::PrefixOp(op, expr) => {
                write!(f, "{}{}", op, expr)
            }
            AstTree::InfixOp(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::PostfixOp(expr, op) => {
                write!(f, "{}{}", expr, op)
            }
            AstTree::CellRef(cellref) => {
                write!(f, "{}", cellref)
            }
        }
    }
}

/// Prefix operands.
#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    /// TODO:
    Plus(Span<'a>),
    /// TODO:
    Minus(Span<'a>),
}

impl<'a> OFPrefixOp<'a> {
    /// Returns the span for each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFPrefixOp::Plus(span) => *span,
            OFPrefixOp::Minus(span) => *span,
        }
    }
}

impl<'a> Display for OFPrefixOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFPrefixOp::Plus(_) => write!(f, "+"),
            OFPrefixOp::Minus(_) => write!(f, "-"),
        }
    }
}

impl<'a> PartialEq for OFPrefixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

/// Infix operands.
/// TODO: split this one
#[derive(Debug)]
pub enum OFInfixOp<'a> {
    /// TODO:
    Add(Span<'a>),
    /// TODO:
    Subtract(Span<'a>),
    /// TODO:
    Multiply(Span<'a>),
    /// TODO:
    Divide(Span<'a>),
    /// TODO:
    Power(Span<'a>),
}

impl<'a> OFInfixOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFInfixOp::Add(span) => *span,
            OFInfixOp::Subtract(span) => *span,
            OFInfixOp::Multiply(span) => *span,
            OFInfixOp::Divide(span) => *span,
            OFInfixOp::Power(span) => *span,
        }
    }
}

impl<'a> Display for OFInfixOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFInfixOp::Add(_) => write!(f, "+"),
            OFInfixOp::Subtract(_) => write!(f, "-"),
            OFInfixOp::Multiply(_) => write!(f, "*"),
            OFInfixOp::Divide(_) => write!(f, "/"),
            OFInfixOp::Power(_) => write!(f, "^"),
        }
    }
}

impl<'a> PartialEq for OFInfixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

/// Postfix operands.
#[derive(Debug)]
pub enum OFPostfixOp<'a> {
    /// TODO:
    Percent(Span<'a>),
}

impl<'a> OFPostfixOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFPostfixOp::Percent(span) => *span,
        }
    }
}

impl<'a> Display for OFPostfixOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFPostfixOp::Percent(_) => write!(f, "%"),
        }
    }
}

impl<'a> PartialEq for OFPostfixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

/// Number
#[derive(Debug)]
pub struct OFNumber<'a>(pub f64, pub Span<'a>);

impl<'a> Display for OFNumber<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFNumber<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// String
#[derive(Debug)]
pub struct OFString<'a>(pub String, pub Span<'a>);

impl<'a> Display for OFString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFString<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// CellRef
#[derive(Debug)]
pub struct OFCellRef<'a>(pub CellRef, pub Span<'a>);

impl<'a> Display for OFCellRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFCellRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
