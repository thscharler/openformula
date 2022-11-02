use crate::parse2::refs::CellRef;
use crate::parse2::Span;
use std::fmt::{Display, Formatter};
use std::mem;

#[derive(Debug)]
pub enum AstTree<'a> {
    Empty(),
    Number(OFNumber<'a>),
    String(OFString<'a>),
    CellRef(OFCellRef<'a>),
    Parenthesis(Box<AstTree<'a>>),
    PrefixOp(OFPrefixOp<'a>, Box<AstTree<'a>>),
    InfixOp(Box<AstTree<'a>>, OFInfixOp<'a>, Box<AstTree<'a>>),
    PostfixOp(Box<AstTree<'a>>, OFPostfixOp<'a>),
}

impl<'a> Display for AstTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstTree::Empty() => Ok(()),
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

#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    Plus(Span<'a>),
    Minus(Span<'a>),
}

impl<'a> OFPrefixOp<'a> {
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

#[derive(Debug)]
pub enum OFInfixOp<'a> {
    Add(Span<'a>),
    Subtract(Span<'a>),
    Multiply(Span<'a>),
    Divide(Span<'a>),
    Power(Span<'a>),
}

impl<'a> OFInfixOp<'a> {
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

#[derive(Debug)]
pub enum OFPostfixOp<'a> {
    Percent(Span<'a>),
}

impl<'a> OFPostfixOp<'a> {
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
