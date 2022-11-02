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
    PrefixOp(PrefixToken<'a>, Box<AstTree<'a>>),
    InfixOp(Box<AstTree<'a>>, InfixToken<'a>, Box<AstTree<'a>>),
    PostfixOp(Box<AstTree<'a>>, PostfixToken<'a>),
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
            AstTree::CellRef(_) => {
                todo!("ast cellref");
            }
        }
    }
}

#[derive(Debug)]
pub enum PrefixToken<'a> {
    Plus(Span<'a>),
    Minus(Span<'a>),
}

impl<'a> PrefixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            PrefixToken::Plus(span) => *span,
            PrefixToken::Minus(span) => *span,
        }
    }
}

impl<'a> Display for PrefixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixToken::Plus(_) => write!(f, "+"),
            PrefixToken::Minus(_) => write!(f, "-"),
        }
    }
}

impl<'a> PartialEq for PrefixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum InfixToken<'a> {
    Add(Span<'a>),
    Subtract(Span<'a>),
    Multiply(Span<'a>),
    Divide(Span<'a>),
    Power(Span<'a>),
}

impl<'a> InfixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            InfixToken::Add(span) => *span,
            InfixToken::Subtract(span) => *span,
            InfixToken::Multiply(span) => *span,
            InfixToken::Divide(span) => *span,
            InfixToken::Power(span) => *span,
        }
    }
}

impl<'a> Display for InfixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixToken::Add(_) => write!(f, "+"),
            InfixToken::Subtract(_) => write!(f, "-"),
            InfixToken::Multiply(_) => write!(f, "*"),
            InfixToken::Divide(_) => write!(f, "/"),
            InfixToken::Power(_) => write!(f, "^"),
        }
    }
}

impl<'a> PartialEq for InfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum PostfixToken<'a> {
    Percent(Span<'a>),
}

impl<'a> PostfixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            PostfixToken::Percent(span) => *span,
        }
    }
}

impl<'a> Display for PostfixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PostfixToken::Percent(_) => write!(f, "%"),
        }
    }
}

impl<'a> PartialEq for PostfixToken<'a> {
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
