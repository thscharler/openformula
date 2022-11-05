//!
//! AST for OpenFormula
//!

use crate::parse::Span;
use crate::refs::CellRef;
use crate::{CellRange, ColRange, RowRange};
use nom::Offset;
use std::fmt::{Debug, Display, Formatter};
use std::str::from_utf8_unchecked;
use std::{mem, slice};

/// Defines the AST tree.
pub enum AstTree<'a> {
    /// TODO:
    Number(OFNumber<'a>),
    /// TODO:
    String(OFString<'a>),
    /// TODO:
    CellRef(OFCellRef<'a>),
    /// TODO:
    CellRange(OFCellRange<'a>),
    /// TODO:
    ColRange(OFColRange<'a>),
    /// TODO:
    RowRange(OFRowRange<'a>),
    /// TODO:
    Parenthesis(OFParOpen<'a>, Box<AstTree<'a>>, OFParClose<'a>),
    /// TODO:
    CompareExpr(Box<AstTree<'a>>, OFCompOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    AddExpr(Box<AstTree<'a>>, OFAddOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    MulExpr(Box<AstTree<'a>>, OFMulOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    PowExpr(Box<AstTree<'a>>, OFPowOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    PrefixExpr(OFPrefixOp<'a>, Box<AstTree<'a>>),
    /// TODO:
    PostfixExpr(Box<AstTree<'a>>, OFPostfixOp<'a>),
}

impl<'a> AstTree<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            AstTree::Number(v) => v.1,
            AstTree::String(v) => v.1,
            AstTree::CellRef(v) => v.1,
            AstTree::CellRange(v) => v.1,
            AstTree::ColRange(v) => v.1,
            AstTree::RowRange(v) => v.1,
            AstTree::Parenthesis(o, _ex, c) => unsafe { Self::span_union(o.span(), c.span()) },
            AstTree::CompareExpr(ex1, _op, ex2) => unsafe {
                Self::span_union(ex1.span(), ex2.span())
            },
            AstTree::AddExpr(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::MulExpr(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::PowExpr(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::PrefixExpr(op, ex) => unsafe { Self::span_union(op.span(), ex.span()) },
            AstTree::PostfixExpr(ex, op) => unsafe { Self::span_union(ex.span(), op.span()) },
        }
    }

    // Returns a new Span that reaches from the beginning of span0 to the end of span1.
    //
    // If any of the following conditions are violated, the result is Undefined Behavior:
    // * Both the starting and other pointer must be either in bounds or one byte past the end of the same allocated object.
    //      Should be guaranteed if both were obtained from on parse run.
    // * Both pointers must be derived from a pointer to the same object. (See below for an example.)
    //      Should be guaranteed if both were obtained from on parse run.
    // * The distance between the pointers, in bytes, cannot overflow an isize.
    // * The distance being in bounds cannot rely on “wrapping around” the address space.
    unsafe fn span_union(span0: Span<'a>, span1: Span<'a>) -> Span<'a> {
        let ptr = span0.as_ptr();
        // offset to the start of span1 and add the length of span1.
        let offset = span0.offset(&span1) + span1.len();

        unsafe {
            let slice = slice::from_raw_parts(ptr, offset);
            let str = from_utf8_unchecked(slice);

            Span::new_from_raw_offset(span0.location_offset(), span0.location_line(), str, ())
        }
    }

    /// Number variant
    pub fn number(v: f64, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::Number(OFNumber(v, s)))
    }

    /// String variant
    pub fn string(v: String, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::String(OFString(v, s)))
    }

    /// PrefixExpr variant
    pub fn prefix_expr(op: OFPrefixOp<'a>, expr1: Box<AstTree<'a>>) -> Box<AstTree<'a>> {
        Box::new(AstTree::PrefixExpr(op, expr1))
    }

    /// PostfixExpr variant
    pub fn postfix_expr(expr1: Box<AstTree<'a>>, op: OFPostfixOp<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::PostfixExpr(expr1, op))
    }

    /// CompareExpr variant
    pub fn compare_expr(
        expr0: Box<AstTree<'a>>,
        op: OFCompOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::CompareExpr(expr0, op, expr1))
    }

    /// AddExpr variant
    pub fn add_expr(
        expr0: Box<AstTree<'a>>,
        op: OFAddOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::AddExpr(expr0, op, expr1))
    }

    /// MulExpr variant
    pub fn mul_expr(
        expr0: Box<AstTree<'a>>,
        op: OFMulOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::MulExpr(expr0, op, expr1))
    }

    /// PowExpr variant
    pub fn pow_expr(
        expr0: Box<AstTree<'a>>,
        op: OFPowOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::PowExpr(expr0, op, expr1))
    }

    /// CellRef variant
    pub fn cellref(v: CellRef, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::CellRef(OFCellRef(v, s)))
    }

    /// CellRange variant
    pub fn cellrange(v: CellRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::CellRange(OFCellRange(v, s)))
    }

    /// ColRange variant
    pub fn colrange(v: ColRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::ColRange(OFColRange(v, s)))
    }

    /// RowRange variant
    pub fn rowrange(v: RowRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::RowRange(OFRowRange(v, s)))
    }
}

impl<'a> AstTree<'a> {
    fn indent(&self, indent: u32, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..indent * 4 {
            write!(f, " ")?;
        }
        Ok(())
    }

    fn arrow(&self, indent: u32, f: &mut Formatter<'_>) -> std::fmt::Result {
        if indent > 0 {
            for _ in 0..((indent - 1) * 4) {
                write!(f, " ")?;
            }
            for _ in ((indent - 1) * 4)..(indent * 4) - 2 {
                write!(f, "-")?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }

    fn debug_self(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let selfname = match self {
            AstTree::Number(_) => "number",
            AstTree::String(_) => "string",
            AstTree::CellRef(_) => "cellref",
            AstTree::CellRange(_) => "cellrange",
            AstTree::ColRange(_) => "colrange",
            AstTree::RowRange(_) => "rowrange",
            AstTree::Parenthesis(_, _, _) => "parens",
            AstTree::CompareExpr(_, _, _) => "compare",
            AstTree::AddExpr(_, _, _) => "add",
            AstTree::MulExpr(_, _, _) => "mul",
            AstTree::PowExpr(_, _, _) => "pow",
            AstTree::PrefixExpr(_, _) => "prefix",
            AstTree::PostfixExpr(_, _) => "postfix",
        };

        write!(f, "{}    ", selfname)?;
        self.debug_span(self.span(), f)?;
        writeln!(f)?;

        Ok(())
    }

    fn debug_op(&self, op: &str, span: Span<'a>, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}    ", op)?;
        self.debug_span(span, f)?;
        writeln!(f)?;

        Ok(())
    }

    fn debug_elem<T: Display>(
        &self,
        v: &T,
        span: Span<'a>,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}    ", v)?;
        self.debug_span(span, f)?;
        writeln!(f)?;

        Ok(())
    }

    fn debug_span(&self, span: Span<'a>, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "offset:{}, '{}'", span.location_offset(), *span)
    }

    fn debug(&self, indent: u32, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstTree::Number(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::String(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::CellRef(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::CellRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::ColRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::RowRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::Parenthesis(_o, ex, _c) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
            }
            AstTree::CompareExpr(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::AddExpr(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::MulExpr(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::PowExpr(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::PrefixExpr(op, ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
            }
            AstTree::PostfixExpr(ex, op) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
            }
        }
        Ok(())
    }
}

impl<'a> Debug for AstTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.debug(0, f)
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
            AstTree::Parenthesis(_o, expr, _c) => {
                write!(f, "({})", expr)
            }
            AstTree::PrefixExpr(op, expr) => {
                write!(f, "{}{}", op, expr)
            }
            AstTree::PostfixExpr(expr, op) => {
                write!(f, "{}{}", expr, op)
            }
            AstTree::CellRef(v) => {
                write!(f, "{}", v)
            }
            AstTree::CellRange(v) => {
                write!(f, "{}", v)
            }
            AstTree::RowRange(v) => {
                write!(f, "{}", v)
            }
            AstTree::ColRange(v) => {
                write!(f, "{}", v)
            }
            AstTree::CompareExpr(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::AddExpr(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::MulExpr(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::PowExpr(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
        }
    }
}

macro_rules! op_decl {
    ($name:ident) => {
        impl<'a> PartialEq for $name<'a> {
            fn eq(&self, other: &Self) -> bool {
                mem::discriminant(self) == mem::discriminant(other)
            }
        }
    };
}

/// Prefix operands.
#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    /// TODO:
    Plus(Span<'a>),
    /// TODO:
    Minus(Span<'a>),
}

op_decl!(OFPrefixOp);

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

/// Paren open
#[derive(Debug)]
pub struct OFParOpen<'a> {
    pub span: Span<'a>,
}

impl<'a> OFParOpen<'a> {
    pub fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Paren open
#[derive(Debug)]
pub struct OFParClose<'a> {
    pub span: Span<'a>,
}

impl<'a> OFParClose<'a> {
    pub fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Comparison operands.
#[derive(Debug)]
pub enum OFCompOp<'a> {
    /// TODO:
    Equal(Span<'a>),
    /// TODO:
    Unequal(Span<'a>),
    /// TODO:
    Less(Span<'a>),
    /// TODO:
    LessEqual(Span<'a>),
    /// TODO:
    Greater(Span<'a>),
    /// TODO:
    GreaterEqual(Span<'a>),
}

op_decl!(OFCompOp);

impl<'a> OFCompOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFCompOp::Equal(s) => *s,
            OFCompOp::Unequal(s) => *s,
            OFCompOp::Less(s) => *s,
            OFCompOp::LessEqual(s) => *s,
            OFCompOp::Greater(s) => *s,
            OFCompOp::GreaterEqual(s) => *s,
        }
    }
}

impl<'a> Display for OFCompOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFCompOp::Equal(_) => write!(f, "="),
            OFCompOp::Unequal(_) => write!(f, "<>"),
            OFCompOp::Less(_) => write!(f, "<"),
            OFCompOp::LessEqual(_) => write!(f, "<="),
            OFCompOp::Greater(_) => write!(f, ">"),
            OFCompOp::GreaterEqual(_) => write!(f, ">="),
        }
    }
}

/// Additive operands.
#[derive(Debug)]
pub enum OFAddOp<'a> {
    /// TODO:
    Add(Span<'a>),
    /// TODO:
    Subtract(Span<'a>),
}

op_decl!(OFAddOp);

impl<'a> OFAddOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFAddOp::Add(span) => *span,
            OFAddOp::Subtract(span) => *span,
        }
    }
}

impl<'a> Display for OFAddOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFAddOp::Add(_) => write!(f, "+"),
            OFAddOp::Subtract(_) => write!(f, "-"),
        }
    }
}

/// Multiplicative operands.
#[derive(Debug)]
pub enum OFMulOp<'a> {
    /// TODO:
    Multiply(Span<'a>),
    /// TODO:
    Divide(Span<'a>),
}

op_decl!(OFMulOp);

impl<'a> OFMulOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFMulOp::Multiply(span) => *span,
            OFMulOp::Divide(span) => *span,
        }
    }
}

impl<'a> Display for OFMulOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFMulOp::Multiply(_) => write!(f, "*"),
            OFMulOp::Divide(_) => write!(f, "/"),
        }
    }
}

/// Power operand.
#[derive(Debug)]
pub enum OFPowOp<'a> {
    Power(Span<'a>),
}

op_decl!(OFPowOp);

impl<'a> OFPowOp<'a> {
    /// Extracts the span from each variant.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFPowOp::Power(span) => *span,
        }
    }
}

impl<'a> Display for OFPowOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFPowOp::Power(_) => write!(f, "^"),
        }
    }
}

/// Postfix operands.
#[derive(Debug)]
pub enum OFPostfixOp<'a> {
    /// TODO:
    Percent(Span<'a>),
}

op_decl!(OFPostfixOp);

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

/// CellRange
#[derive(Debug)]
pub struct OFCellRange<'a>(pub CellRange, pub Span<'a>);

impl<'a> Display for OFCellRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFCellRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// RowRange
#[derive(Debug)]
pub struct OFRowRange<'a>(pub RowRange, pub Span<'a>);

impl<'a> Display for OFRowRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFRowRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// ColRange
#[derive(Debug)]
pub struct OFColRange<'a>(pub ColRange, pub Span<'a>);

impl<'a> Display for OFColRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFColRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
