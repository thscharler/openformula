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
#[derive(PartialEq)]
pub enum AstTree<'a> {
    /// Empty expression
    NodeEmpty(OFEmpty<'a>),

    /// Comparison expression.
    NodeCompare(Box<AstTree<'a>>, OFCompOp<'a>, Box<AstTree<'a>>),
    /// Additive expression.
    NodeAdd(Box<AstTree<'a>>, OFAddOp<'a>, Box<AstTree<'a>>),
    /// Multiplicative expression.
    NodeMul(Box<AstTree<'a>>, OFMulOp<'a>, Box<AstTree<'a>>),
    /// Exponential expression.
    NodePow(Box<AstTree<'a>>, OFPowOp<'a>, Box<AstTree<'a>>),
    /// Postfix expression.
    NodePostfix(Box<AstTree<'a>>, OFPostfixOp<'a>),
    /// Prefix expression.
    NodePrefix(OFPrefixOp<'a>, Box<AstTree<'a>>),

    /// Number
    NodeNumber(OFNumber<'a>),
    /// String
    NodeString(OFString<'a>),

    /// CellRef
    NodeCellRef(OFCellRef<'a>),
    /// CellRange
    NodeCellRange(OFCellRange<'a>),
    /// ColRange
    NodeColRange(OFColRange<'a>),
    /// RowRange
    NodeRowRange(OFRowRange<'a>),

    /// Expression in parenthesis.
    NodeParenthesis(OFParOpen<'a>, Box<AstTree<'a>>, OFParClose<'a>),
    /// Function call expression.
    NodeFnCall(
        OFFnName<'a>,
        OFParOpen<'a>,
        Vec<AstTree<'a>>,
        OFParClose<'a>,
    ),
}

impl<'a> AstTree<'a> {
    /// Calculates the span of the complete AST tree.
    pub fn span(&self) -> Span<'a> {
        match self {
            AstTree::NodeEmpty(v) => v.1,

            AstTree::NodeCompare(ex1, _op, ex2) => unsafe {
                Self::span_union(ex1.span(), ex2.span())
            },
            AstTree::NodeAdd(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::NodeMul(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::NodePow(ex1, _op, ex2) => unsafe { Self::span_union(ex1.span(), ex2.span()) },
            AstTree::NodePrefix(op, ex) => unsafe { Self::span_union(op.span(), ex.span()) },
            AstTree::NodePostfix(ex, op) => unsafe { Self::span_union(ex.span(), op.span()) },

            AstTree::NodeNumber(v) => v.1,
            AstTree::NodeString(v) => v.1,

            AstTree::NodeCellRef(v) => v.1,
            AstTree::NodeCellRange(v) => v.1,
            AstTree::NodeColRange(v) => v.1,
            AstTree::NodeRowRange(v) => v.1,

            AstTree::NodeParenthesis(o, _ex, c) => unsafe { Self::span_union(o.span(), c.span()) },
            AstTree::NodeFnCall(name, _o, _v, c) => unsafe {
                Self::span_union(name.span(), c.span())
            },
        }
    }

    // Returns a new Span that reaches from the beginning of span0 to the end of span1.
    //
    // If any of the following conditions are violated, the result is Undefined Behavior:
    // * Both the starting and other pointer must be either in bounds or one byte past the end of the same allocated object.
    //      Should be guaranteed if both were obtained from on parse run.
    // * Both pointers must be derived from a pointer to the same object.
    //      Should be guaranteed if both were obtained from on parse run.
    // * The distance between the pointers, in bytes, cannot overflow an isize.
    // * The distance being in bounds cannot rely on “wrapping around” the address space.
    unsafe fn span_union(span0: Span<'a>, span1: Span<'a>) -> Span<'a> {
        let ptr = span0.as_ptr();
        // offset to the start of span1 and add the length of span1.
        let size = span0.offset(&span1) + span1.len();

        unsafe {
            // The size should be within the original allocation, if both spans are from
            // the same parse run. We must ensure that the parse run doesn't generate
            // Spans out of nothing that end in the ast.
            let slice = slice::from_raw_parts(ptr, size);
            // This is all from a str originally and we never got down to bytes.
            let str = from_utf8_unchecked(slice);

            // As span0 was ok the offset used here is ok too.
            Span::new_from_raw_offset(span0.location_offset(), span0.location_line(), str, ())
        }
    }

    /// Empty variant
    pub fn empty(s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeEmpty(OFEmpty((), s)))
    }

    /// CompareExpr variant
    pub fn compare(
        expr0: Box<AstTree<'a>>,
        op: OFCompOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeCompare(expr0, op, expr1))
    }

    /// AddExpr variant
    pub fn add(
        expr0: Box<AstTree<'a>>,
        op: OFAddOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeAdd(expr0, op, expr1))
    }

    /// MulExpr variant
    pub fn mul(
        expr0: Box<AstTree<'a>>,
        op: OFMulOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeMul(expr0, op, expr1))
    }

    /// PowExpr variant
    pub fn pow(
        expr0: Box<AstTree<'a>>,
        op: OFPowOp<'a>,
        expr1: Box<AstTree<'a>>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodePow(expr0, op, expr1))
    }

    /// PostfixExpr variant
    pub fn postfix(expr1: Box<AstTree<'a>>, op: OFPostfixOp<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodePostfix(expr1, op))
    }

    /// PrefixExpr variant
    pub fn prefix(op: OFPrefixOp<'a>, expr1: Box<AstTree<'a>>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodePrefix(op, expr1))
    }

    /// Number variant
    pub fn number(v: f64, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeNumber(OFNumber(v, s)))
    }

    /// String variant
    pub fn string(v: String, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeString(OFString(v, s)))
    }

    /// CellRef variant
    pub fn cell_ref(v: CellRef, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeCellRef(OFCellRef(v, s)))
    }

    /// CellRange variant
    pub fn cell_range(v: CellRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeCellRange(OFCellRange(v, s)))
    }

    /// ColRange variant
    pub fn col_range(v: ColRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeColRange(OFColRange(v, s)))
    }

    /// RowRange variant
    pub fn row_range(v: RowRange, s: Span<'a>) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeRowRange(OFRowRange(v, s)))
    }

    /// Parenthesis variant
    pub fn parenthesis(
        o: OFParOpen<'a>,
        expr: Box<AstTree<'a>>,
        c: OFParClose<'a>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeParenthesis(o, expr, c))
    }

    /// FnCall variant
    pub fn fn_call(
        name: Span<'a>,
        o: Span<'a>,
        v: Vec<AstTree<'a>>,
        c: Span<'a>,
    ) -> Box<AstTree<'a>> {
        Box::new(AstTree::NodeFnCall(
            OFFnName {
                name: name.to_string(),
                span: name,
            },
            OFParOpen { span: o },
            v,
            OFParClose { span: c },
        ))
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
        let self_name = match self {
            AstTree::NodeEmpty(_) => "empty",

            AstTree::NodeCompare(_, _, _) => "compare",
            AstTree::NodeAdd(_, _, _) => "add",
            AstTree::NodeMul(_, _, _) => "mul",
            AstTree::NodePow(_, _, _) => "pow",
            AstTree::NodePrefix(_, _) => "prefix",
            AstTree::NodePostfix(_, _) => "postfix",

            AstTree::NodeNumber(_) => "number",
            AstTree::NodeString(_) => "string",

            AstTree::NodeCellRef(_) => "cell_ref",
            AstTree::NodeCellRange(_) => "cell_range",
            AstTree::NodeColRange(_) => "col_range",
            AstTree::NodeRowRange(_) => "row_range",

            AstTree::NodeParenthesis(_, _, _) => "parens",
            AstTree::NodeFnCall(_, _, _, _) => "function",
        };

        write!(f, "{}    ", self_name)?;
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
            AstTree::NodeEmpty(v) => {
                write!(f, "()    ")?;
                self.debug_span(v.1, f)?;
                writeln!(f)?;
            }

            AstTree::NodeCompare(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::NodeAdd(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::NodeMul(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::NodePow(ex1, op, ex2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex1.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex2.debug(indent + 1, f)?;
            }
            AstTree::NodePrefix(op, ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
            }
            AstTree::NodePostfix(ex, op) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(op.to_string().as_str(), op.span(), f)?;
            }

            AstTree::NodeNumber(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::NodeString(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }

            AstTree::NodeCellRef(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::NodeCellRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::NodeColRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            AstTree::NodeRowRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }

            AstTree::NodeParenthesis(_o, ex, _c) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.debug(indent + 1, f)?;
            }
            AstTree::NodeFnCall(name, _par1, v, _par2) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                write!(f, "{} (   ", name)?;
                self.debug_span(name.span(), f)?;
                writeln!(f)?;

                for e in v {
                    self.indent(indent + 2, f)?;
                    e.debug(indent + 2, f)?;
                }

                self.indent(indent + 1, f)?;
                writeln!(f, ")")?;
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
            AstTree::NodeEmpty(v) => {
                write!(f, "{}", v)
            }

            AstTree::NodeCompare(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::NodeAdd(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::NodeMul(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::NodePow(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            AstTree::NodePostfix(expr, op) => {
                write!(f, "{}{}", expr, op)
            }
            AstTree::NodePrefix(op, expr) => {
                write!(f, "{}{}", op, expr)
            }

            AstTree::NodeNumber(v) => {
                write!(f, "{}", v)
            }
            AstTree::NodeString(v) => {
                write!(f, "{}", v)
            }

            AstTree::NodeCellRef(v) => {
                write!(f, "{}", v)
            }
            AstTree::NodeCellRange(v) => {
                write!(f, "{}", v)
            }
            AstTree::NodeRowRange(v) => {
                write!(f, "{}", v)
            }
            AstTree::NodeColRange(v) => {
                write!(f, "{}", v)
            }

            AstTree::NodeParenthesis(_o, expr, _c) => {
                write!(f, "({})", expr)
            }
            AstTree::NodeFnCall(name, _par1, v, _par2) => {
                write!(f, "{}(", name)?;
                for (i, e) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ";")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")?;
                Ok(())
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

/// Trait to extract the span.
pub trait HaveSpan<'a> {
    /// Returns the span of.
    fn span(&self) -> Span<'a>;
}

#[allow(clippy::manual_non_exhaustive)]
/// Empty
#[derive(Debug)]
pub struct OFEmpty<'a>((), pub Span<'a>);

impl<'a> HaveSpan<'a> for OFEmpty<'a> {
    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Display for OFEmpty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl<'a> PartialEq for OFEmpty<'a> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

/// Comparison operands.
#[derive(Debug)]
pub enum OFCompOp<'a> {
    /// Operator =
    Equal(Span<'a>),
    /// Operator <>
    Unequal(Span<'a>),
    /// Operator <
    Less(Span<'a>),
    /// Operator <=
    LessEqual(Span<'a>),
    /// Operator >
    Greater(Span<'a>),
    /// Operator >=
    GreaterEqual(Span<'a>),
}

op_decl!(OFCompOp);

impl<'a> HaveSpan<'a> for OFCompOp<'a> {
    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
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
    /// Operator +
    Add(Span<'a>),
    /// Operator -
    Subtract(Span<'a>),
}

op_decl!(OFAddOp);

impl<'a> HaveSpan<'a> for OFAddOp<'a> {
    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
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
    /// Operator *
    Multiply(Span<'a>),
    /// Operator /
    Divide(Span<'a>),
}

op_decl!(OFMulOp);

impl<'a> HaveSpan<'a> for OFMulOp<'a> {
    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
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
    /// Operator ^
    Power(Span<'a>),
}

op_decl!(OFPowOp);

impl<'a> HaveSpan<'a> for OFPowOp<'a> {
    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
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
    /// Operator %
    Percent(Span<'a>),
}

op_decl!(OFPostfixOp);

impl<'a> HaveSpan<'a> for OFPostfixOp<'a> {
    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
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

/// Prefix operands.
#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    /// Operator +
    Plus(Span<'a>),
    /// Operator -
    Minus(Span<'a>),
}

op_decl!(OFPrefixOp);

impl<'a> HaveSpan<'a> for OFPrefixOp<'a> {
    /// Returns the span for each variant.
    fn span(&self) -> Span<'a> {
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

/// Number
#[derive(Debug)]
pub struct OFNumber<'a>(pub f64, pub Span<'a>);

impl<'a> HaveSpan<'a> for OFNumber<'a> {
    fn span(&self) -> Span<'a> {
        self.1
    }
}

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

impl<'a> HaveSpan<'a> for OFString<'a> {
    fn span(&self) -> Span<'a> {
        self.1
    }
}

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

/// Paren open
#[derive(Debug, Eq, PartialEq)]
pub struct OFParOpen<'a> {
    /// Span
    pub span: Span<'a>,
}

impl<'a> HaveSpan<'a> for OFParOpen<'a> {
    /// Span
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Paren open
#[derive(Debug, Eq, PartialEq)]
pub struct OFParClose<'a> {
    /// Span
    pub span: Span<'a>,
}

impl<'a> HaveSpan<'a> for OFParClose<'a> {
    /// Span
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Function name.
#[derive(Debug, Eq, PartialEq)]
pub struct OFFnName<'a> {
    /// Function name.
    pub name: String,
    /// Span
    pub span: Span<'a>,
}

impl<'a> HaveSpan<'a> for OFFnName<'a> {
    /// Returns the span for each variant.
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Display for OFFnName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
