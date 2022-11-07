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
pub enum OFAst<'a> {
    /// Empty expression
    NodeEmpty(OFEmpty<'a>),

    /// Comparison expression.
    NodeCompare(OFCompare<'a>),
    /// Additive expression.
    NodeAdd(OFAdd<'a>),
    /// Multiplicative expression.
    NodeMul(OFMul<'a>),
    /// Exponential expression.
    NodePow(OFPow<'a>),
    /// Postfix expression.
    NodePostfix(OFPostfix<'a>),
    /// Prefix expression.
    NodePrefix(OFPrefix<'a>),

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

    /// Expression in parentheses.
    NodeParens(OFParens<'a>),
    /// Function call expression.
    NodeFnCall(OFFnCall<'a>),
}

impl<'a> OFAst<'a> {
    /// Calculates the span of the complete AST tree.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFAst::NodeEmpty(v) => v.1,
            OFAst::NodeCompare(ex) => ex.span(),
            OFAst::NodeAdd(ex) => ex.span(),
            OFAst::NodeMul(ex) => ex.span(),
            OFAst::NodePow(ex) => ex.span(),
            OFAst::NodePrefix(ex) => ex.span(),
            OFAst::NodePostfix(ex) => ex.span(),
            OFAst::NodeNumber(v) => v.1,
            OFAst::NodeString(v) => v.1,
            OFAst::NodeCellRef(v) => v.1,
            OFAst::NodeCellRange(v) => v.1,
            OFAst::NodeColRange(v) => v.1,
            OFAst::NodeRowRange(v) => v.1,
            OFAst::NodeParens(ex) => ex.span(),
            OFAst::NodeFnCall(ex) => ex.span(),
        }
    }

    /// Empty variant
    pub fn empty(s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeEmpty(OFEmpty((), s)))
    }

    /// CompareExpr variant
    pub fn compare(
        expr0: Box<OFAst<'a>>,
        op: OFCompOp<'a>,
        expr1: Box<OFAst<'a>>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCompare(OFCompare {
            left: expr0,
            op,
            right: expr1,
        }))
    }

    /// AddExpr variant
    pub fn add(expr0: Box<OFAst<'a>>, op: OFAddOp<'a>, expr1: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeAdd(OFAdd {
            left: expr0,
            op,
            right: expr1,
        }))
    }

    /// MulExpr variant
    pub fn mul(expr0: Box<OFAst<'a>>, op: OFMulOp<'a>, expr1: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeMul(OFMul {
            left: expr0,
            op,
            right: expr1,
        }))
    }

    /// PowExpr variant
    pub fn pow(expr0: Box<OFAst<'a>>, op: OFPowOp<'a>, expr1: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePow(OFPow {
            left: expr0,
            op,
            right: expr1,
        }))
    }

    /// PostfixExpr variant
    pub fn postfix(expr1: Box<OFAst<'a>>, op: OFPostfixOp<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePostfix(OFPostfix { expr: expr1, op }))
    }

    /// PrefixExpr variant
    pub fn prefix(op: OFPrefixOp<'a>, expr1: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePrefix(OFPrefix { op, expr: expr1 }))
    }

    /// Number variant
    pub fn number(v: f64, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeNumber(OFNumber(v, s)))
    }

    /// String variant
    pub fn string(v: String, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeString(OFString(v, s)))
    }

    /// CellRef variant
    pub fn cell_ref(v: CellRef, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCellRef(OFCellRef(v, s)))
    }

    /// CellRange variant
    pub fn cell_range(v: CellRange, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCellRange(OFCellRange(v, s)))
    }

    /// ColRange variant
    pub fn col_range(v: ColRange, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeColRange(OFColRange(v, s)))
    }

    /// RowRange variant
    pub fn row_range(v: RowRange, s: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeRowRange(OFRowRange(v, s)))
    }

    /// Parens variant
    pub fn parens(o: OFParOpen<'a>, expr: Box<OFAst<'a>>, c: OFParClose<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeParens(OFParens { o, expr, c }))
    }

    /// FnCall variant
    pub fn fn_call(
        name: Span<'a>,
        o: Span<'a>,
        arg: Vec<OFAst<'a>>,
        c: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeFnCall(OFFnCall {
            name: OFFnName {
                name: "".to_string(),
                span: name,
            },
            o: OFParOpen { span: o },
            arg,
            c: OFParClose { span: c },
        }))
    }
}

impl<'a> OFAst<'a> {
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
            OFAst::NodeEmpty(_) => "empty",
            OFAst::NodeCompare(_) => "compare",
            OFAst::NodeAdd(_) => "add",
            OFAst::NodeMul(_) => "mul",
            OFAst::NodePow(_) => "pow",
            OFAst::NodePrefix(_) => "prefix",
            OFAst::NodePostfix(_) => "postfix",
            OFAst::NodeNumber(_) => "number",
            OFAst::NodeString(_) => "string",
            OFAst::NodeCellRef(_) => "cell_ref",
            OFAst::NodeCellRange(_) => "cell_range",
            OFAst::NodeColRange(_) => "col_range",
            OFAst::NodeRowRange(_) => "row_range",
            OFAst::NodeParens(_) => "parens",
            OFAst::NodeFnCall(_) => "function",
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
            OFAst::NodeEmpty(v) => {
                write!(f, "()    ")?;
                self.debug_span(v.1, f)?;
                writeln!(f)?;
            }
            OFAst::NodeCompare(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.left.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.right.debug(indent + 1, f)?;
            }
            OFAst::NodeAdd(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.left.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.right.debug(indent + 1, f)?;
            }
            OFAst::NodeMul(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.left.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.right.debug(indent + 1, f)?;
            }
            OFAst::NodePow(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.left.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.right.debug(indent + 1, f)?;
            }
            OFAst::NodePrefix(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
                self.indent(indent + 1, f)?;
                ex.expr.debug(indent + 1, f)?;
            }
            OFAst::NodePostfix(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.expr.debug(indent + 1, f)?;
                self.indent(indent + 1, f)?;
                self.debug_op(ex.op.to_string().as_str(), ex.op.span(), f)?;
            }
            OFAst::NodeNumber(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeString(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeCellRef(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeCellRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeColRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeRowRange(v) => {
                self.debug_elem(&v.0, v.1, f)?;
            }
            OFAst::NodeParens(ex) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                ex.expr.debug(indent + 1, f)?;
            }
            OFAst::NodeFnCall(ff) => {
                self.debug_self(f)?;

                self.arrow(indent + 1, f)?;
                write!(f, "{} (   ", ff.name)?;
                self.debug_span(ff.name.span(), f)?;
                writeln!(f)?;

                for ex in &ff.arg {
                    self.indent(indent + 2, f)?;
                    ex.debug(indent + 2, f)?;
                }

                self.indent(indent + 1, f)?;
                writeln!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl<'a> Debug for OFAst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.debug(0, f)
    }
}

impl<'a> Display for OFAst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OFAst::NodeEmpty(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeCompare(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodeAdd(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodeMul(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodePow(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodePostfix(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodePrefix(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodeNumber(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeString(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeCellRef(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeCellRange(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeRowRange(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeColRange(v) => {
                write!(f, "{}", v)
            }
            OFAst::NodeParens(ex) => {
                write!(f, "{}", ex)
            }
            OFAst::NodeFnCall(ex) => {
                write!(f, "{}", ex)
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

/// Comparison expression.
#[derive(Debug, PartialEq)]
pub struct OFCompare<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFCompOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> HaveSpan<'a> for OFCompare<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> Display for OFCompare<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

/// Comparison operators.
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

/// Additive expression
#[derive(Debug, PartialEq)]
pub struct OFAdd<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFAddOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> HaveSpan<'a> for OFAdd<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> Display for OFAdd<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

/// Additive operators.
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

/// Multiplication expression.
#[derive(Debug, PartialEq)]
pub struct OFMul<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFMulOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> HaveSpan<'a> for OFMul<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> Display for OFMul<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

/// Multiplicative operators.
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

/// Power expression
#[derive(Debug, PartialEq)]
pub struct OFPow<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFPowOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> HaveSpan<'a> for OFPow<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> Display for OFPow<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

/// Power operator.
#[derive(Debug)]
pub enum OFPowOp<'a> {
    /// Operator ^
    Power(Span<'a>),
}

op_decl!(OFPowOp);

impl<'a> HaveSpan<'a> for OFPowOp<'a> {
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

/// Postfix expression
#[derive(Debug, PartialEq)]
pub struct OFPostfix<'a> {
    /// Expression
    pub expr: Box<OFAst<'a>>,
    /// Operator
    pub op: OFPostfixOp<'a>,
}

impl<'a> HaveSpan<'a> for OFPostfix<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.expr.span(), self.op.span()) }
    }
}

impl<'a> Display for OFPostfix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.expr, self.op)
    }
}

/// Postfix operators.
#[derive(Debug)]
pub enum OFPostfixOp<'a> {
    /// Operator %
    Percent(Span<'a>),
}

op_decl!(OFPostfixOp);

impl<'a> HaveSpan<'a> for OFPostfixOp<'a> {
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

/// Prefix expression.
#[derive(Debug, PartialEq)]
pub struct OFPrefix<'a> {
    /// Operator
    pub op: OFPrefixOp<'a>,
    /// Expression
    pub expr: Box<OFAst<'a>>,
}

impl<'a> HaveSpan<'a> for OFPrefix<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.op.span(), self.expr.span()) }
    }
}

impl<'a> Display for OFPrefix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
}

/// Prefix operators.
#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    /// Operator +
    Plus(Span<'a>),
    /// Operator -
    Minus(Span<'a>),
}

op_decl!(OFPrefixOp);

impl<'a> HaveSpan<'a> for OFPrefixOp<'a> {
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

/// Expression in parentheses.
#[derive(Debug, PartialEq)]
pub struct OFParens<'a> {
    /// Open parentheses
    pub o: OFParOpen<'a>,
    /// Expression
    pub expr: Box<OFAst<'a>>,
    /// Closing parentheses
    pub c: OFParClose<'a>,
}

impl<'a> HaveSpan<'a> for OFParens<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.o.span(), self.c.span()) }
    }
}

impl<'a> Display for OFParens<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)
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

/// Function call
#[derive(Debug, PartialEq)]
pub struct OFFnCall<'a> {
    /// Name
    pub name: OFFnName<'a>,
    /// Open parentheses
    pub o: OFParOpen<'a>,
    /// Args
    pub arg: Vec<OFAst<'a>>,
    /// Closing parentheses
    pub c: OFParClose<'a>,
}

impl<'a> HaveSpan<'a> for OFFnCall<'a> {
    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.name.span(), self.c.span()) }
    }
}

impl<'a> Display for OFFnCall<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, expr) in self.arg.iter().enumerate() {
            if i > 0 {
                write!(f, ";")?;
            }
            write!(f, "{}", expr)?;
        }
        write!(f, ")")?;
        Ok(())
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

// Returns a new Span that reaches from the beginning of span0 to the end of span1.
//
// If any of the following conditions are violated, the result is Undefined Behavior:
// * Both the starting and other pointer must be either in bounds or one byte past the end of the same allocated object.
//      Should be guaranteed if both were obtained from on parse run.
// * Both pointers must be derived from a pointer to the same object.
//      Should be guaranteed if both were obtained from on parse run.
// * The distance between the pointers, in bytes, cannot overflow an isize.
// * The distance being in bounds cannot rely on “wrapping around” the address space.
unsafe fn span_union<'a>(span0: Span<'a>, span1: Span<'a>) -> Span<'a> {
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
