//!
//! AST for OpenFormula.
//!

use crate::{conv, format, tokens};
use iparse::span::span_union;
use iparse::Span;
use std::fmt::{Debug, Display};
use std::{fmt, mem};

mod debug_ast;

/// Defines the AST tree.
#[allow(clippy::enum_variant_names)]
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
    /// Named expression
    NodeNamed(OFNamed<'a>),

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
    /// Returns the contained value as a &dyn Node.
    pub fn node(&self) -> &dyn Node<'a> {
        match self {
            OFAst::NodeAdd(v) => v,
            OFAst::NodeCellRange(v) => v,
            OFAst::NodeCellRef(v) => v,
            OFAst::NodeColRange(v) => v,
            OFAst::NodeCompare(v) => v,
            OFAst::NodeEmpty(v) => v,
            OFAst::NodeFnCall(v) => v,
            OFAst::NodeMul(v) => v,
            OFAst::NodeNamed(v) => v,
            OFAst::NodeNumber(v) => v,
            OFAst::NodeParens(v) => v,
            OFAst::NodePostfix(v) => v,
            OFAst::NodePow(v) => v,
            OFAst::NodePrefix(v) => v,
            OFAst::NodeRowRange(v) => v,
            OFAst::NodeString(v) => v,
        }
    }
}

impl<'a> Node<'a> for OFAst<'a> {
    fn name(&self) -> &str {
        "AST"
    }

    fn span(&self) -> Span<'a> {
        self.node().span()
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node().encode(f)
    }
}

//
// Functions that return some OFxxx
//
impl<'a> OFAst<'a> {
    /// Creates a OFIri
    pub fn iri(iri: String, span: Span<'a>) -> OFIri<'a> {
        OFIri { iri, span }
    }

    /// Creates a OFSheetName
    pub fn sheet_name(abs: bool, name: String, span: Span<'a>) -> OFSheetName<'a> {
        OFSheetName { abs, name, span }
    }

    /// Creates a OFRow
    pub fn row(abs: bool, row: u32, span: Span<'a>) -> OFRow<'a> {
        OFRow { abs, row, span }
    }

    /// Creates a OFCol
    pub fn col(abs: bool, col: u32, span: Span<'a>) -> OFCol<'a> {
        OFCol { abs, col, span }
    }

    /// Creates a OFIdentifier
    pub fn simple_named(ident: String, span: Span<'a>) -> OFSimpleNamed<'a> {
        OFSimpleNamed { ident, span }
    }

    /// Creates a OFFnName
    pub fn fn_name(name: String, span: Span<'a>) -> OFFnName<'a> {
        OFFnName { name, span }
    }
}

//
// Functions that return a Box<OFAst>
//
impl<'a> OFAst<'a> {
    /// Empty variant
    pub fn empty(span: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeEmpty(OFEmpty { span }))
    }

    /// CompareExpr variant
    pub fn compare(
        left: Box<OFAst<'a>>,
        op: OFCompOp<'a>,
        right: Box<OFAst<'a>>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCompare(OFCompare { left, op, right }))
    }

    /// AddExpr variant
    pub fn add(left: Box<OFAst<'a>>, op: OFAddOp<'a>, right: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeAdd(OFAdd { left, op, right }))
    }

    /// MulExpr variant
    pub fn mul(left: Box<OFAst<'a>>, op: OFMulOp<'a>, right: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeMul(OFMul { left, op, right }))
    }

    /// PowExpr variant
    pub fn pow(left: Box<OFAst<'a>>, op: OFPowOp<'a>, right: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePow(OFPow { left, op, right }))
    }

    /// PostfixExpr variant
    pub fn postfix(expr: Box<OFAst<'a>>, op: OFPostfixOp<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePostfix(OFPostfix { expr, op }))
    }

    /// PrefixExpr variant
    pub fn prefix(op: OFPrefixOp<'a>, expr: Box<OFAst<'a>>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodePrefix(OFPrefix { op, expr }))
    }

    /// Number variant
    pub fn number(num: f64, span: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeNumber(OFNumber { num, span }))
    }

    /// String variant
    pub fn string(str: String, span: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeString(OFString { str, span }))
    }

    /// Named variant
    pub fn named(
        iri: Option<OFIri<'a>>,
        sheet_name: Option<OFSheetName<'a>>,
        identifier: OFSimpleNamed<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeNamed(OFNamed {
            iri,
            sheet_name,
            simple: identifier,
        }))
    }

    /// CellRef variant
    pub fn cell_ref(
        iri: Option<OFIri<'a>>,
        table: Option<OFSheetName<'a>>,
        row: OFRow<'a>,
        col: OFCol<'a>,
        span: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCellRef(OFCellRef {
            iri,
            table,
            row,
            col,
            span,
        }))
    }

    /// CellRange variant
    #[allow(clippy::too_many_arguments)]
    pub fn cell_range(
        iri: Option<OFIri<'a>>,
        table: Option<OFSheetName<'a>>,
        row: OFRow<'a>,
        col: OFCol<'a>,
        to_table: Option<OFSheetName<'a>>,
        to_row: OFRow<'a>,
        to_col: OFCol<'a>,
        span: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeCellRange(OFCellRange {
            iri,
            table,
            row,
            col,
            to_table,
            to_row,
            to_col,
            span,
        }))
    }

    /// ColRange variant
    pub fn col_range(
        iri: Option<OFIri<'a>>,
        table: Option<OFSheetName<'a>>,
        col: OFCol<'a>,
        to_table: Option<OFSheetName<'a>>,
        to_col: OFCol<'a>,
        span: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeColRange(OFColRange {
            iri,
            table,
            col,
            to_table,
            to_col,
            span,
        }))
    }

    /// RowRange variant
    pub fn row_range(
        iri: Option<OFIri<'a>>,
        table: Option<OFSheetName<'a>>,
        row: OFRow<'a>,
        to_table: Option<OFSheetName<'a>>,
        to_row: OFRow<'a>,
        span: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeRowRange(OFRowRange {
            iri,
            table,
            row,
            to_table,
            to_row,
            span,
        }))
    }

    /// Parens variant
    pub fn parens(o: Span<'a>, expr: Box<OFAst<'a>>, c: Span<'a>) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeParens(OFParens { o, expr, c }))
    }

    /// FnCall variant
    pub fn fn_call(
        name: OFFnName<'a>,
        o: Span<'a>,
        arg: Vec<OFAst<'a>>,
        c: Span<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeFnCall(OFFnCall { name, o, arg, c }))
    }
}

impl<'a> Display for OFAst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OFAst::NodeEmpty(v) => Display::fmt(v, f),
            OFAst::NodeCompare(v) => Display::fmt(v, f),
            OFAst::NodeAdd(v) => Display::fmt(v, f),
            OFAst::NodeMul(v) => Display::fmt(v, f),
            OFAst::NodePow(v) => Display::fmt(v, f),
            OFAst::NodePostfix(v) => Display::fmt(v, f),
            OFAst::NodePrefix(v) => Display::fmt(v, f),
            OFAst::NodeNumber(v) => Display::fmt(v, f),
            OFAst::NodeString(v) => Display::fmt(v, f),
            OFAst::NodeCellRef(v) => Display::fmt(v, f),
            OFAst::NodeCellRange(v) => Display::fmt(v, f),
            OFAst::NodeRowRange(v) => Display::fmt(v, f),
            OFAst::NodeColRange(v) => Display::fmt(v, f),
            OFAst::NodeParens(v) => Display::fmt(v, f),
            OFAst::NodeFnCall(v) => Display::fmt(v, f),
            OFAst::NodeNamed(v) => Display::fmt(v, f),
        }
    }
}

#[allow(missing_debug_implementations)]
/// Helper struct that implements Display an calls encode for the node.
/// This seems to be the pattern to implement an alternate Display trait ...
pub struct NodeEncoder<'a>(&'a dyn Node<'a>);

impl<'a> Display for NodeEncoder<'a> {
    /// Calls f with the given fmt::Formatter.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.encode(f)
    }
}

/// General Node trait.
pub trait Node<'a> {
    /// Name for Debug.
    fn name(&self) -> &str;

    /// Returns the span of.
    fn span(&self) -> Span<'a>;

    /// Writes an encoded version of the Node. This is a string, that can be parsed again.
    /// This is mainly the string encoding with double-'
    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// Recursive call for encode masked in impl Display.
    fn enc(&'a self) -> NodeEncoder<'a>
    where
        Self: Sized,
    {
        NodeEncoder(self)
    }
}

/// Trait for binary operation nodes.
pub trait BinaryNode<'a>: Node<'a> {
    /// Type of the operator.
    type Op: Operator<'a>;

    /// Left operand.
    fn left(&self) -> &OFAst<'a>;
    /// Operator.
    fn op(&self) -> &Self::Op;
    /// Right operand.
    fn right(&self) -> &OFAst<'a>;
}

/// Identifies an operator node.
pub trait Operator<'a>: Node<'a> + Display {
    /// Returns the operator token.
    fn op(&self) -> &str;
}

//
// TOKENS
//

// OFEmpty ***************************************************************

#[allow(clippy::manual_non_exhaustive)]
/// Empty
pub struct OFEmpty<'a> {
    /// Span of nothing.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFEmpty<'a> {
    fn name(&self) -> &str {
        "Empty"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl<'a> Display for OFEmpty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl<'a> PartialEq for OFEmpty<'a> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

// OFCompare *************************************************************

/// Comparison expression.
#[derive(PartialEq)]
pub struct OFCompare<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFCompOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> Node<'a> for OFCompare<'a> {
    fn name(&self) -> &str {
        "Compare"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left.enc(),
            self.op.enc(),
            self.right.enc()
        )
    }
}

impl<'a> BinaryNode<'a> for OFCompare<'a> {
    type Op = OFCompOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        self.left.as_ref()
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        self.right.as_ref()
    }
}

impl<'a> Display for OFCompare<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

// OFCompOp **************************************************************

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

impl<'a> PartialEq for OFCompOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFCompOp<'a> {
    fn op(&self) -> &str {
        match self {
            OFCompOp::Equal(_) => "=",
            OFCompOp::Unequal(_) => "<>",
            OFCompOp::Less(_) => "<",
            OFCompOp::LessEqual(_) => "<=",
            OFCompOp::Greater(_) => ">",
            OFCompOp::GreaterEqual(_) => ">=",
        }
    }
}

impl<'a> Node<'a> for OFCompOp<'a> {
    fn name(&self) -> &str {
        "CompareOp"
    }

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

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFCompOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFAdd *****************************************************************

/// Additive expression
#[derive(PartialEq)]
pub struct OFAdd<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFAddOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> Node<'a> for OFAdd<'a> {
    fn name(&self) -> &str {
        "Add"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left.enc(),
            self.op.enc(),
            self.right.enc()
        )
    }
}

impl<'a> BinaryNode<'a> for OFAdd<'a> {
    type Op = OFAddOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        self.left.as_ref()
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        self.right.as_ref()
    }
}

impl<'a> Display for OFAdd<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

// OFAddOp ***************************************************************

/// Additive operators.
#[derive(Debug)]
pub enum OFAddOp<'a> {
    /// Operator +
    Add(Span<'a>),
    /// Operator -
    Subtract(Span<'a>),
}

impl<'a> PartialEq for OFAddOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFAddOp<'a> {
    fn op(&self) -> &str {
        match self {
            OFAddOp::Add(_) => "+",
            OFAddOp::Subtract(_) => "-",
        }
    }
}

impl<'a> Node<'a> for OFAddOp<'a> {
    fn name(&self) -> &str {
        "AddOp"
    }

    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
        match self {
            OFAddOp::Add(span) => *span,
            OFAddOp::Subtract(span) => *span,
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFAddOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFMul *****************************************************************

/// Multiplication expression.
#[derive(PartialEq)]
pub struct OFMul<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFMulOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> Node<'a> for OFMul<'a> {
    fn name(&self) -> &str {
        "Mul"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left.enc(),
            self.op.enc(),
            self.right.enc()
        )
    }
}

impl<'a> BinaryNode<'a> for OFMul<'a> {
    type Op = OFMulOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        self.left.as_ref()
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        self.right.as_ref()
    }
}

impl<'a> Display for OFMul<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

// OFMulOp ***************************************************************

/// Multiplicative operators.
#[derive(Debug)]
pub enum OFMulOp<'a> {
    /// Operator *
    Multiply(Span<'a>),
    /// Operator /
    Divide(Span<'a>),
}

impl<'a> PartialEq for OFMulOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFMulOp<'a> {
    fn op(&self) -> &str {
        match self {
            OFMulOp::Multiply(_) => "*",
            OFMulOp::Divide(_) => "/",
        }
    }
}

impl<'a> Node<'a> for OFMulOp<'a> {
    fn name(&self) -> &str {
        "MulOp"
    }

    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
        match self {
            OFMulOp::Multiply(span) => *span,
            OFMulOp::Divide(span) => *span,
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFMulOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFPow *****************************************************************

/// Power expression
#[derive(PartialEq)]
pub struct OFPow<'a> {
    /// Left operand
    pub left: Box<OFAst<'a>>,
    /// Operator
    pub op: OFPowOp<'a>,
    /// Right operand
    pub right: Box<OFAst<'a>>,
}

impl<'a> Node<'a> for OFPow<'a> {
    fn name(&self) -> &str {
        "Pow"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left.enc(),
            self.op.enc(),
            self.right.enc()
        )
    }
}

impl<'a> BinaryNode<'a> for OFPow<'a> {
    type Op = OFPowOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        self.left.as_ref()
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        self.right.as_ref()
    }
}

impl<'a> Display for OFPow<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

// OFPowOp ***************************************************************

/// Power operator.
#[derive(Debug)]
pub enum OFPowOp<'a> {
    /// Operator ^
    Power(Span<'a>),
}

impl<'a> PartialEq for OFPowOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFPowOp<'a> {
    fn op(&self) -> &str {
        "^"
    }
}

impl<'a> Node<'a> for OFPowOp<'a> {
    fn name(&self) -> &str {
        "PowOp"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPowOp::Power(span) => *span,
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFPowOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFPostfix *************************************************************

/// Postfix expression
#[derive(PartialEq)]
pub struct OFPostfix<'a> {
    /// Expression
    pub expr: Box<OFAst<'a>>,
    /// Operator
    pub op: OFPostfixOp<'a>,
}

impl<'a> Node<'a> for OFPostfix<'a> {
    fn name(&self) -> &str {
        "Postfix"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.expr.span(), self.op.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.expr.enc(), self.op.enc())
    }
}

impl<'a> Display for OFPostfix<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.expr, self.op)
    }
}

// OFPostfixOp ***********************************************************

/// Postfix operators.
#[derive(Debug)]
pub enum OFPostfixOp<'a> {
    /// Operator %
    Percent(Span<'a>),
}

impl<'a> PartialEq for OFPostfixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFPostfixOp<'a> {
    fn op(&self) -> &str {
        "%"
    }
}

impl<'a> Node<'a> for OFPostfixOp<'a> {
    fn name(&self) -> &str {
        "PostfixOp"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPostfixOp::Percent(span) => *span,
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFPostfixOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFPrefix **************************************************************

/// Prefix expression.
#[derive(PartialEq)]
pub struct OFPrefix<'a> {
    /// Operator
    pub op: OFPrefixOp<'a>,
    /// Expression
    pub expr: Box<OFAst<'a>>,
}

impl<'a> Node<'a> for OFPrefix<'a> {
    fn name(&self) -> &str {
        "Prefix"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.op.span(), self.expr.span()) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.op.enc(), self.expr.enc())
    }
}

impl<'a> Display for OFPrefix<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
}

// OFPrefixOp ************************************************************

/// Prefix operators.
#[derive(Debug)]
pub enum OFPrefixOp<'a> {
    /// Operator +
    Plus(Span<'a>),
    /// Operator -
    Minus(Span<'a>),
}

impl<'a> PartialEq for OFPrefixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFPrefixOp<'a> {
    fn op(&self) -> &str {
        match self {
            OFPrefixOp::Plus(_) => "+",
            OFPrefixOp::Minus(_) => "-",
        }
    }
}

impl<'a> Node<'a> for OFPrefixOp<'a> {
    fn name(&self) -> &str {
        "PrefixOp"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPrefixOp::Plus(span) => *span,
            OFPrefixOp::Minus(span) => *span,
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

impl<'a> Display for OFPrefixOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.op())
    }
}

// OFNumber **************************************************************

/// Number
pub struct OFNumber<'a> {
    /// Number
    pub num: f64,
    /// Span
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFNumber<'a> {
    fn name(&self) -> &str {
        "Number"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.num)
    }
}

impl<'a> Display for OFNumber<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.num)
    }
}

impl<'a> PartialEq for OFNumber<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.num == other.num
    }
}

// OFString **************************************************************

/// String
pub struct OFString<'a> {
    /// String
    pub str: String,
    /// Span
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFString<'a> {
    fn name(&self) -> &str {
        "String"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", conv::quote_double(&self.str))
    }
}

impl<'a> Display for OFString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl<'a> PartialEq for OFString<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.str == other.str
    }
}

// OFIri *****************************************************************

/// Represents an external source reference.
pub struct OFIri<'a> {
    /// IRI
    pub iri: String,
    /// Span
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFIri<'a> {
    fn name(&self) -> &str {
        "Iri"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'#", conv::quote_single(&self.iri))
    }
}

impl<'a> Display for OFIri<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'#", self.iri)
    }
}

impl<'a> PartialEq for OFIri<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
    }
}

// OFSheetName ***********************************************************

/// Sheet name.
pub struct OFSheetName<'a> {
    /// Absolute reference.
    pub abs: bool,
    /// Sheet name.
    pub name: String,
    /// Span.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFSheetName<'a> {
    fn name(&self) -> &str {
        "SheetName"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        write!(f, "'{}'.", conv::quote_single(&self.name))
    }
}

impl<'a> Display for OFSheetName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        write!(f, "'{}'.", self.name)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFSheetName<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.abs == other.abs && self.name == other.name
    }
}

// OFRow *****************************************************************

/// Row data for any reference.
pub struct OFRow<'a> {
    /// Absolute flag
    pub abs: bool,
    /// Row
    pub row: u32,
    /// Span for all
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFRow<'a> {
    fn name(&self) -> &str {
        "Row"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        format::fmt_row_name(f, self.row)?;
        Ok(())
    }
}

impl<'a> Display for OFRow<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        format::fmt_row_name(f, self.row)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFRow<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.abs == other.abs && self.row == other.row
    }
}

// OFCol *****************************************************************

/// Column data for any reference.
pub struct OFCol<'a> {
    /// Absolute flag
    pub abs: bool,
    /// Col
    pub col: u32,
    /// Span for all
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFCol<'a> {
    fn name(&self) -> &str {
        "Col"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        format::fmt_col_name(f, self.col)?;
        Ok(())
    }
}

impl<'a> Display for OFCol<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format::fmt_abs(f, self.abs)?;
        format::fmt_col_name(f, self.col)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFCol<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.abs == other.abs && self.col == other.col
    }
}

// OFIdentifier **********************************************************

/// Identifier.
pub struct OFSimpleNamed<'a> {
    /// Name
    pub ident: String,
    /// Span
    pub span: Span<'a>,
}

impl<'a> OFSimpleNamed<'a> {
    fn need_quotes(&self) -> bool {
        match tokens::identifier(Span::new(&self.ident)) {
            Ok((rest, _)) => !rest.is_empty(),
            Err(_) => true,
        }
    }
}

impl<'a> Node<'a> for OFSimpleNamed<'a> {
    fn name(&self) -> &str {
        "SimpleNamed"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.need_quotes() {
            write!(f, "$$'{}'", conv::quote_single(&self.ident))
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl<'a> Display for OFSimpleNamed<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.need_quotes() {
            write!(f, "$$'{}'", conv::quote_single(&self.ident))
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl<'a> PartialEq for OFSimpleNamed<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

// OFNamed ***************************************************************

/// A named expression.
pub struct OFNamed<'a> {
    /// External source
    pub iri: Option<OFIri<'a>>,
    /// Sheet name
    pub sheet_name: Option<OFSheetName<'a>>,
    /// Identifier
    pub simple: OFSimpleNamed<'a>,
}

impl<'a> Node<'a> for OFNamed<'a> {
    fn name(&self) -> &str {
        "Identifier"
    }

    fn span(&self) -> Span<'a> {
        if let Some(iri) = &self.iri {
            unsafe { span_union(iri.span(), self.simple.span()) }
        } else if let Some(sheet_name) = &self.sheet_name {
            unsafe { span_union(sheet_name.span(), self.simple.span()) }
        } else {
            self.simple.span()
        }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(sheet_name) = &self.sheet_name {
            write!(f, "{}", sheet_name)?;
        }
        write!(f, "{}", self.simple)?;
        Ok(())
    }
}

impl<'a> Display for OFNamed<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(sheet_name) = &self.sheet_name {
            write!(f, "{}", sheet_name)?;
        }
        write!(f, "{}", self.simple)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFNamed<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri && self.sheet_name == other.sheet_name && self.simple == other.simple
    }
}

// OFCellRef *************************************************************

/// CellRef
pub struct OFCellRef<'a> {
    /// External source
    pub iri: Option<OFIri<'a>>,
    /// Sheet for reference.
    pub table: Option<OFSheetName<'a>>,
    /// Row
    pub row: OFRow<'a>,
    /// Col
    pub col: OFCol<'a>,
    /// Span of the complete reference.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFCellRef<'a> {
    fn name(&self) -> &str {
        "CellRef"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri.enc())?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table.enc())?;
        }
        write!(f, "{}{}", self.col.enc(), self.row.enc())
    }
}

impl<'a> Display for OFCellRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table)?;
        }
        write!(f, "{}{}", self.col, self.row)
    }
}

impl<'a> PartialEq for OFCellRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
            && self.table == other.table
            && self.col == other.col
            && self.row == other.row
    }
}

// OFCellRange **********************************************************

/// CellRange
pub struct OFCellRange<'a> {
    /// IRI
    pub iri: Option<OFIri<'a>>,
    /// First table
    pub table: Option<OFSheetName<'a>>,
    /// Start row
    pub row: OFRow<'a>,
    /// Start column.
    pub col: OFCol<'a>,
    /// Second table.
    pub to_table: Option<OFSheetName<'a>>,
    /// End row.
    pub to_row: OFRow<'a>,
    /// End column.
    pub to_col: OFCol<'a>,
    /// Span.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFCellRange<'a> {
    fn name(&self) -> &str {
        "CellRange"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri.enc())?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table.enc())?;
        }
        write!(f, "{}{}", self.col.enc(), self.row.enc())?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table.enc())?;
        }
        write!(f, "{}{}", self.to_col.enc(), self.to_row.enc())?;
        Ok(())
    }
}

impl<'a> Display for OFCellRange<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table)?;
        }
        write!(f, "{}{}", self.col, self.row)?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table)?;
        }
        write!(f, "{}{}", self.to_col, self.to_row)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFCellRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
            && self.table == other.table
            && self.col == other.col
            && self.row == other.row
            && self.to_table == other.to_table
            && self.to_col == other.to_col
            && self.to_row == other.to_row
    }
}

// OFRowRange ************************************************************

/// RowRange
pub struct OFRowRange<'a> {
    /// External source
    pub iri: Option<OFIri<'a>>,
    /// Sheet for reference.
    pub table: Option<OFSheetName<'a>>,
    /// Row
    pub row: OFRow<'a>,
    /// Sheet for reference.
    pub to_table: Option<OFSheetName<'a>>,
    /// Row
    pub to_row: OFRow<'a>,
    /// Span of the complete reference.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFRowRange<'a> {
    fn name(&self) -> &str {
        "RowRange"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri.enc())?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table.enc())?;
        }
        write!(f, "{}", self.row.enc())?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table.enc())?;
        }
        write!(f, "{}", self.to_row.enc())?;
        Ok(())
    }
}

impl<'a> Display for OFRowRange<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table)?;
        }
        write!(f, "{}", self.row)?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table)?;
        }
        write!(f, "{}", self.to_row)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFRowRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
            && self.table == other.table
            && self.row == other.row
            && self.to_table == other.to_table
            && self.to_row == other.to_row
    }
}

// ColRange **************************************************************

/// ColRange
pub struct OFColRange<'a> {
    /// External source
    pub iri: Option<OFIri<'a>>,
    /// Sheet for reference.
    pub table: Option<OFSheetName<'a>>,
    /// Col
    pub col: OFCol<'a>,
    /// Sheet for reference.
    pub to_table: Option<OFSheetName<'a>>,
    /// Col
    pub to_col: OFCol<'a>,
    /// Span of the complete reference.
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFColRange<'a> {
    fn name(&self) -> &str {
        "ColRange"
    }

    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri.enc())?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table.enc())?;
        }
        write!(f, "{}", self.col.enc())?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table.enc())?;
        }
        write!(f, "{}", self.to_col.enc())?;
        Ok(())
    }
}

impl<'a> Display for OFColRange<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(table) = &self.table {
            write!(f, "{}", table)?;
        }
        write!(f, "{}", self.col)?;
        write!(f, ":")?;
        if let Some(to_table) = &self.to_table {
            write!(f, "{}", to_table)?;
        }
        write!(f, "{}", self.to_col)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFColRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
            && self.table == other.table
            && self.col == other.col
            && self.to_table == other.to_table
            && self.to_col == other.to_col
    }
}

// OFParens **************************************************************

/// Expression in parentheses.
#[derive(PartialEq)]
pub struct OFParens<'a> {
    /// Open parentheses
    pub o: Span<'a>,
    /// Expression
    pub expr: Box<OFAst<'a>>,
    /// Closing parentheses
    pub c: Span<'a>,
}

impl<'a> Node<'a> for OFParens<'a> {
    fn name(&self) -> &str {
        "Parens"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.o, self.c) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr.enc())
    }
}

impl<'a> Display for OFParens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
}

// OFFnCall **************************************************************

/// Function call
#[derive(PartialEq)]
pub struct OFFnCall<'a> {
    /// Name
    pub name: OFFnName<'a>,
    /// Open parentheses
    pub o: Span<'a>,
    /// Args
    pub arg: Vec<OFAst<'a>>,
    /// Closing parentheses
    pub c: Span<'a>,
}

impl<'a> Node<'a> for OFFnCall<'a> {
    fn name(&self) -> &str {
        "FnCall"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.name.span(), self.c) }
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name.enc())?;
        for (i, expr) in self.arg.iter().enumerate() {
            if i > 0 {
                write!(f, ";")?;
            }
            write!(f, "{}", expr.enc())?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<'a> Display for OFFnCall<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

// OFFnName **************************************************************

/// Function name.
#[derive(Debug, Eq, PartialEq)]
pub struct OFFnName<'a> {
    /// Function name.
    pub name: String,
    /// Span
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFFnName<'a> {
    fn name(&self) -> &str {
        "FnName"
    }

    /// Returns the span for each variant.
    fn span(&self) -> Span<'a> {
        self.span
    }

    fn encode(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> Display for OFFnName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
