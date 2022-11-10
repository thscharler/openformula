//!
//! AST for OpenFormula
//!

use crate::dbg_ast;
use crate::parse::Span;
use nom::Offset;
use spreadsheet_ods_cellref::{CellRange, CellRef, ColRange, RowRange};
use std::fmt::{Debug, Display, Formatter};
use std::str::from_utf8_unchecked;
use std::{fmt, mem, slice};

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
    /// Named expression
    // TODO: check rest for a better structure.
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
    /// Calculates the span of the complete AST tree.
    pub fn span(&self) -> Span<'a> {
        match self {
            OFAst::NodeEmpty(v) => v.span(),
            OFAst::NodeCompare(v) => v.span(),
            OFAst::NodeAdd(v) => v.span(),
            OFAst::NodeMul(v) => v.span(),
            OFAst::NodePow(v) => v.span(),
            OFAst::NodePrefix(v) => v.span(),
            OFAst::NodePostfix(v) => v.span(),
            OFAst::NodeNumber(v) => v.span(),
            OFAst::NodeString(v) => v.span(),
            OFAst::NodeCellRef(v) => v.span(),
            OFAst::NodeCellRange(v) => v.span(),
            OFAst::NodeColRange(v) => v.span(),
            OFAst::NodeRowRange(v) => v.span(),
            OFAst::NodeParens(v) => v.span(),
            OFAst::NodeFnCall(v) => v.span(),
            OFAst::NodeNamed(v) => v.span(),
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

    /// Named variant
    pub fn named(
        iri: Option<OFIri<'a>>,
        sheet_name: Option<OFSheetName<'a>>,
        identifier: OFIdentifier<'a>,
    ) -> Box<OFAst<'a>> {
        Box::new(OFAst::NodeNamed(OFNamed {
            iri,
            sheet_name,
            identifier,
        }))
    }

    /// Creates a OFIri
    pub fn iri(iri: Option<Span<'a>>) -> Option<OFIri<'a>> {
        iri.map(|v| OFIri(v.to_string(), v))
    }

    /// Creates a OFSheetName
    pub fn sheet_name(
        abs: Option<Span<'a>>,
        sheet_name: Option<Span<'a>>,
    ) -> Option<OFSheetName<'a>> {
        if let Some(sheet_name) = sheet_name {
            unsafe {
                if let Some(abs) = abs {
                    let complete_span = span_union(abs, sheet_name);
                    Some(OFSheetName(
                        *abs == "$",
                        sheet_name.to_string(),
                        complete_span,
                    ))
                } else {
                    Some(OFSheetName(false, sheet_name.to_string(), sheet_name))
                }
            }
        } else {
            None
        }
    }

    /// Creates a OFIdentifier
    pub fn identifier(ident: Span<'a>) -> OFIdentifier<'a> {
        OFIdentifier(ident.to_string(), ident)
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
                name: name.to_string(),
                span: name,
            },
            o: OFParOpen { span: o },
            arg,
            c: OFParClose { span: c },
        }))
    }
}

impl<'a> Debug for OFAst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_ast(self, 0, f)
    }
}

impl<'a> Display for OFAst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

/// General Node trait.
pub trait Node<'a> {
    /// Name for Debug.
    fn name(&self) -> &str;

    /// Returns the span of.
    fn span(&self) -> Span<'a>;
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
pub trait Operator<'a>: Node<'a> + Display {}

#[allow(clippy::manual_non_exhaustive)]
/// Empty
pub struct OFEmpty<'a>((), pub Span<'a>);

impl<'a> Node<'a> for OFEmpty<'a> {
    fn name(&self) -> &str {
        "empty"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFEmpty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_empty(self, f)
    }
}

impl<'a> Display for OFEmpty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl<'a> PartialEq for OFEmpty<'a> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

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
        "compare"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> BinaryNode<'a> for OFCompare<'a> {
    type Op = OFCompOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        &*self.left
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        &*self.right
    }
}

impl<'a> Debug for OFCompare<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_binary(self, 0, f)
    }
}

impl<'a> Display for OFCompare<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'a> PartialEq for OFCompOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFCompOp<'a> {}

impl<'a> Node<'a> for OFCompOp<'a> {
    fn name(&self) -> &str {
        "compare op"
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
}

impl<'a> Display for OFCompOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
        "add"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> BinaryNode<'a> for OFAdd<'a> {
    type Op = OFAddOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        &*self.left
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        &*self.right
    }
}

impl<'a> Debug for OFAdd<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_binary(self, 0, f)
    }
}

impl<'a> Display for OFAdd<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'a> PartialEq for OFAddOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFAddOp<'a> {}

impl<'a> Node<'a> for OFAddOp<'a> {
    fn name(&self) -> &str {
        "add op"
    }

    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
        match self {
            OFAddOp::Add(span) => *span,
            OFAddOp::Subtract(span) => *span,
        }
    }
}

impl<'a> Display for OFAddOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OFAddOp::Add(_) => write!(f, "+"),
            OFAddOp::Subtract(_) => write!(f, "-"),
        }
    }
}

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
        "mul"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> BinaryNode<'a> for OFMul<'a> {
    type Op = OFMulOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        &*self.left
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        &*self.right
    }
}

impl<'a> Debug for OFMul<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_binary(self, 0, f)
    }
}

impl<'a> Display for OFMul<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'a> PartialEq for OFMulOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFMulOp<'a> {}

impl<'a> Node<'a> for OFMulOp<'a> {
    fn name(&self) -> &str {
        "mul op"
    }

    /// Extracts the span from each variant.
    fn span(&self) -> Span<'a> {
        match self {
            OFMulOp::Multiply(span) => *span,
            OFMulOp::Divide(span) => *span,
        }
    }
}

impl<'a> Display for OFMulOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OFMulOp::Multiply(_) => write!(f, "*"),
            OFMulOp::Divide(_) => write!(f, "/"),
        }
    }
}

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
        "pow"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.left.span(), self.right.span()) }
    }
}

impl<'a> BinaryNode<'a> for OFPow<'a> {
    type Op = OFPowOp<'a>;

    fn left(&self) -> &OFAst<'a> {
        &*self.left
    }

    fn op(&self) -> &Self::Op {
        &self.op
    }

    fn right(&self) -> &OFAst<'a> {
        &*self.right
    }
}

impl<'a> Debug for OFPow<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_binary(self, 0, f)
    }
}

impl<'a> Display for OFPow<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

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

impl<'a> Operator<'a> for OFPowOp<'a> {}

impl<'a> Node<'a> for OFPowOp<'a> {
    fn name(&self) -> &str {
        "pow op"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPowOp::Power(span) => *span,
        }
    }
}

impl<'a> Display for OFPowOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OFPowOp::Power(_) => write!(f, "^"),
        }
    }
}

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
        "postfix"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.expr.span(), self.op.span()) }
    }
}

impl<'a> Debug for OFPostfix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_postfix(self, 0, f)
    }
}

impl<'a> Display for OFPostfix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.expr, self.op)
    }
}

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

impl<'a> Operator<'a> for OFPostfixOp<'a> {}

impl<'a> Node<'a> for OFPostfixOp<'a> {
    fn name(&self) -> &str {
        "postfix op"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPostfixOp::Percent(span) => *span,
        }
    }
}

impl<'a> Display for OFPostfixOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OFPostfixOp::Percent(_) => write!(f, "%"),
        }
    }
}

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
        "prefix"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.op.span(), self.expr.span()) }
    }
}

impl<'a> Debug for OFPrefix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_prefix(self, 0, f)
    }
}

impl<'a> Display for OFPrefix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'a> PartialEq for OFPrefixOp<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Operator<'a> for OFPrefixOp<'a> {}

impl<'a> Node<'a> for OFPrefixOp<'a> {
    fn name(&self) -> &str {
        "prefix op"
    }

    fn span(&self) -> Span<'a> {
        match self {
            OFPrefixOp::Plus(span) => *span,
            OFPrefixOp::Minus(span) => *span,
        }
    }
}

impl<'a> Display for OFPrefixOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OFPrefixOp::Plus(_) => write!(f, "+"),
            OFPrefixOp::Minus(_) => write!(f, "-"),
        }
    }
}

/// Number
pub struct OFNumber<'a>(pub f64, pub Span<'a>);

impl<'a> Node<'a> for OFNumber<'a> {
    fn name(&self) -> &str {
        "number"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFNumber<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFNumber<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFNumber<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// String
pub struct OFString<'a>(pub String, pub Span<'a>);

impl<'a> Node<'a> for OFString<'a> {
    fn name(&self) -> &str {
        "string"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFString<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// Represents an external source reference.
pub struct OFIri<'a>(String, Span<'a>);

impl<'a> Node<'a> for OFIri<'a> {
    fn name(&self) -> &str {
        "iri"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFIri<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFIri<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFIri<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// Sheet name.
pub struct OFSheetName<'a>(bool, String, Span<'a>);

impl<'a> Node<'a> for OFSheetName<'a> {
    fn name(&self) -> &str {
        "sheet_name"
    }

    fn span(&self) -> Span<'a> {
        self.2
    }
}

impl<'a> Debug for OFSheetName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFSheetName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}'{}'.", if self.0 { "$" } else { "" }, self.1)
    }
}

impl<'a> PartialEq for OFSheetName<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

/// Identifier.
pub struct OFIdentifier<'a>(String, Span<'a>);

impl<'a> Node<'a> for OFIdentifier<'a> {
    fn name(&self) -> &str {
        "identifier"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFIdentifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFIdentifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFIdentifier<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// A named expression.
pub struct OFNamed<'a> {
    /// External source
    pub iri: Option<OFIri<'a>>,
    /// Sheet name
    pub sheet_name: Option<OFSheetName<'a>>,
    /// Identifier
    pub identifier: OFIdentifier<'a>,
}

impl<'a> Node<'a> for OFNamed<'a> {
    fn name(&self) -> &str {
        "identifier"
    }

    fn span(&self) -> Span<'a> {
        if let Some(iri) = &self.iri {
            unsafe { span_union(iri.span(), self.identifier.span()) }
        } else if let Some(sheet_name) = &self.sheet_name {
            unsafe { span_union(sheet_name.span(), self.identifier.span()) }
        } else {
            self.identifier.1
        }
    }
}

impl<'a> Debug for OFNamed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFNamed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}", iri)?;
        }
        if let Some(sheet_name) = &self.sheet_name {
            write!(f, "{}", sheet_name)?;
        }
        write!(f, "{}", self.identifier)?;
        Ok(())
    }
}

impl<'a> PartialEq for OFNamed<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.iri == other.iri
            && self.sheet_name == other.sheet_name
            && self.identifier == other.identifier
    }
}

/// CellRef
pub struct OFCellRef<'a>(pub CellRef, pub Span<'a>);

impl<'a> Node<'a> for OFCellRef<'a> {
    fn name(&self) -> &str {
        "cell-ref"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFCellRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFCellRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFCellRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// CellRange
pub struct OFCellRange<'a>(pub CellRange, pub Span<'a>);

impl<'a> Node<'a> for OFCellRange<'a> {
    fn name(&self) -> &str {
        "cell-range"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFCellRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFCellRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFCellRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// RowRange
pub struct OFRowRange<'a>(pub RowRange, pub Span<'a>);

impl<'a> Node<'a> for OFRowRange<'a> {
    fn name(&self) -> &str {
        "row-range"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFRowRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFRowRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFRowRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// ColRange
pub struct OFColRange<'a>(pub ColRange, pub Span<'a>);

impl<'a> Node<'a> for OFColRange<'a> {
    fn name(&self) -> &str {
        "col-range"
    }

    fn span(&self) -> Span<'a> {
        self.1
    }
}

impl<'a> Debug for OFColRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_elem(self, f)
    }
}

impl<'a> Display for OFColRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> PartialEq for OFColRange<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/// Expression in parentheses.
#[derive(PartialEq)]
pub struct OFParens<'a> {
    /// Open parentheses
    pub o: OFParOpen<'a>,
    /// Expression
    pub expr: Box<OFAst<'a>>,
    /// Closing parentheses
    pub c: OFParClose<'a>,
}

impl<'a> Node<'a> for OFParens<'a> {
    fn name(&self) -> &str {
        "parens"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.o.span(), self.c.span()) }
    }
}

impl<'a> Debug for OFParens<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_parens(self, 0, f)
    }
}

impl<'a> Display for OFParens<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
}

/// Paren open
#[derive(Debug, Eq, PartialEq)]
pub struct OFParOpen<'a> {
    /// Span
    pub span: Span<'a>,
}

impl<'a> Node<'a> for OFParOpen<'a> {
    fn name(&self) -> &str {
        "("
    }

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

impl<'a> Node<'a> for OFParClose<'a> {
    fn name(&self) -> &str {
        ")"
    }

    /// Span
    fn span(&self) -> Span<'a> {
        self.span
    }
}

/// Function call
#[derive(PartialEq)]
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

impl<'a> Node<'a> for OFFnCall<'a> {
    fn name(&self) -> &str {
        "fn-call"
    }

    fn span(&self) -> Span<'a> {
        unsafe { span_union(self.name.span(), self.c.span()) }
    }
}

impl<'a> Debug for OFFnCall<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        dbg_ast::debug_fn_call(self, 0, f)
    }
}

impl<'a> Display for OFFnCall<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'a> Node<'a> for OFFnName<'a> {
    fn name(&self) -> &str {
        "fn-name"
    }

    /// Returns the span for each variant.
    fn span(&self) -> Span<'a> {
        self.span
    }
}

impl<'a> Display for OFFnName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
