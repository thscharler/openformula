//!
//! Parses and creates the AST.
//!
//! The parser and look-ahead functions expect to be called with a clean string with no
//! leading whitespace. Internal calls are cared for, but the return value is not necessarily
//! clean itself.
//!
//! The look-ahead functions are called internally at certain branching points.

use crate::ast::AstTree;
use crate::conv::{try_bool_from_abs_flag, try_u32_from_colname, try_u32_from_rowname};
use crate::error::ParseExprError;
use crate::parse::{ParseResult, Span, Tracer};
use crate::{
    tokens, CRef, CellRange, CellRef, ColRange, HaveSpan, OFAddOp, OFCellRange, OFCellRef,
    OFColRange, OFCompOp, OFMulOp, OFParClose, OFParOpen, OFPostfixOp, OFPowOp, OFPrefixOp,
    OFRowRange, RowRange,
};
use nom::character::complete::multispace0;
use nom::combinator::{consumed, opt, recognize};
use nom::sequence::tuple;
use nom::InputTake;

// Expression ::=
// Whitespace* (
//      Array |
//      QuotedLabel |
//      AutomaticIntersection |
//      NamedExpression |
//      Error |
//
//      Number |
//      String |
//      Reference |
//      PrefixOp Expression |
//      Expression InfixOp Expression |
//      Expression PostfixOp |
//      '(' Expression ')' |
//      FunctionName '(' ParameterList ')' |
// ) Whitespace*
// SingleQuoted ::= "'" ([^'] | "''")+ "'"
//
// PrefixOp ::= '+' | '-'
// PostfixOp ::= '%'
// InfixOp ::= ArithmeticOp | ComparisonOp | StringOp | ReferenceOp
// ArithmeticOp ::= '+' | '-' | '*' | '/' | '^'
// ComparisonOp ::= '=' | '<>' | '<' | '>' | '<=' | '>='
// StringOp ::= '&
//
// FunctionName ::= LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
// ParameterList ::= /* empty */ |
//                      Parameter (Separator EmptyOrParameter )* |
//                      Separator EmptyOrParameter /* First param empty */ (Separator EmptyOrParameter )*
// EmptyOrParameter ::= /* empty */ Whitespace* | Parameter
// Parameter ::= Expression
// Separator ::= ';'
//
// ReferenceOp ::= IntersectionOp | ReferenceConcatenationOp | RangeOp
// IntersectionOp ::= '!'
// ReferenceConcatenationOp ::= '~'
// RangeOp ::= ':'
//
// ops precedence
// :            left        Range.
// !            left        Reference intersection ([.A1:.C4]![.B1:.B5] is [.B1:.B4]). Displayed as
//                          the space character in some implementations.
// ~            left        Reference union.
//                          Note: Displayed as the function parameter separator in some implementations.
// + -          right       Prefix unary operators, e.g., -5 or -[.A1]. Note that these have a
//                          different precedence than add and subtract.
// %            left        Postfix unary operator % (divide by 100). Note that this is legal
//                          with expressions (e.g., [.B1]%).
// ^            left        Power (2 ^ 3 is 8).
// * /          left        Multiply, divide.
// + -          left        Binary operations add, subtract. Note that unary (prefix) + and -
//                          have a different precedence.
// &            left        Binary operation string concatenation. Note that unary (prefix) +
//                          and - have a different precedence.
// =, <>, <, <=,
// >, >=        left        Comparison operators equal to, not equal to, less than, less than
//                          or equal to, greater than, greater than or equal to

/// Trait for a parser.
pub trait GeneralExpr<'s> {
    /// Get a name for debug.
    fn name() -> &'static str;

    /// Run a look-ahead.
    fn lah(i: Span<'s>) -> bool;

    /// Parses the expression.
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>>;
}

/// Conversion function for a binary expression into an AstTree
type AstTreeFn<'a, T> =
    fn(expr0: Box<AstTree<'a>>, op: T, expr1: Box<AstTree<'a>>) -> Box<AstTree<'a>>;

/// Conversion function into a ParseExprError
type ParseExprErrorFn = for<'a> fn(span: Span<'a>) -> ParseExprError;

/// Special trait for binary expressions.
pub trait BinaryExpr<'s> {
    /// Operator type.
    type Operator;
    /// Operand type.
    type Operand: GeneralExpr<'s>;

    /// Conversion function into an AstTree
    const AST_NODE: AstTreeFn<'s, Self::Operator>;
    /// Conversion function into and ParseExprError
    const AST_ERR: ParseExprErrorFn;

    /// Get a name for debug.
    fn name() -> &'static str;

    /// Run a look-ahead.
    fn lah(i: Span<'s>) -> bool {
        Self::Operand::lah(i)
    }

    /// Parses all the binary expressions.
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match Self::operand(trace, i) {
            Ok((mut loop_rest, Some(mut expr1))) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, Some(op))) => {
                            //
                            match Self::operand(trace, eat_space(rest2)) {
                                Ok((rest3, Some(expr2))) => {
                                    loop_rest = rest3;
                                    expr1 = Self::ast(expr1, op, expr2);
                                }
                                Ok((rest3, None)) => break Err(trace.ast_err(rest3, Self::ast_err)),
                                Err(e) => break Err(trace.parse_err(e)),
                            }
                        }
                        Ok((rest2, None)) => break Ok(trace.ok(expr1.span(), rest2, Some(expr1))),
                        Err(e) => break Err(trace.parse_err(e)),
                    }
                }
            }
            Ok((rest1, None)) => Ok(trace.ok(Span::new(""), rest1, None)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }

    /// Parse the sub expression.
    fn operand<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        Self::Operand::parse(trace, i)
    }

    /// Parse the operator.
    /// Parses and maps the Span to an OFCompOp
    fn operator<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Self::Operator>>;

    /// Creates an AST node.
    fn ast(
        expr0: Box<AstTree<'s>>,
        op: Self::Operator,
        expr1: Box<AstTree<'s>>,
    ) -> Box<AstTree<'s>> {
        Self::AST_NODE(expr0, op, expr1)
    }

    /// Creates an ParseExprError
    fn ast_err(span: Span<'s>) -> ParseExprError {
        Self::AST_ERR(span)
    }
}

/// GeneralExpr can be expressed in terms of BinaryExpr.
impl<'s, T> GeneralExpr<'s> for T
where
    T: BinaryExpr<'s>,
{
    fn name() -> &'static str {
        <Self as BinaryExpr>::name()
    }

    fn lah(i: Span<'s>) -> bool {
        <Self as BinaryExpr>::lah(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        <Self as BinaryExpr>::parse(trace, i)
    }
}

/// Any expression.
#[derive(Debug)]
pub struct Expr;

impl<'s> GeneralExpr<'s> for Expr {
    fn name() -> &'static str {
        "expr"
    }

    fn lah(i: Span<'s>) -> bool {
        <CompareExpr as BinaryExpr>::lah(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match <CompareExpr as BinaryExpr>::parse(trace, i) {
            Ok((rest, Some(expr))) => Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

struct CompareExpr;

impl<'s> BinaryExpr<'s> for CompareExpr {
    type Operator = OFCompOp<'s>;
    type Operand = AddExpr;
    const AST_NODE: AstTreeFn<'s, Self::Operator> = AstTree::compare_expr;
    const AST_ERR: ParseExprErrorFn = ParseExprError::comp;

    fn name() -> &'static str {
        "compare"
    }

    fn operator<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFCompOp<'s>>> {
        trace.enter("operator", i);
        match tokens::comparison_op(i) {
            Ok((rest, tok)) => match *tok {
                "=" => Ok(trace.ok(tok, rest, Some(OFCompOp::Equal(tok)))),
                "<>" => Ok(trace.ok(tok, rest, Some(OFCompOp::Unequal(tok)))),
                "<" => Ok(trace.ok(tok, rest, Some(OFCompOp::Less(tok)))),
                ">" => Ok(trace.ok(tok, rest, Some(OFCompOp::Greater(tok)))),
                "<=" => Ok(trace.ok(tok, rest, Some(OFCompOp::LessEqual(tok)))),
                ">=" => Ok(trace.ok(tok, rest, Some(OFCompOp::GreaterEqual(tok)))),
                _ => unreachable!(),
            },

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

struct AddExpr;

impl<'s> BinaryExpr<'s> for AddExpr {
    type Operator = OFAddOp<'s>;
    type Operand = MulExpr;
    const AST_NODE: AstTreeFn<'s, Self::Operator> = AstTree::add_expr;
    const AST_ERR: ParseExprErrorFn = ParseExprError::add;

    fn name() -> &'static str {
        "add"
    }

    fn operator<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Self::Operator>> {
        trace.enter("operator", i);
        match tokens::add_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "+" => Ok(trace.ok(tok, rest, Some(OFAddOp::Add(tok)))),
                "-" => Ok(trace.ok(tok, rest, Some(OFAddOp::Subtract(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),

            Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

struct MulExpr;

impl<'s> BinaryExpr<'s> for MulExpr {
    type Operator = OFMulOp<'s>;
    type Operand = PowExpr;

    const AST_NODE: AstTreeFn<'s, Self::Operator> = AstTree::mul_expr;
    const AST_ERR: ParseExprErrorFn = ParseExprError::mul;

    fn name() -> &'static str {
        "mul"
    }

    fn operator<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Self::Operator>> {
        trace.enter("operator", i);
        match tokens::mul_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "*" => Ok(trace.ok(tok, rest, Some(OFMulOp::Multiply(tok)))),
                "/" => Ok(trace.ok(tok, rest, Some(OFMulOp::Divide(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),

            Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

struct PowExpr;

impl<'s> BinaryExpr<'s> for PowExpr {
    type Operator = OFPowOp<'s>;
    type Operand = PostFixExpr;
    const AST_NODE: AstTreeFn<'s, Self::Operator> = AstTree::pow_expr;
    const AST_ERR: ParseExprErrorFn = ParseExprError::pow;

    fn name() -> &'static str {
        "pow"
    }

    fn operator<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Self::Operator>> {
        trace.enter("operator", i);
        match tokens::pow_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "^" => Ok(trace.ok(tok, rest, Some(OFPowOp::Power(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),

            Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

struct PostFixExpr;

impl PostFixExpr {
    /// Parses and maps the Span to an OFPostfixOp
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFPostfixOp<'s>>> {
        trace.enter("operator", i);
        match tokens::postfix_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "%" => Ok(trace.ok(tok, rest, Some(OFPostfixOp::Percent(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),

            Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

impl<'s> GeneralExpr<'s> for PostFixExpr {
    fn name() -> &'static str {
        "postfix"
    }

    fn lah(i: Span<'s>) -> bool {
        PrefixExpr::lah(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match PrefixExpr::parse(trace, i) {
            Ok((mut loop_rest, Some(mut expr))) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest1, Some(tok))) => {
                            trace.step("op", tok.span());
                            loop_rest = rest1;
                            expr = AstTree::postfix_expr(expr, tok);
                        }
                        Ok((i2, None)) => break Ok(trace.ok(expr.span(), i2, Some(expr))),
                        Err(e) => break Err(trace.parse_err(e)),
                    }
                }
            }
            Ok((rest1, None)) => Ok(trace.ok(Span::new(""), rest1, None)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

struct PrefixExpr;

impl PrefixExpr {
    /// Parses and maps the Span to a OFPrefixOp.
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFPrefixOp<'s>>> {
        trace.enter("operator", i);
        match tokens::prefix_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "+" => Ok(trace.ok(tok, rest, Some(OFPrefixOp::Plus(tok)))),
                "-" => Ok(trace.ok(tok, rest, Some(OFPrefixOp::Minus(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

impl<'s> GeneralExpr<'s> for PrefixExpr {
    fn name() -> &'static str {
        "prefix"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_prefix_op(i) || ElementaryExpr::lah(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match Self::operator(trace, i) {
            Ok((rest1, Some(tok))) => {
                //
                match PrefixExpr::parse(trace, eat_space(rest1)) {
                    Ok((rest2, Some(expr))) => {
                        let ast = AstTree::prefix_expr(tok, expr);
                        Ok(trace.ok(ast.span(), rest2, Some(ast)))
                    }
                    Ok((rest2, None)) => Err(trace.ast_err(rest2, ParseExprError::prefix)),
                    Err(e) => Err(trace.parse_err(e)),
                }
            }
            Ok((rest1, None)) => {
                //
                match ElementaryExpr::parse(trace, eat_space(rest1)) {
                    Ok((rest2, Some(expr))) => Ok(trace.ok(expr.span(), rest2, Some(expr))),
                    Ok((rest2, None)) => Ok(trace.ok(Span::new(""), rest2, None)),
                    Err(e) => Err(trace.parse_err(e)),
                }
            }
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

/// Parser for the lowest expression level.
#[derive(Debug)]
struct ElementaryExpr;

impl<'s> GeneralExpr<'s> for ElementaryExpr {
    fn name() -> &'static str {
        "elementary"
    }

    fn lah(i: Span<'s>) -> bool {
        NumberExpr::lah(i)
            || StringExpr::lah(i)
            || ParenthesisExpr::lah(i)
            || ReferenceExpr::lah(i)
            || FnCallExpr::lah(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        if NumberExpr::lah(i) {
            match NumberExpr::parse(trace, i) {
                Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
                Ok((_rest, None)) => { /* skip */ }
                Err(e) => return Err(trace.parse_err(e)),
            }
        }

        if StringExpr::lah(i) {
            match StringExpr::parse(trace, i) {
                Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
                Ok((_rest, None)) => { /* skip */ }
                Err(e) => return Err(trace.parse_err(e)),
            }
        }

        if ParenthesisExpr::lah(i) {
            match ParenthesisExpr::parse(trace, i) {
                Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
                Ok((_, None)) => { /* skip */ }
                Err(e) => return Err(trace.parse_err(e)),
            }
        }

        if ReferenceExpr::lah(i) {
            match ReferenceExpr::parse(trace, i) {
                Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
                Ok((_, None)) => { /* skip */ }
                Err(e) => return Err(trace.parse_err(e)),
            }
        }

        if FnCallExpr::lah(i) {
            match FnCallExpr::parse(trace, i) {
                Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
                Ok((_, None)) => { /* skip */ }
                Err(e) => return Err(trace.parse_err(e)),
            }
        }

        Ok(trace.ok(Span::new(""), i, None))
    }
}

/// Parser for number.
#[derive(Debug)]
struct NumberExpr;

impl<'s> GeneralExpr<'s> for NumberExpr {
    fn name() -> &'static str {
        "number"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_number(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match opt(super::tokens::number)(i) {
            Ok((rest, Some(tok))) => match (*tok).parse::<f64>() {
                Ok(val) => {
                    let ast = AstTree::number(val, tok);
                    Ok(trace.ok(ast.span(), rest, Some(ast)))
                }
                Err(_) => unreachable!(),
            },
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),
            Err(e) => Err(trace.err(i, ParseExprError::number, e)),
        }
    }
}

/// Parser for strings.
#[derive(Debug)]
struct StringExpr;

impl<'s> GeneralExpr<'s> for StringExpr {
    fn name() -> &'static str {
        "string"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_string(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match opt(super::tokens::string)(i) {
            Ok((rest, Some(tok))) => {
                let ast = AstTree::string(tok.to_string(), tok);
                Ok(trace.ok(ast.span(), rest, Some(ast)))
            }
            Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),
            Err(e) => Err(trace.err(i, ParseExprError::string, e)),
        }
    }
}

//
// Reference ::= '[' (Source? RangeAddress) | ReferenceError ']'
// RangeAddress ::=
// SheetLocatorOrEmpty '.' Column Row (':' '.' Column Row )? |
// SheetLocatorOrEmpty '.' Column ':' '.' Column |
// SheetLocatorOrEmpty '.' Row ':' '.' Row |
// SheetLocator '.' Column Row ':' SheetLocator '.' Column Row |
// SheetLocator '.' Column ':' SheetLocator '.' Column |
// SheetLocator '.' Row ':' SheetLocator '.' Row
// SheetLocatorOrEmpty ::= SheetLocator | /* empty */
// SheetLocator ::= SheetName ('.' SubtableCell)*
// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
// QuotedSheetName ::= '$'? SingleQuoted
// SubtableCell ::= ( Column Row ) | QuotedSheetName
// ReferenceError ::= "#REF!"
// Column ::= '$'? [A-Z]+
// Row ::= '$'? [1-9] [0-9]*
// Source ::= "'" IRI "'" "#"
// CellAddress ::= SheetLocatorOrEmpty '.' Column Row /* Not used directly */
/// Parses any cell reference.
#[derive(Debug)]
pub struct ReferenceExpr;

impl<'s> GeneralExpr<'s> for ReferenceExpr {
    fn name() -> &'static str {
        "reference"
    }

    fn lah(i: Span<'s>) -> bool {
        CellRefExpr::lah(i)
    }

    /// Tries to parse a cell reference.
    #[allow(clippy::manual_map)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match CellRangeExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match CellRefExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match ColRangeExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match RowRangeExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }

        Ok(trace.ok(Span::new(""), i, None))
    }
}

/// Parses a simple cell reference.
#[derive(Debug)]
pub struct CellRefExpr;

impl CellRefExpr {
    /// Parses the full string as CellRef.
    pub fn parse_full<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, CellRef> {
        trace.enter(Self::name(), i);

        match CellRefExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => {
                check_eof(rest, ParseExprError::cellref)?;
                let AstTree::CellRef(OFCellRef(cellref, span)) = *expr else {
                        panic!("Expected a CellRef");
                    };
                Ok(trace.ok(span, rest, cellref))
            }
            Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::cellref)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for CellRefExpr {
    fn name() -> &'static str {
        "cellref"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match consumed(tuple((
            opt(tokens::iri),
            opt(tokens::sheet_name),
            tokens::dot, // TODO: this is not user-facing but the stored format.
            tokens::col,
            tokens::row,
        )))(i)
        {
            Ok((rest, (tok, (iri, sheet_name, _dot, col, row)))) => {
                //
                let cellref = CellRef {
                    iri: iri.map(|v| (*v).to_string()),
                    sheet: match sheet_name {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    cell: CRef {
                        abs_row: try_bool_from_abs_flag(row.0),
                        row: trace.re_err(try_u32_from_rowname(row.1))?,
                        abs_col: try_bool_from_abs_flag(col.0),
                        col: trace.re_err(try_u32_from_colname(col.1))?,
                    },
                };
                let ast = AstTree::cellref(cellref, tok);
                Ok(trace.ok(tok, rest, Some(ast)))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

/// Parses a cell range.
#[derive(Debug)]
pub struct CellRangeExpr;

impl CellRangeExpr {
    /// Parses the full string as CellRange.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, CellRange> {
        trace.enter(Self::name(), i);

        match CellRangeExpr::parse(trace, i) {
            Ok((rest, Some(expr))) => {
                check_eof(rest, ParseExprError::cellrange)?;
                let AstTree::CellRange(OFCellRange(cellrange, span)) = *expr else {
                        panic!("Expected a CellRange");
                    };
                Ok(trace.ok(span, rest, cellrange))
            }
            Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::cellrange)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for CellRangeExpr {
    fn name() -> &'static str {
        "cellrange"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match consumed(tuple((
            opt(tokens::iri),
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::col,
            tokens::row,
            tokens::colon,
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::col,
            tokens::row,
        )))(i)
        {
            Ok((
                rest,
                (
                    tok,
                    (
                        iri,
                        sheet_name_0,
                        _dot_0,
                        col_0,
                        row_0,
                        _colon,
                        sheet_name_1,
                        _dot_1,
                        col_1,
                        row_1,
                    ),
                ),
            )) => {
                //
                let cellrange = CellRange {
                    iri: iri.map(|v| (*v).to_string()),
                    from_sheet: match sheet_name_0 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    from: CRef {
                        abs_row: try_bool_from_abs_flag(row_0.0),
                        row: trace.re_err(try_u32_from_rowname(row_0.1))?,
                        abs_col: try_bool_from_abs_flag(col_0.0),
                        col: trace.re_err(try_u32_from_colname(col_0.1))?,
                    },
                    to_sheet: match sheet_name_1 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    to: CRef {
                        abs_row: try_bool_from_abs_flag(row_1.0),
                        row: trace.re_err(try_u32_from_rowname(row_1.1))?,
                        abs_col: try_bool_from_abs_flag(col_1.0),
                        col: trace.re_err(try_u32_from_colname(col_1.1))?,
                    },
                };
                let ast = AstTree::cellrange(cellrange, tok);

                Ok(trace.ok(tok, rest, Some(ast)))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

/// Parses a column range.
#[derive(Debug)]
pub struct ColRangeExpr;

impl ColRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, ColRange> {
        trace.enter("parse_colrange", i);

        match Self::parse(trace, i) {
            Ok((rest, Some(expr))) => {
                check_eof(rest, ParseExprError::colrange)?;
                let AstTree::ColRange(OFColRange(colrange, span)) = *expr else {
                        panic!("Expected a ColRange");
                    };
                Ok(trace.ok(span, rest, colrange))
            }
            Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::colrange)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for ColRangeExpr {
    fn name() -> &'static str {
        "colrange"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match consumed(tuple((
            opt(tokens::iri),
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::col,
            tokens::colon,
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::col,
        )))(i)
        {
            Ok((
                rest,
                (tok, (iri, sheet_name_0, _dot_0, col_0, _colon, sheet_name_1, _dot_1, col_1)),
            )) => {
                //
                let colrange = ColRange {
                    iri: iri.map(|v| (*v).to_string()),
                    from_sheet: match sheet_name_0 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    abs_from_col: try_bool_from_abs_flag(col_0.0),
                    from_col: trace.re_err(try_u32_from_colname(col_0.1))?,
                    to_sheet: match sheet_name_1 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    abs_to_col: try_bool_from_abs_flag(col_1.0),
                    to_col: trace.re_err(try_u32_from_colname(col_1.1))?,
                };
                let ast = AstTree::colrange(colrange, tok);

                Ok(trace.ok(tok, rest, Some(ast)))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

/// Parses a row range.
#[derive(Debug)]
pub struct RowRangeExpr;

impl RowRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, RowRange> {
        trace.enter(Self::name(), i);

        match Self::parse(trace, i) {
            Ok((rest, Some(expr))) => {
                check_eof(rest, ParseExprError::rowrange)?;
                let AstTree::RowRange(OFRowRange(rowrange, span)) = *expr else {
                        panic!("Expected a RowRange");
                    };
                Ok(trace.ok(span, rest, rowrange))
            }
            Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::rowrange)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for RowRangeExpr {
    fn name() -> &'static str {
        "rowrange"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match consumed(tuple((
            opt(tokens::iri),
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::row,
            tokens::colon,
            opt(tokens::sheet_name),
            tokens::dot,
            tokens::row,
        )))(i)
        {
            Ok((
                rest,
                (tok, (iri, sheet_name_0, _dot_0, row_0, _colon, sheet_name_1, _dot_1, row_1)),
            )) => {
                //
                let rowrange = RowRange {
                    iri: iri.map(|v| (*v).to_string()),
                    from_sheet: match sheet_name_0 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    abs_from_row: try_bool_from_abs_flag(row_0.0),
                    from_row: trace.re_err(try_u32_from_rowname(row_0.1))?,
                    to_sheet: match sheet_name_1 {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    abs_to_row: try_bool_from_abs_flag(row_1.0),
                    to_row: trace.re_err(try_u32_from_rowname(row_1.1))?,
                };
                let ast = AstTree::rowrange(rowrange, tok);

                Ok(trace.ok(tok, rest, Some(ast)))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

struct ParenthesisExpr;

impl<'s> GeneralExpr<'s> for ParenthesisExpr {
    fn name() -> &'static str {
        "parenthesis"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_parenthesis_open(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        match super::tokens::parenthesis_open(i) {
            Ok((rest1, par1)) => {
                match Expr::parse(trace, eat_space(rest1)) {
                    Ok((rest2, Some(expr))) => {
                        //
                        match super::tokens::parenthesis_close(eat_space(rest2)) {
                            Ok((rest3, par2)) => {
                                let o = OFParOpen { span: par1 };
                                let c = OFParClose { span: par2 };
                                let ast = Box::new(AstTree::Parenthesis(o, expr, c));
                                Ok(trace.ok(ast.span(), rest3, Some(ast)))
                            }
                            Err(e) => Err(trace.err(rest1, ParseExprError::parenthesis, e)),
                        }
                    }
                    Ok((rest2, None)) => Err(trace.ast_err(rest2, ParseExprError::parenthesis)),
                    Err(e) => Err(trace.parse_err(e)),
                }
            }
            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::parenthesis, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    }
}

// FunctionName '(' ParameterList ')' |
// FunctionName ::= LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
// ParameterList ::= /* empty */ |
//                      Parameter (Separator EmptyOrParameter )* |
//                      Separator EmptyOrParameter /* First param empty */ (Separator EmptyOrParameter )*
// EmptyOrParameter ::= /* empty */ Whitespace* | Parameter
// Parameter ::= Expression
// Separator ::= ';'

/// Parses a function call.
#[derive(Debug)]
struct FnCallExpr;

impl<'s> GeneralExpr<'s> for FnCallExpr {
    fn name() -> &'static str {
        "fn_call"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_fn_name(i)
    }

    #[allow(clippy::collapsible_else_if)]
    #[allow(clippy::needless_return)]
    fn parse<'t>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
        trace.enter(Self::name(), i);

        let (rest1, fn_name) = match recognize(tokens::fn_name)(i) {
            Ok((rest, tok)) => (eat_space(rest), tok),
            Err(nom::Err::Error(_)) => return Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => return Err(trace.err(i, ParseExprError::fn_call, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        };

        trace.step("name", fn_name);

        let mut args = Vec::new();

        match tokens::parenthesis_open(eat_space(rest1)) {
            Ok((mut loop_rest, par1)) => {
                // First separator is checked before the arguments as arguments can be empty.
                let sep1 = match tokens::separator(eat_space(loop_rest)) {
                    Ok((rest2, sep1)) => {
                        loop_rest = rest2;
                        trace.step("initial arg", Span::new(""));
                        trace.step("initial separator", sep1);
                        Some(sep1)
                    }
                    Err(nom::Err::Error(_)) => {
                        // Not necessary
                        None
                    }
                    Err(e @ nom::Err::Failure(_)) => {
                        return Err(trace.err(i, ParseExprError::fn_call, e));
                    }
                    Err(nom::Err::Incomplete(_)) => unreachable!(),
                };

                // Collecting args.
                if let Some(sep1) = sep1 {
                    trace.step2("expr+sep");
                    let ast = AstTree::empty(sep1.take(0));
                    args.push(*ast);
                }

                // Loops and eats from loop_rest.
                loop {
                    // Parse argument.
                    loop_rest = eat_space(loop_rest);
                    let expr = if Expr::lah(loop_rest) {
                        match Expr::parse(trace, loop_rest) {
                            Ok((rest2, Some(expr))) => {
                                loop_rest = rest2;
                                trace.step("arg", expr.span());
                                Some(expr)
                            }
                            Ok((rest2, None)) => {
                                loop_rest = rest2;
                                trace.step("arg", Span::new(""));
                                None
                            }
                            Err(e) => return Err(trace.parse_err(e)),
                        }
                    } else {
                        None
                    };

                    // Followed by a separator.
                    let sep1 = match tokens::separator(eat_space(loop_rest)) {
                        Ok((rest2, sep1)) => {
                            loop_rest = rest2;
                            trace.step("separator", sep1);
                            Some(sep1)
                        }
                        Err(nom::Err::Error(_)) => None,
                        Err(e @ nom::Err::Failure(_)) => {
                            return Err(trace.err(i, ParseExprError::fn_call, e));
                        }
                        Err(nom::Err::Incomplete(_)) => unreachable!(),
                    };

                    #[derive(PartialEq)]
                    enum Parens {
                        Needed,
                        Optional,
                    }

                    // Collecting args.
                    let parens = if let Some(expr) = expr {
                        if sep1.is_some() {
                            trace.step2("expr+sep");
                            args.push(*expr);
                            Parens::Optional
                        } else {
                            trace.step2("expr+none");
                            args.push(*expr);
                            Parens::Needed
                        }
                    } else {
                        if let Some(sep1) = sep1 {
                            trace.step2("none+sep");
                            let ast = AstTree::empty(sep1.take(0));
                            args.push(*ast);
                            Parens::Optional
                        } else {
                            // no arguments and no separator. empty argument lists are ok.
                            trace.step2("None+None");
                            Parens::Needed
                        }
                    };

                    // find a closing paren next
                    match tokens::parenthesis_close(eat_space(loop_rest)) {
                        Ok((rest2, par2)) => {
                            let ast = AstTree::fn_call(fn_name, par1, args, par2);
                            return Ok(trace.ok(ast.span(), rest2, Some(ast)));
                        }
                        Err(e @ nom::Err::Error(_)) => {
                            if parens == Parens::Needed {
                                return Err(trace.err(i, ParseExprError::fn_call, e));
                            }
                        }
                        Err(e @ nom::Err::Failure(_)) => {
                            return Err(trace.err(i, ParseExprError::fn_call, e))
                        }
                        Err(nom::Err::Incomplete(_)) => unreachable!(),
                    }
                }
            }
            Err(e @ nom::Err::Error(_)) => return Err(trace.err(i, ParseExprError::fn_call, e)),
            Err(e @ nom::Err::Failure(_)) => return Err(trace.err(i, ParseExprError::fn_call, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        };
    }
}

// NamedExpression ::= SimpleNamedExpression | SheetLocalNamedExpression | ExternalNamedExpression
// SimpleNamedExpression ::= Identifier | '$$' (Identifier | SingleQuoted)
// SheetLocalNamedExpression ::= QuotedSheetName '.' SimpleNamedExpression
// ExternalNamedExpression ::= Source (SimpleNamedExpression | SheetLocalNamedExpression)
// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee])

/// Eats the leading whitespace.
pub fn eat_space<'a>(i: Span<'a>) -> Span<'a> {
    match multispace0::<Span<'a>, nom::error::Error<_>>(i) {
        Ok((rest, _white)) => rest,
        Err(nom::Err::Error(_)) => i,
        Err(nom::Err::Failure(_)) => i,
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Fails if the string was not fully parsed.
pub fn check_eof<'a>(
    i: Span<'a>,
    err: fn(span: Span<'a>) -> ParseExprError,
) -> Result<(), ParseExprError> {
    if (*i).is_empty() {
        Ok(())
    } else {
        Err(err(i))
    }
}

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast_parser::{
        CellRefExpr, ElementaryExpr, Expr, GeneralExpr, NumberExpr, ReferenceExpr,
    };
    use crate::error::OFError;
    use crate::parse::tracer::Tracer;
    use crate::parse::Span;
    use crate::{AstTree, CellRef, OFCellRef, ParseResult};
    use nom_locate::LocatedSpan;

    fn run_test2<'s>(
        str: &'s str,
        testfn: for<'t> fn(
            &'t Tracer<'s>,
            Span<'s>,
        ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, Some(tok))) => {
                    println!("{} | {}", tok, rest);
                }
                Ok((rest, None)) => {
                    println!(" | {}", rest);
                }
                Err(e) => {
                    println!("{:?}", e);
                }
            }
            println!("{:?}", &tracer);
        }
    }

    #[test]
    fn test_elementary() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test2(test, ElementaryExpr::parse);
        }
    }

    #[test]
    fn test_number() {
        let test_ok = ["25", "25e+5", "25.", "25.001", "25.003e-7"];
        for test in test_ok {
            run_test2(test, NumberExpr::parse);
        }

        let test_err = ["invalid", "2x5", "25ex+5", "25.x", "25.x001", "25x.003e-7"];
        for test in test_err {
            run_test2(test, NumberExpr::parse);
        }
    }
    #[test]
    fn test_expr() {
        let tests = [
            "471",
            r#""strdata""#,
            "1+1",
            "(1+1)",
            "X",
            "4*5+1",
            "4+5*2",
            "22 * FUN ( 77 ) ",
            "17 + FUN(  )",
            "11 ^ FUN(   ;;66)",
            "1+2*3^4",
            "27+(19*.A5)",
        ];
        for test in tests {
            run_test2(test, Expr::parse);
        }
    }

    #[test]
    fn test_cellref2() -> Result<(), OFError> {
        let trace = Tracer::new();
        unsafe {
            let (rest, expr) = CellRefExpr::parse(&trace, Span::new(".A21"))?;
            assert_eq!(
                CellRefExpr::parse(&trace, Span::new(".A21"))?,
                (
                    Span::new_from_raw_offset(4, 1, "", ()),
                    Some(Box::new(AstTree::CellRef(OFCellRef(
                        CellRef::local(21, 0),
                        Span::new_from_raw_offset(1, 1, ".A21", ())
                    ))))
                )
            );
        }
        Ok(())
    }

    #[test]
    fn test_cellref() -> Result<(), OFError> {
        // let _trace = Tracer::new();
        // unsafe {
        //     TODO:this
        //     assert_eq!(
        //         CellRefExpr::parse(&trace, Span::new(".A21"))?,
        //         (
        //             LocatedSpan::new_from_raw_offset(4, 1, "", ()),
        //             Some((
        //                 LocatedSpan::new_from_raw_offset(0, 1, ".A21", ()),
        //                 CellRef::local(21, 0)
        //             ))
        //         )
        //     );
        // }
        Ok(())
    }

    #[test]
    #[allow(unused_must_use)]
    fn test_cellref3() -> Result<(), OFError> {
        let trace = Tracer::new();
        dbg!(ReferenceExpr::parse(&trace, Span::new(".A1")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(ReferenceExpr::parse(&trace, Span::new(".A0")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(ReferenceExpr::parse(&trace, Span::new(".A1:.C4")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(ReferenceExpr::parse(&trace, Span::new(".A:.C")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(ReferenceExpr::parse(&trace, Span::new(".34:.37")));
        dbg!(&trace);

        Ok(())
    }
}
