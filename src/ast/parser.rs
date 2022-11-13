//!
//! Parses and creates the AST.
//!
//! The parser and look-ahead functions expect to be called with a clean string with no
//! leading whitespace. Internal calls are cared for, but the return value is not necessarily
//! clean itself.
//!
//! The look-ahead functions are called internally at certain branching points.

use crate::ast::tokens::lah_prefix_op;
use crate::ast::{
    conv, span_union_opt, tokens, tracer::Tracer, Node, OFAddOp, OFAst, OFCellRange, OFCellRef,
    OFColRange, OFCompOp, OFMulOp, OFPostfixOp, OFPowOp, OFPrefixOp, OFRowRange, ParseResult, Span,
};
use crate::error::{LocateError, ParseOFError};
use nom::character::complete::multispace0;
use nom::combinator::recognize;
use nom::sequence::tuple;
use nom::InputTake;
use spreadsheet_ods_cellref::parser as refs_parser;
use spreadsheet_ods_cellref::tokens as refs_tokens;
use spreadsheet_ods_cellref::{CellRange, ColRange, RowRange};

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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>>;
}

/// Special trait for binary expressions.
pub trait BinaryExpr<'s> {
    /// Operator type.
    type Operator;

    /// Get a name for debug.
    fn name() -> &'static str;

    /// Run a look-ahead.
    fn lah(i: Span<'s>) -> bool;

    /// Parses all the binary expressions.
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match Self::operand(trace, i) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            //
                            match Self::operand(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = Self::ast(expr1, op, expr2);
                                }
                                Err(e) => break Err(trace.parse(e)),
                            }
                        }
                        Err(ParseOFError::ErrNomError(_, _)) => {
                            break Ok(trace.ok(expr1.span(), loop_rest, expr1))
                        }
                        Err(e) => break Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }

    /// Parse the sub expression.
    fn operand<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>>;

    /// Parse the operator.
    /// Parses and maps the Span to an OFCompOp
    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Self::Operator>;

    /// Creates an AST node.
    fn ast(left: Box<OFAst<'s>>, op: Self::Operator, right: Box<OFAst<'s>>) -> Box<OFAst<'s>>;
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

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        <Self as BinaryExpr>::parse(trace, i)
    }
}

/// Any expression.
pub struct Expr;

impl<'s> GeneralExpr<'s> for Expr {
    fn name() -> &'static str {
        "expr"
    }

    fn lah(i: Span<'s>) -> bool {
        <CompareExpr as BinaryExpr>::lah(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);
        match <CompareExpr as BinaryExpr>::parse(trace, i) {
            Ok((rest, expr)) => {
                //
                Ok(trace.ok(expr.span(), rest, expr))
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct CompareExpr;

impl<'s> BinaryExpr<'s> for CompareExpr {
    type Operator = OFCompOp<'s>;

    fn name() -> &'static str {
        "compare"
    }

    fn lah(i: Span<'s>) -> bool {
        <AddExpr as BinaryExpr>::lah(i)
    }

    fn operand<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        <AddExpr as BinaryExpr>::parse(trace, i)
    }

    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, OFCompOp<'s>> {
        trace.enter("comp-operator", i);
        trace.optional("comp-operator");
        match tokens::comparison_op(i) {
            Ok((rest, tok)) => match *tok {
                "=" => Ok(trace.ok(tok, rest, OFCompOp::Equal(tok))),
                "<>" => Ok(trace.ok(tok, rest, OFCompOp::Unequal(tok))),
                "<" => Ok(trace.ok(tok, rest, OFCompOp::Less(tok))),
                ">" => Ok(trace.ok(tok, rest, OFCompOp::Greater(tok))),
                "<=" => Ok(trace.ok(tok, rest, OFCompOp::LessEqual(tok))),
                ">=" => Ok(trace.ok(tok, rest, OFCompOp::GreaterEqual(tok))),
                _ => unreachable!(),
            },

            Err(e) => Err(trace.nom(e)),
        }
    }

    fn ast(left: Box<OFAst<'s>>, op: Self::Operator, right: Box<OFAst<'s>>) -> Box<OFAst<'s>> {
        OFAst::compare(left, op, right)
    }
}

struct AddExpr;

impl<'s> BinaryExpr<'s> for AddExpr {
    type Operator = OFAddOp<'s>;

    fn name() -> &'static str {
        "add"
    }

    fn lah(i: Span<'s>) -> bool {
        <MulExpr as BinaryExpr>::lah(i)
    }

    fn operand<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        <MulExpr as BinaryExpr>::parse(trace, i)
    }

    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Self::Operator> {
        trace.enter("add-operator", i);
        trace.optional("add-operator");
        match tokens::add_op(i) {
            Ok((rest, tok)) => match *tok {
                "+" => Ok(trace.ok(tok, rest, OFAddOp::Add(tok))),
                "-" => Ok(trace.ok(tok, rest, OFAddOp::Subtract(tok))),
                _ => unreachable!(),
            },

            Err(e) => Err(trace.nom(e)),
        }
    }

    fn ast(left: Box<OFAst<'s>>, op: Self::Operator, right: Box<OFAst<'s>>) -> Box<OFAst<'s>> {
        OFAst::add(left, op, right)
    }
}

struct MulExpr;

impl<'s> BinaryExpr<'s> for MulExpr {
    type Operator = OFMulOp<'s>;

    fn name() -> &'static str {
        "mul"
    }

    fn lah(i: Span<'s>) -> bool {
        PostFixExpr::lah(i)
    }

    fn operand<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        PostFixExpr::parse(trace, i)
    }

    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Self::Operator> {
        trace.enter("mul-operator", i);
        trace.optional("mul-operator");
        match tokens::mul_op(i) {
            Ok((rest, tok)) => match *tok {
                "*" => Ok(trace.ok(tok, rest, OFMulOp::Multiply(tok))),
                "/" => Ok(trace.ok(tok, rest, OFMulOp::Divide(tok))),
                _ => unreachable!(),
            },
            Err(e) => Err(trace.nom(e)),
        }
    }

    fn ast(left: Box<OFAst<'s>>, op: Self::Operator, right: Box<OFAst<'s>>) -> Box<OFAst<'s>> {
        OFAst::mul(left, op, right)
    }
}

struct PowExpr;

impl<'s> BinaryExpr<'s> for PowExpr {
    type Operator = OFPowOp<'s>;

    fn name() -> &'static str {
        "pow"
    }

    fn lah(i: Span<'s>) -> bool {
        PostFixExpr::lah(i)
    }

    fn operand<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        PostFixExpr::parse(trace, i)
    }

    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Self::Operator> {
        trace.enter("pow-operator", i);
        trace.optional("pow-operator");
        match tokens::pow_op(i) {
            Ok((rest, tok)) => match *tok {
                "^" => Ok(trace.ok(tok, rest, OFPowOp::Power(tok))),
                _ => unreachable!(),
            },
            Err(e) => Err(trace.nom(e)),
        }
    }

    fn ast(left: Box<OFAst<'s>>, op: Self::Operator, right: Box<OFAst<'s>>) -> Box<OFAst<'s>> {
        OFAst::pow(left, op, right)
    }
}

struct PostFixExpr;

impl PostFixExpr {
    /// Parses and maps the Span to an OFPostfixOp
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, OFPostfixOp<'s>> {
        trace.enter("postfix-operator", i);
        trace.optional("postfix-operator");
        match tokens::postfix_op(i) {
            Ok((rest, tok)) => match *tok {
                "%" => Ok(trace.ok(tok, rest, OFPostfixOp::Percent(tok))),
                _ => unreachable!(),
            },
            Err(e) => Err(trace.nom(e)),
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

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match PrefixExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr)) => {
                //
                loop {
                    if tokens::lah_postfix_op(loop_rest) {
                        match Self::operator(trace, eat_space(loop_rest)) {
                            Ok((rest1, tok)) => {
                                trace.step("op", tok.span());
                                loop_rest = rest1;
                                expr = OFAst::postfix(expr, tok);
                            }
                            // We can rely on lah_postfix_op, so this can only fail
                            // for other reasons, not a simple "does not parse".
                            Err(e) => break Err(trace.parse(e)),
                        }
                    } else {
                        break Ok(trace.ok(expr.span(), loop_rest, expr));
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct PrefixExpr;

impl PrefixExpr {
    /// Parses and maps the Span to a OFPrefixOp.
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, OFPrefixOp<'s>> {
        trace.enter("prefix-operator", i);
        trace.optional("prefix-operator");
        match tokens::prefix_op(i) {
            Ok((rest, tok)) => match *tok {
                "+" => Ok(trace.ok(tok, rest, OFPrefixOp::Plus(tok))),
                "-" => Ok(trace.ok(tok, rest, OFPrefixOp::Minus(tok))),
                _ => unreachable!(),
            },
            Err(e) => Err(trace.nom(e)),
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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        if lah_prefix_op(rest) {
            match Self::operator(trace, rest) {
                Ok((rest1, op)) => {
                    // Potentially recursive
                    match PrefixExpr::parse(trace, eat_space(rest1)) {
                        Ok((rest2, expr)) => {
                            let ast = OFAst::prefix(op, expr);
                            Ok(trace.ok(ast.span(), rest2, ast))
                        }
                        Err(e) => Err(trace.parse(e)),
                    }
                }
                // We can rely on lah_prefix_op, so this can only fail
                // for other reasons, not a simple "does not parse".
                Err(e) => Err(trace.parse(e)),
            }
        } else {
            match ElementaryExpr::parse(trace, eat_space(rest)) {
                Ok((rest2, expr)) => Ok(trace.ok(expr.span(), rest2, expr)),
                Err(e) => Err(trace.parse(e)),
            }
        }
    }
}

/// Parser for the lowest expression level.
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

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        if NumberExpr::lah(i) {
            trace.optional(NumberExpr::name());
            match NumberExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        if StringExpr::lah(i) {
            trace.optional(StringExpr::name());
            match StringExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        if ParenthesisExpr::lah(i) {
            trace.optional(ParenthesisExpr::name());
            match ParenthesisExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        if ReferenceExpr::lah(i) {
            trace.optional(ReferenceExpr::name());
            match ReferenceExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_, _)) => { /* skip, some token error */ }
                Err(ParseOFError::ErrReference(_)) => { /* skip, no reference */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        if FnCallExpr::lah(i) {
            trace.optional(FnCallExpr::name());
            match FnCallExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        Err(trace.parse(ParseOFError::elementary(i)))
    }
}

/// Parser for number.
struct NumberExpr;

impl<'s> GeneralExpr<'s> for NumberExpr {
    fn name() -> &'static str {
        "number"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_number(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match tokens::number(rest) {
            Ok((rest2, number)) => {
                //
                match (*number).parse::<f64>() {
                    Ok(val) => {
                        let ast = OFAst::number(val, number);
                        Ok(trace.ok(ast.span(), rest2, ast))
                    }
                    Err(_) => unreachable!(),
                }
            }
            Err(e) => Err(trace.nom(e)),
        }
    }
}

/// Parser for strings.
struct StringExpr;

impl<'s> GeneralExpr<'s> for StringExpr {
    fn name() -> &'static str {
        "string"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_string(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match tokens::string(rest) {
            Ok((rest2, tok)) => {
                let ast = OFAst::string(conv::unquote_double(tok), tok);
                Ok(trace.ok(ast.span(), rest2, ast))
            }
            Err(e) => Err(trace.nom(e)),
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
pub struct ReferenceExpr;

impl<'s> GeneralExpr<'s> for ReferenceExpr {
    fn name() -> &'static str {
        "reference"
    }

    fn lah(i: Span<'s>) -> bool {
        CellRefExpr::lah(i)
    }

    /// Tries to ast a cell reference.
    #[allow(clippy::manual_map)]
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        trace.optional(CellRangeExpr::name());
        match CellRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }
        trace.optional(CellRangeExpr::name());
        match CellRefExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }
        trace.optional(CellRangeExpr::name());
        match ColRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }
        trace.optional(CellRangeExpr::name());
        match RowRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_, _)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }

        Err(trace.parse(ParseOFError::reference(i)))
    }
}

/// Parses a simple cell reference.
pub struct CellRefExpr;

impl CellRefExpr {
    /// Parses the full string as CellRef.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, OFCellRef<'s>> {
        trace.enter(Self::name(), i);

        match CellRefExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::parse_incomplete)?;
                let OFAst::NodeCellRef(cell_ref) = *expr else {
                    panic!("Expected a CellRef");
                };
                Ok(trace.ok(cell_ref.span, rest, cell_ref))
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for CellRefExpr {
    fn name() -> &'static str {
        "cell_ref"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i) || refs_tokens::lah_sheet_name(i) || refs_tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match refs_parser::parse_cell_ref_raw(i) {
            Ok((rest, cell_ref)) => {
                let iri = cell_ref.iri.map(|v| OFAst::iri(conv::unquote_single(v), v));
                let table = cell_ref.table.map(|v| {
                    OFAst::sheet_name(
                        refs_parser::try_bool_from_abs_flag(cell_ref.abs_table),
                        conv::unquote_single(v),
                        v,
                    )
                });
                let row = OFAst::row(
                    refs_parser::try_bool_from_abs_flag(cell_ref.abs_row),
                    refs_parser::try_u32_from_rowname(cell_ref.row).locate_err(i)?,
                    cell_ref.row,
                );
                let col = OFAst::col(
                    refs_parser::try_bool_from_abs_flag(cell_ref.abs_col),
                    refs_parser::try_u32_from_colname(cell_ref.col).locate_err(i)?,
                    cell_ref.col,
                );

                let ast = OFAst::cell_ref(iri, table, row, col, cell_ref.token);
                Ok(trace.ok(cell_ref.token, rest, ast))
            }
            Err(e) => Err(trace.nom(e)),
        }
    }
}

/// Parses a cell range.
pub struct CellRangeExpr;

impl CellRangeExpr {
    /// Parses the full string as CellRange.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, CellRange> {
        trace.enter(Self::name(), i);

        match CellRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::cell_range)?;
                let OFAst::NodeCellRange(OFCellRange(cell_range, span)) = *expr else {
                    panic!("Expected a CellRange");
                };
                Ok(trace.ok(span, rest, cell_range))
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for CellRangeExpr {
    fn name() -> &'static str {
        "cell_range"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i) || refs_tokens::lah_sheet_name(i) || refs_tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match refs_parser::parse_cell_range(i) {
            Ok((rest, (cell_range, tok))) => {
                let ast = OFAst::cell_range(cell_range, tok);
                Ok(trace.ok(tok, rest, ast))
            }
            Err(e) => Err(trace.reference(i, e)),
        }
    }
}

/// Parses a column range.
pub struct ColRangeExpr;

impl ColRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, ColRange> {
        trace.enter("parse_colrange", i);

        match Self::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::col_range)?;
                let OFAst::NodeColRange(OFColRange(col_range, span)) = *expr else {
                    panic!("Expected a ColRange");
                };
                Ok(trace.ok(span, rest, col_range))
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for ColRangeExpr {
    fn name() -> &'static str {
        "col_range"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i) || refs_tokens::lah_sheet_name(i) || refs_tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);
        match refs_parser::parse_col_range(i) {
            Ok((rest, (col_range, tok))) => {
                let ast = OFAst::col_range(col_range, tok);
                Ok(trace.ok(tok, rest, ast))
            }
            Err(e) => Err(trace.reference(i, e)),
        }
    }
}

/// Parses a row range.
pub struct RowRangeExpr;

impl RowRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, RowRange> {
        trace.enter(Self::name(), i);

        match Self::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::row_range)?;
                let OFAst::NodeRowRange(OFRowRange(row_range, span)) = *expr else {
                    panic!("Expected a RowRange");
                };
                Ok(trace.ok(span, rest, row_range))
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

impl<'s> GeneralExpr<'s> for RowRangeExpr {
    fn name() -> &'static str {
        "row_range"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i) || refs_tokens::lah_sheet_name(i) || refs_tokens::lah_dot(i)
    }

    #[allow(clippy::manual_map)]
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match refs_parser::parse_row_range(i) {
            Ok((rest, (row_range, tok))) => {
                let ast = OFAst::row_range(row_range, tok);
                Ok(trace.ok(tok, rest, ast))
            }
            Err(e) => Err(trace.reference(i, e)),
        }
    }
}

struct ParenthesisExpr;

impl<'s> GeneralExpr<'s> for ParenthesisExpr {
    fn name() -> &'static str {
        "parentheses"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_parentheses_open(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match tokens::parentheses_open(i) {
            Ok((rest1, par1)) => {
                match Expr::parse(trace, eat_space(rest1)) {
                    Ok((rest2, expr)) => {
                        //
                        match tokens::parentheses_close(eat_space(rest2)) {
                            Ok((rest3, par2)) => {
                                let ast = OFAst::parens(par1, expr, par2);
                                Ok(trace.ok(ast.span(), rest3, ast))
                            }
                            Err(e) => Err(trace.nom(e)),
                        }
                    }
                    Err(e) => Err(trace.parse(e)),
                }
            }
            Err(e) => Err(trace.nom(e)),
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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        let (rest1, fn_name) = match recognize(tokens::fn_name)(i) {
            Ok((rest, tok)) => {
                //
                (eat_space(rest), tok)
            }
            Err(e) => return Err(trace.nom(e)),
        };

        trace.step("name", fn_name);

        let mut args = Vec::new();

        match tokens::parentheses_open(eat_space(rest1)) {
            Ok((mut loop_rest, par1)) => {
                // First separator is checked before the arguments as arguments can be empty.
                match tokens::separator(eat_space(loop_rest)) {
                    Ok((rest2, sep1)) => {
                        loop_rest = rest2;
                        trace.step("initial arg", Span::new(""));
                        trace.step("initial separator", sep1);

                        let ast = OFAst::empty(sep1.take(0));
                        args.push(*ast);
                    }
                    Err(nom::Err::Error(_)) => {
                        // Optional
                    }
                    Err(e) => {
                        return Err(trace.nom(e));
                    }
                }

                // Loops and eats from loop_rest.
                loop {
                    // Parse argument.
                    loop_rest = eat_space(loop_rest);
                    let expr = if Expr::lah(loop_rest) {
                        match Expr::parse(trace, loop_rest) {
                            Ok((rest2, expr)) => {
                                loop_rest = rest2;
                                trace.step("arg", expr.span());
                                Some(expr)
                            }
                            Err(ParseOFError::ErrNomError(_, _)) => {
                                // Optional
                                trace.step("arg", Span::new(""));
                                None
                            }
                            Err(e) => return Err(trace.parse(e)),
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
                        Err(nom::Err::Error(_)) => {
                            // Optional
                            None
                        }
                        Err(e) => {
                            return Err(trace.nom(e));
                        }
                    };

                    #[derive(PartialEq)]
                    enum Parens {
                        Needed,
                        Optional,
                    }

                    // Collecting args.
                    let parens = if let Some(expr) = expr {
                        if let Some(sep1) = sep1 {
                            trace.step("expr+sep", sep1.take(0));
                            args.push(*expr);
                            Parens::Optional
                        } else {
                            trace.step("expr+none", expr.span().take(0));
                            args.push(*expr);
                            Parens::Needed
                        }
                    } else {
                        if let Some(sep1) = sep1 {
                            trace.step("none+sep", sep1.take(0));
                            let ast = OFAst::empty(sep1.take(0));
                            args.push(*ast);
                            Parens::Optional
                        } else {
                            // no arguments and no separator. empty argument lists are ok.
                            trace.step("None+None", loop_rest.take(0));
                            Parens::Needed
                        }
                    };

                    // find a closing paren next
                    match tokens::parentheses_close(eat_space(loop_rest)) {
                        Ok((rest2, par2)) => {
                            let fn_name = OFAst::fn_name(fn_name.to_string(), fn_name);
                            let ast = OFAst::fn_call(fn_name, par1, args, par2);
                            return Ok(trace.ok(ast.span(), rest2, ast));
                        }
                        Err(e @ nom::Err::Error(_)) => {
                            // Fail if closing parentheses are required.
                            if parens == Parens::Needed {
                                return Err(trace.nom(e));
                            }
                        }
                        Err(e) => return Err(trace.nom(e)),
                    }
                }
            }
            Err(e) => return Err(trace.nom(e)),
        };
    }
}

// NamedExpression ::= SimpleNamedExpression | SheetLocalNamedExpression | ExternalNamedExpression
// SimpleNamedExpression ::= Identifier | '$$' (Identifier | SingleQuoted)
// SheetLocalNamedExpression ::= QuotedSheetName '.' SimpleNamedExpression
// ExternalNamedExpression ::= Source (SimpleNamedExpression | SheetLocalNamedExpression)
// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )  # means no cell reference
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee]) # true or false

struct NamedExpr;

impl<'s> GeneralExpr<'s> for NamedExpr {
    fn name() -> &'static str {
        "named"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i)
            || refs_tokens::lah_sheet_name(i)
            || tokens::lah_dollar_dollar(i)
            || tokens::lah_identifier(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        // NamedExpression := IRI?
        //                      (QuotedSheetName '.')?
        //                      (Identifier | '$$' (Identifier | SingleQuoted)
        //

        // IRI?
        let (rest, iri) = match refs_tokens::iri(rest) {
            Ok((rest1, iri)) => {
                //
                (rest1, Some(OFAst::iri(conv::unquote_single(iri), iri)))
            }
            Err(nom::Err::Error(_)) => {
                //
                (rest, None)
            }
            Err(e) => return Err(trace.nom(e)),
        };

        // (QuotedSheetName '.')?
        let (rest, sheet_name) = match tuple((refs_tokens::sheet_name, tokens::dot))(rest) {
            Ok((rest1, ((abs, sheet_name), _dot))) => unsafe {
                (
                    rest1,
                    Some(OFAst::sheet_name(
                        refs_parser::try_bool_from_abs_flag(abs),
                        conv::unquote_single(sheet_name),
                        span_union_opt(abs, sheet_name),
                    )),
                )
            },
            Err(nom::Err::Error(_)) => {
                //
                (rest, None)
            }
            Err(e) => return Err(trace.nom(e)),
        };

        // (Identifier | '$$' (Identifier | SingleQuoted)

        // Identifier
        let (rest, named) = match tokens::identifier(rest) {
            Ok((rest1, ident)) => {
                // Identifier
                (rest1, Some(OFAst::simple_named(ident.to_string(), ident)))
            }
            Err(nom::Err::Error(_)) => (rest, None),
            Err(e) => return Err(trace.nom(e)),
        };

        // '$$' (Identifier | SingleQuoted)
        let (rest, named) = if named.is_some() {
            (rest, named.unwrap())
        } else {
            // '$$'
            match tokens::dollar_dollar(rest) {
                Ok((_rest1, _tag)) => {}
                Err(e) => return Err(trace.nom(e)),
            }

            // Identifier
            let (rest, named) = match tokens::identifier(rest) {
                Ok((rest1, ident)) => {
                    //
                    (rest1, Some(OFAst::simple_named(ident.to_string(), ident)))
                }
                Err(nom::Err::Error(_)) => (rest, None),
                Err(e) => return Err(trace.nom(e)),
            };

            // SingleQuoted
            let (rest, named) = if named.is_some() {
                (rest, named.unwrap())
            } else {
                match tokens::quoted('\'')(rest) {
                    Ok((rest1, ident)) => {
                        //
                        let named_str = conv::unquote_single(ident);
                        (rest1, OFAst::simple_named(named_str, ident))
                    }
                    Err(e) => return Err(trace.nom(e)),
                }
            };

            (rest, named)
        };

        let ast = OFAst::named(iri, sheet_name, named);
        Ok(trace.ok(ast.span(), rest, ast))
    }
}

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
pub fn check_eof<'s>(
    i: Span<'s>,
    err: fn(span: Span<'s>) -> ParseOFError<'s>,
) -> Result<(), ParseOFError<'s>> {
    if (*i).is_empty() {
        Ok(())
    } else {
        Err(err(i))
    }
}

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast::parser::GeneralExpr;
    use crate::ast::parser::NumberExpr;
    use crate::ast::parser::{ElementaryExpr, Expr};
    use crate::ast::tracer::Tracer;
    use crate::ast::{OFAst, ParseResult, Span};

    fn run_test2<'s>(
        str: &'s str,
        testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, tok)) => {
                    println!("{:?} | {}", tok, rest);
                }
                Err(e) => {
                    println!("{}", e);
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

    // #[test]
    // fn test_cellref2() -> Result<(), OFError> {
    //     TODO: repair
    //     let trace = Tracer::new();
    //     unsafe {
    //         assert_eq!(
    //             CellRefExpr::ast(&trace, Span::new(".A21"))?,
    //             (
    //                 Span::new_from_raw_offset(4, 1, "", ()),
    //                 Some(Box::new(OFAst::NodeCellRef(OFCellRef(
    //                     CellRef::local(20, 0),
    //                     Span::new_from_raw_offset(1, 1, ".A21", ())
    //                 ))))
    //             )
    //         );
    //     }
    //     Ok(())
    // }

    // #[test]
    // #[allow(unused_must_use)]
    // fn test_cellref3() -> Result<(), OFError> {
    //     let trace = Tracer::new();
    //     dbg!(ReferenceExpr::ast(&trace, Span::new(".A1")));
    //     dbg!(&trace);
    //
    //     let trace = Tracer::new();
    //     dbg!(ReferenceExpr::ast(&trace, Span::new(".A0")));
    //     dbg!(&trace);
    //
    //     let trace = Tracer::new();
    //     dbg!(ReferenceExpr::ast(&trace, Span::new(".A1:.C4")));
    //     dbg!(&trace);
    //
    //     let trace = Tracer::new();
    //     dbg!(ReferenceExpr::ast(&trace, Span::new(".A:.C")));
    //     dbg!(&trace);
    //
    //     let trace = Tracer::new();
    //     dbg!(ReferenceExpr::ast(&trace, Span::new(".34:.37")));
    //     dbg!(&trace);
    //
    //     Ok(())
    // }
}
