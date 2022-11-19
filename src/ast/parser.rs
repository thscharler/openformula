//!
//! Parses and creates the AST.
//!
//! The parser and look-ahead functions expect to be called with a clean string with no
//! leading whitespace. Internal calls are cared for, but the return value is not necessarily
//! clean itself.
//!
//! The look-ahead functions are called internally at certain branching points.

use crate::ast::nomtokens::eat_space;
use crate::ast::tokens::empty;
use crate::ast::tracer::TrackParseResult;
use crate::ast::{
    conv, span_union, span_union_opt, tokens, tracer::Tracer, Node, OFAddOp, OFAst, OFCellRange,
    OFCellRef, OFCol, OFColRange, OFCompOp, OFIri, OFMulOp, OFPostfixOp, OFPowOp, OFPrefixOp,
    OFRow, OFRowRange, OFSheetName, ParseResult, Span,
};
use crate::error::OFCode::*;
use crate::error::{LocateError, OFCode, ParseOFError};
use spreadsheet_ods_cellref::parser as refs_parser;
use spreadsheet_ods_cellref::tokens as refs_tokens;

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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>>;
}

///
pub trait GeneralTerm<'s, O> {
    /// Get a name for debug.
    fn name() -> &'static str;

    /// Run a look-ahead.
    fn lah(i: Span<'s>) -> bool;

    /// Parses the expression.
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, O>;
}

/// Any expression.
pub struct Expr;

impl<'s> GeneralExpr<'s> for Expr {
    fn name() -> &'static str {
        "expr"
    }

    fn lah(i: Span<'s>) -> bool {
        CompareExpr::lah(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);
        match CompareExpr::parse(trace, rest) {
            Ok((rest, expr)) => trace.ok(expr.span(), rest, expr),
            Err(e) => trace.err_parse(e),
        }
    }
}

struct CompareExpr;

impl<'s> CompareExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFCompOp<'s>> {
        trace.enter("comp-operator", rest);
        trace.optional("comp-operator");
        match tokens::comparison_op(rest) {
            Ok((rest, tok)) => match *tok {
                "=" => trace.ok(tok, rest, OFCompOp::Equal(tok)),
                "<>" => trace.ok(tok, rest, OFCompOp::Unequal(tok)),
                "<" => trace.ok(tok, rest, OFCompOp::Less(tok)),
                ">" => trace.ok(tok, rest, OFCompOp::Greater(tok)),
                "<=" => trace.ok(tok, rest, OFCompOp::LessEqual(tok)),
                ">=" => trace.ok(tok, rest, OFCompOp::GreaterEqual(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
        }
    }
}

impl<'s> GeneralExpr<'s> for CompareExpr {
    fn name() -> &'static str {
        "compare"
    }

    fn lah(i: Span<'s>) -> bool {
        AddExpr::lah(i)
    }

    /// Parses all the binary expressions.
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match AddExpr::parse(trace, rest) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggest();
                            //
                            match AddExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::compare(expr1, op, expr2);
                                }
                                Err(e) => break trace.err_parse(e),
                            }
                        }
                        Err(e) if e.code == OFCompOp => {
                            trace.suggest(OFCompOp);
                            break trace.ok(expr1.span(), loop_rest, expr1);
                        }
                        Err(e) => break trace.err_parse(e),
                    }
                }
            }
            Err(e) => trace.err_parse(e),
        }
    }
}

struct AddExpr;

impl<'s> AddExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFAddOp<'s>> {
        trace.enter("add-operator", rest);
        trace.optional("add-operator");
        match tokens::add_op(rest) {
            Ok((rest, tok)) => match *tok {
                "+" => trace.ok(tok, rest, OFAddOp::Add(tok)),
                "-" => trace.ok(tok, rest, OFAddOp::Subtract(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
        }
    }
}

impl<'s> GeneralExpr<'s> for AddExpr {
    fn name() -> &'static str {
        "add"
    }

    fn lah(i: Span<'s>) -> bool {
        MulExpr::lah(i)
    }

    /// Parses all the binary expressions.
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match MulExpr::parse(trace, rest) {
            Ok((mut loop_rest, mut expr1)) => {
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggest();
                            //
                            match MulExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::add(expr1, op, expr2);
                                }
                                Err(e) => {
                                    trace.suggest(OFMul);
                                    break trace.err_parse(e);
                                }
                            }
                        }
                        Err(e) if e.code == OFAddOp => {
                            trace.suggest(OFAddOp);
                            break trace.ok(expr1.span(), loop_rest, expr1);
                        }
                        Err(e) => break trace.err_parse(e),
                    }
                }
            }
            Err(e) => trace.err_parse(e),
        }
    }
}

struct MulExpr;

impl<'s> MulExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFMulOp<'s>> {
        trace.enter("mul-operator", rest);
        trace.optional("mul-operator");
        match tokens::mul_op(rest) {
            Ok((rest, tok)) => match *tok {
                "*" => trace.ok(tok, rest, OFMulOp::Multiply(tok)),
                "/" => trace.ok(tok, rest, OFMulOp::Divide(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
        }
    }
}

impl<'s> GeneralExpr<'s> for MulExpr {
    fn name() -> &'static str {
        "mul"
    }

    fn lah(i: Span<'s>) -> bool {
        PowExpr::lah(i)
    }

    /// Parses all the binary expressions.
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match PowExpr::parse(trace, rest) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggest();

                            match PowExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::mul(expr1, op, expr2);
                                }
                                Err(e) => {
                                    trace.suggest(OFPow);
                                    break trace.err_parse(e);
                                }
                            }
                        }
                        Err(e) if e.code == OFMulOp => {
                            trace.suggest(OFMulOp);
                            break trace.ok(expr1.span(), loop_rest, expr1);
                        }
                        Err(e) => break trace.err_parse(e),
                    }
                }
            }
            Err(e) => trace.err_parse(e),
        }
    }
}

struct PowExpr;

impl<'s> PowExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFPowOp<'s>> {
        trace.enter("pow-operator", rest);
        trace.optional("pow-operator");
        match tokens::pow_op(rest) {
            Ok((rest, tok)) => match *tok {
                "^" => trace.ok(tok, rest, OFPowOp::Power(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
        }
    }
}

impl<'s> GeneralExpr<'s> for PowExpr {
    fn name() -> &'static str {
        "pow"
    }

    fn lah(i: Span<'s>) -> bool {
        PostfixExpr::lah(i)
    }

    /// Parses all the binary expressions.
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match PostfixExpr::parse(trace, rest) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    trace.suggest(OFPowOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggest();
                            //
                            match PostfixExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::pow(expr1, op, expr2);
                                }
                                Err(e) => {
                                    trace.suggest(OFPostfix);
                                    break trace.err_parse(e);
                                }
                            }
                        }
                        Err(e) if e.code == OFPowOp => {
                            trace.suggest(OFPowOp);
                            break trace.ok(expr1.span(), loop_rest, expr1);
                        }
                        Err(e) => break trace.err_parse(e),
                    }
                }
            }
            Err(e) => trace.err_parse(e),
        }
    }
}

struct PostfixExpr;

impl PostfixExpr {
    /// Parses and maps the Span to an OFPostfixOp
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, OFPostfixOp<'s>> {
        trace.enter("postfix-operator", rest);
        trace.optional("postfix-operator");
        match tokens::postfix_op(rest) {
            Ok((rest, tok)) => match *tok {
                "%" => trace.ok(tok, rest, OFPostfixOp::Percent(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
        }
    }
}

impl<'s> GeneralExpr<'s> for PostfixExpr {
    fn name() -> &'static str {
        "postfix"
    }

    fn lah(i: Span<'s>) -> bool {
        PrefixExpr::lah(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        let (rest, ast) = match PrefixExpr::parse(trace, rest) {
            Ok((mut loop_rest, mut expr)) => {
                //
                loop {
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest1, tok)) => {
                            loop_rest = rest1;
                            expr = OFAst::postfix(expr, tok);
                        }
                        Err(e) if e.code == OFPostfixOp => {
                            trace.suggest(OFPostfixOp);
                            break (loop_rest, expr);
                        }
                        Err(e) => return trace.err_parse(e),
                    }
                }
            }
            Err(e) => return trace.err_parse(e),
        };

        trace.ok(ast.span(), rest, ast)
    }
}

struct PrefixExpr;

impl PrefixExpr {
    /// Parses and maps the Span to a OFPrefixOp.
    pub fn operator<'s, 't>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, OFPrefixOp<'s>> {
        trace.enter("prefix-operator", rest);
        trace.optional("prefix-operator");
        match tokens::prefix_op(rest) {
            Ok((rest, tok)) => match *tok {
                "+" => trace.ok(tok, rest, OFPrefixOp::Plus(tok)),
                "-" => trace.ok(tok, rest, OFPrefixOp::Minus(tok)),
                _ => unreachable!(),
            },
            Err(e) => trace.err_parse(e),
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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        let mut op_vec = Vec::new();
        let mut loop_rest = rest;
        // eat all prefix ops and keep them save.
        let rest = loop {
            match Self::operator(trace, eat_space(loop_rest)) {
                Ok((rest1, op)) => {
                    loop_rest = rest1;
                    op_vec.push(op);
                }
                Err(e) if e.code == OFPrefixOp => {
                    trace.suggest(OFPrefixOp);
                    break loop_rest;
                }
                Err(e) => return trace.err_parse(e),
            }
        };

        // parse the expression itself
        let (rest, expr) = match ElementaryExpr::parse(trace, eat_space(rest)) {
            Ok((rest1, expr)) => (rest1, expr),
            Err(e) => return trace.err_parse(e),
        };

        // join everything up
        let mut ast = expr;
        while let Some(op) = op_vec.pop() {
            ast = OFAst::prefix(op, ast);
        }

        trace.ok(ast.span(), rest, ast)
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
            || ParenthesesExpr::lah(i)
            || ReferenceExpr::lah(i)
            || FnCallExpr::lah(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        if NumberExpr::lah(rest) {
            trace.optional(NumberExpr::name());
            match NumberExpr::parse(trace, rest) {
                Ok((rest, expr)) => {
                    return trace.ok(expr.span(), rest, expr);
                }
                Err(e) if e.code == OFNomError => {
                    trace.suggest(OFNumber);
                    /* skip */
                }
                Err(e) => return trace.err_parse(e),
            }
        }

        if StringExpr::lah(rest) {
            trace.optional(StringExpr::name());
            match StringExpr::parse(trace, rest) {
                Ok((rest, expr)) => {
                    return trace.ok(expr.span(), rest, expr);
                }
                Err(e) if e.code == OFNomError => {
                    trace.suggest(OFString);
                    /* skip */
                }
                Err(e) => return trace.err_parse(e),
            }
        }

        if ParenthesesExpr::lah(rest) {
            trace.optional(ParenthesesExpr::name());
            match ParenthesesExpr::parse(trace, rest) {
                Ok((rest, expr)) => {
                    return trace.ok(expr.span(), rest, expr);
                }
                Err(e) if e.code == OFNomError => {
                    trace.suggest(OFParentheses);
                    /* skip */
                }
                Err(e) => return trace.err_parse(e),
            }
        }

        if ReferenceExpr::lah(rest) {
            trace.optional(ReferenceExpr::name());
            match ReferenceExpr::parse(trace, rest) {
                Ok((rest, expr)) => {
                    return trace.ok(expr.span(), rest, expr);
                }
                Err(e) if e.code == OFReference => {
                    /* skip, no reference */
                    trace.suggest(OFReference);
                }
                Err(e) => return trace.err_parse(e),
            }
        }

        if FnCallExpr::lah(rest) {
            trace.optional(FnCallExpr::name());
            match FnCallExpr::parse(trace, rest) {
                Ok((rest, expr)) => {
                    return trace.ok(expr.span(), rest, expr);
                }
                Err(e) if e.code == OFNomError => {
                    /* skip */
                    trace.suggest(OFFnCall);
                }
                Err(e) => return trace.err_parse(e),
            }
        }

        // TODO: NamedExpr

        trace.err_parse(ParseOFError::elementary(rest))
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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match tokens::number(rest) {
            Ok((rest2, number)) => {
                //
                match (*number).parse::<f64>() {
                    Ok(val) => {
                        let ast = OFAst::number(val, number);
                        trace.ok(ast.span(), rest2, ast)
                    }
                    Err(_) => unreachable!(),
                }
            }
            Err(e) => trace.err_map_parse(e, OFCode::OFNumber),
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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        match tokens::string(rest) {
            Ok((rest2, tok)) => {
                let ast = OFAst::string(conv::unquote_double(tok), tok);
                trace.ok(ast.span(), rest2, ast)
            }
            Err(e) => trace.err_track_map_parse(e, OFCode::OFString),
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.optional(CellRangeExpr::name());
        match CellRangeExpr::parse(trace, rest) {
            Ok((rest, token)) => {
                return trace.ok(token.span(), rest, token);
            }
            Err(e) if e.code == OFCellRange => {
                //trace.suggest(OFCellRange);
                // Not matched, ok.
            }
            Err(e) => trace.panic_parse(e),
        };

        trace.optional(CellRefExpr::name());
        match CellRefExpr::parse(trace, rest) {
            Ok((rest, token)) => {
                return trace.ok(token.span(), rest, token);
            }
            Err(e) if e.code == OFCellRef => {
                //trace.suggest(OFCellRef);
                // Not matched, ok.
            }
            Err(e) => trace.panic_parse(e),
        }

        trace.optional(ColRangeExpr::name());
        match ColRangeExpr::parse(trace, rest) {
            Ok((rest, token)) => {
                return trace.ok(token.span(), rest, token);
            }
            Err(e) if e.code == OFColRange => {
                //trace.suggest(OFColRange);
                // Not matched, ok.
            }
            Err(e) => trace.panic_parse(e),
        }

        trace.optional(RowRangeExpr::name());
        match RowRangeExpr::parse(trace, rest) {
            Ok((rest, token)) => {
                return trace.ok(token.span(), rest, token);
            }
            Err(e) if e.code == OFRowRange => {
                //trace.suggest(OFRowRange);
                // Not matched, ok.
            }
            Err(e) => trace.panic_parse(e),
        }

        trace.expect(OFReference);
        trace.err_parse(ParseOFError::reference(rest))
    }
}

pub struct ColTerm;

impl<'s> GeneralTerm<'s, OFCol<'s>> for ColTerm {
    fn name() -> &'static str {
        "col"
    }

    fn lah(_i: Span<'s>) -> bool {
        todo!()
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFCol<'s>> {
        trace.enter(Self::name(), rest);

        let (rest, col) = match tokens::col(rest) {
            Ok((rest, col)) => (rest, col),
            Err(e) if e.code == OFDollar => {
                return trace.err_track_map_parse(e, OFCode::OFCol);
            }
            Err(e) if e.code == OFAlpha => {
                return trace.err_track_map_parse(e, OFCode::OFCol);
            }
            Err(e) => {
                trace.panic_parse(e);
            }
        };

        let col = OFAst::col(
            refs_parser::try_bool_from_abs_flag(col.0),
            refs_parser::try_u32_from_colname(col.1).locate_err(rest)?,
            unsafe { span_union_opt(col.0, col.1) },
        );

        trace.ok(col.span, rest, col)
    }
}

pub struct RowTerm;

impl<'s> GeneralTerm<'s, OFRow<'s>> for RowTerm {
    fn name() -> &'static str {
        "row"
    }

    fn lah(_i: Span<'s>) -> bool {
        todo!()
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, OFRow<'s>> {
        trace.enter(Self::name(), rest);

        let (rest, row) = match tokens::row(rest) {
            Ok((rest, row)) => (rest, row),
            Err(e) if e.code == OFDollar => {
                return trace.err_track_map_parse(e, OFCode::OFRow);
            }
            Err(e) if e.code == OFDigit => {
                return trace.err_track_map_parse(e, OFCode::OFRow);
            }
            Err(e) => {
                trace.panic_parse(e);
            }
        };

        let row = OFAst::row(
            refs_parser::try_bool_from_abs_flag(row.0),
            refs_parser::try_u32_from_rowname(row.1).locate_err(rest)?,
            unsafe { span_union_opt(row.0, row.1) },
        );

        trace.ok(row.span(), rest, row)
    }
}

/// Parses a simple cell reference.
pub struct CellRefExpr;

impl CellRefExpr {
    /// Parses the full string as CellRef.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, OFCellRef<'s>> {
        trace.enter(Self::name(), i);

        match CellRefExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::parse_incomplete)?;
                let OFAst::NodeCellRef(cell_ref) = *expr else {
                    panic!("Expected a CellRef");
                };
                trace.ok(cell_ref.span, rest, cell_ref)
            }
            Err(e) => trace.err_parse(e),
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.optional(IriTerm::name());
        let (rest, iri) = IriTerm::parse(trace, rest).trace(trace, OFCellRef)?;
        trace.optional(SheetNameTerm::name());
        let (rest, sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFCellRef)?;
        let (rest, dot) = DotTerm::parse(trace, rest).trace(trace, OFCellRef)?;
        let (rest, col) = ColTerm::parse(trace, rest).trace(trace, OFCellRef)?;
        let (rest, row) = RowTerm::parse(trace, rest).trace(trace, OFCellRef)?;

        let tok = if let Some(iri) = &iri {
            unsafe { span_union(iri.span(), row.span()) }
        } else if let Some(sheet_name) = &sheet {
            unsafe { span_union(sheet_name.span(), row.span()) }
        } else {
            unsafe { span_union(dot, row.span()) }
        };

        let ast = OFAst::cell_ref(iri, sheet, row, col, tok);
        trace.ok(tok, rest, ast)
    }
}

/// Parses a cell range.
pub struct CellRangeExpr;

impl CellRangeExpr {
    /// Parses the full string as CellRange.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, OFCellRange<'s>> {
        trace.enter(Self::name(), rest);

        match CellRangeExpr::parse(trace, rest) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::cell_range)?;
                let OFAst::NodeCellRange(cell_range) = *expr else {
                    panic!("Expected a CellRange");
                };
                trace.ok(cell_range.span, rest, cell_range)
            }
            Err(e) => trace.err_parse(e),
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.optional(IriTerm::name());
        let (rest, iri) = IriTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        trace.optional(SheetNameTerm::name());
        let (rest, sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, dot) = DotTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, col) = ColTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, row) = RowTerm::parse(trace, rest).trace(trace, OFCellRange)?;

        let (rest, _colon) = ColonTerm::parse(trace, rest).trace(trace, OFCellRange)?;

        trace.optional(SheetNameTerm::name());
        let (rest, to_sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, _to_dot) = DotTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, to_col) = ColTerm::parse(trace, rest).trace(trace, OFCellRange)?;
        let (rest, to_row) = RowTerm::parse(trace, rest).trace(trace, OFCellRange)?;

        let tok = if let Some(iri) = &iri {
            unsafe { span_union(iri.span(), to_row.span()) }
        } else if let Some(sheet_name) = &sheet {
            unsafe { span_union(sheet_name.span(), to_row.span()) }
        } else {
            unsafe { span_union(dot, to_row.span()) }
        };

        let ast = OFAst::cell_range(iri, sheet, row, col, to_sheet, to_row, to_col, tok);
        trace.ok(tok, rest, ast)
    }
}

/// Parses a column range.
pub struct ColRangeExpr;

impl ColRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, OFColRange<'s>> {
        trace.enter(Self::name(), rest);

        match Self::parse(trace, rest) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::col_range)?;
                let OFAst::NodeColRange(col_range) = *expr else {
                    panic!("Expected a ColRange");
                };
                trace.ok(col_range.span(), rest, col_range)
            }
            Err(e) => trace.err_parse(e),
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.optional(IriTerm::name());
        let (rest, iri) = IriTerm::parse(trace, rest).trace(trace, OFColRange)?;
        trace.optional(SheetNameTerm::name());
        let (rest, sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFColRange)?;
        let (rest, dot) = DotTerm::parse(trace, rest).trace(trace, OFColRange)?;
        let (rest, col) = ColTerm::parse(trace, rest).trace(trace, OFColRange)?;

        let (rest, _colon) = ColonTerm::parse(trace, rest).trace(trace, OFColRange)?;

        trace.optional(SheetNameTerm::name());
        let (rest, to_sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFColRange)?;
        let (rest, _to_dot) = DotTerm::parse(trace, rest).trace(trace, OFColRange)?;
        let (rest, to_col) = ColTerm::parse(trace, rest).trace(trace, OFColRange)?;

        let tok = if let Some(iri) = &iri {
            unsafe { span_union(iri.span(), to_col.span()) }
        } else if let Some(sheet_name) = &sheet {
            unsafe { span_union(sheet_name.span(), to_col.span()) }
        } else {
            unsafe { span_union(dot, to_col.span()) }
        };

        let ast = OFAst::col_range(iri, sheet, col, to_sheet, to_col, tok);
        trace.ok(tok, rest, ast)
    }
}

/// Parses a row range.
pub struct RowRangeExpr;

impl RowRangeExpr {
    /// Parses the full string as ColRange.
    pub fn parse_full<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, OFRowRange<'s>> {
        trace.enter(Self::name(), i);

        match Self::parse(trace, i) {
            Ok((rest, expr)) => {
                check_eof(rest, ParseOFError::row_range)?;
                let OFAst::NodeRowRange(row_range) = *expr else {
                    panic!("Expected a RowRange");
                };
                trace.ok(row_range.span(), rest, row_range)
            }
            Err(e) => trace.err_parse(e),
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        // TODO: Check suggestions. Maybe add a span to the suggestion?
        trace.optional(IriTerm::name());
        let (rest, iri) = IriTerm::parse(trace, rest).trace(trace, OFRowRange)?;
        trace.optional(SheetNameTerm::name());
        let (rest, sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFRowRange)?;
        let (rest, dot) = DotTerm::parse(trace, rest).trace(trace, OFRowRange)?;
        let (rest, row) = RowTerm::parse(trace, rest).trace(trace, OFRowRange)?;

        let (rest, _colon) = ColonTerm::parse(trace, rest).trace(trace, OFRowRange)?;

        trace.optional(SheetNameTerm::name());
        let (rest, to_sheet) = SheetNameTerm::parse(trace, rest).trace(trace, OFRowRange)?;
        let (rest, _to_dot) = DotTerm::parse(trace, rest).trace(trace, OFRowRange)?;
        let (rest, to_row) = RowTerm::parse(trace, rest).trace(trace, OFRowRange)?;

        let tok = if let Some(iri) = &iri {
            unsafe { span_union(iri.span(), to_row.span()) }
        } else if let Some(sheet_name) = &sheet {
            unsafe { span_union(sheet_name.span(), to_row.span()) }
        } else {
            unsafe { span_union(dot, to_row.span()) }
        };

        let ast = OFAst::row_range(iri, sheet, row, to_sheet, to_row, tok);
        trace.ok(tok, rest, ast)
    }
}

pub struct ParenthesesExpr;

impl<'s> GeneralExpr<'s> for ParenthesesExpr {
    fn name() -> &'static str {
        "parentheses"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_parentheses_open(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        let (rest, par1) = match tokens::parentheses_open(rest) {
            Ok((rest1, par1)) => (rest1, par1),
            Err(e) if e.code == OFParenthesesOpen => {
                return trace.err_track_map_parse(e, OFCode::OFParentheses);
            }
            Err(e) => trace.panic_parse(e),
        };

        let (rest, expr) =
            Expr::parse(trace, eat_space(rest)).trace(trace, OFCode::OFParentheses)?;

        let (rest, par2) = match tokens::parentheses_close(eat_space(rest)) {
            Ok((rest1, par2)) => (rest1, par2),
            Err(e) if e.code == OFParenthesesClose => {
                return trace.err_track_map_parse(e, OFCode::OFParentheses);
            }
            Err(e) => trace.panic_parse(e),
        };

        let ast = OFAst::parens(par1, expr, par2);
        trace.ok(ast.span(), rest, ast)
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
pub struct FnCallExpr;

impl<'s> GeneralExpr<'s> for FnCallExpr {
    fn name() -> &'static str {
        "fn_call"
    }

    fn lah(i: Span<'s>) -> bool {
        tokens::lah_fn_name(i)
    }

    #[allow(clippy::collapsible_else_if)]
    #[allow(clippy::needless_return)]
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        let (rest, fn_name) = match tokens::fn_name(eat_space(rest)) {
            Ok((rest1, tok)) => (rest1, tok),
            Err(e) if e.code == OFFnName => {
                return trace.err_track_map_parse(e, OFCode::OFFnCall);
            }
            Err(e) => trace.panic_parse(e),
        };
        trace.step("name", fn_name);

        let mut args = Vec::new();

        match tokens::parentheses_open(eat_space(rest)) {
            Ok((mut loop_rest, par1)) => {
                // Should be a function call now.

                // First separator is checked before the arguments as arguments can be empty.
                match tokens::semikolon(eat_space(loop_rest)) {
                    Ok((rest2, sep1)) => {
                        loop_rest = rest2;
                        trace.step("initial arg", Span::new(""));
                        trace.step("initial separator", sep1);

                        let ast = OFAst::empty(empty(sep1));
                        args.push(*ast);
                    }
                    Err(e) if e.code == OFSemikolon => {
                        // Optional
                        trace.suggest(OFSemikolon);
                    }
                    Err(e) => {
                        trace.panic_parse(e);
                    }
                }

                // Loops and eats from loop_rest.
                loop {
                    // Parse argument.
                    let expr = match Expr::parse(trace, eat_space(loop_rest)) {
                        Ok((rest2, expr)) => {
                            loop_rest = rest2;
                            trace.step("arg", expr.span());
                            Some(expr)
                        }
                        Err(e) if e.code == OFElementary => {
                            // Optional
                            trace.step("arg", Span::new(""));
                            None
                        }
                        Err(e) => return trace.err_parse(e),
                    };
                    trace.clear_suggest();

                    // Followed by a separator.
                    trace.suggest(OFSeparator);
                    let sep1 = match tokens::semikolon(eat_space(loop_rest)) {
                        Ok((rest2, sep1)) => {
                            loop_rest = rest2;
                            Some(sep1)
                        }
                        Err(e) if e.code == OFSemikolon => {
                            // Optional
                            trace.suggest(OFSemikolon);
                            None
                        }
                        Err(e) => {
                            trace.panic_parse(e);
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
                            // argument and separator.
                            trace.step("expr+sep", empty(sep1));
                            args.push(*expr);
                            Parens::Optional
                        } else {
                            // argument but no separator.
                            trace.step("expr+none", empty(expr.span()));
                            args.push(*expr);
                            Parens::Needed
                        }
                    } else {
                        if let Some(sep1) = sep1 {
                            // no argument but a separator.
                            trace.step("none+sep", empty(sep1));
                            let ast = OFAst::empty(empty(sep1));
                            args.push(*ast);
                            Parens::Optional
                        } else {
                            // no argument and no separator. empty argument lists are ok.
                            trace.step("None+None", empty(loop_rest));
                            Parens::Needed
                        }
                    };

                    // find a closing paren next
                    match tokens::parentheses_close(eat_space(loop_rest)) {
                        Ok((rest2, par2)) => {
                            let fn_name = OFAst::fn_name(fn_name.to_string(), fn_name);
                            let ast = OFAst::fn_call(fn_name, par1, args, par2);
                            return trace.ok(ast.span(), rest2, ast);
                        }
                        Err(e) if e.code == OFParenthesesClose => {
                            // Fail if closing parentheses are required. Fine otherwise.
                            if parens == Parens::Needed {
                                return trace.err_track_map_parse(e, OFCode::OFFnCall);
                            }
                        }
                        Err(e) => {
                            trace.panic_parse(e);
                        }
                    }
                }
            }

            Err(e) if e.code == OFParenthesesOpen => {
                return trace.err_track_map_parse(e, OFCode::OFFnCall);
            }

            Err(e) => {
                trace.panic_parse(e);
            }
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

pub struct IriTerm;

impl<'s> GeneralTerm<'s, Option<OFIri<'s>>> for IriTerm {
    fn name() -> &'static str {
        "iri"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Option<OFIri<'s>>> {
        trace.enter(Self::name(), rest);

        // (IRI '#')?
        match tokens::iri(rest) {
            Ok((rest1, iri)) => {
                let term = OFAst::iri(conv::unquote_single(iri), iri);
                trace.ok(iri, rest1, Some(term))
            }

            // Fail to start any of these
            Err(e) if e.code == OFSingleQuoteStart || e.code == OFHashtag => {
                trace.suggest(OFIri);
                trace.ok(empty(rest), rest, None)
            }
            Err(e) if e.code == OFString => trace.err_track_map_parse(e, OFCode::OFIri),
            Err(e) if e.code == OFSingleQuoteEnd => trace.err_track_map_parse(e, OFCode::OFIri),

            Err(e) => trace.panic_parse(e),
        }
    }
}

pub struct SheetNameTerm;

impl<'s> GeneralTerm<'s, Option<OFSheetName<'s>>> for SheetNameTerm {
    fn name() -> &'static str {
        "sheet_name"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_sheet_name(i)
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, Option<OFSheetName<'s>>> {
        trace.enter(Self::name(), rest);

        // QuotedSheetName ::= '$'? SingleQuoted "."
        let (span, rest, sheet_name) = match tokens::quoted_sheet_name(rest) {
            Ok((rest1, (abs, sheet_name))) => {
                let span = unsafe { span_union_opt(abs, sheet_name) };
                let term = OFAst::sheet_name(
                    refs_parser::try_bool_from_abs_flag(abs),
                    conv::unquote_single(sheet_name),
                    span,
                );

                (span, rest1, Some(term))
            }
            Err(e) if e.code == OFSingleQuoteStart => {
                trace.suggest(OFSheetName);
                (empty(rest), rest, None)
            }

            Err(e) if e.code == OFString => {
                return trace.err_track_map_parse(e, OFCode::OFSheetName);
            }
            Err(e) if e.code == OFSingleQuoteEnd => {
                return trace.err_track_map_parse(e, OFCode::OFSheetName);
            }
            Err(e) => trace.panic_parse(e),
        };

        trace.ok(span, rest, sheet_name)
    }
}

pub struct ColonTerm;

impl<'s> GeneralTerm<'s, ()> for ColonTerm {
    fn name() -> &'static str {
        "colon"
    }

    fn lah(_i: Span<'s>) -> bool {
        // TODO: refs_tokens::lah_colon(i)
        false
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, ()> {
        trace.enter(Self::name(), rest);

        // required dot
        let (rest, dot) = match tokens::colon(rest) {
            Ok((rest1, dot)) => (rest1, dot),
            Err(e) if e.code == OFColon => {
                return trace.err_track_map_parse(e, OFCode::OFColon);
            }
            Err(e) => trace.panic_parse(e),
        };

        trace.ok(dot, rest, ())
    }
}

pub struct DotTerm;

impl<'s> GeneralTerm<'s, Span<'s>> for DotTerm {
    fn name() -> &'static str {
        "dot"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_dot(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Span<'s>> {
        trace.enter(Self::name(), rest);

        // required dot
        let (rest, dot) = match tokens::dot(rest) {
            Ok((rest1, dot)) => (rest1, dot),

            Err(e) if e.code == OFDot => {
                return trace.err_track_map_parse(e, OFCode::OFDot);
            }

            Err(e) => trace.panic_parse(e),
        };

        trace.ok(dot, rest, dot)
    }
}

pub struct NamedExpr;

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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>> {
        // NamedExpression := IRI?
        //                      (QuotedSheetName '.')?
        //                      (Identifier | '$$' (Identifier | SingleQuoted)
        //
        trace.enter(Self::name(), rest);

        trace.optional(IriTerm::name());
        let (rest, iri) = IriTerm::parse(trace, rest).trace(trace, OFNamed)?;
        trace.optional(SheetNameTerm::name());
        let (rest, sheet_name) = SheetNameTerm::parse(trace, rest).trace(trace, OFNamed)?;
        let (rest, _dot) = DotTerm::parse(trace, rest).trace(trace, OFNamed)?;

        // (Identifier | '$$' (Identifier | SingleQuoted)

        // Identifier
        let (rest, named) = match tokens::identifier(rest) {
            Ok((rest1, ident)) => {
                let term = OFAst::simple_named(ident.to_string(), ident);
                (rest1, Some(term))
            }
            Err(e) if e.code == OFIdentifier => {
                trace.suggest(OFIdentifier);
                (rest, None)
            }
            Err(e) => trace.panic_parse(e),
        };

        // If we found a name we're good.
        let (rest, named) = if let Some(named) = named {
            (rest, named)
        } else {
            // '$$' (Identifier | SingleQuoted)

            // '$$'
            let (rest, _) = match tokens::dollar_dollar(rest) {
                Ok((rest1, tag)) => (rest1, tag),
                Err(e) if e.code == OFDollarDollar => {
                    return trace.err_track_map_parse(e, OFCode::OFDollarDollar);
                }
                Err(e) => trace.panic_parse(e),
            };

            // Identifier
            let (rest, named) = match tokens::identifier(rest) {
                Ok((rest1, ident)) => {
                    let term = OFAst::simple_named(ident.to_string(), ident);
                    (rest1, Some(term))
                }
                Err(e) if e.code == OFIdentifier => {
                    trace.suggest(OFIdentifier);

                    (rest, None)
                }
                Err(e) => trace.panic_parse(e),
            };

            // SingleQuoted
            let (rest, named) = if let Some(named) = named {
                (rest, named)
            } else {
                match tokens::single_quoted(rest) {
                    Ok((rest1, ident)) => {
                        let named_str = conv::unquote_single(ident);
                        let term = OFAst::simple_named(named_str, ident);
                        (rest1, term)
                    }
                    Err(e) if e.code == OFSingleQuoteStart => {
                        return trace.err_track_map_parse(e, OFCode::OFSingleQuoted);
                    }
                    Err(e) if e.code == OFString => {
                        return trace.err_track_map_parse(e, OFCode::OFSingleQuoted);
                    }
                    Err(e) if e.code == OFSingleQuoteEnd => {
                        return trace.err_track_map_parse(e, OFCode::OFSingleQuoted);
                    }
                    Err(e) => trace.panic_parse(e),
                }
            };

            (rest, named)
        };

        let ast = OFAst::named(iri, sheet_name, named);
        trace.ok(ast.span(), rest, ast)
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
    use crate::ast::parser::NamedExpr;
    use crate::ast::parser::{ElementaryExpr, Expr, ParenthesesExpr};
    use crate::ast::tracer::Tracer;
    use crate::ast::{OFAst, ParseResult, Span};

    fn run_test2<'s>(
        str: &'s str,
        testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, Box<OFAst<'s>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, tok)) => {
                    println!("{:?}", &tracer);
                    println!("=> {:?} | {}", tok, rest);
                }
                Err(e) => {
                    println!("{:?}", &tracer);
                    println!();
                    println!("=> {}", e);
                    println!(
                        "   Found '{}' expected {} suggest {}",
                        e.span(),
                        tracer.expect_str(),
                        tracer.suggest_str()
                    );
                }
            }
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
    fn test_comp() {
        let tests = ["471 >0", "1==1", "(1<>1)"];
        for test in tests {
            run_test2(test, Expr::parse);
        }
    }

    #[test]
    fn test_parentheses() {
        let tests = ["(21)", "21", "(21"];
        for test in tests {
            run_test2(test, ParenthesesExpr::parse);
        }
    }

    #[test]
    fn test_named() {
        let tests = [
            "Pi",
            "$$Tau",
            "'xref'.Rho",
            "'xref'Foo",
            "$$'nice and clean'",
        ];
        for test in tests {
            run_test2(test, NamedExpr::parse);
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
    fn test_expr_fail() {
        let tests = [
            // "471X",
            // r#""strdata"#,
            // "1+",
            // "(1+1",
            // "XX",
            // "4*5+",
            // "4+5*",
            "22 * $FUN()",
            "22 * FUN ( 77  ",
            "22 * FUN ( 77  ",
            "22 * FUN 77 ) ",
            "17 + FUN(  ",
            "17 + FUN  )",
            "11 ^ FUN(   ;;66)",
            "11 ^ FUN(   ;;X)",
        ];
        for test in tests {
            run_test2(test, Expr::parse);
        }
    }
}
