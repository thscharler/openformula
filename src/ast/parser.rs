//!
//! Parses and creates the AST.
//!
//! The parser and look-ahead functions expect to be called with a clean string with no
//! leading whitespace. Internal calls are cared for, but the return value is not necessarily
//! clean itself.
//!
//! The look-ahead functions are called internally at certain branching points.

use crate::ast::nomtokens::eat_space;
use crate::ast::tokens::{col, empty, row, TokenError};
use crate::ast::tracer::Suggest;
use crate::ast::{
    conv, span_union, span_union_opt, tokens, tracer::Tracer, Node, OFAddOp, OFAst, OFCellRange,
    OFCellRef, OFCol, OFColRange, OFCompOp, OFIri, OFMulOp, OFPostfixOp, OFPowOp, OFPrefixOp,
    OFRow, OFRowRange, OFSheetName, ParseResult, Span,
};
use crate::error::{LocateError, ParseOFError};
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

///
pub trait GeneralTerm<'s, O> {
    /// Get a name for debug.
    fn name() -> &'static str;

    /// Run a look-ahead.
    fn lah(i: Span<'s>) -> bool;

    /// Parses the expression.
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, O>;
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

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);
        match CompareExpr::parse(trace, i) {
            Ok((rest, expr)) => Ok(trace.ok(expr.span(), rest, expr)),
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct CompareExpr;

impl<'s> CompareExpr {
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
            Err(e @ TokenError::TokComparisonOp(_)) => Err(trace.map_tok(ParseOFError::comp_op, e)),
            Err(e) => Err(trace.tok(e)),
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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match AddExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    trace.suggest(Suggest::CompOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggestions();
                            //
                            match AddExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::compare(expr1, op, expr2);
                                }
                                Err(e) => break Err(trace.parse(e)),
                            }
                        }
                        Err(ParseOFError::ErrComparisonOp(_)) => {
                            break Ok(trace.ok(expr1.span(), loop_rest, expr1))
                        }
                        Err(e) => break Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct AddExpr;

impl<'s> AddExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, OFAddOp<'s>> {
        trace.enter("add-operator", i);
        trace.optional("add-operator");
        match tokens::add_op(i) {
            Ok((rest, tok)) => match *tok {
                "+" => Ok(trace.ok(tok, rest, OFAddOp::Add(tok))),
                "-" => Ok(trace.ok(tok, rest, OFAddOp::Subtract(tok))),
                _ => unreachable!(),
            },
            Err(e @ TokenError::TokAddOp(_)) => Err(trace.map_tok(ParseOFError::add_op, e)),
            Err(e) => Err(trace.tok(e)),
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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match MulExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    trace.suggest(Suggest::AddOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggestions();
                            //
                            match MulExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::add(expr1, op, expr2);
                                }
                                Err(e) => break Err(trace.parse(e)),
                            }
                        }
                        Err(ParseOFError::ErrAddOp(_)) => {
                            break Ok(trace.ok(expr1.span(), loop_rest, expr1))
                        }
                        Err(e) => break Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct MulExpr;

impl<'s> MulExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, OFMulOp<'s>> {
        trace.enter("mul-operator", i);
        trace.optional("mul-operator");
        match tokens::mul_op(i) {
            Ok((rest, tok)) => match *tok {
                "*" => Ok(trace.ok(tok, rest, OFMulOp::Multiply(tok))),
                "/" => Ok(trace.ok(tok, rest, OFMulOp::Divide(tok))),
                _ => unreachable!(),
            },
            Err(e @ TokenError::TokMulOp(_)) => Err(trace.map_tok(ParseOFError::mul_op, e)),
            Err(e) => Err(trace.tok(e)),
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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match PowExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    trace.suggest(Suggest::MulOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggestions();
                            //
                            match PowExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::mul(expr1, op, expr2);
                                }
                                Err(e) => break Err(trace.parse(e)),
                            }
                        }
                        Err(ParseOFError::ErrMulOp(_)) => {
                            break Ok(trace.ok(expr1.span(), loop_rest, expr1))
                        }
                        Err(e) => break Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct PowExpr;

impl<'s> PowExpr {
    fn operator<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, OFPowOp<'s>> {
        trace.enter("pow-operator", i);
        trace.optional("pow-operator");
        match tokens::pow_op(i) {
            Ok((rest, tok)) => match *tok {
                "^" => Ok(trace.ok(tok, rest, OFPowOp::Power(tok))),
                _ => unreachable!(),
            },
            Err(e @ TokenError::TokPowOp(_)) => Err(trace.map_tok(ParseOFError::pow_op, e)),
            Err(e) => Err(trace.tok(e)),
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
    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        match PostfixExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr1)) => {
                //
                loop {
                    trace.suggest(Suggest::PowOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest2, op)) => {
                            trace.clear_suggestions();
                            //
                            match PostfixExpr::parse(trace, eat_space(rest2)) {
                                Ok((rest3, expr2)) => {
                                    loop_rest = rest3;
                                    expr1 = OFAst::pow(expr1, op, expr2);
                                }
                                Err(e) => break Err(trace.parse(e)),
                            }
                        }
                        Err(ParseOFError::ErrPowOp(_)) => {
                            break Ok(trace.ok(expr1.span(), loop_rest, expr1))
                        }
                        Err(e) => break Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => Err(trace.parse(e)),
        }
    }
}

struct PostfixExpr;

impl PostfixExpr {
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
            Err(e @ TokenError::TokPostfixOp(_)) => Err(trace.map_tok(ParseOFError::postfix_op, e)),
            Err(e) => Err(trace.tok(e)),
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

    fn parse<'t>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), i);

        let (rest, ast) = match PrefixExpr::parse(trace, i) {
            Ok((mut loop_rest, mut expr)) => {
                //
                loop {
                    trace.suggest(Suggest::PostfixOp);
                    match Self::operator(trace, eat_space(loop_rest)) {
                        Ok((rest1, tok)) => {
                            loop_rest = rest1;
                            expr = OFAst::postfix(expr, tok);
                        }
                        Err(ParseOFError::ErrPostfixOp(_)) => break (loop_rest, expr),
                        Err(e) => return Err(trace.parse(e)),
                    }
                }
            }
            Err(e) => return Err(trace.parse(e)),
        };

        Ok(trace.ok(ast.span(), rest, ast))
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
            Err(e @ TokenError::TokPrefixOp(_)) => {
                trace.expect(Suggest::PrefixOp);
                Err(trace.map_tok(ParseOFError::prefix_op, e))
            }
            Err(e) => Err(trace.tok(e)),
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

        trace.suggest(Suggest::PrefixOp);
        let mut op_vec = Vec::new();
        let mut loop_rest = rest;
        // eat all prefix ops and keep them save.
        let rest = loop {
            match Self::operator(trace, eat_space(loop_rest)) {
                Ok((rest1, op)) => {
                    loop_rest = rest1;
                    op_vec.push(op);
                }
                Err(ParseOFError::ErrPrefixOp(_)) => {
                    break loop_rest;
                }
                Err(e) => return Err(trace.parse(e)),
            }
        };

        // parse the expression itself
        let (rest, expr) = match ElementaryExpr::parse(trace, eat_space(rest)) {
            Ok((rest1, expr)) => (rest1, expr),
            Err(e) => return Err(trace.parse(e)),
        };

        // join everything up
        let mut ast = expr;
        while let Some(op) = op_vec.pop() {
            ast = OFAst::prefix(op, ast);
        }

        Ok(trace.ok(ast.span(), rest, ast))
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

        trace.suggest(Suggest::Number);
        if NumberExpr::lah(i) {
            trace.optional(NumberExpr::name());
            match NumberExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_)) => {
                    /* skip */
                    trace.expect(Suggest::Number);
                }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        trace.suggest(Suggest::String);
        if StringExpr::lah(i) {
            trace.optional(StringExpr::name());
            match StringExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_)) => {
                    /* skip */
                    trace.expect(Suggest::String);
                }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        trace.suggest(Suggest::Parentheses);
        if ParenthesisExpr::lah(i) {
            trace.optional(ParenthesisExpr::name());
            match ParenthesisExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_)) => { /* skip */ }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        trace.suggest(Suggest::Reference);
        if ReferenceExpr::lah(i) {
            trace.optional(ReferenceExpr::name());
            match ReferenceExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_)) => {
                    /* skip, some token error */
                    trace.expect(Suggest::Reference);
                }
                Err(ParseOFError::ErrReference(_)) => {
                    /* skip, no reference */
                    trace.expect(Suggest::Reference);
                }
                Err(e) => return Err(trace.parse(e)),
            }
        }

        trace.suggest(Suggest::FnCall);
        if FnCallExpr::lah(i) {
            trace.optional(FnCallExpr::name());
            match FnCallExpr::parse(trace, i) {
                Ok((rest, expr)) => {
                    return Ok(trace.ok(expr.span(), rest, expr));
                }
                Err(ParseOFError::ErrNomError(_)) => {
                    /* skip */
                    trace.expect(Suggest::FnCall);
                }
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
            Err(e) => {
                trace.expect(Suggest::Number);
                Err(trace.map_tok(ParseOFError::number, e))
            }
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
            Err(e @ TokenError::TokStartQuote(_)) => {
                trace.expect(Suggest::StartQuote);
                Err(trace.map_tok(ParseOFError::string, e))
            }
            Err(e @ TokenError::TokEndQuote(_)) => {
                trace.expect(Suggest::EndQuote);
                Err(trace.map_tok(ParseOFError::string, e))
            }
            Err(e @ TokenError::TokString(_)) => {
                trace.expect(Suggest::String);
                Err(trace.map_tok(ParseOFError::string, e))
            }
            Err(e) => {
                trace.expect(Suggest::String);
                Err(trace.map_tok(ParseOFError::string, e))
            }
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
            Err(ParseOFError::ErrNomError(_)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }

        trace.optional(CellRefExpr::name());
        match CellRefExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }

        trace.optional(ColRangeExpr::name());
        match ColRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }

        trace.optional(RowRangeExpr::name());
        match RowRangeExpr::parse(trace, i) {
            Ok((rest, expr)) => {
                return Ok(trace.ok(expr.span(), rest, expr));
            }
            Err(ParseOFError::ErrNomError(_)) => { /* skip */ }
            Err(e) => return Err(trace.parse(e)),
        }

        trace.expect(Suggest::Reference);
        Err(trace.parse(ParseOFError::reference(i)))
    }
}

struct ColRowTerm;

impl<'s> GeneralTerm<'s, (OFCol<'s>, OFRow<'s>)> for ColRowTerm {
    fn name() -> &'static str {
        "ColRow"
    }

    fn lah(_i: Span<'s>) -> bool {
        todo!()
    }

    fn parse<'t>(
        trace: &'t Tracer<'s>,
        rest: Span<'s>,
    ) -> ParseResult<'s, 't, (OFCol<'s>, OFRow<'s>)> {
        trace.enter(Self::name(), rest);

        let (rest, col) = match col(rest) {
            Ok((rest, col)) => (rest, col),
            Err(TokenError::TokDollar(s)) => {
                trace.expect(Suggest::Dollar);
                return Err(ParseOFError::dollar(s));
            }
            Err(TokenError::TokAlpha(s)) => {
                trace.expect(Suggest::Alpha);
                return Err(ParseOFError::alpha(s));
            }
            Err(e) => {
                trace.expect(Suggest::Col);
                return Err(ParseOFError::col(*e.span()));
            }
        };

        let (rest, row) = match row(rest) {
            Ok((rest, row)) => (rest, row),
            Err(TokenError::TokDollar(s)) => {
                trace.expect(Suggest::Dollar);
                return Err(ParseOFError::dollar(s));
            }
            Err(TokenError::TokDigit(s)) => {
                trace.expect(Suggest::Digit);
                return Err(ParseOFError::digit(s));
            }
            Err(e) => {
                trace.expect(Suggest::Row);
                return Err(ParseOFError::row(*e.span()));
            }
        };

        let col = OFAst::col(
            refs_parser::try_bool_from_abs_flag(col.0),
            refs_parser::try_u32_from_colname(col.1).locate_err(rest)?,
            unsafe { span_union_opt(col.0, col.1) },
        );
        let row = OFAst::row(
            refs_parser::try_bool_from_abs_flag(row.0),
            refs_parser::try_u32_from_rowname(row.1).locate_err(rest)?,
            unsafe { span_union_opt(row.0, row.1) },
        );

        Ok((rest, (col, row)))
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        let (rest, iri) = IriTerm::parse(trace, rest)?;
        let (rest, sheet) = SheetNameTerm::parse(trace, rest)?;
        let (rest, (col, row)) = ColRowTerm::parse(trace, rest)?;

        let tok = if let Some(iri) = &iri {
            unsafe { span_union(iri.span(), row.span()) }
        } else if let Some(sheet_name) = &sheet {
            unsafe { span_union(sheet_name.span(), row.span()) }
        } else {
            unsafe { span_union(col.span(), row.span()) }
        };

        let ast = OFAst::cell_ref(iri, sheet, row, col, tok);
        Ok(trace.ok(tok, rest, ast))
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

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.suggest(Suggest::ParenthesesOpen);
        let (rest, par1) = match tokens::parentheses_open(rest) {
            Ok((rest1, par1)) => (rest1, par1),
            Err(e) => {
                trace.expect(Suggest::ParenthesesOpen);
                return Err(trace.tok(e));
            }
        };
        trace.clear_suggestions();

        trace.suggest(Suggest::Expr);
        let (rest, expr) = match Expr::parse(trace, eat_space(rest)) {
            Ok((rest1, expr)) => (rest1, expr),
            Err(e) => return Err(trace.parse(e)),
        };

        let (rest, par2) = match tokens::parentheses_close(eat_space(rest)) {
            Ok((rest1, par2)) => (rest1, par2),
            Err(e @ TokenError::TokParenthesesClose(_)) => {
                trace.expect(Suggest::ParenthesesClose);
                return Err(trace.tok(e));
            }
            Err(e) => return Err(trace.tok(e)),
        };

        let ast = OFAst::parens(par1, expr, par2);
        Ok(trace.ok(ast.span(), rest, ast))
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
    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Box<OFAst<'s>>> {
        trace.enter(Self::name(), rest);

        trace.suggest(Suggest::FnName);
        let (rest, fn_name) = match tokens::fn_name(rest) {
            Ok((rest1, tok)) => (rest1, tok),
            Err(e) => return Err(trace.map_tok(ParseOFError::fn_name, e)),
        };
        trace.step("name", fn_name);

        let mut args = Vec::new();

        trace.suggest(Suggest::ParenthesesOpen);
        match tokens::parentheses_open(eat_space(rest)) {
            Ok((mut loop_rest, par1)) => {
                // Should be a function call now.
                trace.clear_suggestions();

                // First separator is checked before the arguments as arguments can be empty.
                match tokens::semikolon(eat_space(loop_rest)) {
                    Ok((rest2, sep1)) => {
                        loop_rest = rest2;
                        trace.step("initial arg", Span::new(""));
                        trace.step("initial separator", sep1);

                        let ast = OFAst::empty(empty(sep1));
                        args.push(*ast);
                    }
                    Err(TokenError::TokSemikolon(_)) => {
                        // Optional
                    }
                    Err(e) => {
                        return Err(trace.tok(e));
                    }
                }

                // Loops and eats from loop_rest.
                loop {
                    // Parse argument.
                    trace.suggest(Suggest::Expr);
                    let expr = match Expr::parse(trace, eat_space(loop_rest)) {
                        Ok((rest2, expr)) => {
                            loop_rest = rest2;
                            trace.step("arg", expr.span());
                            Some(expr)
                        }
                        Err(ParseOFError::ErrElementary(_)) => {
                            // Optional
                            trace.step("arg", Span::new(""));
                            None
                        }
                        Err(e) => return Err(trace.parse(e)),
                    };

                    // Followed by a separator.
                    trace.suggest(Suggest::Separator);
                    let sep1 = match tokens::semikolon(eat_space(loop_rest)) {
                        Ok((rest2, sep1)) => {
                            loop_rest = rest2;
                            // separator separates
                            trace.step("separator", sep1);
                            trace.clear_suggestions();
                            Some(sep1)
                        }
                        Err(TokenError::TokSemikolon(_)) => {
                            // Optional
                            None
                        }
                        Err(e) => {
                            return Err(trace.tok(e));
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
                            return Ok(trace.ok(ast.span(), rest2, ast));
                        }
                        Err(e @ TokenError::TokParenthesesClose(_)) => {
                            // Fail if closing parentheses are required. Fine otherwise.
                            if parens == Parens::Needed {
                                trace.expect(Suggest::ParenthesesClose);
                                return Err(trace.map_tok(ParseOFError::fn_call, e));
                            }
                        }
                        Err(e) => {
                            return Err(trace.tok(e));
                        }
                    }
                }
            }
            Err(e) => {
                trace.expect(Suggest::ParenthesesOpen);
                return Err(trace.tok(e));
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

struct IriTerm;

impl<'s> GeneralTerm<'s, Option<OFIri<'s>>> for IriTerm {
    fn name() -> &'static str {
        "iri"
    }

    fn lah(i: Span<'s>) -> bool {
        refs_tokens::lah_iri(i)
    }

    fn parse<'t>(trace: &'t Tracer<'s>, rest: Span<'s>) -> ParseResult<'s, 't, Option<OFIri<'s>>> {
        trace.enter(Self::name(), rest);

        // (IRI '#')?
        trace.suggest(Suggest::Iri);
        let (rest, iri) = match tokens::iri(rest) {
            Ok((rest1, iri)) => (rest1, Some(OFAst::iri(conv::unquote_single(iri), iri))),
            Err(TokenError::TokStartSingleQuote(_)) => (rest, None),
            Err(TokenError::Hash(_)) => (rest, None),
            Err(e) => return Err(trace.tok(e)),
        };

        Ok((rest, iri))
    }
}

struct SheetNameTerm;

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
    ) -> ParseResult<'s, 't, Option<OFSheetName<'s>>> {
        trace.enter(Self::name(), rest);

        // QuotedSheetName ::= '$'? SingleQuoted "."
        trace.suggest(Suggest::SheetName);
        let (rest, sheet_name) = match tokens::quoted_sheet_name(rest) {
            Ok((rest1, (abs, sheet_name))) => {
                let span = unsafe { span_union_opt(abs, sheet_name) };
                (
                    rest1,
                    Some(OFAst::sheet_name(
                        refs_parser::try_bool_from_abs_flag(abs),
                        conv::unquote_single(sheet_name),
                        span,
                    )),
                )
            }
            Err(TokenError::TokStartSingleQuote(_)) => (rest, None),
            Err(e) => return Err(trace.tok(e)),
        };

        let rest = if sheet_name.is_some() {
            match tokens::dot(rest) {
                Ok((rest1, _dot)) => rest1,
                Err(e) => {
                    trace.expect(Suggest::Dot);
                    return Err(trace.tok(e));
                }
            }
        } else {
            rest
        };

        Ok((rest, sheet_name))
    }
}

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
        trace.enter(Self::name(), rest);

        // (IRI '#')?
        let (rest, iri) = IriTerm::parse(trace, rest)?;

        // QuotedSheetName ::= '$'? SingleQuoted "."
        let (rest, sheet_name) = SheetNameTerm::parse(trace, rest)?;

        // (Identifier | '$$' (Identifier | SingleQuoted)

        // Identifier
        trace.suggest(Suggest::Identifier);
        let (rest, named) = match tokens::identifier(rest) {
            Ok((rest1, ident)) => {
                // Identifier
                (rest1, Some(OFAst::simple_named(ident.to_string(), ident)))
            }
            Err(TokenError::TokIdentifier(_)) => (rest, None),
            Err(e) => return Err(trace.tok(e)),
        };

        // If we found a name we're good.
        let (rest, named) = if let Some(named) = named {
            (rest, named)
        } else {
            // '$$' (Identifier | SingleQuoted)

            // '$$'
            trace.suggest(Suggest::DollarDollar);
            let (rest, _) = match tokens::dollar_dollar(rest) {
                Ok((rest1, tag)) => (rest1, tag),
                Err(e) => return Err(trace.tok(e)),
            };

            // Identifier
            trace.suggest(Suggest::Identifier);
            let (rest, named) = match tokens::identifier(rest) {
                Ok((rest1, ident)) => {
                    //
                    (rest1, Some(OFAst::simple_named(ident.to_string(), ident)))
                }
                Err(TokenError::TokIdentifier(_)) => (rest, None),
                Err(e) => return Err(trace.tok(e)),
            };

            // SingleQuoted
            let (rest, named) = if let Some(named) = named {
                (rest, named)
            } else {
                trace.suggest(Suggest::SingleQuoted);
                match tokens::single_quoted(rest) {
                    Ok((rest1, ident)) => {
                        //
                        let named_str = conv::unquote_single(ident);
                        (rest1, OFAst::simple_named(named_str, ident))
                    }
                    Err(e @ TokenError::TokStartQuote(_)) => {
                        trace.suggest(Suggest::StartSingleQuote);
                        return Err(trace.tok(e));
                    }
                    Err(e @ TokenError::TokString(_)) => {
                        trace.suggest(Suggest::String);
                        return Err(trace.tok(e));
                    }
                    Err(e @ TokenError::TokEndQuote(_)) => {
                        trace.suggest(Suggest::EndSingleQuote);
                        return Err(trace.tok(e));
                    }
                    Err(e) => return Err(trace.tok(e)),
                }
            };

            (rest, named)
        };

        let ast = OFAst::named(iri, sheet_name, named);
        Ok(trace.ok(ast.span(), rest, ast))
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
    use crate::ast::parser::{ElementaryExpr, Expr, ParenthesisExpr};
    use crate::ast::parser::{GeneralExpr, StringExpr};
    use crate::ast::parser::{NamedExpr, NumberExpr};
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
                        tracer.expectations(),
                        tracer.suggestions()
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
    fn test_string() {
        let tests = ["4", "\"", "\"\"", "\"X"];
        for test in tests {
            run_test2(test, StringExpr::parse);
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
            run_test2(test, ParenthesisExpr::parse);
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
