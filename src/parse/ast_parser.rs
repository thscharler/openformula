//!
//! Parses and creates the AST.
//!

use crate::ast::{AstTree, OFInfixOp, OFNumber, OFPostfixOp, OFPrefixOp};
use crate::error::ParseExprError;
use crate::parse::conv::{try_bool_from_abs_flag, try_u32_from_colname, try_u32_from_rowname};
use crate::parse::{tokens, ParseResult, Span, Tracer};
use crate::refs::{CRef, CellRange, CellRef, ColRange, RowRange};
use crate::OFCompOp;
use nom::combinator::{consumed, opt};
use nom::sequence::tuple;

// Expression ::=
// Whitespace* (
//      Array |
//      FunctionName '(' ParameterList ')' |
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
// ReferenceOp ::= IntersectionOp | ReferenceConcatenationOp | RangeOp
// IntersectionOp ::= '!'
// ReferenceConcatenationOp ::= '~'
// RangeOp ::= ':'
//
// ops
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

/// Number.
pub fn number<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("number", i);

    match super::tokens::number(i) {
        Ok((rest, tok)) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(AstTree::Number(OFNumber(val, tok))))),
            Err(_) => unreachable!(),
        },
        Err(e) => Err(trace.err(i, ParseExprError::number, e)),
    }
}

/// Optional number.
pub fn opt_number<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("opt_number", i);

    match opt(super::tokens::number)(i) {
        Ok((rest, Some(tok))) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Some(AstTree::number(val, tok)))),
            Err(_) => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),
        Err(e) => Err(trace.err(i, ParseExprError::number, e)),
    }
}

/// Optional string.
pub fn opt_string<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("opt_string", i);

    match opt(super::tokens::string)(i) {
        Ok((rest, Some(tok))) => Ok(trace.ok(rest, Some(AstTree::string(tok.to_string(), tok)))),
        Ok((rest, None)) => Ok(trace.ok(rest, None)),
        Err(e) => Err(trace.err(i, ParseExprError::string, e)),
    }
}

/// Any expression.
pub fn expr<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_expression", i);

    match infix_expr(trace, i) {
        Ok((rest, expr)) => Ok(trace.ok(rest, expr)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Comparisions
pub fn comp_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("comp_expr", i);

    match infix_expr(trace, i) {
        Ok((mut rest1, mut expr1)) => {
            //
            loop {
                match comp_op_mapped(trace, rest1) {
                    Ok((rest2, Some(tok))) => {
                        //
                        match infix_expr(trace, rest2) {
                            Ok((rest3, expr2)) => {
                                rest1 = rest3;
                                expr1 = AstTree::compare_expr(expr1, tok, expr2);
                            }
                            Err(e) => break Err(trace.parse_err(e)),
                        }
                    }
                    Ok((rest, None)) => break Ok(trace.ok(rest, expr1)),
                    Err(e) => break Err(trace.parse_err(e)),
                }
            }
        }
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Infix expressions.
pub fn infix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("infix_expr", i);

    match postfix_expr(trace, i) {
        Ok((mut rest1, mut expr1)) => {
            //
            loop {
                match infix_op_mapped(trace, rest1) {
                    Ok((rest2, Some(tok))) => {
                        //
                        match postfix_expr(trace, rest2) {
                            Ok((rest3, expr2)) => {
                                rest1 = rest3;
                                expr1 = AstTree::infix_expr(expr1, tok, expr2);
                            }
                            Err(e) => break Err(trace.parse_err(e)),
                        }
                    }
                    Ok((rest2, None)) => break Ok(trace.ok(rest2, expr1)),
                    Err(e) => break Err(trace.parse_err(e)),
                }
            }
        }
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Postfix expressions.
pub fn postfix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i0: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_postfix", i0);

    match prefix_expr(trace, i0) {
        Ok((mut rest1, mut expr)) => {
            //
            loop {
                match postfix_op_mapped(trace, rest1) {
                    Ok((rest2, Some(tok))) => {
                        trace.step("op", tok.span());
                        rest1 = rest2;
                        expr = AstTree::postfix_expr(expr, tok);
                    }
                    Ok((i2, None)) => break Ok(trace.ok(i2, expr)),
                    Err(e) => break Err(trace.parse_err(e)),
                }
            }
        }
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Prefix expressions.
pub fn prefix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_prefix", i);

    match prefix_op_mapped(trace, i) {
        Ok((rest1, Some(tok))) => {
            //
            match prefix_expr(trace, rest1) {
                Ok((rest2, expr)) => Ok(trace.ok(rest2, AstTree::prefix_expr(tok, expr))),
                Err(e) => Err(trace.parse_err(e)),
            }
        }
        Ok((rest1, None)) => {
            //
            match elementary(trace, rest1) {
                Ok((rest2, expr)) => Ok(trace.ok(rest2, expr)),
                Err(e) => Err(trace.parse_err(e)),
            }
        }
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Expression in parenthesis.
pub fn opt_expr_parentheses<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("opt_expr_parentheses", i);

    match super::tokens::parenthesis_open(i) {
        Ok((rest1, _tok)) => {
            let (rest2, expr) = expr(trace, rest1)?;
            match super::tokens::parenthesis_close(rest2) {
                Ok((i, _tok)) => Ok(trace.ok(i, Some(Box::new(AstTree::Parenthesis(expr))))),
                Err(e) => Err(trace.err(rest1, ParseExprError::parenthesis, e)),
            }
        }
        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::parenthesis, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Leafs of the expression tree.
pub fn elementary<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_elementary", i);

    match opt_number(trace, i) {
        Ok((rest, Some(expr))) => return Ok(trace.ok(rest, expr)),
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }

    match opt_string(trace, i) {
        Ok((rest, Some(expr))) => return Ok(trace.ok(rest, expr)),
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }

    match opt_reference(trace, i) {
        Ok((rest, Some(expr))) => return Ok(trace.ok(rest, expr)),
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }

    match opt_expr_parentheses(trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, tok));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }

    Err(trace.ast_err(i, ParseExprError::elementary))
}

/// Parses and maps the Span to a OFPrefixOp.
pub fn prefix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFPrefixOp<'s>>> {
    trace.enter("prefix_op", i);
    match tokens::prefix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "+" => Ok(trace.ok(rest, Some(OFPrefixOp::Plus(tok)))),
            "-" => Ok(trace.ok(rest, Some(OFPrefixOp::Minus(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses and maps the Span to an OFCompOp
pub fn comp_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFCompOp<'s>>> {
    trace.enter("comp_op_mapped", i);
    match tokens::comparison_op(i) {
        Ok((rest, tok)) => match *tok {
            "=" => Ok(trace.ok(rest, Some(OFCompOp::Equal(tok)))),
            "<>" => Ok(trace.ok(rest, Some(OFCompOp::Unequal(tok)))),
            "<" => Ok(trace.ok(rest, Some(OFCompOp::Less(tok)))),
            ">" => Ok(trace.ok(rest, Some(OFCompOp::Greater(tok)))),
            "<=" => Ok(trace.ok(rest, Some(OFCompOp::LessEqual(tok)))),
            ">=" => Ok(trace.ok(rest, Some(OFCompOp::GreaterEqual(tok)))),
            _ => unreachable!(),
        },

        Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses and maps the Span to an OFInfixOp
pub fn infix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFInfixOp<'s>>> {
    trace.enter("infix_op", i);
    match tokens::infix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "+" => Ok(trace.ok(rest, Some(OFInfixOp::Add(tok)))),
            "-" => Ok(trace.ok(rest, Some(OFInfixOp::Subtract(tok)))),
            "*" => Ok(trace.ok(rest, Some(OFInfixOp::Multiply(tok)))),
            "/" => Ok(trace.ok(rest, Some(OFInfixOp::Divide(tok)))),
            "^" => Ok(trace.ok(rest, Some(OFInfixOp::Power(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses and maps the Span to an OFPostfixOp
pub fn postfix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFPostfixOp<'s>>> {
    trace.enter("postfix_op", i);
    match tokens::postfix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "%" => Ok(trace.ok(rest, Some(OFPostfixOp::Percent(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(e @ nom::Err::Error(_)) => Err(trace.err(i, ParseExprError::nom_error, e)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

fn check_eof<'a>(
    rest: Span<'a>,
    err: fn(span: Span<'a>) -> ParseExprError,
) -> Result<(), ParseExprError> {
    if (*rest).is_empty() {
        Ok(())
    } else {
        Err(err(rest))
    }
}

/// Tries to parse a cell reference.
#[allow(clippy::manual_map)]
pub fn opt_reference<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("opt_reference", i);

    match opt_cellrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            return Ok(trace.ok(rest, Some(AstTree::cellrange(tok, span))))
        }
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }
    match opt_cellref(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            return Ok(trace.ok(rest, Some(AstTree::cellref(tok, span))))
        }
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }
    match opt_colrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            return Ok(trace.ok(rest, Some(AstTree::colrange(tok, span))))
        }
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }
    match opt_rowrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            return Ok(trace.ok(rest, Some(AstTree::rowrange(tok, span))))
        }
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(trace.parse_err(e)),
    }

    Ok((i, None))
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
/// Parses the full string as CellRef.
pub fn parse_cellref<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, (Span<'s>, CellRef)> {
    trace.enter("parse_cellref", i);

    match opt_cellref(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            check_eof(rest, ParseExprError::cellref)?;
            Ok(trace.ok(rest, (span, tok)))
        }
        Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::cellref)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Tries to parse a cell reference.
#[allow(clippy::manual_map)]
pub fn opt_cellref<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<(Span<'s>, CellRef)>> {
    trace.enter("opt_cellref", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot, // TODO: this is not user-facing but the stored format.
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((rest, (all, (iri, sheetname, _dot, col, row)))) => {
            //
            Ok(trace.ok(
                rest,
                Some((
                    all,
                    CellRef {
                        iri: iri.map(|v| (*v).to_string()),
                        sheet: match sheetname {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        cell: CRef {
                            abs_row: try_bool_from_abs_flag(row.0),
                            row: trace.re_err(try_u32_from_rowname(row.1))?,
                            abs_col: try_bool_from_abs_flag(col.0),
                            col: trace.re_err(try_u32_from_colname(col.1))?,
                        },
                    },
                )),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses the full string as CellRange.
pub fn parse_cellrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, (Span<'s>, CellRange)> {
    trace.enter("parse_cellrange", i);

    match opt_cellrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            check_eof(rest, ParseExprError::cellrange)?;
            Ok(trace.ok(rest, (span, tok)))
        }
        Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::cellrange)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Tries to parse a cell reference.
#[allow(clippy::manual_map)]
pub fn opt_cellrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<(Span<'s>, CellRange)>> {
    trace.enter("opt_cellrange", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot,
        tokens::col,
        tokens::row,
        tokens::colon,
        opt(tokens::sheetname),
        tokens::dot,
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((
            rest,
            (
                tok,
                (iri, sheetname_0, _dot_0, col_0, row_0, _colon, sheetname_1, _dot_1, col_1, row_1),
            ),
        )) => {
            //
            Ok(trace.ok(
                rest,
                Some((
                    tok,
                    CellRange {
                        iri: iri.map(|v| (*v).to_string()),
                        from_sheet: match sheetname_0 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        from: CRef {
                            abs_row: try_bool_from_abs_flag(row_0.0),
                            row: trace.re_err(try_u32_from_rowname(row_0.1))?,
                            abs_col: try_bool_from_abs_flag(col_0.0),
                            col: trace.re_err(try_u32_from_colname(col_0.1))?,
                        },
                        to_sheet: match sheetname_1 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        to: CRef {
                            abs_row: try_bool_from_abs_flag(row_1.0),
                            row: trace.re_err(try_u32_from_rowname(row_1.1))?,
                            abs_col: try_bool_from_abs_flag(col_1.0),
                            col: trace.re_err(try_u32_from_colname(col_1.1))?,
                        },
                    },
                )),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses the full string as ColRange.
pub fn parse_colrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, (Span<'s>, ColRange)> {
    trace.enter("parse_colrange", i);

    match opt_colrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            check_eof(rest, ParseExprError::colrange)?;
            Ok(trace.ok(rest, (span, tok)))
        }
        Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::colrange)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Tries to parse a ColRange reference.
#[allow(clippy::manual_map)]
pub fn opt_colrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<(Span<'s>, ColRange)>> {
    trace.enter("opt_colrange", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot,
        tokens::col,
        tokens::colon,
        opt(tokens::sheetname),
        tokens::dot,
        tokens::col,
    )))(i)
    {
        Ok((
            rest,
            (tok, (iri, sheetname_0, _dot_0, col_0, _colon, sheetname_1, _dot_1, col_1)),
        )) => {
            //
            Ok(trace.ok(
                rest,
                Some((
                    tok,
                    ColRange {
                        iri: iri.map(|v| (*v).to_string()),
                        from_sheet: match sheetname_0 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        abs_from_col: try_bool_from_abs_flag(col_0.0),
                        from_col: trace.re_err(try_u32_from_colname(col_0.1))?,
                        to_sheet: match sheetname_1 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        abs_to_col: try_bool_from_abs_flag(col_1.0),
                        to_col: trace.re_err(try_u32_from_colname(col_1.1))?,
                    },
                )),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses the full string as ColRange.
pub fn parse_rowrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, (Span<'s>, RowRange)> {
    trace.enter("parse_rowrange", i);

    match opt_rowrange(trace, i) {
        Ok((rest, Some((span, tok)))) => {
            check_eof(rest, ParseExprError::rowrange)?;
            Ok(trace.ok(rest, (span, tok)))
        }
        Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::rowrange)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Tries to parse a RowRange reference.
#[allow(clippy::manual_map)]
pub fn opt_rowrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<(Span<'s>, RowRange)>> {
    trace.enter("opt_rowrange", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot,
        tokens::row,
        tokens::colon,
        opt(tokens::sheetname),
        tokens::dot,
        tokens::row,
    )))(i)
    {
        Ok((
            rest,
            (tok, (iri, sheetname_0, _dot_0, row_0, _colon, sheetname_1, _dot_1, row_1)),
        )) => {
            //
            Ok(trace.ok(
                rest,
                Some((
                    tok,
                    RowRange {
                        iri: iri.map(|v| (*v).to_string()),
                        from_sheet: match sheetname_0 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        abs_from_row: try_bool_from_abs_flag(row_0.0),
                        from_row: trace.re_err(try_u32_from_rowname(row_0.1))?,
                        to_sheet: match sheetname_1 {
                            None => None,
                            Some((_, v)) => Some((*v).to_string()),
                        },
                        abs_to_row: try_bool_from_abs_flag(row_1.0),
                        to_row: trace.re_err(try_u32_from_rowname(row_1.1))?,
                    },
                )),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast_parser::opt_reference;
    use crate::error::OFError;
    use crate::parse::ast_parser::opt_cellref;
    use crate::parse::tracer::Tracer;
    use crate::parse::Span;
    use crate::refs::CellRef;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_cellref2() -> Result<(), OFError> {
        let trace = Tracer::new();
        unsafe {
            assert_eq!(
                opt_cellref(&trace, Span::new(".A21"))?,
                (
                    LocatedSpan::new_from_raw_offset(4, 1, "", ()),
                    Some((
                        LocatedSpan::new_from_raw_offset(1, 1, ".A21", ()),
                        CellRef::local(21, 0)
                    ))
                )
            );
        }
        Ok(())
    }

    #[test]
    fn test_cellref() -> Result<(), OFError> {
        let trace = Tracer::new();
        unsafe {
            assert_eq!(
                opt_cellref(&trace, Span::new(".A21"))?,
                (
                    LocatedSpan::new_from_raw_offset(4, 1, "", ()),
                    Some((
                        LocatedSpan::new_from_raw_offset(0, 1, ".A21", ()),
                        CellRef::local(21, 0)
                    ))
                )
            );
            Ok(())
        }
    }

    #[test]
    #[allow(unused_must_use)]
    fn test_cellref3() -> Result<(), OFError> {
        let trace = Tracer::new();
        dbg!(opt_reference(&trace, Span::new(".A1")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(opt_reference(&trace, Span::new(".A0")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(opt_reference(&trace, Span::new(".A1:.C4")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(opt_reference(&trace, Span::new(".A:.C")));
        dbg!(&trace);

        let trace = Tracer::new();
        dbg!(opt_reference(&trace, Span::new(".34:.37")));
        dbg!(&trace);

        Ok(())
    }
}
