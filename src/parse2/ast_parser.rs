//!
//! Parses and creates the AST.
//!

use crate::parse2::ast::{AstTree, OFInfixOp, OFNumber, OFPostfixOp, OFPrefixOp};
use crate::parse2::conv::{try_bool_from_abs_flag, try_u32_from_colname, try_u32_from_rowname};
use crate::parse2::refs::{CRef, CellRange, CellRef};
use crate::parse2::{tokens, ParseExprError, ParseResult, Span, Tracer};
use nom::combinator::{consumed, opt};
use nom::sequence::tuple;

// Expression ::=
// Whitespace* (
// Array |
// FunctionName '(' ParameterList ')' |
// Reference |
// QuotedLabel |
// AutomaticIntersection |
// NamedExpression |
// Error
// ) Whitespace*
// SingleQuoted ::= "'" ([^'] | "''")+ "'"
//
//
// Number |
// String |
// PrefixOp Expression |
// Expression InfixOp Expression |
// Expression PostfixOp |
// '(' Expression ')' |

/// Number.
pub fn number<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("number", i);

    match super::tokens::number(i) {
        Ok((rest, tok)) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(AstTree::Number(OFNumber(val, tok))))),
            Err(_) => unreachable!(),
        },
        Err(e) => Err(trace.err(ParseExprError::Number(i.into()), e)),
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
        Err(e) => Err(trace.err(ParseExprError::Number(i.into()), e)),
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
        Err(e) => Err(trace.err(ParseExprError::String, e)),
    }
}

/// Any expression.
pub fn expr<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_expression", i);

    match infix_expr(trace, i) {
        Ok((rest, expr)) => Ok(trace.ok(rest, expr)),
        Err(e) => Err(e),
    }
}

/// Infix expressions.
pub fn infix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_infix", i);

    match postfix_expr(trace, i) {
        Ok((mut rest1, mut expr1)) => loop {
            match infix_op_mapped(trace, rest1) {
                Ok((rest2, Some(tok))) => match postfix_expr(trace, rest2) {
                    Ok((rest3, expr2)) => {
                        rest1 = rest3;
                        expr1 = Box::new(AstTree::InfixOp(expr1, tok, expr2));
                    }
                    Err(e) => break Err(e),
                },
                Ok((rest2, None)) => {
                    break Ok(trace.ok(rest2, expr1));
                }
                Err(e) => break Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

/// Postfix expressions.
pub fn postfix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i0: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_postfix", i0);

    match prefix_expr(trace, i0) {
        Ok((mut rest1, mut expr)) => loop {
            match postfix_op_mapped(trace, rest1) {
                Ok((rest2, Some(tok))) => {
                    trace.step("op", tok.span());
                    rest1 = rest2;
                    expr = Box::new(AstTree::PostfixOp(expr, tok));
                }
                Ok((i2, None)) => break Ok(trace.ok(i2, expr)),
                Err(e) => break Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

/// Prefix expressions.
pub fn prefix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_prefix", i);

    match prefix_op_mapped(trace, i) {
        Ok((rest1, Some(tok))) => match prefix_expr(trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, Box::new(AstTree::PrefixOp(tok, expr)))),
            Err(e) => Err(e),
        },
        Ok((rest1, None)) => match elementary(trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, expr)),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
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
                Err(e) => Err(trace.err(ParseExprError::Parenthesis, e)),
            }
        }
        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::Parenthesis, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

// Number |
// String |
// '(' Expression ')' |

/// Leafs of the expression tree.
pub fn elementary<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_elementary", i);

    match opt_number(trace, i) {
        Ok((rest, Some(expr))) => return Ok(trace.ok(rest, expr)),
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    match opt_string(trace, i) {
        Ok((rest, Some(expr))) => return Ok(trace.ok(rest, expr)),
        Ok((_rest, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    // match opt_cellref(&trace, i) {
    //     Ok((i, Some(o))) => {
    //         trace.clear_leafs();
    //         return Ok((i, Box::new(ParseTree::CellRef(o))));
    //     }
    //     Ok((_, None)) => { /* skip */ }
    //     Err(e) => return Err(e),
    // }

    match opt_expr_parentheses(trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, tok));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    Err(trace.ast_err(ParseExprError::Elementary))
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
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::NomFailure, e)),
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

        Err(e @ nom::Err::Error(_)) => Err(trace.err(ParseExprError::NomError, e)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::NomFailure, e)),
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

        Err(e @ nom::Err::Error(_)) => Err(trace.err(ParseExprError::NomError, e)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::NomFailure, e)),
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
pub fn parse_cellref<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, CellRef> {
    trace.enter("parse_cellref", i);

    match opt_cellref(trace, i) {
        Ok((rest, Some(tok))) => {
            check_eof(rest, ParseExprError::expr)?;
            Ok((rest, tok))
        }
        Ok((rest, None)) => Err(trace.ast_err(ParseExprError::Expr(rest.into()))),
        Err(e) => Err(e),
    }
}

/// Tries to parse a cell reference.
#[allow(clippy::manual_map)]
pub fn opt_cellref<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<CellRef>> {
    trace.enter("parse_cellref", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot, // TODO: this is not user-facing but the stored format.
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((rest, (_p, (iri, sheetname, _dot, col, row)))) => {
            //
            Ok(trace.ok(
                rest,
                Some(CellRef {
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
                }),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parses the full string as CellRange.
pub fn parse_cellrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, CellRange> {
    trace.enter("parse_cellrange", i);

    match opt_cellrange(trace, i) {
        Ok((rest, Some(tok))) => {
            check_eof(rest, ParseExprError::expr)?;
            Ok((rest, tok))
        }
        Ok((rest, None)) => Err(trace.ast_err(ParseExprError::Expr(rest.into()))),
        Err(e) => Err(e),
    }
}

/// Tries to parse a cell reference.
#[allow(clippy::manual_map)]
pub fn opt_cellrange<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<CellRange>> {
    trace.enter("parse_cellref", i);

    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheetname),
        tokens::dot,
        tokens::col,
        tokens::row,
        tokens::colon,
        tokens::dot,
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((rest, (_p, (iri, sheetname, _dot_0, col_0, row_0, _colon, _dot_1, col_1, row_1)))) => {
            //
            Ok(trace.ok(
                rest,
                Some(CellRange {
                    iri: iri.map(|v| (*v).to_string()),
                    sheet: match sheetname {
                        None => None,
                        Some((_, v)) => Some((*v).to_string()),
                    },
                    from: CRef {
                        abs_row: try_bool_from_abs_flag(row_0.0),
                        row: trace.re_err(try_u32_from_rowname(row_0.1))?,
                        abs_col: try_bool_from_abs_flag(col_0.0),
                        col: trace.re_err(try_u32_from_colname(col_0.1))?,
                    },
                    to_sheet: None,
                    to: CRef {
                        abs_row: try_bool_from_abs_flag(row_1.0),
                        row: trace.re_err(try_u32_from_rowname(row_1.1))?,
                        abs_col: try_bool_from_abs_flag(col_1.0),
                        col: trace.re_err(try_u32_from_colname(col_1.1))?,
                    },
                }),
            ))
        }

        Err(nom::Err::Error(_)) => Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}
