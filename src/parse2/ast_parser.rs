//!
//! Parses and creates the AST.
//!

use crate::parse::column;
use crate::parse2::ast::{AstTree, OFInfixOp, OFNumber, OFPostfixOp, OFPrefixOp, OFString};
use crate::parse2::conv::{try_bool_from_abs_flag, try_u32_from_colname, try_u32_from_rowname};
use crate::parse2::refs::{CRef, CellRef};
use crate::parse2::tokens::{dot, infix_op, iri, postfix_op, prefix_op, row, sheetname};
use crate::parse2::{ParseExprError, ParseResult, Span, Tracer};
use nom::combinator::{consumed, opt};
use nom::error::ErrorKind;
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

pub fn number<'s, 't>(trace: &'t Tracer<'s>, i: Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("opt_number", i);

    match super::tokens::number(i) {
        Ok((rest, tok)) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(AstTree::Number(OFNumber(val, tok))))),
            Err(e) => Err(trace.dyn_err(ParseExprError::Number, Box::new(e))),
        },
        Err(e) => Err(trace.err(ParseExprError::Number, e)),
    }
}

pub fn opt_number<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("opt_number", i);

    match opt(super::tokens::number)(i) {
        Ok((rest, Some(tok))) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(AstTree::Number(OFNumber(val, tok))))),
            Err(_) => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, Box::new(AstTree::Empty()))),
        Err(e) => Err(trace.err(ParseExprError::Number, e)),
    }
}

pub fn opt_string<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("opt_string", i);

    match opt(super::tokens::string)(i) {
        Ok((rest, Some(tok))) => Ok(trace.ok(
            rest,
            Box::new(AstTree::String(OFString(tok.to_string(), tok))),
        )),
        Ok((rest, None)) => Ok(trace.ok(rest, Box::new(AstTree::Empty()))),
        Err(e) => Err(trace.err(ParseExprError::String, e)),
    }
}

pub fn parse_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_expression", i);

    match parse_infix_expr(&trace, i) {
        Ok((rest, expr)) => Ok(trace.ok(rest, expr)),
        Err(e) => Err(e),
    }
}

pub fn parse_infix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_infix", i);

    match parse_postfix_expr(&trace, i) {
        Ok((mut rest1, mut expr1)) => loop {
            match infix_op_mapped(trace, rest1) {
                Ok((rest2, Some(tok))) => match parse_postfix_expr(trace, rest2) {
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

pub fn parse_postfix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i0: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_postfix", i0);

    match parse_prefix(trace, i0) {
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

pub fn parse_prefix<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_prefix", i);

    match prefix_op_mapped(trace, i) {
        Ok((rest1, Some(tok))) => match parse_prefix(&trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, Box::new(AstTree::PrefixOp(tok, expr)))),
            Err(e) => Err(e),
        },
        Ok((rest1, None)) => match parse_elem(&trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, expr)),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}

pub fn opt_expr_parentheses<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("opt_expr_parentheses", i);

    match super::tokens::parenthesis_open(i) {
        Ok((rest1, _tok)) => {
            let (rest2, expr) = parse_expr(&trace, rest1)?;
            return match super::tokens::parenthesis_close(rest2) {
                Ok((i, _tok)) => Ok(trace.ok(i, Some(Box::new(AstTree::Parenthesis(expr))))),
                Err(e) => Err(trace.err(ParseExprError::Parenthesis, e)),
            };
        }
        Err(nom::Err::Error(_)) => return Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(ParseExprError::Parenthesis, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

// Number |
// '(' Expression ')' |
pub fn parse_elem<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<AstTree<'s>>> {
    trace.enter("parse_elementary", i);

    match opt_number(&trace, i) {
        Ok((rest, expr)) => {
            match &*expr {
                AstTree::Empty() => { /* skip */ }
                AstTree::Number(_) => return Ok(trace.ok(rest, expr)),
                _ => unreachable!(),
            }
        }
        Err(e) => return Err(e),
    }

    match opt_string(&trace, i) {
        Ok((rest, expr)) => {
            match &*expr {
                AstTree::Empty() => { /* skip */ }
                AstTree::String(_) => return Ok(trace.ok(rest, expr)),
                _ => unreachable!(),
            }
        }
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

    match opt_expr_parentheses(&trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, tok));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    Err(trace.err(
        ParseExprError::Elementary,
        nom::Err::Error(nom::error::Error::new(i, ErrorKind::Float)),
    ))
}

pub fn prefix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFPrefixOp<'s>>> {
    trace.enter("prefix_op", i);
    match prefix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "+" => Ok(trace.ok(rest, Some(OFPrefixOp::Plus(tok)))),
            "-" => Ok(trace.ok(rest, Some(OFPrefixOp::Minus(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(nom::Err::Error(_)) => return Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn infix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFInfixOp<'s>>> {
    trace.enter("infix_op", i);
    match infix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "+" => Ok(trace.ok(rest, Some(OFInfixOp::Add(tok)))),
            "-" => Ok(trace.ok(rest, Some(OFInfixOp::Subtract(tok)))),
            "*" => Ok(trace.ok(rest, Some(OFInfixOp::Multiply(tok)))),
            "/" => Ok(trace.ok(rest, Some(OFInfixOp::Divide(tok)))),
            "^" => Ok(trace.ok(rest, Some(OFInfixOp::Power(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(e @ nom::Err::Error(_)) => return Err(trace.err(ParseExprError::NomError, e)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn postfix_op_mapped<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<OFPostfixOp<'s>>> {
    trace.enter("postfix_op", i);
    match postfix_op(i) {
        Ok((rest, Some(tok))) => match *tok {
            "%" => Ok(trace.ok(rest, Some(OFPostfixOp::Percent(tok)))),
            _ => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),

        Err(e @ nom::Err::Error(_)) => return Err(trace.err(ParseExprError::NomError, e)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

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
// CellAddress ::= SheetLocatorOrEmpty '.' Column Row /* Not used
// directly */
pub fn parse_cellref<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<CellRef>> {
    trace.enter("parse_cellref", i);
    match consumed(tuple((opt(iri), opt(sheetname), dot, column, row)))(i) {
        Ok((rest, (_p, (iri, sheetname, _dot, col, row)))) => {
            return Ok(trace.ok(
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

        Err(nom::Err::Error(_)) => return Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(ParseExprError::NomFailure, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}
