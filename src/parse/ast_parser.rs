//!
//! Parses and creates the AST.
//!
//! The parser and look-ahead functions expect to be called with a clean string with no
//! leading whitespace. Internal calls are cared for, but the return value is not necessarily
//! clean itself.
//!
//! The look-ahead functions are called internally at certain branching points.

use crate::ast::AstTree;
use crate::error::ParseExprError;
use crate::parse::{ParseResult, Span, Tracer};
use crate::{HaveSpan, OFParClose, OFParOpen};
use nom::combinator::{opt, recognize};
use nom::InputTake;

pub use crate::ast_parser::ops::*;
pub use crate::ast_parser::refs::*;
use crate::tokens::{
    eat_space, fn_name, lah_fn_name, lah_number, lah_parenthesis_open, lah_prefix_op, lah_string,
    parenthesis_close, parenthesis_open, separator,
};

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

/// Optional number.
pub fn opt_number<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("number", i);

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

/// Optional string.
pub fn opt_string<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("string", i);

    match opt(super::tokens::string)(i) {
        Ok((rest, Some(tok))) => {
            let ast = AstTree::string(tok.to_string(), tok);
            Ok(trace.ok(ast.span(), rest, Some(ast)))
        }
        Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),
        Err(e) => Err(trace.err(i, ParseExprError::string, e)),
    }
}

/// Lookahead parens
pub fn lah_expr<'s>(i: Span<'s>) -> bool {
    lah_compare_expr(i)
}

/// Any expression.
pub fn opt_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("expr", i);

    match opt_compare_expr(trace, i) {
        Ok((rest, Some(expr))) => Ok(trace.ok(expr.span(), rest, Some(expr))),
        Ok((rest, None)) => Ok(trace.ok(Span::new(""), rest, None)),
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Lookahead parens
pub fn lah_compare_expr<'s>(i: Span<'s>) -> bool {
    lah_add_expr(i)
}

/// Compare expressions.
pub fn opt_compare_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("compare", i);

    match opt_add_expr(trace, i) {
        Ok((mut loop_rest, Some(mut expr1))) => {
            //
            loop {
                match ops::comp_op_mapped(trace, eat_space(loop_rest)) {
                    Ok((rest2, Some(op))) => {
                        //
                        match opt_add_expr(trace, eat_space(rest2)) {
                            Ok((rest3, Some(expr2))) => {
                                loop_rest = rest3;
                                expr1 = AstTree::compare_expr(expr1, op, expr2);
                            }
                            Ok((rest3, None)) => {
                                break Err(trace.ast_err(rest3, ParseExprError::comp))
                            }
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

/// Lookahead parens
pub fn lah_add_expr<'s>(i: Span<'s>) -> bool {
    lah_mul_expr(i)
}

/// Additive expressions.
pub fn opt_add_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("add", i);

    match opt_mul_expr(trace, i) {
        Ok((mut loop_rest, Some(mut expr1))) => {
            //
            loop {
                match ops::add_op_mapped(trace, eat_space(loop_rest)) {
                    Ok((rest2, Some(op))) => {
                        //
                        match opt_mul_expr(trace, eat_space(rest2)) {
                            Ok((rest3, Some(expr2))) => {
                                loop_rest = rest3;
                                expr1 = AstTree::add_expr(expr1, op, expr2);
                            }
                            Ok((rest3, None)) => {
                                break Err(trace.ast_err(rest3, ParseExprError::add))
                            }
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

/// Lookahead parens
pub fn lah_mul_expr<'s>(i: Span<'s>) -> bool {
    lah_pow_expr(i)
}

/// Multiplicative expressions.
pub fn opt_mul_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("mul", i);

    match opt_pow_expr(trace, i) {
        Ok((mut loop_rest, Some(mut expr1))) => {
            //
            loop {
                match ops::mul_op_mapped(trace, eat_space(loop_rest)) {
                    Ok((rest2, Some(op))) => {
                        //
                        match opt_pow_expr(trace, eat_space(rest2)) {
                            Ok((rest3, Some(expr2))) => {
                                loop_rest = rest3;
                                expr1 = AstTree::mul_expr(expr1, op, expr2);
                            }
                            Ok((rest3, None)) => {
                                break Err(trace.ast_err(rest3, ParseExprError::mul))
                            }
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

/// Lookahead parens
pub fn lah_pow_expr<'s>(i: Span<'s>) -> bool {
    lah_postfix_expr(i)
}

/// Power expressions.
pub fn opt_pow_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("pow", i);

    match opt_postfix_expr(trace, i) {
        Ok((mut loop_rest, Some(mut expr1))) => {
            //
            loop {
                match ops::pow_op_mapped(trace, eat_space(loop_rest)) {
                    Ok((rest2, Some(op))) => {
                        //
                        match opt_postfix_expr(trace, eat_space(rest2)) {
                            Ok((rest3, Some(expr2))) => {
                                loop_rest = rest3;
                                expr1 = AstTree::pow_expr(expr1, op, expr2);
                            }
                            Ok((rest3, None)) => {
                                break Err(trace.ast_err(rest3, ParseExprError::pow))
                            }
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

/// Lookahead parens
pub fn lah_postfix_expr<'s>(i: Span<'s>) -> bool {
    lah_prefix_expr(i)
}

/// Postfix expressions.
pub fn opt_postfix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("postfix", i);

    match opt_prefix_expr(trace, i) {
        Ok((mut loop_rest, Some(mut expr))) => {
            //
            loop {
                match ops::postfix_op_mapped(trace, eat_space(loop_rest)) {
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

/// Lookahead parens
pub fn lah_prefix_expr<'s>(i: Span<'s>) -> bool {
    lah_prefix_op(i) || lah_elementary(i)
}

/// Prefix expressions.
pub fn opt_prefix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("prefix", i);

    match ops::prefix_op_mapped(trace, i) {
        Ok((rest1, Some(tok))) => {
            //
            match opt_prefix_expr(trace, eat_space(rest1)) {
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
            match opt_elementary(trace, eat_space(rest1)) {
                Ok((rest2, Some(expr))) => Ok(trace.ok(expr.span(), rest2, Some(expr))),
                Ok((rest2, None)) => Ok(trace.ok(Span::new(""), rest2, None)),
                Err(e) => Err(trace.parse_err(e)),
            }
        }
        Err(e) => Err(trace.parse_err(e)),
    }
}

/// Lookahead parens
pub fn lah_expr_parenthesis<'s>(i: Span<'s>) -> bool {
    lah_parenthesis_open(i)
}

/// Expression in parenthesis.
pub fn opt_expr_parentheses<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("parentheses", i);

    match super::tokens::parenthesis_open(i) {
        Ok((rest1, par1)) => {
            match opt_expr(trace, eat_space(rest1)) {
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

/// Lookahead elementary
pub fn lah_elementary<'s>(i: Span<'s>) -> bool {
    lah_number(i) || lah_string(i) || lah_parenthesis_open(i) || lah_reference(i) || lah_fn_call(i)
}

/// Leafs of the expression tree.
pub fn opt_elementary<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("elementary", i);

    if lah_number(i) {
        match opt_number(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
    }

    if lah_string(i) {
        match opt_string(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
    }

    if lah_parenthesis_open(i) {
        match opt_expr_parentheses(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
    }

    if lah_reference(i) {
        match refs::opt_reference(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
    }

    if lah_fn_call(i) {
        match opt_fn_call(trace, i) {
            Ok((rest, Some(expr))) => return Ok(trace.ok(expr.span(), rest, Some(expr))),
            Ok((_, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
    }

    Ok(trace.ok(Span::new(""), i, None))
}

/// Lookahead fn_call
pub fn lah_fn_call<'s>(i: Span<'s>) -> bool {
    lah_fn_name(i)
}

// FunctionName '(' ParameterList ')' |
// FunctionName ::= LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
// ParameterList ::= /* empty */ |
//                      Parameter (Separator EmptyOrParameter )* |
//                      Separator EmptyOrParameter /* First param empty */ (Separator EmptyOrParameter )*
// EmptyOrParameter ::= /* empty */ Whitespace* | Parameter
// Parameter ::= Expression
// Separator ::= ';'
#[allow(clippy::collapsible_else_if)]
#[allow(clippy::needless_return)]
/// Parses a function call.
pub fn opt_fn_call<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>> {
    trace.enter("fn_call", i);

    let (rest1, fn_name) = match recognize(fn_name)(i) {
        Ok((rest, tok)) => (eat_space(rest), tok),
        Err(nom::Err::Error(_)) => return Ok(trace.ok(Span::new(""), i, None)),
        Err(e @ nom::Err::Failure(_)) => return Err(trace.err(i, ParseExprError::fn_call, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    };

    trace.step("name", fn_name);

    let mut args = Vec::new();

    match parenthesis_open(eat_space(rest1)) {
        Ok((mut loop_rest, par1)) => {
            // First separator is checked before the arguments as arguments can be empty.
            let sep1 = match separator(eat_space(loop_rest)) {
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
                let expr = if lah_expr(loop_rest) {
                    match opt_expr(trace, loop_rest) {
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
                let sep1 = match separator(eat_space(loop_rest)) {
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
                match parenthesis_close(eat_space(loop_rest)) {
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

mod ops {
    use crate::tracer::Tracer;
    use crate::{
        tokens, OFAddOp, OFCompOp, OFMulOp, OFPostfixOp, OFPowOp, OFPrefixOp, ParseExprError,
        ParseResult, Span,
    };

    /// Parses and maps the Span to a OFPrefixOp.
    pub fn prefix_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFPrefixOp<'s>>> {
        trace.enter("prefix_op_mapped", i);
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

    /// Parses and maps the Span to an OFCompOp
    pub fn comp_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFCompOp<'s>>> {
        trace.enter("comp_op_mapped", i);
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

    /// Parses and maps the Span to an OFAddOP
    pub fn add_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFAddOp<'s>>> {
        trace.enter("add_op_mapped", i);
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

    /// Parses and maps the Span to an OFMulOp
    pub fn mul_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFMulOp<'s>>> {
        trace.enter("mul_op_mapped", i);
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

    /// Parses and maps the Span to an OFPowOp
    pub fn pow_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFPowOp<'s>>> {
        trace.enter("pow_op_mapped", i);
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

    /// Parses and maps the Span to an OFPostfixOp
    pub fn postfix_op_mapped<'s, 't>(
        trace: &'t Tracer<'s>,
        i: Span<'s>,
    ) -> ParseResult<'s, 't, Option<OFPostfixOp<'s>>> {
        trace.enter("postfix_op_mapped", i);
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

mod refs {
    use crate::ast_parser::check_eof;
    use crate::conv::{try_bool_from_abs_flag, try_u32_from_colname, try_u32_from_rowname};
    use crate::tokens::{lah_dot, lah_iri, lah_sheet_name};
    use crate::tracer::Tracer;
    use crate::{
        tokens, AstTree, CRef, CellRange, CellRef, ColRange, ParseExprError, ParseResult, RowRange,
        Span,
    };
    use nom::combinator::{consumed, opt};
    use nom::sequence::tuple;

    /// Lookahead for any reference.
    pub fn lah_reference<'a>(i: Span<'a>) -> bool {
        lah_cellref(i)
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
                return Ok(trace.ok(span, rest, Some(AstTree::cellrange(tok, span))))
            }
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match opt_cellref(trace, i) {
            Ok((rest, Some((span, tok)))) => {
                return Ok(trace.ok(span, rest, Some(AstTree::cellref(tok, span))))
            }
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match opt_colrange(trace, i) {
            Ok((rest, Some((span, tok)))) => {
                return Ok(trace.ok(span, rest, Some(AstTree::colrange(tok, span))))
            }
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }
        match opt_rowrange(trace, i) {
            Ok((rest, Some((span, tok)))) => {
                return Ok(trace.ok(span, rest, Some(AstTree::rowrange(tok, span))))
            }
            Ok((_rest, None)) => { /* skip */ }
            Err(e) => return Err(trace.parse_err(e)),
        }

        Ok(trace.ok(Span::new(""), i, None))
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
                Ok(trace.ok(span, rest, (span, tok)))
            }
            Ok((rest, None)) => Err(trace.ast_err(rest, ParseExprError::cellref)),
            Err(e) => Err(trace.parse_err(e)),
        }
    }

    ///
    pub fn lah_cellref<'s>(i: Span<'s>) -> bool {
        lah_iri(i) || lah_sheet_name(i) || lah_dot(i)
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
            opt(tokens::sheet_name),
            tokens::dot, // TODO: this is not user-facing but the stored format.
            tokens::col,
            tokens::row,
        )))(i)
        {
            Ok((rest, (tok, (iri, sheet_name, _dot, col, row)))) => {
                //
                Ok(trace.ok(
                    tok,
                    rest,
                    Some((
                        tok,
                        CellRef {
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
                        },
                    )),
                ))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
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
                Ok(trace.ok(span, rest, (span, tok)))
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
                Ok(trace.ok(
                    tok,
                    rest,
                    Some((
                        tok,
                        CellRange {
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
                        },
                    )),
                ))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
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
                Ok(trace.ok(span, rest, (span, tok)))
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
                Ok(trace.ok(
                    tok,
                    rest,
                    Some((
                        tok,
                        ColRange {
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
                        },
                    )),
                ))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
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
                Ok(trace.ok(span, rest, (span, tok)))
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
                Ok(trace.ok(
                    tok,
                    rest,
                    Some((
                        tok,
                        RowRange {
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
                        },
                    )),
                ))
            }

            Err(nom::Err::Error(_)) => Ok(trace.ok(Span::new(""), i, None)),
            Err(e @ nom::Err::Failure(_)) => Err(trace.err(i, ParseExprError::nom_failure, e)),
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
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

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast_parser::{opt_expr, opt_reference};
    use crate::error::OFError;
    use crate::parse::ast_parser::opt_cellref;
    use crate::parse::tracer::Tracer;
    use crate::parse::Span;
    use crate::refs::CellRef;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_expr() -> Result<(), OFError> {
        let test = vec![
            "22 * FUN ( 77 ) ",
            "17 + FUN(  )",
            "11 ^ FUN(   ;;66)",
            "1+2*3^4",
            "27+(19*.A5)",
        ];
        for test in test {
            let trace = Tracer::new();
            match opt_expr(&trace, Span::new(test)) {
                Ok((_s, Some(v))) => {
                    println!("{}", v);
                    println!("{:?}", v);
                }
                Ok((_s, None)) => println!("None"),
                Err(e) => println!("{:?}", e),
            }
            println!("{:?}", trace);
        }

        Ok(())
    }

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
