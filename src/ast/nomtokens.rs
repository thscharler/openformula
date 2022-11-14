//!
//! Contains all token parsers. Operates on and returns only spans.
//! These are the parsers that are formulated in nom style.
//! tokens contains the mapping functions to our own errors.
//!

use crate::ast::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{alpha1, multispace0, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::many1;
use nom::sequence::tuple;
use nom::IResult;

/// Eats the leading whitespace.
pub fn eat_space<'a>(i: Span<'a>) -> Span<'a> {
    match multispace0::<Span<'a>, nom::error::Error<_>>(i) {
        Ok((rest, _white)) => rest,
        Err(nom::Err::Error(_)) => i,
        Err(nom::Err::Failure(_)) => i,
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Sequence of digits.
pub fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(one_of("0123456789")))(input)
}

// LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
/// Function name.
pub fn fn_name_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(|c: char| unicode_ident::is_xid_continue(c) || c == '_' || c == '.'),
    )))(i)
}

/// Parse comparison operators.
pub fn comparison_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((
        tag("="),
        tag("<>"),
        tag("<"),
        tag(">"),
        tag("<="),
        tag(">="),
    ))(i)
}

/// Parse string operators.
pub fn string_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

/// Parse reference operators.
pub fn reference_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

/// Parse reference intersection.
pub fn ref_intersection_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("!")(i)
}

/// Parse concat operator..
pub fn ref_concat_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("~")(i)
}

/// Parse separator char for function args.
pub fn dollar_dollar_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("$$")(rest)
}

/// Parse separator char for function args.
pub fn dollar_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("$")(rest)
}

/// Hashtag
pub fn hashtag_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("#")(rest)
}

/// Parse separator char for function args.
pub fn semikolon_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(";")(rest)
}

/// Parse dot
pub fn dot_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

/// Parse colon
pub fn colon_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(":")(i)
}

/// Parse open parentheses.
pub fn parentheses_open_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

/// Parse closing parentheses.
pub fn parentheses_close_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(")")(rest)
}

/// Parse open brackets.
pub fn brackets_open_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("[")(i)
}

/// Parse closing brackets.
pub fn brackets_close_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("]")(i)
}

/// Tries to parses any additive operator.
pub fn add_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to parses any multiplicative operator.
pub fn mul_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("*"), tag("/")))(i)
}

/// Tries to parses the power operator.
pub fn pow_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("^")(i)
}

/// Tries to ast any prefix operator.
pub fn prefix_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to ast any postfix operator.
pub fn postfix_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("%")(i)
}

// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )  # means no cell reference
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee]) # true and false
/// Identifier.
pub fn identifier_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(unicode_ident::is_xid_continue),
    )))(i)
}

// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
// QuotedSheetName ::= '$'? SingleQuoted
pub fn sheet_name_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(none_of("]. #$'")))(i)
}

// Row ::= '$'? [1-9] [0-9]*
/// Row label
pub fn row_nom(i: Span<'_>) -> IResult<Span<'_>, (Option<Span<'_>>, Span<'_>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, row) = recognize(many1(one_of("0123456789")))(i)?;

    Ok((i, (abs, row)))
}

// Column ::= '$'? [A-Z]+
/// Column label
pub fn col_nom(i: Span<'_>) -> IResult<Span<'_>, (Option<Span<'_>>, Span<'_>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, col) = alpha1(i)?;
    Ok((i, (abs, col)))
}
