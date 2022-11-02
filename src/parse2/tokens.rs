//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::parse2::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, char as nchar, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, terminated, tuple};
use nom::IResult;

#[allow(unused_imports)]

/// Standard number.
///
/// Number ::= StandardNumber |
/// '.' [0-9]+ ([eE] [-+]? [0-9]+)?
/// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
pub fn number<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((
        // Case one: .42
        recognize(tuple((
            nchar('.'),
            decimal,
            opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
        ))),
        // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(tuple((nchar('.'), opt(decimal)))),
            one_of("eE"),
            opt(one_of("+-")),
            decimal,
        ))),
        // Case three: 42 and 42. and 42.42
        recognize(tuple((
            decimal, //
            opt(tuple((
                nchar('.'), //
                opt(decimal),
            ))), //
        ))),
    ))(i)
}

/// Standard string
pub fn string<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    quoted('"')(i)
}

fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(one_of("0123456789")))(input)
}

pub fn comparison_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((
        tag("="),
        tag("<>"),
        tag("<"),
        tag(">"),
        tag("<="),
        tag(">="),
    ))(i)
}

pub fn string_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

pub fn reference_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

pub fn ref_intersection_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("!")(i)
}

pub fn ref_concatenation_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("~")(i)
}

pub fn separator<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(";")(i)
}

pub fn dot<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

pub fn parenthesis_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

pub fn parenthesis_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(")")(i)
}

pub fn brackets_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("[")(i)
}

pub fn brackets_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("]")(i)
}

pub fn infix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("^"))))(i)
}

pub fn prefix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"))))(i)
}

pub fn postfix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(tag("%"))(i)
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
/// IRI
pub fn iri<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    let (i, iri) = terminated(quoted('\''), tag("#"))(i)?;
    Ok((i, iri))
}

/// Sheet name
pub fn sheetname<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, name) = alt((quoted('\''), recognize(many1(none_of("]. #$'")))))(i)?;

    Ok((i, (abs, name)))
}

/// Row label
pub fn row<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, row) = recognize(many1(one_of("0123456789")))(i)?;

    Ok((i, (abs, row)))
}

/// Column label
pub fn col<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, col) = alpha1(i)?;
    Ok((i, (abs, col)))
}

/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn quoted<'a>(quote: char) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    move |i| {
        let (i, r) = delimited(
            nchar(quote),
            recognize(many0(alt((
                take_while1(|v| v != quote),
                recognize(count(nchar(quote), 2)),
            )))),
            nchar(quote),
        )(i)?;

        Ok((i, r))
    }
}
