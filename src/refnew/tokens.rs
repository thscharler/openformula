use crate::refnew::refs_parser::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, char as nchar, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, terminated};
use nom::IResult;

/// Lookahead for an IRI.
pub fn lah_iri<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('\'')(i).is_ok()
}

// Source ::= "'" IRI "'" "#"
/// IRI
pub fn iri<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    let (i, iri) = terminated(quoted('\''), tag("#"))(i)?;
    Ok((i, iri))
}

/// Lookahead for a sheet-name
pub fn lah_sheet_name<'s>(i: Span<'s>) -> bool {
    // TODO: none_of("]. #$") is a very wide definition.
    alt::<_, char, nom::error::Error<_>, _>((nchar('$'), nchar('\''), none_of("]. #$")))(i).is_ok()
}

// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
/// Sheet name
pub fn sheet_name<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, name) = alt((quoted('\''), recognize(many1(none_of("]. #$'")))))(i)?;

    Ok((i, (abs, name)))
}

/// Lookahead for a dot.
pub fn lah_dot<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Parse dot
pub fn dot<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

/// Parse colon
pub fn colon<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(":")(i)
}

// Row ::= '$'? [1-9] [0-9]*
/// Row label
pub fn row<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, row) = recognize(many1(one_of("0123456789")))(i)?;

    Ok((i, (abs, row)))
}

// Column ::= '$'? [A-Z]+
/// Column label
pub fn col<'a>(i: Span<'a>) -> IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, col) = alpha1(i)?;
    Ok((i, (abs, col)))
}

// SingleQuoted ::= "'" ([^'] | "''")+ "'"
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
