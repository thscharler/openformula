//!
//! Contains all token parsers. Operates on and returns only spans.
//! These are the parsers that are formulated in nom style.
//! tokens contains the mapping functions to our own errors.
//!

use crate::iparse::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{alpha1, char as nchar, multispace0, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::tuple;
use nom::IResult;

const DOUBLE_QUOTE: char = '\"';
const SINGLE_QUOTE: char = '\'';

/// Eats the leading whitespace.
pub fn eat_space(i: Span<'_>) -> Span<'_> {
    match multispace0::<Span<'_>, nom::error::Error<_>>(i) {
        Ok((rest, _white)) => rest,
        Err(nom::Err::Error(_)) => i,
        Err(nom::Err::Failure(_)) => i,
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parse one space
pub fn space(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag(" ")(i)
}

/// Lookahead for a number
pub fn lah_number(i: Span<'_>) -> bool {
    alt::<Span<'_>, char, nom::error::Error<_>, _>((nchar('.'), one_of("0123456789")))(i).is_ok()
}

/// Lookahead for a string.
pub fn lah_string(i: Span<'_>) -> bool {
    nchar::<Span<'_>, nom::error::Error<_>>('"')(i).is_ok()
}

/// Lookahead for a function name.
pub fn lah_fn_name(i: Span<'_>) -> bool {
    match (*i).chars().next() {
        None => false,
        Some(c) => unicode_ident::is_xid_start(c),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar_dollar(rest: Span<'_>) -> bool {
    tag::<&str, Span<'_>, nom::error::Error<_>>("$$")(rest).is_ok()
}

/// Parse separator char for function args.
pub fn lah_dollar(rest: Span<'_>) -> bool {
    tag::<&str, Span<'_>, nom::error::Error<_>>("$")(rest).is_ok()
}

/// Lookahead for a dot.
pub fn lah_dot(i: Span<'_>) -> bool {
    nchar::<Span<'_>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Lookahead for opening parentheses.
pub fn lah_parentheses_open(i: Span<'_>) -> bool {
    nchar::<Span<'_>, nom::error::Error<_>>('(')(i).is_ok()
}

/// Lookahead for any prefix operator.
pub fn lah_prefix_op(i: Span<'_>) -> bool {
    one_of::<Span<'_>, _, nom::error::Error<_>>("+-")(i).is_ok()
}

/// Tries to ast any postfix operator.
pub fn lah_postfix_op(i: Span<'_>) -> bool {
    one_of::<Span<'_>, _, nom::error::Error<_>>("%")(i).is_ok()
}

/// Simple lookahead for a identifier.
pub fn lah_identifier(i: Span<'_>) -> bool {
    match (*i).chars().next() {
        None => false,
        Some(c) => unicode_ident::is_xid_start(c),
    }
}

/// Lookahead for a sheet-name
pub fn lah_sheet_name(i: Span<'_>) -> bool {
    // NOTE: none_of("]. #$") is a very wide definition.
    alt::<_, char, nom::error::Error<_>, _>((nchar('$'), nchar('\''), none_of("]. #$")))(i).is_ok()
}

/// Lookahead for an IRI.
pub fn lah_iri(i: Span<'_>) -> bool {
    nchar::<Span<'_>, nom::error::Error<_>>('\'')(i).is_ok()
}

/// Numeric value.
pub fn number_nom(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
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
    ))(input)
}

/// Sequence of digits.
pub fn decimal(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(many1(one_of("0123456789")))(input)
}

/// A quote "
pub fn double_quote_nom(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(nchar::<Span<'_>, nom::error::Error<Span<'_>>>(DOUBLE_QUOTE))(input)
}

/// A string containing double "" and ending (excluding) with a quote "
pub fn double_string_nom(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(many0(alt((
        take_while1(|v| v != DOUBLE_QUOTE),
        recognize(count(
            nchar::<Span<'_>, nom::error::Error<Span<'_>>>(DOUBLE_QUOTE),
            2,
        )),
    ))))(input)
}

/// A quote '
pub fn single_quote_nom(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(nchar::<Span<'_>, nom::error::Error<Span<'_>>>(SINGLE_QUOTE))(input)
}

/// A string containing double ''' and ending (excluding) with a quote '
pub fn single_string_nom(input: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(many0(alt((
        take_while1(|v| v != SINGLE_QUOTE),
        recognize(count(
            nchar::<Span<'_>, nom::error::Error<Span<'_>>>(SINGLE_QUOTE),
            2,
        )),
    ))))(input)
}

// LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
/// Function name.
pub fn fn_name_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(|c: char| unicode_ident::is_xid_continue(c) || c == '_' || c == '.'),
    )))(i)
}

/// Parse comparison operators.
pub fn comparison_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
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
pub fn string_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("&")(i)
}

/// Parse reference operators.
pub fn reference_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("&")(i)
}

/// Parse reference intersection.
pub fn ref_intersection_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("!")(i)
}

/// Parse concat operator..
pub fn ref_concat_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("~")(i)
}

/// Parse separator char for function args.
pub fn dollar_dollar_nom(rest: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("$$")(rest)
}

/// Parse separator char for function args.
pub fn dollar_nom(rest: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("$")(rest)
}

/// Hashtag
pub fn hashtag_nom(rest: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("#")(rest)
}

/// Parse separator char for function args.
pub fn semikolon_nom(rest: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag(";")(rest)
}

/// Parse dot
pub fn dot_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag(".")(i)
}

/// Parse colon
pub fn colon_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag(":")(i)
}

/// Parse open parentheses.
pub fn parentheses_open_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("(")(i)
}

/// Parse closing parentheses.
pub fn parentheses_close_nom(rest: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag(")")(rest)
}

/// Parse open brackets.
pub fn brackets_open_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("[")(i)
}

/// Parse closing brackets.
pub fn brackets_close_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("]")(i)
}

/// Tries to parses any additive operator.
pub fn add_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to parses any multiplicative operator.
pub fn mul_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    alt((tag("*"), tag("/")))(i)
}

/// Tries to parses the power operator.
pub fn pow_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("^")(i)
}

/// Tries to ast any prefix operator.
pub fn prefix_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to ast any postfix operator.
pub fn postfix_op_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    tag("%")(i)
}

// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )  # means no cell reference
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee]) # true and false
/// Identifier.
pub fn identifier_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(unicode_ident::is_xid_continue),
    )))(i)
}

// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
// QuotedSheetName ::= '$'? SingleQuoted
/// Sheet name.
pub fn sheet_name_nom(i: Span<'_>) -> IResult<Span<'_>, Span<'_>> {
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
