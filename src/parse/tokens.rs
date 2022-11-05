//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::parse::Span;
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

fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(one_of("0123456789")))(input)
}

/// Standard string
pub fn string<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    quoted('"')(i)
}

/// Parse comparison operators.
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

/// Parse string operators.
pub fn string_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

/// Parse reference operators.
pub fn reference_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("&")(i)
}

/// Parse referenc intersection.
pub fn ref_intersection_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("!")(i)
}

/// Parse concat operator..
pub fn ref_concatenation_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("~")(i)
}

/// Parse separator char for function args.
pub fn separator<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(";")(i)
}

/// Parse dot
pub fn dot<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

/// Parse colon
pub fn colon<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(":")(i)
}

/// Parse open parenthesis.
pub fn parenthesis_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

/// Parse closing parenthesis.
pub fn parenthesis_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(")")(i)
}

/// Parse open brackets.
pub fn brackets_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("[")(i)
}

/// Parse closing brackets.
pub fn brackets_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("]")(i)
}

/// Tries to parses any additive operator.
pub fn add_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"))))(i)
}

/// Tries to parses any multiplicative operator.
pub fn mul_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("*"), tag("/"))))(i)
}

/// Tries to parses the power operator.
pub fn pow_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(tag("^"))(i)
}

/// Tries to parse any prefix operator.
pub fn prefix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"))))(i)
}

/// Tries to parse any postfix operator.
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

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::parse::tokens::{col, iri, quoted, row, sheetname};
    use crate::parse::Span;
    use nom::error::ErrorKind;
    use nom::error::ParseError;

    #[test]
    fn test_quoted() {
        unsafe {
            assert_eq!(
                quoted('\'')(Span::new("'")),
                Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    Span::new_from_raw_offset(1, 1, "", ()),
                    ErrorKind::Char
                )))
            );
            assert_eq!(
                quoted('\'')(Span::new("''")),
                Ok((
                    Span::new_from_raw_offset(2, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'''")),
                Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    Span::new_from_raw_offset(3, 1, "", ()),
                    ErrorKind::Char
                )))
            );
            assert_eq!(
                quoted('\'')(Span::new("''''")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "''", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'abcd'")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "abcd", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'a'bcd'")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "bcd'", ()),
                    Span::new_from_raw_offset(1, 1, "a", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'a''bcd'")),
                Ok((
                    Span::new_from_raw_offset(8, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "a''bcd", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'a'''bcd'")),
                Ok((
                    Span::new_from_raw_offset(5, 1, "bcd'", ()),
                    Span::new_from_raw_offset(1, 1, "a''", ())
                ))
            );
        }
    }

    #[test]
    fn test_column() {
        unsafe {
            assert_eq!(
                col(Span::new("A")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "A", ()))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "AAAA", ()))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA ")),
                Ok((
                    Span::new_from_raw_offset(4, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "AAAA", ()))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA1234")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "1234", ()),
                    (None, Span::new_from_raw_offset(0, 1, "AAAA", ()))
                ))
            );
        }
    }

    #[test]
    fn test_column2() {
        unsafe {
            assert_eq!(
                col(Span::new("A")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::from("A"))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA ")),
                Ok((
                    Span::new_from_raw_offset(4, 1, " ", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                col(Span::new("AAAA1234")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "1234", ()),
                    (None, Span::from("AAAA"))
                ))
            );
        }
    }

    #[test]
    fn test_row() {
        unsafe {
            assert_eq!(
                row(Span::new("1")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "1", ()))
                ))
            );
            assert_eq!(
                row(Span::new("123")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "123", ()))
                ))
            );
            assert_eq!(
                row(Span::new("123 ")),
                Ok((
                    Span::new_from_raw_offset(3, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "123", ()))
                ))
            );
        }
    }

    #[test]
    fn test_sheetname() {
        unsafe {
            assert_eq!(
                sheetname(Span::new("sheet1")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("sheet1]")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "]", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("sheet1.")),
                Ok((
                    Span::new_from_raw_offset(6, 1, ".", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("sheet1$")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "$", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("sheet1 ")),
                Ok((
                    Span::new_from_raw_offset(6, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("sheet1#")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "#", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheetname(Span::new("'sheet1'")),
                Ok((
                    Span::new_from_raw_offset(8, 1, "", ()),
                    (None, Span::new_from_raw_offset(1, 1, "sheet1", ()))
                ))
            );
        }
    }
    #[test]
    fn test_iri() {
        unsafe {
            assert_eq!(
                iri(Span::new("'file:c:x.txt'#")),
                Ok((
                    Span::new_from_raw_offset(15, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "file:c:x.txt", ())
                ))
            );
        }
    }
}
