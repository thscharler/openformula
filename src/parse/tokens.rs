//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::parse::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{char as nchar, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, tuple};
use nom::IResult;

/// Lookahead for a number
pub fn lah_number<'a>(i: Span<'a>) -> bool {
    alt::<Span<'a>, char, nom::error::Error<_>, _>((nchar('.'), one_of("0123456789")))(i).is_ok()
}

// Number ::= StandardNumber | '.' [0-9]+ ([eE] [-+]? [0-9]+)?
// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
/// Any number.
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

/// Sequence of digits.
fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(one_of("0123456789")))(input)
}

/// Lookahead for a string.
pub fn lah_string<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('"')(i).is_ok()
}

/// Standard string
pub fn string<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    quoted('"')(i)
}

/// Lookahead for a function name.
pub fn lah_fn_name<'a>(i: Span<'a>) -> bool {
    match (*i).chars().next() {
        None => false,
        Some(c) => unicode_ident::is_xid_start(c),
    }
}

// LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
/// Function name.
pub fn fn_name<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(|c: char| unicode_ident::is_xid_continue(c) || c == '_' || c == '.'),
    )))(i)
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

/// Parse reference intersection.
pub fn ref_intersection_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("!")(i)
}

/// Parse concat operator..
pub fn ref_concatenation_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("~")(i)
}

/// Parse separator char for function args.
pub fn lah_dollar_dollar<'a>(i: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$$")(i).is_ok()
}

/// Parse separator char for function args.
pub fn dollar_dollar<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("$$")(i)
}

/// Parse separator char for function args.
pub fn separator<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(";")(i)
}

/// Lookahead for a dot.
// TODO: WATCH
pub fn lah_dot<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Parse dot
// TODO: WATCH
pub fn dot<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

/// Parse colon
// TODO: WATCH
pub fn colon<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(":")(i)
}

/// Lookahead for opening parentheses.
pub fn lah_parentheses_open<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('(')(i).is_ok()
}

/// Parse open parentheses.
pub fn parentheses_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

/// Parse closing parentheses.
pub fn parentheses_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
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
// TODO: do we need this opt here (and somewhere else too).
pub fn add_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"))))(i)
}

/// Tries to parses any multiplicative operator.
// TODO: do we need this opt here (and somewhere else too).
pub fn mul_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("*"), tag("/"))))(i)
}

/// Tries to parses the power operator.
// TODO: do we need this opt here (and somewhere else too).
pub fn pow_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(tag("^"))(i)
}

/// Lookahead for any prefix operator.
// TODO: do we need this opt here (and somewhere else too).
pub fn lah_prefix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("+-")(i).is_ok()
}

/// Tries to parse any prefix operator.
// TODO: do we need this opt here (and somewhere else too).
pub fn prefix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(alt((tag("+"), tag("-"))))(i)
}

/// Tries to parse any postfix operator.
// TODO: do we need this opt here (and somewhere else too).
pub fn postfix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
    opt(tag("%"))(i)
}

/// Simple lookahead for a identifier.
pub fn lah_identifier<'a>(i: Span<'a>) -> bool {
    match (*i).chars().next() {
        None => false,
        Some(c) => unicode_ident::is_xid_start(c),
    }
}

// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )  # means no cell reference
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee]) # true and false
/// Identifier.
pub fn identifier<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while1(unicode_ident::is_xid_continue),
    )))(i)
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

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast_parser::eat_space;
    use crate::parse::tokens::quoted;
    use crate::parse::Span;
    use crate::tokens::{lah_number, lah_parentheses_open};
    use nom::error::ErrorKind;
    use nom::error::ParseError;
    use spreadsheet_ods_cellref::tokens::{col, iri, row, sheet_name};

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
                quoted('\'')(Span::new("'text'")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "text", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'t'ext'")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "ext'", ()),
                    Span::new_from_raw_offset(1, 1, "t", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'t''ext'")),
                Ok((
                    Span::new_from_raw_offset(8, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "t''ext", ())
                ))
            );
            assert_eq!(
                quoted('\'')(Span::new("'t'''ext'")),
                Ok((
                    Span::new_from_raw_offset(5, 1, "ext'", ()),
                    Span::new_from_raw_offset(1, 1, "t''", ())
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
    fn test_sheet_name() {
        unsafe {
            assert_eq!(
                sheet_name(Span::new("sheet1")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("sheet1]")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "]", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("sheet1.")),
                Ok((
                    Span::new_from_raw_offset(6, 1, ".", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("sheet1$")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "$", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("sheet1 ")),
                Ok((
                    Span::new_from_raw_offset(6, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("sheet1#")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "#", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                sheet_name(Span::new("'sheet1'")),
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

    #[test]
    fn test_lah() {
        dbg!(lah_number(Span::new("222")));
        dbg!(lah_number(Span::new("ABC")));
        dbg!(lah_parentheses_open(Span::new("()")));
        let rest = eat_space(Span::new("  ()"));
        dbg!(lah_parentheses_open(rest));
    }
}
