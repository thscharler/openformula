//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::ast::nomtokens::{
    add_op_nom, brackets_close_nom, brackets_open_nom, col_nom, colon_nom, comparison_op_nom,
    decimal, dollar_dollar_nom, dollar_nom, dot_nom, fn_name_nom, hashtag_nom, identifier_nom,
    mul_op_nom, parentheses_close_nom, parentheses_open_nom, postfix_op_nom, pow_op_nom,
    prefix_op_nom, ref_concat_op_nom, ref_intersection_op_nom, reference_op_nom, row_nom,
    semikolon_nom, sheet_name_nom, string_op_nom,
};
use crate::ast::{span_union, ParseResult, Span};
use crate::error::OFCode::*;
use crate::error::ParseOFError;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char as nchar, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::error::ErrorKind::*;
use nom::multi::{count, many0};
use nom::sequence::tuple;
use nom::InputTake;

/// Returns an empty token. But still technically a slice of the given span.
pub fn empty<'a>(i: Span<'a>) -> Span<'a> {
    i.take(0)
}

/// Lookahead for a number
pub fn lah_number<'a>(i: Span<'a>) -> bool {
    alt::<Span<'a>, char, nom::error::Error<_>, _>((nchar('.'), one_of("0123456789")))(i).is_ok()
}

// Number ::= StandardNumber | '.' [0-9]+ ([eE] [-+]? [0-9]+)?
// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
/// Any number.
pub fn number<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match alt((
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
    ))(rest)
    {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Char || e.code == OneOf => {
            Err(ParseOFError::number(rest))
        }
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Lookahead for a string.
pub fn lah_string<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('"')(i).is_ok()
}

/// Standard string
pub fn string<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    const QUOTE: char = '\"';

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(nom::Err::Error(e)) if e.code == Char => return Err(ParseOFError::start_quote(rest)),
        Err(e) => {
            return Err(ParseOFError::nom(e));
        }
    };

    let (rest, string) = match recognize(many0(alt((
        take_while1(|v| v != QUOTE),
        recognize(count(
            nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE),
            2,
        )),
    ))))(rest)
    {
        Ok((rest, tok)) => (rest, tok),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == Char => {
            return Err(ParseOFError::string(rest));
        }
        Err(e) => {
            return Err(ParseOFError::nom(e));
        }
    };

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(ParseOFError::end_quote(rest));
        }
        Err(e) => {
            return Err(ParseOFError::nom(e));
        }
    };

    Ok((rest, string))
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
pub fn fn_name<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match fn_name_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(ParseOFError::fn_name(rest))
        }
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse comparison operators.
pub fn comparison_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match comparison_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::comp_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse string operators.
pub fn string_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match string_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::string_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse reference operators.
pub fn reference_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match reference_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::ref_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse reference intersection.
pub fn ref_intersection_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match ref_intersection_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::ref_intersect_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse concat operator..
pub fn ref_concat_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match ref_concat_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::ref_concat_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$$")(rest).is_ok()
}

/// Parse separator char for function args.
pub fn dollar_dollar<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::dollardollar(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$")(rest).is_ok()
}

/// Parse separator char for function args.
pub fn dollar<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::dollar(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Hashtag
pub fn hashtag<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match hashtag_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::hashtag(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn semikolon<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match semikolon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::semikolon(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Lookahead for a dot.
pub fn lah_dot<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Parse dot
pub fn dot<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match dot_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::dot(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse colon
pub fn colon<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match colon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::colon(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Lookahead for opening parentheses.
pub fn lah_parentheses_open<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('(')(i).is_ok()
}

/// Parse open parentheses.
pub fn parentheses_open<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match parentheses_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::parens_open(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse closing parentheses.
pub fn parentheses_close<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match parentheses_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::parens_close(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse open brackets.
pub fn brackets_open<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match brackets_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::brackets_open(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Parse closing brackets.
pub fn brackets_close<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match brackets_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::brackets_close(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Tries to parses any additive operator.
pub fn add_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match add_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::add_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Tries to parses any multiplicative operator.
pub fn mul_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match mul_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::mul_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Tries to parses the power operator.
pub fn pow_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match pow_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::pow_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Lookahead for any prefix operator.
pub fn lah_prefix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("+-")(i).is_ok()
}

/// Tries to ast any prefix operator.
pub fn prefix_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match prefix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::prefix_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Tries to ast any postfix operator.
pub fn lah_postfix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("%")(i).is_ok()
}

/// Tries to ast any postfix operator.
pub fn postfix_op<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match postfix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::postfix_op(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
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
pub fn identifier<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    match identifier_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(ParseOFError::identifier(rest))
        }
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

/// Lookahead for a sheet-name
// TODO: sync with spreadsheet_ods_cellref
pub fn lah_sheet_name(i: Span<'_>) -> bool {
    // TODO: none_of("]. #$") is a very wide definition.
    alt::<_, char, nom::error::Error<_>, _>((nchar('$'), nchar('\''), none_of("]. #$")))(i).is_ok()
}

// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
// QuotedSheetName ::= '$'? SingleQuoted
// TODO: sync with spreadsheet_ods_cellref
/// Sheet name
pub fn sheet_name(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>)> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, abs)) => (rest, abs),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(ParseOFError::dollar(rest));
        }
        Err(e) => return Err(ParseOFError::nom(e)),
    };

    let (rest, name) = match single_quoted(rest) {
        Ok((rest, name)) => (rest, Some(name)),
        Err(e) if e.code == OFSingleQuoteStart => (rest, None),
        Err(e) if e.code == OFString || e.code == OFSingleQuoteEnd => return Err(e),
        Err(e) => return Err(ParseOFError::unexpected(e)),
    };

    let (rest, name) = if let Some(name) = name {
        (rest, name)
    } else {
        match sheet_name_nom(rest) {
            Ok((rest, tok)) => (rest, tok),
            Err(nom::Err::Error(e)) if e.code == NoneOf => {
                return Err(ParseOFError::sheet_name(rest));
            }
            Err(e) => return Err(ParseOFError::nom(e)),
        }
    };

    Ok((rest, (abs, name)))
}

// QuotedSheetName ::= '$'? SingleQuoted
// TODO: sync with spreadsheet_ods_cellref
/// Sheet name
pub fn quoted_sheet_name(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>)> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(ParseOFError::nom(e)),
    };
    let (rest, name) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e)
            if e.code == OFSingleQuoteStart || e.code == OFString || e.code == OFSingleQuoteEnd =>
        {
            return Err(e)
        }
        Err(e) => return Err(ParseOFError::unexpected(e)),
    };

    Ok((rest, (abs, name)))
}

// Source ::= "'" IRI "'" "#"
/// IRI
pub fn iri(rest: Span<'_>) -> ParseResult<'_, Span<'_>> {
    let (rest, iri) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e)
            if e.code == OFSingleQuoteStart || e.code == OFString || e.code == OFSingleQuoteEnd =>
        {
            return Err(e)
        }
        Err(e) => return Err(ParseOFError::unexpected(e)),
    };
    let (rest,) = match hashtag_nom(rest) {
        Ok((rest, _hash)) => (rest,),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(ParseOFError::hashtag(rest));
        }
        Err(e) => return Err(ParseOFError::nom(e)),
    };

    Ok((rest, iri))
}

// Row ::= '$'? [1-9] [0-9]*
/// Row label
pub fn row(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>)> {
    match row_nom(rest) {
        Ok((rest, (abs, row))) => Ok((rest, (abs, row))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::dollar(rest)),
        Err(nom::Err::Error(e)) if e.code == OneOf => Err(ParseOFError::digit(rest)),
        Err(nom::Err::Error(e)) if e.code == Many1 => Err(ParseOFError::digit(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

// Column ::= '$'? [A-Z]+
/// Column label
pub fn col(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>)> {
    match col_nom(rest) {
        Ok((rest, (abs, col))) => Ok((rest, (abs, col))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(ParseOFError::dollar(rest)),
        Err(nom::Err::Error(e)) if e.code == Alpha => Err(ParseOFError::alpha(rest)),
        Err(e) => Err(ParseOFError::nom(e)),
    }
}

// SingleQuoted ::= "'" ([^'] | "''")+ "'"
/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn single_quoted<'a>(rest: Span<'a>) -> ParseResult<'a, Span<'a>> {
    const QUOTE: char = '\'';

    let (rest, first_quote) =
        match recognize(nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE))(rest) {
            Ok((rest, quote)) => (rest, quote),
            Err(nom::Err::Error(e)) if e.code == Char => {
                return Err(ParseOFError::start_single_quote(rest))
            }
            Err(e) => return Err(ParseOFError::nom(e)),
        };

    let (rest, _string) = match recognize(many0(alt((
        take_while1(|v| v != QUOTE),
        recognize(count(
            nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE),
            2,
        )),
    ))))(rest)
    {
        Ok((rest, tok)) => (rest, tok),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 => return Err(ParseOFError::string(rest)),
        Err(nom::Err::Error(e)) if e.code == Char => return Err(ParseOFError::string(rest)),
        Err(e) => return Err(ParseOFError::nom(e)),
    };

    let (rest, last_quote) =
        match recognize(nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE))(rest) {
            Ok((rest, quote)) => (rest, quote),
            Err(nom::Err::Error(e)) if e.code == Char => {
                return Err(ParseOFError::end_single_quote(rest))
            }
            Err(e) => return Err(ParseOFError::nom(e)),
        };

    let token = unsafe { span_union(first_quote, last_quote) };
    Ok((rest, token))
}
