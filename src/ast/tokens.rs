//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::ast::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{char as nchar, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, tuple};
use nom::IResult;
use std::fmt::{Display, Formatter};

type TokenResult<'a, O> = Result<(Span<'a>, O), TokenError<'a>>;

#[derive(Debug, PartialEq)]
pub enum TokenError<'s> {
    /// Nom ast error.
    TokNomError(Span<'s>),
    /// Nom failure.
    TokNomFailure(Span<'s>),

    TokStartQuote(Span<'s>),
    TokEndQuote(Span<'s>),
    TokStartSingleQuote(Span<'s>),
    TokEndSingleQuote(Span<'s>),
    TokString(Span<'s>),
    TokNumber(Span<'s>),
    TokFnName(Span<'s>),
    TokComparisonOp(Span<'s>),
    TokDollar(Span<'s>),
    TokDollarDollar(Span<'s>),
    TokIdentifier(Span<'s>),
    TokDot(Span<'s>),
    TokSheetName(Span<'s>),
    Hash(Span<'s>),
    TokParenthesesOpen(Span<'s>),
    TokParenthesesClose(Span<'s>),
    TokSemikolon(Span<'s>),
    TokPrefixOp(Span<'s>),
    TokPostfixOp(Span<'s>),
    TokAddOp(Span<'s>),
    TokMulOp(Span<'s>),
    TokPowOp(Span<'s>),
}

impl<'s> TokenError<'s> {
    pub fn span(&self) -> &Span<'s> {
        match self {
            TokenError::TokNomError(s) => s,
            TokenError::TokNomFailure(s) => s,
            TokenError::TokStartQuote(s) => s,
            TokenError::TokString(s) => s,
            TokenError::TokEndQuote(s) => s,
            TokenError::TokNumber(s) => s,
            TokenError::TokFnName(s) => s,
            TokenError::TokComparisonOp(s) => s,
            TokenError::TokDollarDollar(s) => s,
            TokenError::TokIdentifier(s) => s,
            TokenError::TokDollar(s) => s,
            TokenError::TokStartSingleQuote(s) => s,
            TokenError::TokEndSingleQuote(s) => s,
            TokenError::TokDot(s) => s,
            TokenError::TokSheetName(s) => s,
            TokenError::Hash(s) => s,
            TokenError::TokParenthesesOpen(s) => s,
            TokenError::TokParenthesesClose(s) => s,
            TokenError::TokSemikolon(s) => s,
            TokenError::TokPrefixOp(s) => s,
            TokenError::TokPostfixOp(s) => s,
            TokenError::TokAddOp(s) => s,
            TokenError::TokMulOp(s) => s,
            TokenError::TokPowOp(s) => s,
        }
    }
}

impl<'s> Display for TokenError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::TokNomError(s) => write!(f, "NomError {}", s),
            TokenError::TokNomFailure(s) => write!(f, "NomFailure {}", s),
            TokenError::TokStartQuote(s) => write!(f, "QuoteStart {}", s),
            TokenError::TokString(s) => write!(f, "String {}", s),
            TokenError::TokEndQuote(s) => write!(f, "QuoteEnd {}", s),
            TokenError::TokNumber(s) => write!(f, "Number {}", s),
            TokenError::TokFnName(s) => write!(f, "FnName {}", s),
            TokenError::TokComparisonOp(s) => write!(f, "CompOp {}", s),
            TokenError::TokDollarDollar(s) => write!(f, "DollarDollar {}", s),
            TokenError::TokIdentifier(s) => write!(f, "DollarDollar {}", s),
            TokenError::TokDollar(s) => write!(f, "Dollar {}", s),
            TokenError::TokStartSingleQuote(s) => write!(f, "SingleQuoteStart {}", s),
            TokenError::TokEndSingleQuote(s) => write!(f, "SingleQuoteEnd {}", s),
            TokenError::TokDot(s) => write!(f, "Dot {}", s),
            TokenError::TokSheetName(s) => write!(f, "SheetName {}", s),
            TokenError::Hash(s) => write!(f, "SheetName {}", s),
            TokenError::TokParenthesesOpen(s) => write!(f, "ParenthesesOpen {}", s),
            TokenError::TokParenthesesClose(s) => write!(f, "ParenthesesClose {}", s),
            TokenError::TokSemikolon(s) => write!(f, "Semikolon {}", s),
            TokenError::TokPrefixOp(s) => write!(f, "PrefixOp {}", s),
            TokenError::TokPostfixOp(s) => write!(f, "PostfixOp {}", s),
            TokenError::TokAddOp(s) => write!(f, "AddOp {}", s),
            TokenError::TokMulOp(s) => write!(f, "MulOp {}", s),
            TokenError::TokPowOp(s) => write!(f, "PowOp {}", s),
        }
    }
}

/// Lookahead for a number
pub fn lah_number<'a>(i: Span<'a>) -> bool {
    alt::<Span<'a>, char, nom::error::Error<_>, _>((nchar('.'), one_of("0123456789")))(i).is_ok()
}

// Number ::= StandardNumber | '.' [0-9]+ ([eE] [-+]? [0-9]+)?
// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
/// Any number.
pub fn number<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
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
        Err(_) => Err(TokenError::TokNumber(rest)),
    }
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
pub fn string<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    const QUOTE: char = '\"';

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(_) => {
            return Err(TokenError::TokStartQuote(rest));
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
        Err(_) => {
            return Err(TokenError::TokString(rest));
        }
    };

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(_) => {
            return Err(TokenError::TokEndQuote(rest));
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
fn fn_name_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(|c: char| unicode_ident::is_xid_continue(c) || c == '_' || c == '.'),
    )))(i)
}

// LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
/// Function name.
pub fn fn_name<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match fn_name_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokFnName(rest)),
    }
}

/// Parse comparison operators.
fn comparison_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((
        tag("="),
        tag("<>"),
        tag("<"),
        tag(">"),
        tag("<="),
        tag(">="),
    ))(i)
}

/// Parse comparison operators.
pub fn comparison_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match comparison_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokComparisonOp(rest)),
    }
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
pub fn lah_dollar_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$$")(rest).is_ok()
}

/// Parse separator char for function args.
fn dollar_dollar_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("$$")(rest)
}

/// Parse separator char for function args.
pub fn dollar_dollar<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokDollarDollar(rest)),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$")(rest).is_ok()
}

/// Parse separator char for function args.
fn dollar_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("$")(rest)
}

/// Parse separator char for function args.
pub fn dollar<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokDollar(rest)),
    }
}

/// Hashtag
fn hashtag_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("#")(rest)
}

/// Parse separator char for function args.
fn semikolon_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(";")(rest)
}

/// Parse separator char for function args.
pub fn semikolon<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match semikolon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokSemikolon(rest)),
    }
}

/// Lookahead for a dot.
pub fn lah_dot<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Parse dot
fn dot_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

/// Parse dot
pub fn dot<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dot_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokDot(rest)),
    }
}

/// Parse colon
pub fn colon<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(":")(i)
}

/// Lookahead for opening parentheses.
pub fn lah_parentheses_open<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('(')(i).is_ok()
}

/// Parse open parentheses.
fn parentheses_open_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

/// Parse open parentheses.
pub fn parentheses_open<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match parentheses_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokParenthesesOpen(rest)),
    }
}

/// Parse closing parentheses.
fn parentheses_close_nom<'a>(rest: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(")")(rest)
}

/// Parse closing parentheses.
pub fn parentheses_close<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match parentheses_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokParenthesesClose(rest)),
    }
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
fn add_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to parses any additive operator.
pub fn add_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match add_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokAddOp(rest)),
    }
}

/// Tries to parses any multiplicative operator.
fn mul_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("*"), tag("/")))(i)
}

/// Tries to parses any multiplicative operator.
pub fn mul_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match mul_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokMulOp(rest)),
    }
}

/// Tries to parses the power operator.
fn pow_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("^")(i)
}

/// Tries to parses the power operator.
pub fn pow_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match pow_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokPowOp(rest)),
    }
}

/// Lookahead for any prefix operator.
pub fn lah_prefix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("+-")(i).is_ok()
}

/// Tries to ast any prefix operator.
fn prefix_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((tag("+"), tag("-")))(i)
}

/// Tries to ast any prefix operator.
pub fn prefix_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match prefix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokPrefixOp(rest)),
    }
}

/// Tries to ast any postfix operator.
pub fn lah_postfix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("%")(i).is_ok()
}

/// Tries to ast any postfix operator.
fn postfix_op_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("%")(i)
}

/// Tries to ast any postfix operator.
pub fn postfix_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match postfix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokPostfixOp(rest)),
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
fn identifier_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(tuple((
        take_while1(unicode_ident::is_xid_start),
        take_while(unicode_ident::is_xid_continue),
    )))(i)
}

/// Identifier.
pub fn identifier<'a>(i: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match identifier_nom(i) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(_) => Err(TokenError::TokIdentifier(i)),
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

fn sheet_name_nom<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    recognize(many1(none_of("]. #$'")))(i)
}

// TODO: sync with spreadsheet_ods_cellref
/// Sheet name
pub fn sheet_name(rest: Span<'_>) -> TokenResult<'_, (Option<Span<'_>>, Span<'_>)> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(_) => return Err(TokenError::TokDollar(rest)),
    };
    let (rest, name) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, Some(tok)),
        Err(TokenError::TokStartSingleQuote(_)) => (rest, None),
        Err(e) => return Err(e),
    };
    let (rest, name) = if let Some(name) = name {
        (rest, name)
    } else {
        match sheet_name_nom(rest) {
            Ok((rest, tok)) => (rest, tok),
            Err(_) => return Err(TokenError::TokSheetName(rest)),
        }
    };

    Ok((rest, (abs, name)))
}

// QuotedSheetName ::= '$'? SingleQuoted
// TODO: sync with spreadsheet_ods_cellref
/// Sheet name
pub fn quoted_sheet_name(rest: Span<'_>) -> TokenResult<'_, (Option<Span<'_>>, Span<'_>)> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(_) => return Err(TokenError::TokDollar(rest)),
    };
    let (rest, name) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(e),
    };

    Ok((rest, (abs, name)))
}

// Source ::= "'" IRI "'" "#"
/// IRI
pub fn iri(rest: Span<'_>) -> TokenResult<'_, Span<'_>> {
    let (rest, iri) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(e),
    };
    let (rest,) = match hashtag_nom(rest) {
        Ok((rest, _hash)) => (rest,),
        Err(_) => return Err(TokenError::Hash(rest)),
    };

    Ok((rest, iri))
}

// SingleQuoted ::= "'" ([^'] | "''")+ "'"
/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
// TODO: remove
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

/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn single_quoted<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    const QUOTE: char = '\'';

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(_) => {
            return Err(TokenError::TokStartSingleQuote(rest));
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
        Err(_) => {
            return Err(TokenError::TokString(rest));
        }
    };

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(_) => {
            return Err(TokenError::TokEndSingleQuote(rest));
        }
    };

    Ok((rest, string))
}

#[allow(unsafe_code)]
#[cfg(test)]
mod tests {
    use crate::ast::tokens::{identifier, iri, TokenError};
    use crate::ast::Span;
    use crate::ast::{parser, tokens};
    use nom::error::ErrorKind;
    use nom::error::ParseError;
    use spreadsheet_ods_cellref::tokens as refs_tokens;

    #[test]
    fn test_identifier() {
        println!("{:?}", identifier(Span::new("Name")));
    }

    #[test]
    fn test_quoted() {
        unsafe {
            assert_eq!(
                tokens::quoted('\'')(Span::new("'")),
                Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    Span::new_from_raw_offset(1, 1, "", ()),
                    ErrorKind::Char
                )))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("''")),
                Ok((
                    Span::new_from_raw_offset(2, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "", ())
                ))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("'''")),
                Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    Span::new_from_raw_offset(3, 1, "", ()),
                    ErrorKind::Char
                )))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("''''")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "''", ())
                ))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("'text'")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "text", ())
                ))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("'t'ext'")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "ext'", ()),
                    Span::new_from_raw_offset(1, 1, "t", ())
                ))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("'t''ext'")),
                Ok((
                    Span::new_from_raw_offset(8, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "t''ext", ())
                ))
            );
            assert_eq!(
                tokens::quoted('\'')(Span::new("'t'''ext'")),
                Ok((
                    Span::new_from_raw_offset(5, 1, "ext'", ()),
                    Span::new_from_raw_offset(1, 1, "t''", ())
                ))
            );
        }
    }

    #[test]
    fn test_single_quoted() {
        unsafe {
            assert_eq!(
                tokens::single_quoted(Span::new("")),
                Err(TokenError::TokStartQuote(Span::new_from_raw_offset(
                    0,
                    1,
                    "",
                    ()
                )))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'")),
                Err(TokenError::TokEndQuote(Span::new_from_raw_offset(
                    1,
                    1,
                    "",
                    ()
                )))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("''")),
                Ok((
                    Span::new_from_raw_offset(2, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "", ())
                ))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'''")),
                Err(TokenError::TokEndQuote(Span::new_from_raw_offset(
                    3,
                    1,
                    "",
                    ()
                )))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("''''")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "''", ())
                ))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'text'")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "text", ())
                ))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'t'ext'")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "ext'", ()),
                    Span::new_from_raw_offset(1, 1, "t", ())
                ))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'t''ext'")),
                Ok((
                    Span::new_from_raw_offset(8, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "t''ext", ())
                ))
            );
            assert_eq!(
                tokens::single_quoted(Span::new("'t'''ext'")),
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
                refs_tokens::col(Span::new("A")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "A", ()))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "AAAA", ()))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA ")),
                Ok((
                    Span::new_from_raw_offset(4, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "AAAA", ()))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA1234")),
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
                refs_tokens::col(Span::new("A")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::from("A"))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA ")),
                Ok((
                    Span::new_from_raw_offset(4, 1, " ", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                refs_tokens::col(Span::new("AAAA1234")),
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
                refs_tokens::row(Span::new("1")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::row(Span::new("123")),
                Ok((
                    Span::new_from_raw_offset(3, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "123", ()))
                ))
            );
            assert_eq!(
                refs_tokens::row(Span::new("123 ")),
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
                refs_tokens::sheet_name(Span::new("sheet1")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("sheet1]")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "]", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("sheet1.")),
                Ok((
                    Span::new_from_raw_offset(6, 1, ".", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("sheet1$")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "$", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("sheet1 ")),
                Ok((
                    Span::new_from_raw_offset(6, 1, " ", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("sheet1#")),
                Ok((
                    Span::new_from_raw_offset(6, 1, "#", ()),
                    (None, Span::new_from_raw_offset(0, 1, "sheet1", ()))
                ))
            );
            assert_eq!(
                refs_tokens::sheet_name(Span::new("'sheet1'")),
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
            println!("{:?}", iri(Span::new("Name")));

            assert_eq!(
                refs_tokens::iri(Span::new("'file:c:x.txt'#")),
                Ok((
                    Span::new_from_raw_offset(15, 1, "", ()),
                    Span::new_from_raw_offset(1, 1, "file:c:x.txt", ())
                ))
            );
        }
    }

    #[test]
    fn test_lah() {
        dbg!(tokens::lah_number(Span::new("222")));
        dbg!(tokens::lah_number(Span::new("ABC")));
        dbg!(tokens::lah_parentheses_open(Span::new("()")));
        let rest = parser::eat_space(Span::new("  ()"));
        dbg!(tokens::lah_parentheses_open(rest));
    }
}
