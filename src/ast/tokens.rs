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
use crate::ast::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char as nchar, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::multi::{count, many0};
use nom::sequence::tuple;
use nom::InputTake;
use std::fmt::{Debug, Display, Formatter};

use nom::error::ErrorKind::*;

pub type TokenResult<'a, O> = Result<(Span<'a>, O), TokenError<'a>>;

#[derive(PartialEq)]
pub enum TokenError<'s> {
    /// Nom ast error.
    TokNomError(Span<'s>),
    /// Nom failure.
    TokNomFailure(Span<'s>),
    /// An unknown token error. A unexpected TokenError was found.
    TokUnexpected(Span<'s>, Box<TokenError<'s>>),

    TokHash(Span<'s>),
    TokAddOp(Span<'s>),
    TokAlpha(Span<'s>),
    TokBracketsClose(Span<'s>),
    TokBracketsOpen(Span<'s>),
    TokCol(Span<'s>),
    TokColon(Span<'s>),
    TokComparisonOp(Span<'s>),
    TokDigit(Span<'s>),
    TokDollar(Span<'s>),
    TokDollarDollar(Span<'s>),
    TokDot(Span<'s>),
    TokEndQuote(Span<'s>),
    TokEndSingleQuote(Span<'s>),
    TokFnName(Span<'s>),
    TokHashtag(Span<'s>),
    TokIdentifier(Span<'s>),
    TokMulOp(Span<'s>),
    TokNumber(Span<'s>),
    TokParenthesesClose(Span<'s>),
    TokParenthesesOpen(Span<'s>),
    TokPostfixOp(Span<'s>),
    TokPowOp(Span<'s>),
    TokPrefixOp(Span<'s>),
    TokRefConcatOp(Span<'s>),
    TokRefIntersectionOp(Span<'s>),
    TokReferenceOp(Span<'s>),
    TokRow(Span<'s>),
    TokSemikolon(Span<'s>),
    TokSheetName(Span<'s>),
    TokStartQuote(Span<'s>),
    TokStartSingleQuote(Span<'s>),
    TokString(Span<'s>),
    TokStringOp(Span<'s>),
}

impl<'s> TokenError<'s> {
    pub fn name(&self) -> &str {
        match self {
            TokenError::TokNomError(_) => "NomError",
            TokenError::TokNomFailure(_) => "NomFailure",
            TokenError::TokUnexpected(_, _) => "Unexpected",

            TokenError::TokAddOp(_) => "AddOp",
            TokenError::TokAlpha(_) => "Alpha",
            TokenError::TokBracketsClose(_) => "BracketsClose",
            TokenError::TokBracketsOpen(_) => "BracketsOpen",
            TokenError::TokCol(_) => "Col",
            TokenError::TokColon(_) => "Colon",
            TokenError::TokComparisonOp(_) => "CompOp",
            TokenError::TokDigit(_) => "Digit",
            TokenError::TokDollar(_) => "Dollar",
            TokenError::TokDollarDollar(_) => "DollarDollar",
            TokenError::TokDot(_) => "Dot",
            TokenError::TokEndQuote(_) => "QuoteEnd",
            TokenError::TokEndSingleQuote(_) => "SingleQuoteEnd",
            TokenError::TokFnName(_) => "FnName",
            TokenError::TokHash(_) => "SheetName",
            TokenError::TokHashtag(_) => "Hashtag",
            TokenError::TokIdentifier(_) => "DollarDollar",
            TokenError::TokMulOp(_) => "MulOp",
            TokenError::TokNumber(_) => "Number",
            TokenError::TokParenthesesClose(_) => "ParenthesesClose",
            TokenError::TokParenthesesOpen(_) => "ParenthesesOpen",
            TokenError::TokPostfixOp(_) => "PostfixOp",
            TokenError::TokPowOp(_) => "PowOp",
            TokenError::TokPrefixOp(_) => "PrefixOp",
            TokenError::TokRefConcatOp(_) => "RefConcatOp",
            TokenError::TokRefIntersectionOp(_) => "RefIntersectionOp",
            TokenError::TokReferenceOp(_) => "ReferenceOp",
            TokenError::TokRow(_) => "Row",
            TokenError::TokSemikolon(_) => "Semikolon",
            TokenError::TokSheetName(_) => "SheetName",
            TokenError::TokStartQuote(_) => "QuoteStart",
            TokenError::TokStartSingleQuote(_) => "SingleQuoteStart",
            TokenError::TokString(_) => "String",
            TokenError::TokStringOp(_) => "StringOp",
        }
    }

    pub fn span(&self) -> &Span<'s> {
        match self {
            TokenError::TokNomError(s) => s,
            TokenError::TokNomFailure(s) => s,
            TokenError::TokUnexpected(s, _) => s,

            TokenError::TokAddOp(s) => s,
            TokenError::TokAlpha(s) => s,
            TokenError::TokBracketsClose(s) => s,
            TokenError::TokBracketsOpen(s) => s,
            TokenError::TokCol(s) => s,
            TokenError::TokColon(s) => s,
            TokenError::TokComparisonOp(s) => s,
            TokenError::TokDigit(s) => s,
            TokenError::TokDollar(s) => s,
            TokenError::TokDollarDollar(s) => s,
            TokenError::TokDot(s) => s,
            TokenError::TokEndQuote(s) => s,
            TokenError::TokEndSingleQuote(s) => s,
            TokenError::TokFnName(s) => s,
            TokenError::TokHash(s) => s,
            TokenError::TokHashtag(s) => s,
            TokenError::TokIdentifier(s) => s,
            TokenError::TokMulOp(s) => s,
            TokenError::TokNumber(s) => s,
            TokenError::TokParenthesesClose(s) => s,
            TokenError::TokParenthesesOpen(s) => s,
            TokenError::TokPostfixOp(s) => s,
            TokenError::TokPowOp(s) => s,
            TokenError::TokPrefixOp(s) => s,
            TokenError::TokRefConcatOp(s) => s,
            TokenError::TokRefIntersectionOp(s) => s,
            TokenError::TokReferenceOp(s) => s,
            TokenError::TokRow(s) => s,
            TokenError::TokSemikolon(s) => s,
            TokenError::TokSheetName(s) => s,
            TokenError::TokStartQuote(s) => s,
            TokenError::TokStartSingleQuote(s) => s,
            TokenError::TokString(s) => s,
            TokenError::TokStringOp(s) => s,
        }
    }
}

impl<'s> TokenError<'s> {
    pub fn unexpected(e: TokenError<'s>) -> TokenError<'s> {
        TokenError::TokUnexpected(*e.span(), Box::new(e))
    }

    pub fn nom(e: nom::Err<nom::error::Error<Span<'s>>>) -> TokenError<'s> {
        match e {
            nom::Err::Error(e) => TokenError::TokNomError(e.input),
            nom::Err::Failure(e) => TokenError::TokNomFailure(e.input),
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }
}

impl<'s> Debug for TokenError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.name())?;
        let span = self.span();
        write!(
            f,
            "{}::{}:{} '{}'",
            span.location_offset(),
            span.location_line(),
            span.get_column(),
            span.fragment()
        )?;
        Ok(())
    }
}

impl<'s> Display for TokenError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::TokUnexpected(s, t) => write!(f, "{} '{}' {:?}", self.name(), s, t),
            _ => write!(f, "{} '{}'", self.name(), self.span()),
        }
    }
}

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
        Err(nom::Err::Error(e)) if e.code == Char || e.code == OneOf => {
            Err(TokenError::TokNumber(rest))
        }
        Err(e) => Err(TokenError::nom(e)),
    }
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
        Err(nom::Err::Error(e)) if e.code == Char => return Err(TokenError::TokStartQuote(rest)),
        Err(e) => {
            return Err(TokenError::nom(e));
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
            return Err(TokenError::TokString(rest));
        }
        Err(e) => {
            return Err(TokenError::nom(e));
        }
    };

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(TokenError::TokEndQuote(rest));
        }
        Err(e) => {
            return Err(TokenError::nom(e));
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
pub fn fn_name<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match fn_name_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(TokenError::TokFnName(rest))
        }
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse comparison operators.
pub fn comparison_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match comparison_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokComparisonOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse string operators.
pub fn string_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match string_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokStringOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse reference operators.
pub fn reference_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match reference_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokReferenceOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse reference intersection.
pub fn ref_intersection_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match ref_intersection_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokRefIntersectionOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse concat operator..
pub fn ref_concat_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match ref_concat_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokRefConcatOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$$")(rest).is_ok()
}

/// Parse separator char for function args.
pub fn dollar_dollar<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokDollarDollar(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn lah_dollar<'a>(rest: Span<'a>) -> bool {
    tag::<&str, Span<'a>, nom::error::Error<_>>("$")(rest).is_ok()
}

/// Parse separator char for function args.
pub fn dollar<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokDollar(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Hashtag
pub fn hashtag<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match hashtag_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokHashtag(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn semikolon<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match semikolon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokSemikolon(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Lookahead for a dot.
pub fn lah_dot<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('.')(i).is_ok()
}

/// Parse dot
pub fn dot<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match dot_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokDot(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse colon
pub fn colon<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match colon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokColon(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Lookahead for opening parentheses.
pub fn lah_parentheses_open<'a>(i: Span<'a>) -> bool {
    nchar::<Span<'a>, nom::error::Error<_>>('(')(i).is_ok()
}

/// Parse open parentheses.
pub fn parentheses_open<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match parentheses_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokParenthesesOpen(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse closing parentheses.
pub fn parentheses_close<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match parentheses_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokParenthesesClose(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse open brackets.
pub fn brackets_open<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match brackets_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokBracketsOpen(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Parse closing brackets.
pub fn brackets_close<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match brackets_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokBracketsClose(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Tries to parses any additive operator.
pub fn add_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match add_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokAddOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Tries to parses any multiplicative operator.
pub fn mul_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match mul_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokMulOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Tries to parses the power operator.
pub fn pow_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match pow_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokPowOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Lookahead for any prefix operator.
pub fn lah_prefix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("+-")(i).is_ok()
}

/// Tries to ast any prefix operator.
pub fn prefix_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match prefix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokPrefixOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

/// Tries to ast any postfix operator.
pub fn lah_postfix_op<'a>(i: Span<'a>) -> bool {
    one_of::<Span<'a>, _, nom::error::Error<_>>("%")(i).is_ok()
}

/// Tries to ast any postfix operator.
pub fn postfix_op<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match postfix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokPostfixOp(rest)),
        Err(e) => Err(TokenError::nom(e)),
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
pub fn identifier<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    match identifier_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(TokenError::TokIdentifier(rest))
        }
        Err(e) => Err(TokenError::nom(e)),
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
pub fn sheet_name(rest: Span<'_>) -> TokenResult<'_, (Option<Span<'_>>, Span<'_>)> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, abs)) => (rest, abs),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(TokenError::TokDollar(rest));
        }
        Err(e) => return Err(TokenError::nom(e)),
    };

    let (rest, name) = match single_quoted(rest) {
        Ok((rest, name)) => (rest, Some(name)),
        Err(TokenError::TokStartSingleQuote(_)) => (rest, None),
        Err(e @ TokenError::TokString(_)) => return Err(e),
        Err(e @ TokenError::TokEndSingleQuote(_)) => return Err(e),
        Err(e) => return Err(TokenError::unexpected(e)),
    };

    let (rest, name) = if let Some(name) = name {
        (rest, name)
    } else {
        match sheet_name_nom(rest) {
            Ok((rest, tok)) => (rest, tok),
            Err(nom::Err::Error(e)) if e.code == NoneOf => {
                return Err(TokenError::TokSheetName(rest));
            }
            Err(e) => return Err(TokenError::nom(e)),
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
        Err(e @ TokenError::TokStartSingleQuote(_)) => return Err(e),
        Err(e @ TokenError::TokString(_)) => return Err(e),
        Err(e @ TokenError::TokEndSingleQuote(_)) => return Err(e),
        Err(e) => return Err(TokenError::unexpected(e)),
    };

    Ok((rest, (abs, name)))
}

// Source ::= "'" IRI "'" "#"
/// IRI
pub fn iri(rest: Span<'_>) -> TokenResult<'_, Span<'_>> {
    let (rest, iri) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e @ TokenError::TokStartSingleQuote(_)) => return Err(e),
        Err(e @ TokenError::TokString(_)) => return Err(e),
        Err(e @ TokenError::TokEndSingleQuote(_)) => return Err(e),
        Err(e) => return Err(TokenError::unexpected(e)),
    };
    let (rest,) = match hashtag_nom(rest) {
        Ok((rest, _hash)) => (rest,),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(TokenError::TokHash(rest));
        }
        Err(e) => return Err(TokenError::nom(e)),
    };

    Ok((rest, iri))
}

// Row ::= '$'? [1-9] [0-9]*
/// Row label
pub fn row(rest: Span<'_>) -> TokenResult<'_, (Option<Span<'_>>, Span<'_>)> {
    match row_nom(rest) {
        Ok((rest, (abs, row))) => Ok((rest, (abs, row))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokDollar(rest)),
        Err(nom::Err::Error(e)) if e.code == OneOf => Err(TokenError::TokDigit(rest)),
        Err(nom::Err::Error(e)) if e.code == Many1 => Err(TokenError::TokDigit(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

// Column ::= '$'? [A-Z]+
/// Column label
pub fn col(rest: Span<'_>) -> TokenResult<'_, (Option<Span<'_>>, Span<'_>)> {
    match col_nom(rest) {
        Ok((rest, (abs, col))) => Ok((rest, (abs, col))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(TokenError::TokDollar(rest)),
        Err(nom::Err::Error(e)) if e.code == Alpha => Err(TokenError::TokAlpha(rest)),
        Err(e) => Err(TokenError::nom(e)),
    }
}

// SingleQuoted ::= "'" ([^'] | "''")+ "'"
/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn single_quoted<'a>(rest: Span<'a>) -> TokenResult<'a, Span<'a>> {
    const QUOTE: char = '\'';

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(TokenError::TokStartSingleQuote(rest))
        }
        Err(e) => return Err(TokenError::nom(e)),
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
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 => return Err(TokenError::TokString(rest)),
        Err(nom::Err::Error(e)) if e.code == Char => return Err(TokenError::TokString(rest)),
        Err(e) => return Err(TokenError::nom(e)),
    };

    let rest = match nchar::<Span<'a>, nom::error::Error<Span<'a>>>(QUOTE)(rest) {
        Ok((rest, _)) => rest,
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(TokenError::TokEndSingleQuote(rest))
        }
        Err(e) => return Err(TokenError::nom(e)),
    };

    Ok((rest, string))
}
