//!
//! Contains all token parsers. Operates on and returns only spans.
//!

use crate::error::OFCode::*;
use crate::error::{OFCode, OFParserError};
use nom::combinator::opt;
use nom::error::ErrorKind::*;
use nom::InputTake;

// reexport the lah* fn's. don't want any nomtokens imported anywhere else.
// Still need this pub for the tests to run, but now it's possible to turn
// the pub of for a compile run.
pub mod nomtokens;
use crate::iparse::span::span_union;
use crate::iparse::{ParseResult, Span};
use nomtokens::{
    add_op_nom, brackets_close_nom, brackets_open_nom, col_nom, colon_nom, comparison_op_nom,
    dollar_dollar_nom, dollar_nom, dot_nom, double_quote_nom, double_string_nom, fn_name_nom,
    hashtag_nom, identifier_nom, mul_op_nom, number_nom, parentheses_close_nom,
    parentheses_open_nom, postfix_op_nom, pow_op_nom, prefix_op_nom, ref_concat_op_nom,
    ref_intersection_op_nom, reference_op_nom, row_nom, semikolon_nom, sheet_name_nom,
    single_quote_nom, single_string_nom, string_op_nom,
};
pub use nomtokens::{
    eat_space, lah_dollar_dollar, lah_dot, lah_fn_name, lah_identifier, lah_iri, lah_number,
    lah_parentheses_open, lah_prefix_op, lah_sheet_name, lah_string,
};

/// Result type for token parsers.
pub type TokenResult<'s> = ParseResult<'s, Span<'s>, OFCode>;

/// Returns an empty token. But still technically a slice of the given span.
pub fn empty(i: Span<'_>) -> Span<'_> {
    i.take(0)
}

// Number ::= StandardNumber | '.' [0-9]+ ([eE] [-+]? [0-9]+)?
// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
/// Any number.
pub fn number(rest: Span<'_>) -> TokenResult<'_> {
    match number_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Char || e.code == OneOf => {
            Err(OFParserError::number(rest))
        }
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Standard string
pub fn string(rest: Span<'_>) -> TokenResult<'_> {
    let (rest, first_quote) = match double_quote_nom(rest) {
        Ok((rest, quote)) => (rest, quote),
        Err(nom::Err::Error(e)) if e.code == Char => return Err(OFParserError::start_quote(rest)),
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let (rest, _string) = match double_string_nom(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == Char => {
            return Err(OFParserError::string(rest));
        }
        Err(e) => {
            return Err(OFParserError::nom(e));
        }
    };

    let (rest, last_quote) = match double_quote_nom(rest) {
        Ok((rest, quote)) => (rest, quote),
        Err(nom::Err::Error(e)) if e.code == Char => return Err(OFParserError::end_quote(rest)),
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let token = unsafe { span_union(first_quote, last_quote) };
    Ok((rest, token))
}

// LetterXML (LetterXML | DigitXML | '_' | '.' | CombiningCharXML)*
/// Function name.
pub fn fn_name(rest: Span<'_>) -> TokenResult<'_> {
    match fn_name_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(OFParserError::fn_name(rest))
        }
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse comparison operators.
pub fn comparison_op(rest: Span<'_>) -> TokenResult<'_> {
    match comparison_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::comp_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse string operators.
pub fn string_op(rest: Span<'_>) -> TokenResult<'_> {
    match string_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::string_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse reference operators.
pub fn reference_op(rest: Span<'_>) -> TokenResult<'_> {
    match reference_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::ref_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse reference intersection.
pub fn ref_intersection_op(rest: Span<'_>) -> TokenResult<'_> {
    match ref_intersection_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::ref_intersect_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse concat operator..
pub fn ref_concat_op(rest: Span<'_>) -> TokenResult<'_> {
    match ref_concat_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::ref_concat_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn dollar_dollar(rest: Span<'_>) -> TokenResult<'_> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::dollardollar(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn dollar(rest: Span<'_>) -> TokenResult<'_> {
    match dollar_dollar_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::dollar(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Hashtag
pub fn hashtag(rest: Span<'_>) -> TokenResult<'_> {
    match hashtag_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::hashtag(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse separator char for function args.
pub fn semikolon(rest: Span<'_>) -> TokenResult<'_> {
    match semikolon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::semikolon(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse dot
pub fn dot(rest: Span<'_>) -> TokenResult<'_> {
    match dot_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::dot(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse colon
pub fn colon(rest: Span<'_>) -> TokenResult<'_> {
    match colon_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::colon(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse open parentheses.
pub fn parentheses_open(rest: Span<'_>) -> TokenResult<'_> {
    match parentheses_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::parens_open(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse closing parentheses.
pub fn parentheses_close(rest: Span<'_>) -> TokenResult<'_> {
    match parentheses_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::parens_close(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse open brackets.
pub fn brackets_open(rest: Span<'_>) -> TokenResult<'_> {
    match brackets_open_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::brackets_open(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Parse closing brackets.
pub fn brackets_close(rest: Span<'_>) -> TokenResult<'_> {
    match brackets_close_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::brackets_close(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Tries to parses any additive operator.
pub fn add_op(rest: Span<'_>) -> TokenResult<'_> {
    match add_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::add_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Tries to parses any multiplicative operator.
pub fn mul_op(rest: Span<'_>) -> TokenResult<'_> {
    match mul_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::mul_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Tries to parses the power operator.
pub fn pow_op(rest: Span<'_>) -> TokenResult<'_> {
    match pow_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::pow_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Tries to ast any prefix operator.
pub fn prefix_op(rest: Span<'_>) -> TokenResult<'_> {
    match prefix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::prefix_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

/// Tries to ast any postfix operator.
pub fn postfix_op(rest: Span<'_>) -> TokenResult<'_> {
    match postfix_op_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::postfix_op(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

// Identifier ::= ( LetterXML
//                      (LetterXML | DigitXML | '_' | CombiningCharXML)* )
//                      - ( [A-Za-z]+[0-9]+ )  # means no cell reference
//                      - ([Tt][Rr][Uu][Ee]) - ([Ff][Aa][Ll][Ss][Ee]) # true and false
/// Identifier.
pub fn identifier(rest: Span<'_>) -> TokenResult<'_> {
    match identifier_nom(rest) {
        Ok((rest, tok)) => Ok((rest, tok)),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 || e.code == TakeWhileMN => {
            Err(OFParserError::identifier(rest))
        }
        Err(e) => Err(OFParserError::nom(e)),
    }
}

// SheetName ::= QuotedSheetName | '$'? [^\]\. #$']+
// QuotedSheetName ::= '$'? SingleQuoted
/// Sheet name
pub fn sheet_name(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>), OFCode> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, abs)) => (rest, abs),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(OFParserError::dollar(rest));
        }
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let (rest, name) = match single_quoted(rest) {
        Ok((rest, name)) => (rest, Some(name)),
        Err(e) if e.code == OFCSingleQuoteStart => (rest, None),
        Err(e) => return Err(e),
    };

    let (rest, name) = if let Some(name) = name {
        (rest, name)
    } else {
        match sheet_name_nom(rest) {
            Ok((rest, tok)) => (rest, tok),
            Err(nom::Err::Error(e)) if e.code == NoneOf => {
                return Err(OFParserError::sheet_name(rest));
            }
            Err(e) => return Err(OFParserError::nom(e)),
        }
    };

    Ok((rest, (abs, name)))
}

// QuotedSheetName ::= '$'? SingleQuoted
/// Sheet name
pub fn quoted_sheet_name(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>), OFCode> {
    let (rest, abs) = match opt(dollar_nom)(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let rest = eat_space(rest);

    let (rest, name) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(e),
    };

    Ok((rest, (abs, name)))
}

// Source ::= "'" IRI "'" "#"
/// IRI
pub fn iri(rest: Span<'_>) -> TokenResult<'_> {
    let (rest, iri) = match single_quoted(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(e) => return Err(e),
    };

    let rest = eat_space(rest);

    let (rest,) = match hashtag_nom(rest) {
        Ok((rest, _hash)) => (rest,),
        Err(nom::Err::Error(e)) if e.code == Tag => {
            return Err(OFParserError::hashtag(rest));
        }
        Err(e) => return Err(OFParserError::nom(e)),
    };

    Ok((rest, iri))
}

// Row ::= '$'? [1-9] [0-9]*
/// Row label
pub fn row(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>), OFCode> {
    match row_nom(rest) {
        Ok((rest, (abs, row))) => Ok((rest, (abs, row))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::dollar(rest)),
        Err(nom::Err::Error(e)) if e.code == OneOf => Err(OFParserError::digit(rest)),
        Err(nom::Err::Error(e)) if e.code == Many1 => Err(OFParserError::digit(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

// Column ::= '$'? [A-Z]+
/// Column label
pub fn col(rest: Span<'_>) -> ParseResult<'_, (Option<Span<'_>>, Span<'_>), OFCode> {
    match col_nom(rest) {
        Ok((rest, (abs, col))) => Ok((rest, (abs, col))),
        Err(nom::Err::Error(e)) if e.code == Tag => Err(OFParserError::dollar(rest)),
        Err(nom::Err::Error(e)) if e.code == Alpha => Err(OFParserError::alpha(rest)),
        Err(e) => Err(OFParserError::nom(e)),
    }
}

// SingleQuoted ::= "'" ([^'] | "''")+ "'"
/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn single_quoted(rest: Span<'_>) -> TokenResult<'_> {
    let (rest, first_quote) = match single_quote_nom(rest) {
        Ok((rest, quote)) => (rest, quote),
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(OFParserError::start_single_quote(rest))
        }
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let (rest, _string) = match single_string_nom(rest) {
        Ok((rest, tok)) => (rest, tok),
        Err(nom::Err::Error(e)) if e.code == TakeWhile1 => return Err(OFParserError::string(rest)),
        Err(nom::Err::Error(e)) if e.code == Char => return Err(OFParserError::string(rest)),
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let (rest, last_quote) = match single_quote_nom(rest) {
        Ok((rest, quote)) => (rest, quote),
        Err(nom::Err::Error(e)) if e.code == Char => {
            return Err(OFParserError::end_single_quote(rest))
        }
        Err(e) => return Err(OFParserError::nom(e)),
    };

    let token = unsafe { span_union(first_quote, last_quote) };
    Ok((rest, token))
}
