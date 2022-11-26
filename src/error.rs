pub mod debug_error;

use crate::ast::conv::{ParseColnameError, ParseRownameError};
use crate::ast::Span;
use crate::error::OFCode::*;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};

pub struct ParseOFError<'s> {
    pub code: OFCode,
    pub span: Span<'s>,
    pub tracing: bool,
    pub suggest2: Vec<Suggest2<'s>>,
    pub expect2: Vec<Expect2<'s>>,
}

impl<'s> ParseOFError<'s> {
    pub fn new(code: OFCode, span: Span<'s>) -> Self {
        Self {
            code,
            span,
            tracing: false,
            suggest2: Vec::new(),
            expect2: Vec::new(),
        }
    }

    pub fn is_special(&self) -> bool {
        self.code.is_special()
    }

    pub fn is_parser(&self) -> bool {
        !self.code.is_special()
    }

    pub fn is_expected(&self, code: OFCode) -> bool {
        for exp in &self.expect2 {
            if exp.code == code {
                return true;
            }
        }
        return false;
    }

    pub fn expect_str(&self) -> String {
        let mut buf = String::new();
        for exp in &self.expect2 {
            let _ = write!(buf, "{} ", exp.code);
        }
        buf
    }

    /// Create a ParseOFError from a nom::Err
    pub fn nom(e: nom::Err<nom::error::Error<Span<'s>>>) -> ParseOFError<'s> {
        match e {
            nom::Err::Error(e) => ParseOFError::new(OFCNomError, e.input),
            nom::Err::Failure(e) => ParseOFError::new(OFCNomFailure, e.input),
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParseIncomplete, span)
    }
}

// Simple mappings
impl<'s> ParseOFError<'s> {
    pub fn parens(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParentheses, span)
    }

    pub fn fn_call(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCFnCall, span)
    }

    pub fn elementary(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCElementary, span)
    }

    pub fn string_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCStringOp, span)
    }

    pub fn ref_intersect_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefIntersectOp, span)
    }

    pub fn ref_concat_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefConcatOp, span)
    }

    pub fn ref_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefOp, span)
    }

    pub fn identifier(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCIdentifier, span)
    }

    pub fn start_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCQuoteStart, span)
    }

    pub fn end_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCQuoteEnd, span)
    }

    pub fn start_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoteStart, span)
    }

    pub fn end_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoteEnd, span)
    }

    pub fn reference(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCReference, span)
    }

    pub fn iri(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCIri, span)
    }

    pub fn sheet_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSheetName, span)
    }

    pub fn hashtag(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCHashtag, span)
    }

    pub fn dot(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDot, span)
    }

    pub fn parens_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParenthesesOpen, span)
    }

    pub fn parens_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParenthesesClose, span)
    }

    pub fn brackets_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCBracketsOpen, span)
    }

    pub fn brackets_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCBracketsClose, span)
    }

    pub fn semikolon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSemikolon, span)
    }

    pub fn cell_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCellRange, span)
    }

    pub fn col_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCColRange, span)
    }

    pub fn row_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRowRange, span)
    }

    pub fn string(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCString, span)
    }

    pub fn number(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCNumber, span)
    }

    pub fn fn_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCFnName, span)
    }

    pub fn comp_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCompOp, span)
    }

    pub fn prefix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPrefixOp, span)
    }

    pub fn postfix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPostfixOp, span)
    }

    pub fn add_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCAddOp, span)
    }

    pub fn mul_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCMulOp, span)
    }

    pub fn pow_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPowOp, span)
    }

    pub fn dollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDollar, span)
    }

    pub fn dollardollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDollarDollar, span)
    }

    pub fn single_quoted(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoted, span)
    }

    pub fn alpha(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCAlpha, span)
    }

    pub fn col(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCol, span)
    }

    pub fn row(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRow, span)
    }

    pub fn digit(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDigit, span)
    }

    pub fn colon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCColon, span)
    }
}

impl<'s> Debug for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            debug_error::debug_parse_of_error_multi(f, self)
        } else {
            debug_error::debug_parse_of_error_single(f, self)
        }
    }
}

impl<'s> Display for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        debug_error::display_parse_of_error(f, self)
    }
}

impl<'s> Error for ParseOFError<'s> {}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub enum OFCode {
    #[default]
    OFCDefault,

    /// Nom error.
    OFCNomError,
    /// Nom failure.
    OFCNomFailure,
    /// Parsing didn't parse all of the string.
    OFCParseIncomplete,

    OFCAdd,
    OFCAddOp,
    OFCAlpha,
    OFCBracketsClose,
    OFCBracketsOpen,
    OFCCellRange,
    OFCCellRef,
    OFCCol,
    OFCColRange,
    OFCColname,
    OFCColon,
    OFCComp,
    OFCCompOp,
    OFCDigit,
    OFCDollar,
    OFCDollarDollar,
    OFCDot,
    OFCElementary,
    OFCExpr,
    OFCFnCall,
    OFCFnName,
    OFCHashtag,
    OFCIdentifier,
    OFCIri,
    OFCMul,
    OFCMulOp,
    OFCNamed,
    OFCNumber,
    OFCParentheses,
    OFCParenthesesClose,
    OFCParenthesesOpen,
    OFCPostfix,
    OFCPostfixOp,
    OFCPow,
    OFCPowOp,
    OFCPrefix,
    OFCPrefixOp,
    OFCQuoteEnd,
    OFCQuoteStart,
    OFCRefConcatOp,
    OFCRefIntersectOp,
    OFCRefOp,
    OFCReference,
    OFCRow,
    OFCRowRange,
    OFCRowname,
    OFCSemikolon,
    OFCSeparator,
    OFCSheetName,
    OFCSingleQuoteEnd,
    OFCSingleQuoteStart,
    OFCSingleQuoted,
    OFCString,
    OFCStringOp,
}

impl OFCode {
    pub fn is_special(&self) -> bool {
        match self {
            OFCDefault | OFCNomError | OFCNomFailure | OFCParseIncomplete => true,
            _ => false,
        }
    }
}

impl Display for OFCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        write!(buf, "{:?}", self)?;
        write!(f, "{}", buf.strip_prefix("OFC").unwrap())?;
        Ok(())
    }
}

/// Adds a span as location and converts the error to our own type..
pub trait LocateError<'s, T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>>;
}

impl<'s, T> LocateError<'s, T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(OFCRowname, span)),
        }
    }
}

impl<'s, T> LocateError<'s, T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(OFCColname, span)),
        }
    }
}

// Suggest2 --------------------------------------------------------------

#[derive(Clone)]
pub struct Suggest2<'s> {
    pub code: OFCode,
    pub span: Span<'s>,
    pub parents: Vec<OFCode>,
}

impl<'s> Debug for Suggest2<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        if !self.parents.is_empty() {
            write!(f, "<")?;
            for p in &self.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }
}

// Expect2 ---------------------------------------------------------------

#[derive(Clone)]
pub struct Expect2<'s> {
    pub code: OFCode,
    pub span: Span<'s>,
    pub parents: Vec<OFCode>,
}

impl<'s> Debug for Expect2<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        if !self.parents.is_empty() {
            write!(f, "<")?;
            for p in &self.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }
}
