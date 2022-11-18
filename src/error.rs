use crate::ast::tracer::Suggest;
use crate::ast::Span;
use crate::error::OFError::*;
use spreadsheet_ods_cellref::parser::{ParseColnameError, ParseRownameError};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub struct ParseOFError<'s> {
    pub code: OFError,
    pub span: Span<'s>,
    pub unexpected: Option<Box<ParseOFError<'s>>>,
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum OFError {
    /// Nom ast error.
    ErrNomError,
    /// Nom failure.
    ErrNomFailure,
    /// Unexpected token.
    ErrUnexpected,
    /// Parsing didn't parse all of the string.
    ErrParseIncomplete,

    ErrAddOp,
    ErrAlpha,
    ErrBracketsClose,
    ErrBracketsOpen,
    ErrCellRange,
    ErrCellRef,
    ErrCol,
    ErrColRange,
    ErrColname,
    ErrColon,
    ErrCompOp,
    ErrDigit,
    ErrDollar,
    ErrDollarDollar,
    ErrDot,
    ErrElementary,
    ErrExpr,
    ErrFnCall,
    ErrFnName,
    ErrHashtag,
    ErrIdentifier,
    ErrIri,
    ErrMulOp,
    ErrNamed,
    ErrNumber,
    ErrParentheses,
    ErrParenthesesClose,
    ErrParenthesesOpen,
    ErrPostfixOp,
    ErrPowOp,
    ErrPrefixOp,
    ErrQuoteEnd,
    ErrQuoteStart,
    ErrRefConcatOp,
    ErrRefIntersectOp,
    ErrRefOp,
    ErrReference,
    ErrRow,
    ErrRowRange,
    ErrRowname,
    ErrSemikolon,
    ErrSheetName,
    ErrSingleQuoteEnd,
    ErrSingleQuoteStart,
    ErrSingleQuoted,
    ErrString,
    ErrStringOp,
}

impl OFError {
    pub fn expect(&self) -> Option<Suggest> {
        match self {
            ErrAddOp => Some(Suggest::AddOp),
            ErrAlpha => Some(Suggest::Alpha),
            ErrBracketsClose => Some(Suggest::BracketsClose),
            ErrBracketsOpen => Some(Suggest::BracketsOpen),
            ErrCellRange => Some(Suggest::CellRange),
            ErrCellRef => Some(Suggest::CellRef),
            ErrCol => Some(Suggest::Col),
            ErrColRange => Some(Suggest::ColRange),
            ErrColname => Some(Suggest::Colname),
            ErrColon => Some(Suggest::Colon),
            ErrCompOp => Some(Suggest::CompOp),
            ErrDigit => Some(Suggest::Digit),
            ErrDollar => Some(Suggest::Dollar),
            ErrDollarDollar => Some(Suggest::DollarDollar),
            ErrDot => Some(Suggest::Dot),
            ErrElementary => Some(Suggest::Elementary),
            ErrExpr => Some(Suggest::Expr),
            ErrFnCall => Some(Suggest::FnCall),
            ErrFnName => Some(Suggest::FnName),
            ErrHashtag => Some(Suggest::Hashtag),
            ErrIdentifier => Some(Suggest::Identifier),
            ErrIri => Some(Suggest::Iri),
            ErrMulOp => Some(Suggest::MulOp),
            ErrNamed => Some(Suggest::Named),
            ErrNomError => None,
            ErrNomFailure => None,
            ErrNumber => Some(Suggest::Number),
            ErrParentheses => Some(Suggest::Parentheses),
            ErrParenthesesClose => Some(Suggest::ParenthesesClose),
            ErrParenthesesOpen => Some(Suggest::ParenthesesOpen),
            ErrParseIncomplete => None,
            ErrPostfixOp => Some(Suggest::PostfixOp),
            ErrPowOp => Some(Suggest::PowOp),
            ErrPrefixOp => Some(Suggest::PrefixOp),
            ErrQuoteEnd => Some(Suggest::QuoteEnd),
            ErrQuoteStart => Some(Suggest::QuoteStart),
            ErrRefConcatOp => Some(Suggest::RefConcatOp),
            ErrRefIntersectOp => Some(Suggest::RefIntersectOp),
            ErrRefOp => Some(Suggest::RefOp),
            ErrReference => Some(Suggest::Reference),
            ErrRow => Some(Suggest::Row),
            ErrRowRange => Some(Suggest::RowRange),
            ErrRowname => Some(Suggest::Rowname),
            ErrSemikolon => Some(Suggest::Semikolon),
            ErrSheetName => Some(Suggest::SheetName),
            ErrSingleQuoteEnd => Some(Suggest::SingleQuoteEnd),
            ErrSingleQuoteStart => Some(Suggest::SingleQuoteStart),
            ErrSingleQuoted => Some(Suggest::SingleQuoted),
            ErrString => Some(Suggest::StringContent),
            ErrStringOp => Some(Suggest::StringOp),
            ErrUnexpected => None,
        }
    }
}

impl<'s> ParseOFError<'s> {
    pub fn new(code: OFError, span: Span<'s>) -> Self {
        Self {
            code,
            span,
            unexpected: None,
        }
    }

    /// Return the error code.
    pub fn code(&self) -> OFError {
        self.code
    }

    /// Return the span.
    pub fn span(&self) -> &Span<'s> {
        &self.span
    }

    /// Create a ParseOFError from a nom::Err
    pub fn nom(e: nom::Err<nom::error::Error<Span<'s>>>) -> ParseOFError<'s> {
        match e {
            nom::Err::Error(e) => ParseOFError::new(ErrNomError, e.input),
            nom::Err::Failure(e) => ParseOFError::new(ErrNomFailure, e.input),
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }

    /// NomError variant.
    pub fn err(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrNomError, span)
    }

    /// NomFailure variant.
    pub fn fail(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrNomFailure, span)
    }

    /// Unexpected variant.
    pub fn unexpected(err: ParseOFError<'s>) -> ParseOFError<'s> {
        let mut new = ParseOFError::new(ErrUnexpected, err.span);
        new.unexpected = Some(Box::new(err));
        new
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParseIncomplete, span)
    }
}

// Simple mappings
impl<'s> ParseOFError<'s> {
    pub fn parens(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParentheses, span)
    }

    pub fn fn_call(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrFnCall, span)
    }

    pub fn elementary(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrElementary, span)
    }

    pub fn string_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrStringOp, span)
    }

    pub fn ref_intersect_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrRefIntersectOp, span)
    }

    pub fn ref_concat_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrRefConcatOp, span)
    }

    pub fn ref_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrRefOp, span)
    }

    pub fn identifier(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrIdentifier, span)
    }

    pub fn start_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrQuoteStart, span)
    }

    pub fn end_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrQuoteEnd, span)
    }

    pub fn start_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSingleQuoteStart, span)
    }

    pub fn end_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSingleQuoteEnd, span)
    }

    pub fn reference(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrReference, span)
    }

    pub fn iri(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrIri, span)
    }

    pub fn sheet_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSheetName, span)
    }

    pub fn hashtag(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrHashtag, span)
    }

    pub fn dot(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrDot, span)
    }

    pub fn parens_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParenthesesOpen, span)
    }

    pub fn parens_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParenthesesClose, span)
    }

    pub fn brackets_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrBracketsOpen, span)
    }

    pub fn brackets_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrBracketsClose, span)
    }

    pub fn semikolon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSemikolon, span)
    }

    pub fn cell_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrCellRange, span)
    }

    pub fn col_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrColRange, span)
    }

    pub fn row_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrRowRange, span)
    }

    pub fn string(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrString, span)
    }

    pub fn number(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrNumber, span)
    }

    pub fn fn_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrFnName, span)
    }

    pub fn comp_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrCompOp, span)
    }

    pub fn prefix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrPrefixOp, span)
    }

    pub fn postfix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrPostfixOp, span)
    }

    pub fn add_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrAddOp, span)
    }

    pub fn mul_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrMulOp, span)
    }

    pub fn pow_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrPowOp, span)
    }

    pub fn dollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrDollar, span)
    }

    pub fn dollardollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrDollarDollar, span)
    }

    pub fn single_quoted(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSingleQuoted, span)
    }

    pub fn alpha(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrAlpha, span)
    }

    pub fn col(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrCol, span)
    }

    pub fn row(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrRow, span)
    }

    pub fn digit(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrDigit, span)
    }

    pub fn colon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrColon, span)
    }
}

impl<'s> Error for ParseOFError<'s> {}

/// Adds a span as location and converts the error to our own type..
pub trait LocateError<'s, T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>>;
}

impl<'s, T> LocateError<'s, T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(ErrRowname, span)),
        }
    }
}

impl<'s, T> LocateError<'s, T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(ErrColname, span)),
        }
    }
}

impl<'s> Display for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.code)?;
        write!(
            f,
            "for span={}::{}:{} '{}'",
            self.span.location_offset(),
            self.span.location_line(),
            self.span.get_column(),
            self.span.fragment()
        )?;
        Ok(())
    }
}
