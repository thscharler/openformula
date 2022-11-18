use crate::ast::tokens::TokenError;
use crate::ast::tracer::Suggest;
use crate::ast::Span;
use crate::error::OFError::*;
use spreadsheet_ods_cellref::parser::{ParseColnameError, ParseRownameError};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::mem;

#[derive(Debug)]
pub struct ParseOFError<'s> {
    pub code: OFError,
    pub span: Span<'s>,
    pub tok: Vec<TokenError<'s>>,
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
    /// CellRange parsing error.
    ErrCellRange,
    ErrCellRef,
    ErrCol,
    /// ColRange parsing error.
    ErrColRange,
    /// Error converting a column-name to an u32.
    ErrColname,
    ErrColon,
    ErrComparisonOp,
    ErrDigit,
    ErrDollar,
    ErrDollarDollar,
    /// Elementary expression fails.
    ErrElementary,
    ErrExpr,
    /// Error when parsing a function call
    ErrFnCall,
    ErrFnName,
    ErrIri,
    ErrMulOp,
    /// Error when parsing a named expression
    ErrNamed,
    ErrNumber,
    /// Error when parsing an expression in parentheses.
    ErrParentheses,
    ErrPostfixOp,
    ErrPowOp,
    ErrPrefixOp,
    /// Reference expression fails.
    ErrReference,
    ErrRow,
    /// Rowrange parsing error.
    ErrRowRange,
    /// Error converting a row-name to an u32.
    ErrRowname,
    ErrString,
    ErrSheetName,
    ErrSingleQuoted,
}

impl OFError {
    pub fn expect(&self) -> Option<Suggest> {
        match self {
            ErrNomError => None,
            ErrNomFailure => None,
            ErrUnexpected => None,
            ErrParseIncomplete => None,
            ErrAddOp => Some(Suggest::AddOp),
            ErrAlpha => Some(Suggest::Alpha),
            ErrCellRange => Some(Suggest::CellRange),
            ErrCol => Some(Suggest::Col),
            ErrColRange => Some(Suggest::ColRange),
            ErrColname => Some(Suggest::Colname),
            ErrColon => Some(Suggest::Colon),
            ErrComparisonOp => Some(Suggest::CompOp),
            ErrDigit => Some(Suggest::Digit),
            ErrDollar => Some(Suggest::Dollar),
            ErrDollarDollar => Some(Suggest::DollarDollar),
            ErrElementary => Some(Suggest::Elementary),
            ErrFnCall => Some(Suggest::FnCall),
            ErrFnName => Some(Suggest::FnName),
            ErrIri => Some(Suggest::Iri),
            ErrMulOp => Some(Suggest::MulOp),
            ErrNamed => Some(Suggest::Named),
            ErrNumber => Some(Suggest::Number),
            ErrParentheses => Some(Suggest::Parentheses),
            ErrPostfixOp => Some(Suggest::PostfixOp),
            ErrPowOp => Some(Suggest::PowOp),
            ErrPrefixOp => Some(Suggest::PrefixOp),
            ErrReference => Some(Suggest::Reference),
            ErrRow => Some(Suggest::Row),
            ErrRowRange => Some(Suggest::RowRange),
            ErrRowname => Some(Suggest::Rowname),
            ErrString => Some(Suggest::StringContent),
            ErrSheetName => Some(Suggest::SheetName),
            ErrSingleQuoted => Some(Suggest::SingleQuoted),
            ErrCellRef => Some(Suggest::CellRef),
            ErrExpr => Some(Suggest::Expr),
        }
    }
}

impl<'s> ParseOFError<'s> {
    pub fn new(code: OFError, span: Span<'s>) -> Self {
        Self {
            code,
            span,
            tok: Vec::new(),
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

    /// Contains this token error.
    pub fn has_tok(&self, tok: &TokenError<'s>) -> bool {
        for t in &self.tok {
            return mem::discriminant(t) == mem::discriminant(&tok);
        }
        return false;
    }

    /// Return the causal token errors.
    pub fn tok(&self) -> &Vec<TokenError<'s>> {
        &self.tok
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
    pub fn unexpected(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrUnexpected, span)
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParseIncomplete, span)
    }

    pub fn parens(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrParentheses, span)
    }

    pub fn fn_call(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrFnCall, span)
    }

    /// Elementary
    pub fn elementary(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrElementary, span)
    }

    /// Reference
    pub fn reference(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrReference, span)
    }

    /// Iri
    pub fn iri(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrIri, span)
    }

    /// Sheet name
    pub fn sheet_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrSheetName, span)
    }

    /// CellRange variant.
    pub fn cell_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrCellRange, span)
    }

    /// ColRange variant.
    pub fn col_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(ErrColRange, span)
    }

    /// RowRange variant.
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
        ParseOFError::new(ErrComparisonOp, span)
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
        if !self.tok.is_empty() {
            write!(f, " caused by ")?;
            for tok in &self.tok {
                write!(f, "{:?}, ", tok)?;
            }
        }
        Ok(())
    }
}
