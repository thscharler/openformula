use crate::ast::Span;
use spreadsheet_ods_cellref::parser::{ParseColnameError, ParseRownameError};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
pub enum ParseOFError<'s> {
    /// Nom ast error.
    ErrNomError(Span<'s>),
    /// Nom failure.
    ErrNomFailure(Span<'s>),
    /// Unexpected token.
    ErrUnexpected(Span<'s>),
    /// Parsing didn't parse all of the string.
    ErrParseIncomplete(Span<'s>),

    ErrAddOp(Span<'s>),
    ErrAlpha(Span<'s>),
    /// CellRange parsing error.
    ErrCellRange(Span<'s>),
    ErrCol(Span<'s>),
    /// ColRange parsing error.
    ErrColRange(Span<'s>),
    /// Error converting a column-name to an u32.
    ErrColname(Span<'s>),
    ErrColon(Span<'s>),
    ErrComparisonOp(Span<'s>),
    ErrDigit(Span<'s>),
    ErrDollar(Span<'s>),
    /// Elementary expression fails.
    ErrElementary(Span<'s>),
    /// Error when parsing a function call
    ErrFnCall(Span<'s>),
    ErrFnName(Span<'s>),
    ErrIri(Span<'s>),
    ErrMulOp(Span<'s>),
    /// Error when parsing a named expression
    ErrNamed(Span<'s>),
    ErrNumber(Span<'s>),
    /// Error when parsing an expression in parenthesis.
    ErrParenthesis(Span<'s>),
    ErrPostfixOp(Span<'s>),
    ErrPowOp(Span<'s>),
    ErrPrefixOp(Span<'s>),
    /// Reference expression fails.
    ErrReference(Span<'s>),
    ErrRow(Span<'s>),
    /// Rowrange parsing error.
    ErrRowRange(Span<'s>),
    /// Error converting a row-name to an u32.
    ErrRowname(Span<'s>),
    ErrString(Span<'s>),
}

impl<'s> ParseOFError<'s> {
    /// Return the errspan of any variant.
    pub fn span(&self) -> &Span<'s> {
        match self {
            ParseOFError::ErrAddOp(s) => s,
            ParseOFError::ErrAlpha(s) => s,
            ParseOFError::ErrCellRange(s) => s,
            ParseOFError::ErrCol(s) => s,
            ParseOFError::ErrColRange(s) => s,
            ParseOFError::ErrColname(s) => s,
            ParseOFError::ErrColon(s) => s,
            ParseOFError::ErrComparisonOp(s) => s,
            ParseOFError::ErrDigit(s) => s,
            ParseOFError::ErrDollar(s) => s,
            ParseOFError::ErrElementary(s) => s,
            ParseOFError::ErrFnCall(s) => s,
            ParseOFError::ErrFnName(s) => s,
            ParseOFError::ErrMulOp(s) => s,
            ParseOFError::ErrNamed(s) => s,
            ParseOFError::ErrNomError(s) => s,
            ParseOFError::ErrNomFailure(s) => s,
            ParseOFError::ErrNumber(s) => s,
            ParseOFError::ErrParenthesis(s) => s,
            ParseOFError::ErrParseIncomplete(s) => s,
            ParseOFError::ErrPostfixOp(s) => s,
            ParseOFError::ErrPowOp(s) => s,
            ParseOFError::ErrPrefixOp(s) => s,
            ParseOFError::ErrReference(s) => s,
            ParseOFError::ErrRow(s) => s,
            ParseOFError::ErrRowRange(s) => s,
            ParseOFError::ErrRowname(s) => s,
            ParseOFError::ErrString(s) => s,
            ParseOFError::ErrUnexpected(s) => s,
            ParseOFError::ErrIri(s) => s,
        }
    }

    /// Format span in a more readable way.
    fn fmt_span(span: &Span<'s>, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "span={}::{}:{} '{}'",
            span.location_offset(),
            span.location_line(),
            span.get_column(),
            span.fragment()
        )
    }

    /// NomError variant.
    pub fn err(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrNomError(span)
    }

    /// NomFailure variant.
    pub fn fail(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrNomFailure(span)
    }

    /// Unexpected variant.
    pub fn unexpected(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrUnexpected(span)
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrParseIncomplete(span)
    }

    pub fn parens(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrParenthesis(span)
    }

    pub fn fn_call(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrFnCall(span)
    }

    /// Elementary
    pub fn elementary(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrElementary(span)
    }

    /// Reference
    pub fn reference(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrReference(span)
    }

    /// Iri
    pub fn iri(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrIri(span)
    }

    /// CellRange variant.
    pub fn cell_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrCellRange(span)
    }

    /// ColRange variant.
    pub fn col_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrColRange(span)
    }

    /// RowRange variant.
    pub fn row_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrRowRange(span)
    }

    pub fn string(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrString(span)
    }

    pub fn number(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrNumber(span)
    }

    pub fn fn_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrFnName(span)
    }

    pub fn comp_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrComparisonOp(span)
    }

    pub fn prefix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrPrefixOp(span)
    }

    pub fn postfix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrPostfixOp(span)
    }

    pub fn add_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrAddOp(span)
    }

    pub fn mul_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrMulOp(span)
    }

    pub fn pow_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrPowOp(span)
    }

    pub fn dollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrDollar(span)
    }

    pub fn alpha(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrAlpha(span)
    }

    pub fn col(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrCol(span)
    }

    pub fn row(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrRow(span)
    }

    pub fn digit(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrDigit(span)
    }

    pub fn colon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::ErrColon(span)
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
            Err(_) => Err(ParseOFError::ErrRowname(span)),
        }
    }
}

impl<'s, T> LocateError<'s, T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::ErrColname(span)),
        }
    }
}

impl<'s> Display for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseOFError::ErrAddOp(_) => write!(f, "AddOp ")?,
            ParseOFError::ErrAlpha(_) => write!(f, "Alpha ")?,
            ParseOFError::ErrCellRange(_) => write!(f, "CellRange ")?,
            ParseOFError::ErrCol(_) => write!(f, "Col ")?,
            ParseOFError::ErrColRange(_) => write!(f, "ColRange ")?,
            ParseOFError::ErrColname(_) => write!(f, "ParseColname ")?,
            ParseOFError::ErrColon(_) => write!(f, "Colon ")?,
            ParseOFError::ErrComparisonOp(_) => write!(f, "ComparisonOp ")?,
            ParseOFError::ErrDigit(_) => write!(f, "Digit ")?,
            ParseOFError::ErrDollar(_) => write!(f, "Dollar ")?,
            ParseOFError::ErrElementary(_) => write!(f, "Elementary ")?,
            ParseOFError::ErrFnCall(_) => write!(f, "FnCall ")?,
            ParseOFError::ErrFnName(_) => write!(f, "FnName ")?,
            ParseOFError::ErrMulOp(_) => write!(f, "MulOp ")?,
            ParseOFError::ErrNamed(_) => write!(f, "Named ")?,
            ParseOFError::ErrNomError(_) => write!(f, "NomError ")?,
            ParseOFError::ErrNomFailure(_) => write!(f, "NomFailure ")?,
            ParseOFError::ErrNumber(_) => write!(f, "Number ")?,
            ParseOFError::ErrParenthesis(_) => write!(f, "Parenthesis ")?,
            ParseOFError::ErrParseIncomplete(_) => write!(f, "ParseIncomplete ")?,
            ParseOFError::ErrPostfixOp(_) => write!(f, "PostfixOp ")?,
            ParseOFError::ErrPowOp(_) => write!(f, "PowOp ")?,
            ParseOFError::ErrPrefixOp(_) => write!(f, "PrefixOp ")?,
            ParseOFError::ErrReference(_) => write!(f, "Reference ")?,
            ParseOFError::ErrRow(_) => write!(f, "Row ")?,
            ParseOFError::ErrRowRange(_) => write!(f, "RowRange ")?,
            ParseOFError::ErrRowname(_) => write!(f, "ParseRowname ")?,
            ParseOFError::ErrString(_) => write!(f, "String ")?,
            ParseOFError::ErrUnexpected(_) => write!(f, "Unexpected ")?,
            ParseOFError::ErrIri(_) => write!(f, "Iri ")?,
        }
        Self::fmt_span(self.span(), f)?;
        Ok(())
    }
}
