//!
//! Defines the error type.
//!

use crate::parse::Span;
use spreadsheet_ods_cellref::refs_parser::{ParseColnameError, ParseRownameError};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

///
/// Error type
///
#[derive(Debug)]
#[allow(missing_docs)]
pub enum OFError {
    Ods(String),
    Io(std::io::Error),
    Parse(String),
    ParseInt(std::num::ParseIntError),
    ParseBool(std::str::ParseBoolError),
    ParseFloat(std::num::ParseFloatError),
    ParseExpr(ParseOFError),
    Chrono(chrono::format::ParseError),
    SystemTime(std::time::SystemTimeError),
    Nom(nom::Err<nom::error::Error<String>>),
}

impl Display for OFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OFError::Ods(e) => write!(f, "Ods {}", e)?,
            OFError::Io(e) => write!(f, "IO {}", e)?,
            OFError::Parse(e) => write!(f, "Parse {}", e)?,
            OFError::ParseInt(e) => write!(f, "ParseInt {}", e)?,
            OFError::ParseBool(e) => write!(f, "ParseBool {}", e)?,
            OFError::ParseFloat(e) => write!(f, "ParseFloat {}", e)?,
            OFError::Chrono(e) => write!(f, "Chrono {}", e)?,
            OFError::SystemTime(e) => write!(f, "SystemTime {}", e)?,
            OFError::Nom(e) => write!(f, "Nom {}", e)?,
            OFError::ParseExpr(e) => write!(f, "ParseExpr {}", e)?,
        }

        Ok(())
    }
}

impl std::error::Error for OFError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            OFError::Ods(_) => None,
            OFError::Io(e) => Some(e),
            OFError::Parse(_) => None,
            OFError::ParseInt(e) => Some(e),
            OFError::ParseBool(e) => Some(e),
            OFError::ParseFloat(e) => Some(e),
            OFError::Chrono(e) => Some(e),
            OFError::SystemTime(e) => Some(e),
            OFError::Nom(e) => Some(e),
            OFError::ParseExpr(e) => Some(e),
        }
    }
}

impl From<std::io::Error> for OFError {
    fn from(err: std::io::Error) -> OFError {
        OFError::Io(err)
    }
}

impl From<std::str::ParseBoolError> for OFError {
    fn from(err: std::str::ParseBoolError) -> OFError {
        OFError::ParseBool(err)
    }
}

impl From<std::num::ParseIntError> for OFError {
    fn from(err: std::num::ParseIntError) -> OFError {
        OFError::ParseInt(err)
    }
}

impl From<ParseOFError> for OFError {
    fn from(err: ParseOFError) -> OFError {
        OFError::ParseExpr(err)
    }
}

impl From<std::num::ParseFloatError> for OFError {
    fn from(err: std::num::ParseFloatError) -> OFError {
        OFError::ParseFloat(err)
    }
}

impl From<chrono::format::ParseError> for OFError {
    fn from(err: chrono::format::ParseError) -> OFError {
        OFError::Chrono(err)
    }
}

impl From<std::time::SystemTimeError> for OFError {
    fn from(err: std::time::SystemTimeError) -> OFError {
        OFError::SystemTime(err)
    }
}

impl<I> From<nom::Err<nom::error::Error<I>>> for OFError
where
    I: ToString,
{
    fn from(err: nom::Err<nom::error::Error<I>>) -> OFError {
        OFError::Nom(
            err.map(|e| nom::error::ParseError::from_error_kind(e.input.to_string(), e.code)),
        )
    }
}

/// Error type for the parser.
#[derive(Debug)]
pub enum ParseOFError {
    /// Some nom error occurred.
    ErrNomError(ErrSpan, nom::error::ErrorKind),
    /// Nom failed.
    ErrNomFailure(ErrSpan, nom::error::ErrorKind),

    /// Compare expr parsing error.
    ErrCompare(ErrSpan),
    /// Additive expr parsing error.
    ErrAdd(ErrSpan),
    /// Multiplicative expr parsing error.
    ErrMul(ErrSpan),
    /// Power expr parsing error.
    ErrPow(ErrSpan),
    /// Postfix expr parsing error.
    ErrPostfix(ErrSpan),
    /// Prefix expr parsing error.
    ErrPrefix(ErrSpan),
    /// Elementary expr parsing error.
    ErrElementary(ErrSpan),

    /// Number parsing error.
    ErrNumber(ErrSpan),
    /// String parsing error.
    ErrString(ErrSpan),
    /// Named expr error.
    ErrNamed(ErrSpan),
    /// Parenthesis parsing error.
    ErrParenthesis(ErrSpan),
    /// Function call parsing error.
    ErrFnCall(ErrSpan),
    /// CellRef parsing error.
    ErrCellRef(ErrSpan),
    /// CellRange parsing error.
    ErrCellRange(ErrSpan),
    /// ColRange parsing error.
    ErrColRange(ErrSpan),
    /// Rowrange parsing error.
    ErrRowRange(ErrSpan),

    /// Error converting a row-name part of any Ref to an u32.
    ErrRowname(ErrSpan, ParseRownameError),
    /// Error converting a col-name part of any Ref to an u32.
    ErrColname(ErrSpan, ParseColnameError),
}

impl ParseOFError {
    fn error_kind(err: &nom::Err<nom::error::Error<Span<'_>>>) -> nom::error::ErrorKind {
        match err {
            nom::Err::Incomplete(_e) => nom::error::ErrorKind::Fail,
            nom::Err::Error(e) => e.code,
            nom::Err::Failure(e) => e.code,
        }
    }

    /// NomError variant.
    pub fn nom_error<'a>(
        span: Span<'a>,
        err: &nom::Err<nom::error::Error<Span<'a>>>,
    ) -> ParseOFError {
        ParseOFError::ErrNomError(span.into(), Self::error_kind(err))
    }

    /// NomFailure variant.
    pub fn nom_failure<'a>(
        span: Span<'a>,
        err: &nom::Err<nom::error::Error<Span<'a>>>,
    ) -> ParseOFError {
        ParseOFError::ErrNomFailure(span.into(), Self::error_kind(err))
    }

    /// Comp variant.
    pub fn compare<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrCompare(span.into())
    }

    /// Add variant.
    pub fn add<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrAdd(span.into())
    }

    /// Mul variant.
    pub fn mul<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrMul(span.into())
    }

    /// Pow variant.
    pub fn pow<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrPow(span.into())
    }

    /// PrefixExpr variant.
    pub fn postfix<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrPostfix(span.into())
    }

    /// PrefixExpr variant.
    pub fn prefix<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrPrefix(span.into())
    }

    /// Elementary variant.
    pub fn elementary<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrElementary(span.into())
    }

    /// Number variant.
    pub fn number<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrNumber(span.into())
    }

    /// String variant.
    pub fn string<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrString(span.into())
    }

    /// Named variant.
    pub fn named<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrNamed(span.into())
    }

    /// FnCall variant.
    pub fn fn_call<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrFnCall(span.into())
    }

    /// Parenthesis variant.
    pub fn parentheses<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrParenthesis(span.into())
    }

    /// CellRef variant.
    pub fn cell_ref<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrCellRef(span.into())
    }

    /// CellRange variant.
    pub fn cell_range<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrCellRange(span.into())
    }

    /// ColRange variant.
    pub fn col_range<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrColRange(span.into())
    }

    /// RowRange variant.
    pub fn row_range<'a>(span: Span<'a>) -> ParseOFError {
        ParseOFError::ErrRowRange(span.into())
    }
}

impl Error for ParseOFError {}

/// Adds a span as location and converts the error to our own type..
pub trait LocateError<T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError>;
}

impl<T> LocateError<T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseOFError::ErrRowname(span.into(), e)),
        }
    }
}

impl<T> LocateError<T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseOFError::ErrColname(span.into(), e)),
        }
    }
}

impl Display for ParseOFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseOFError::ErrNomError(s, e) => write!(f, "NomError {} {:?}", s, e),
            ParseOFError::ErrNomFailure(s, e) => write!(f, "NomFailure {} {:?}", s, e),
            ParseOFError::ErrNumber(s) => write!(f, "Number {}", s),
            ParseOFError::ErrString(s) => write!(f, "String {}", s),
            ParseOFError::ErrNamed(s) => write!(f, "Named {}", s),
            ParseOFError::ErrParenthesis(s) => write!(f, "Parenthesis {}", s),
            ParseOFError::ErrFnCall(s) => write!(f, "FnCall {}", s),
            ParseOFError::ErrElementary(s) => write!(f, "Elementary {}", s),
            ParseOFError::ErrPostfix(s) => write!(f, "Postfix {}", s),
            ParseOFError::ErrPrefix(s) => write!(f, "Prefix {}", s),
            ParseOFError::ErrPow(s) => write!(f, "Pow {}", s),
            ParseOFError::ErrMul(s) => write!(f, "Mul {}", s),
            ParseOFError::ErrAdd(s) => write!(f, "Add {}", s),
            ParseOFError::ErrCompare(s) => write!(f, "Comp {}", s),
            ParseOFError::ErrCellRef(s) => write!(f, "CellRef {}", s),
            ParseOFError::ErrCellRange(s) => write!(f, "CellRange {}", s),
            ParseOFError::ErrColRange(s) => write!(f, "ColRange {}", s),
            ParseOFError::ErrRowRange(s) => write!(f, "RowRange {}", s),
            ParseOFError::ErrRowname(s, e) => write!(f, "ParseRowname {} {:?}", s, e),
            ParseOFError::ErrColname(s, e) => write!(f, "ParseColname {} {:?}", s, e),
        }
    }
}

/// For the errors the lifetime is annoying. This is a owning copy of the offending span.
pub struct ErrSpan {
    /// Offset from the start of input.
    pub offset: usize,
    /// Line.
    pub line: u32,
    /// Column.
    pub column: usize,
    /// The offending fragment.
    pub fragment: String,
}

impl Debug for ErrSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ErrSpan(@{}:{} str '{}')",
            self.line, self.column, self.fragment
        )
    }
}

impl Display for ErrSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(@{}:{} str '{}')",
            self.line, self.column, self.fragment
        )
    }
}

impl From<spreadsheet_ods_cellref::error::ErrSpan> for ErrSpan {
    fn from(span: spreadsheet_ods_cellref::error::ErrSpan) -> Self {
        Self {
            offset: span.offset,
            line: span.line,
            column: span.column,
            fragment: span.fragment,
        }
    }
}

impl<'a> From<Span<'a>> for ErrSpan {
    fn from(s: Span<'a>) -> Self {
        Self {
            offset: s.location_offset(),
            line: s.location_line(),
            column: s.get_column(),
            fragment: s.fragment().to_string(),
        }
    }
}
