//!
//! Defines the error type.
//!

use crate::conv::ParseRownameError;
use crate::parse::conv::ParseColnameError;
use crate::parse::Span;
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
    ParseExpr(ParseExprError),
    Chrono(chrono::format::ParseError),
    Duration(time::OutOfRangeError),
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
            OFError::Duration(e) => write!(f, "Duration {}", e)?,
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
            OFError::Duration(e) => Some(e),
            OFError::SystemTime(e) => Some(e),
            OFError::Nom(e) => Some(e),
            OFError::ParseExpr(e) => Some(e),
        }
    }
}

impl From<time::OutOfRangeError> for OFError {
    fn from(err: time::OutOfRangeError) -> OFError {
        OFError::Duration(err)
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

impl From<ParseExprError> for OFError {
    fn from(err: ParseExprError) -> OFError {
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
pub enum ParseExprError {
    /// Some nom error occurred.
    NomError(ErrSpan),
    /// Nom failed.
    NomFailure(ErrSpan),

    /// Compare expr parsing error.
    Comp(ErrSpan),
    /// Additive expr parsing error.
    Add(ErrSpan),
    /// Multiplicative expr parsing error.
    Mul(ErrSpan),
    /// Power expr parsing error.
    Pow(ErrSpan),
    /// Postfix expr parsing error.
    Postfix(ErrSpan),
    /// Prefix expr parsing error.
    Prefix(ErrSpan),
    /// Elementary expr parsing error.
    Elementary(ErrSpan),

    /// Number parsing error.
    Number(ErrSpan),
    /// String parsing error.
    String(ErrSpan),
    /// Parenthesis parsing error.
    Parenthesis(ErrSpan),
    /// Function call parsing error.
    FnCall(ErrSpan),
    /// CellRef parsing error.
    CellRef(ErrSpan),
    /// CellRange parsing error.
    CellRange(ErrSpan),
    /// ColRange parsing error.
    ColRange(ErrSpan),
    /// Rowrange parsing error.
    RowRange(ErrSpan),

    /// Error converting a row-name part of any Ref to an u32.
    ParseRowname(ErrSpan, ParseRownameError),
    /// Error converting a col-name part of any Ref to an u32.
    ParseColname(ErrSpan, ParseColnameError),
}

impl ParseExprError {
    /// NomError variant.
    pub fn nom_error<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::NomError(span.into())
    }

    /// NomFailure variant.
    pub fn nom_failure<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::NomFailure(span.into())
    }

    /// Comp variant.
    pub fn comp<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Comp(span.into())
    }

    /// Add variant.
    pub fn add<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Add(span.into())
    }

    /// Mul variant.
    pub fn mul<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Mul(span.into())
    }

    /// Pow variant.
    pub fn pow<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Pow(span.into())
    }

    /// PrefixExpr variant.
    pub fn postfix<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Postfix(span.into())
    }

    /// PrefixExpr variant.
    pub fn prefix<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Prefix(span.into())
    }

    /// Elementary variant.
    pub fn elementary<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Elementary(span.into())
    }

    /// Number variant.
    pub fn number<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Number(span.into())
    }

    /// String variant.
    pub fn string<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::String(span.into())
    }

    /// FnCall variant.
    pub fn fn_call<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::FnCall(span.into())
    }

    /// Parenthesis variant.
    pub fn parenthesis<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Parenthesis(span.into())
    }

    /// CellRef variant.
    pub fn cellref<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::CellRef(span.into())
    }

    /// CellRange variant.
    pub fn cellrange<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::CellRange(span.into())
    }

    /// ColRange variant.
    pub fn colrange<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::ColRange(span.into())
    }

    /// RowRange variant.
    pub fn rowrange<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::RowRange(span.into())
    }
}

impl Error for ParseExprError {}

impl Display for ParseExprError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseExprError::NomError(s) => write!(f, "NomError {}", s),
            ParseExprError::NomFailure(s) => write!(f, "NomFailure {}", s),
            ParseExprError::Number(s) => write!(f, "Number {}", s),
            ParseExprError::String(s) => write!(f, "String {}", s),
            ParseExprError::Parenthesis(s) => write!(f, "Parenthesis {}", s),
            ParseExprError::FnCall(s) => write!(f, "FnCall {}", s),
            ParseExprError::Elementary(s) => write!(f, "Elementary {}", s),
            ParseExprError::Postfix(s) => write!(f, "Postfix {}", s),
            ParseExprError::Prefix(s) => write!(f, "Prefix {}", s),
            ParseExprError::Pow(s) => write!(f, "Pow {}", s),
            ParseExprError::Mul(s) => write!(f, "Mul {}", s),
            ParseExprError::Add(s) => write!(f, "Add {}", s),
            ParseExprError::Comp(s) => write!(f, "Comp {}", s),
            ParseExprError::CellRef(s) => write!(f, "CellRef {}", s),
            ParseExprError::CellRange(s) => write!(f, "CellRange {}", s),
            ParseExprError::ColRange(s) => write!(f, "ColRange {}", s),
            ParseExprError::RowRange(s) => write!(f, "RowRange {}", s),
            ParseExprError::ParseRowname(s, e) => write!(f, "ParseRowname {} {:?}", s, e),
            ParseExprError::ParseColname(s, e) => write!(f, "ParseColname {} {:?}", s, e),
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
