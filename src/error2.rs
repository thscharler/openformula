use crate::Span;
use spreadsheet_ods_cellref::parser::{ParseColnameError, ParseRownameError};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub enum ParseOFError2 {
    /// Nom
    ErrNomFailure(ErrSpan, nom::error::ErrorKind),
    /// Nom
    ErrNomError(ErrSpan, nom::error::ErrorKind),
    /// Parsing didn't parse all of the string.
    ErrParseIncomplete(ErrSpan),

    ///
    ErrElementary(ErrSpan),
    ///
    ErrReference(ErrSpan),

    ///
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

impl ParseOFError2 {
    /// NomFailure variant.
    pub fn fail(span: Span<'_>, err: nom::error::ErrorKind) -> ParseOFError2 {
        ParseOFError2::ErrNomFailure(span.into(), err)
    }

    /// NomError variant.
    pub fn err(span: Span<'_>, err: nom::error::ErrorKind) -> ParseOFError2 {
        ParseOFError2::ErrNomError(span.into(), err)
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrParseIncomplete(span.into())
    }

    /// Elementary
    pub fn elementary(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrElementary(span.into())
    }

    /// Reference
    pub fn reference(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrReference(span.into())
    }

    /// CellRange variant.
    pub fn cell_range(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrCellRange(span.into())
    }

    /// ColRange variant.
    pub fn col_range(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrColRange(span.into())
    }

    /// RowRange variant.
    pub fn row_range(span: Span<'_>) -> ParseOFError2 {
        ParseOFError2::ErrRowRange(span.into())
    }
}

impl Display for ParseOFError2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ParseOFError2 {}

/// Adds a span as location and converts the error to our own type..
pub trait LocateError<T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError2>;
}

impl<T> LocateError<T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError2> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseOFError2::ErrRowname(span.into(), e)),
        }
    }
}

impl<T> LocateError<T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'_>) -> Result<T, ParseOFError2> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(ParseOFError2::ErrColname(span.into(), e)),
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
