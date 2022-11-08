use crate::refnew::refs_parser::{ParseColnameError, ParseRownameError, Span};
use nom::error::ParseError;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

/// Error type for the parser.
#[derive(Debug)]
pub enum CellRefError {
    /// Some nom error occurred.
    ErrNomError(nom::Err<nom::error::Error<ErrSpan>>, ErrSpan),
    /// Nom failed.
    ErrNomFailure(nom::Err<nom::error::Error<ErrSpan>>, ErrSpan),

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

impl CellRefError {
    /// NomError variant.
    pub fn nom_error<'s>(
        span: Span<'s>,
        err: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> CellRefError {
        CellRefError::ErrNomError(Self::conv_err(err), span.into())
    }

    /// NomFailure variant.
    pub fn nom_failure<'s>(
        span: Span<'s>,
        err: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> CellRefError {
        CellRefError::ErrNomFailure(Self::conv_err(err), span.into())
    }

    fn conv_err<'s>(
        err: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> nom::Err<nom::error::Error<ErrSpan>> {
        match err {
            nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
            nom::Err::Error(e) => {
                nom::Err::Error(nom::error::Error::from_error_kind(e.input.into(), e.code))
            }
            nom::Err::Failure(e) => {
                nom::Err::Failure(nom::error::Error::from_error_kind(e.input.into(), e.code))
            }
        }
    }

    /// CellRef variant.
    pub fn cell_ref<'s>(span: Span<'s>) -> CellRefError {
        CellRefError::ErrCellRef(span.into())
    }

    /// CellRange variant.
    pub fn cell_range<'s>(span: Span<'s>) -> CellRefError {
        CellRefError::ErrCellRange(span.into())
    }

    /// ColRange variant.
    pub fn col_range<'s>(span: Span<'s>) -> CellRefError {
        CellRefError::ErrColRange(span.into())
    }

    /// RowRange variant.
    pub fn row_range<'s>(span: Span<'s>) -> CellRefError {
        CellRefError::ErrRowRange(span.into())
    }
}

impl Error for CellRefError {}

impl Display for CellRefError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CellRefError::ErrNomError(e, s) => write!(f, "NomError {:?} {}", e, s),
            CellRefError::ErrNomFailure(e, s) => write!(f, "NomFailure {:?} {}", e, s),
            CellRefError::ErrCellRef(s) => write!(f, "CellRef {}", s),
            CellRefError::ErrCellRange(s) => write!(f, "CellRange {}", s),
            CellRefError::ErrColRange(s) => write!(f, "ColRange {}", s),
            CellRefError::ErrRowRange(s) => write!(f, "RowRange {}", s),
            CellRefError::ErrRowname(s, e) => write!(f, "ParseRowname {} {:?}", s, e),
            CellRefError::ErrColname(s, e) => write!(f, "ParseColname {} {:?}", s, e),
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
