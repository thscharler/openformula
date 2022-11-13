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
    /// Error converting a row-name part of any Ref to an u32.
    ErrRowname(Span<'s>),
    /// Error converting a col-name part of any Ref to an u32.
    ErrColname(Span<'s>),

    /// Parsing didn't ast all of the string.
    ErrParseIncomplete(Span<'s>),

    /// Error when parsing an expression in parenthesis.
    ErrParenthesis(Span<'s>),
    /// Error when parsing a function call
    ErrFnCall(Span<'s>),

    /// Elementary expression fails.
    ErrElementary(Span<'s>),
    /// Reference expression fails.
    ErrReference(Span<'s>),

    /// Reference expression fails.
    ErrString(Span<'s>),

    /// CellRange parsing error.
    ErrCellRange(Span<'s>),
    /// ColRange parsing error.
    ErrColRange(Span<'s>),
    /// Rowrange parsing error.
    ErrRowRange(Span<'s>),
}

impl<'s> ParseOFError<'s> {
    /// Return the errspan of any variant.
    pub fn span(&self) -> &Span<'s> {
        match self {
            ParseOFError::ErrNomError(s) => s,
            ParseOFError::ErrNomFailure(s) => s,
            ParseOFError::ErrParseIncomplete(s) => s,
            ParseOFError::ErrElementary(s) => s,
            ParseOFError::ErrReference(s) => s,
            ParseOFError::ErrCellRange(s) => s,
            ParseOFError::ErrColRange(s) => s,
            ParseOFError::ErrRowRange(s) => s,
            ParseOFError::ErrRowname(s) => s,
            ParseOFError::ErrColname(s) => s,
            ParseOFError::ErrParenthesis(s) => s,
            ParseOFError::ErrFnCall(s) => s,
            ParseOFError::ErrString(s) => s,
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
            ParseOFError::ErrNomError(_) => write!(f, "NomError ")?,
            ParseOFError::ErrNomFailure(_) => write!(f, "NomFailure ")?,
            ParseOFError::ErrRowname(_) => write!(f, "ParseRowname ")?,
            ParseOFError::ErrColname(_) => write!(f, "ParseColname ")?,
            ParseOFError::ErrParseIncomplete(_) => write!(f, "ParseIncomplete ")?,
            ParseOFError::ErrElementary(_) => write!(f, "Elementary ")?,
            ParseOFError::ErrReference(_) => write!(f, "Reference ")?,
            ParseOFError::ErrCellRange(_) => write!(f, "CellRange ")?,
            ParseOFError::ErrColRange(_) => write!(f, "ColRange ")?,
            ParseOFError::ErrRowRange(_) => write!(f, "RowRange ")?,
            ParseOFError::ErrParenthesis(_) => write!(f, "Parenthesis ")?,
            ParseOFError::ErrFnCall(_) => write!(f, "FnCall ")?,
            ParseOFError::ErrString(_) => write!(f, "String ")?,
        }
        Self::fmt_span(self.span(), f)?;
        Ok(())
    }
}

// /// For the errors the lifetime is annoying. This is a owning copy of the offending span.
// pub struct ErrSpan {
//     /// Offset from the start of input.
//     pub offset: usize,
//     /// Line.
//     pub line: u32,
//     /// Column.
//     pub column: usize,
//     /// The offending fragment.
//     pub fragment: String,
// }
//
// impl Debug for ErrSpan {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "ErrSpan(@{}:{} str '{}')",
//             self.line, self.column, self.fragment
//         )
//     }
// }
//
// impl Display for ErrSpan {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "(@{}:{} str '{}')",
//             self.line, self.column, self.fragment
//         )
//     }
// }
//
// impl From<spreadsheet_ods_cellref::error::ErrSpan> for ErrSpan {
//     fn from(span: spreadsheet_ods_cellref::error::ErrSpan) -> Self {
//         Self {
//             offset: span.offset,
//             line: span.line,
//             column: span.column,
//             fragment: span.fragment,
//         }
//     }
// }
//
// impl<'a> From<Span<'a>> for ErrSpan {
//     fn from(s: Span<'a>) -> Self {
//         Self {
//             offset: s.location_offset(),
//             line: s.location_line(),
//             column: s.get_column(),
//             fragment: s.fragment().to_string(),
//         }
//     }
// }
