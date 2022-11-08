use crate::refnew::error::CellRefError;
use crate::refnew::refs::{CellRange, CellRef, ColRange, RowRange};
use crate::refnew::tokens;
use nom::combinator::{consumed, opt};
use nom::sequence::tuple;
use nom_locate::LocatedSpan;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::IntErrorKind;
use std::str::FromStr;

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, O> = Result<(Span<'s>, O), CellRefError>;

pub fn lah_cell_ref<'s>(i: Span<'s>) -> bool {
    tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
}

/// Parses a simple cell reference.
pub fn parse_cell_ref<'s>(i: Span<'s>) -> ParseResult<'s, (CellRef, Span<'s>)> {
    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheet_name),
        tokens::dot, // TODO: this is not user-facing but the stored format.
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((rest, (tok, (iri, sheet_name, _dot, col, row)))) => {
            let cell_ref = CellRef::new_all(
                iri.map(|v| (*v).to_string()),
                match sheet_name {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(row.0),
                try_u32_from_rowname(row.1)?,
                try_bool_from_abs_flag(col.0),
                try_u32_from_colname(col.1)?,
            );

            Ok((rest, (cell_ref, tok)))
        }

        Err(e @ nom::Err::Error(_)) => Err(CellRefError::nom_error(i, e)),
        Err(e @ nom::Err::Failure(_)) => Err(CellRefError::nom_failure(i, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn lah_cell_range<'s>(i: Span<'s>) -> bool {
    tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
}

/// Parses a cell range.
pub fn parse_cell_range<'s>(i: Span<'s>) -> ParseResult<'s, (CellRange, Span<'s>)> {
    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::col,
        tokens::row,
        tokens::colon,
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::col,
        tokens::row,
    )))(i)
    {
        Ok((
            rest,
            (
                tok,
                (
                    iri,
                    sheet_name_0,
                    _dot_0,
                    col_0,
                    row_0,
                    _colon,
                    sheet_name_1,
                    _dot_1,
                    col_1,
                    row_1,
                ),
            ),
        )) => {
            let cell_range = CellRange::new_all(
                iri.map(|v| (*v).to_string()),
                match sheet_name_0 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(row_0.0),
                try_u32_from_rowname(row_0.1)?,
                try_bool_from_abs_flag(col_0.0),
                try_u32_from_colname(col_0.1)?,
                match sheet_name_1 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(row_1.0),
                try_u32_from_rowname(row_1.1)?,
                try_bool_from_abs_flag(col_1.0),
                try_u32_from_colname(col_1.1)?,
            );

            Ok((rest, (cell_range, tok)))
        }

        Err(e @ nom::Err::Error(_)) => Err(CellRefError::nom_error(i, e)),
        Err(e @ nom::Err::Failure(_)) => Err(CellRefError::nom_failure(i, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn lah_colrange<'s>(i: Span<'s>) -> bool {
    tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
}

/// Parses a column range.
pub fn parse_col_range<'s>(i: Span<'s>) -> ParseResult<'s, (ColRange, Span<'s>)> {
    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::col,
        tokens::colon,
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::col,
    )))(i)
    {
        Ok((
            rest,
            (tok, (iri, sheet_name_0, _dot_0, col_0, _colon, sheet_name_1, _dot_1, col_1)),
        )) => {
            //
            let col_range = ColRange::new_all(
                iri.map(|v| (*v).to_string()),
                match sheet_name_0 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(col_0.0),
                try_u32_from_colname(col_0.1)?,
                match sheet_name_1 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(col_1.0),
                try_u32_from_colname(col_1.1)?,
            );

            Ok((rest, (col_range, tok)))
        }

        Err(e @ nom::Err::Error(_)) => Err(CellRefError::nom_error(i, e)),
        Err(e @ nom::Err::Failure(_)) => Err(CellRefError::nom_failure(i, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn lah_row_range<'s>(i: Span<'s>) -> bool {
    tokens::lah_iri(i) || tokens::lah_sheet_name(i) || tokens::lah_dot(i)
}

/// Parses a row range.
pub fn parse_row_range<'s>(i: Span<'s>) -> ParseResult<'s, (RowRange, Span<'s>)> {
    match consumed(tuple((
        opt(tokens::iri),
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::row,
        tokens::colon,
        opt(tokens::sheet_name),
        tokens::dot,
        tokens::row,
    )))(i)
    {
        Ok((
            rest,
            (tok, (iri, sheet_name_0, _dot_0, row_0, _colon, sheet_name_1, _dot_1, row_1)),
        )) => {
            //
            let row_range = RowRange::new_all(
                iri.map(|v| (*v).to_string()),
                match sheet_name_0 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(row_0.0),
                try_u32_from_rowname(row_0.1)?,
                match sheet_name_1 {
                    None => None,
                    Some((_, v)) => Some((*v).to_string()),
                },
                try_bool_from_abs_flag(row_1.0),
                try_u32_from_rowname(row_1.1)?,
            );

            Ok((rest, (row_range, tok)))
        }

        Err(e @ nom::Err::Error(_)) => Err(CellRefError::nom_error(i, e)),
        Err(e @ nom::Err::Failure(_)) => Err(CellRefError::nom_failure(i, e)),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

/// Parse a bool if a '$' exists.
pub fn try_bool_from_abs_flag<'a>(i: Option<Span<'a>>) -> bool {
    if let Some(i) = i {
        *i == "$"
    } else {
        false
    }
}

/// Error for try_u32_from_rowname.
#[allow(variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseRownameError {
    /// Value being parsed is empty.
    ///
    /// This variant will be constructed when parsing an empty string.
    Empty,
    /// Contains an invalid digit in its context.
    ///
    /// Among other causes, this variant will be constructed when parsing a string that
    /// contains a non-ASCII char.
    ///
    /// This variant is also constructed when a `+` or `-` is misplaced within a string
    /// either on its own or in the middle of a number.
    InvalidDigit,
    /// Integer is too large to store in target integer type.
    PosOverflow,
    /// Integer is too small to store in target integer type.
    NegOverflow,
    /// Value was Zero
    ///
    /// This variant will be emitted when the parsing string has a value of zero, which
    /// would be illegal for non-zero types.
    Zero,
    /// Something else.
    Other,
}

impl Display for ParseRownameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseRownameError::Empty => write!(f, "Input was empty")?,
            ParseRownameError::InvalidDigit => write!(f, "Invalid digit")?,
            ParseRownameError::PosOverflow => write!(f, "Positive overflow")?,
            ParseRownameError::NegOverflow => write!(f, "Negative overflow")?,
            ParseRownameError::Zero => write!(f, "Zero")?,
            ParseRownameError::Other => write!(f, "Other")?,
        }
        Ok(())
    }
}

impl Error for ParseRownameError {}

/// Parse a row number to a row index.
#[allow(clippy::explicit_auto_deref)]
pub fn try_u32_from_rowname<'a>(i: Span<'a>) -> Result<u32, CellRefError> {
    match u32::from_str(*i) {
        Ok(v) if v > 0 => Ok(v - 1),
        Ok(_v) => Err(CellRefError::ErrRowname(i.into(), ParseRownameError::Zero)),
        Err(e) => Err(CellRefError::ErrRowname(
            i.into(),
            match e.kind() {
                IntErrorKind::Empty => ParseRownameError::Empty,
                IntErrorKind::InvalidDigit => ParseRownameError::InvalidDigit,
                IntErrorKind::PosOverflow => ParseRownameError::PosOverflow,
                IntErrorKind::NegOverflow => ParseRownameError::NegOverflow,
                IntErrorKind::Zero => ParseRownameError::Zero,
                _ => ParseRownameError::Other,
            },
        )),
    }
}

/// Error for try_u32_from_colname.
#[allow(variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseColnameError {
    /// Invalid column character.
    InvalidChar(char),
    /// Invalid column name.
    InvalidColname(String),
}

impl Display for ParseColnameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseColnameError::InvalidChar(e) => {
                write!(f, "Invalid char '{}'", e)?;
            }
            ParseColnameError::InvalidColname(e) => {
                write!(f, "Invalid colname {}", e)?;
            }
        }
        Ok(())
    }
}

impl Error for ParseColnameError {}

/// Parse a col label to a column index.
pub fn try_u32_from_colname<'a>(i: Span<'a>) -> Result<u32, CellRefError> {
    let mut col = 0u32;

    for c in (*i).chars() {
        if !('A'..='Z').contains(&c) {
            return Err(CellRefError::ErrColname(
                i.into(),
                ParseColnameError::InvalidChar(c),
            ));
        }

        let mut v = c as u32 - b'A' as u32;
        if v == 25 {
            v = 0;
            col = (col + 1) * 26;
        } else {
            v += 1;
            col *= 26;
        }
        col += v;
    }

    if col == 0 {
        Err(CellRefError::ErrColname(
            i.into(),
            ParseColnameError::InvalidColname(format!("{:?}", i)),
        ))
    } else {
        Ok(col - 1)
    }
}
