//!
//! Parser
//!

mod conv;
mod parse_core;
mod parse_refs;
mod token;

pub use self::conv::*;
pub use self::parse_core::*;
pub use self::parse_refs::*;
pub use self::token::*;
use crate::refs::{CellRange, CellRef, ColRange, Reference, RowRange};
use crate::OFError;
use nom::branch::alt;
use nom::combinator::eof;
use nom::lib::std::ops::{RangeFrom, RangeTo};
use nom::multi::many1;
use nom::sequence::tuple;
use nom::{Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};
use nom_locate::LocatedSpan;
use std::fmt::Debug;

/// Spantype for the parser.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Parse any reference
pub fn parse_reference<'a, I>(i: I) -> Result<Reference, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok, _)) = tuple((range_address, eof))(i)?;

    match tok {
        Token::ErrRef(_) => Ok(Reference::Err),
        Token::CellRef(_, r) => Ok(Reference::Cell(CellRef {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            cell: try_cref_from_token(r.cell)?,
        })),
        Token::RangeRef(_, r) => Ok(Reference::Range(CellRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            from: try_cref_from_token(r.from)?,
            to_table: None,
            to: try_cref_from_token(r.to)?,
        })),
        Token::CellCuboid(_, r) => Ok(Reference::Range(CellRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            from: try_cref_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to: try_cref_from_token(r.to)?,
        })),
        Token::ColRef(_, r) => Ok(Reference::Col(ColRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            col: try_column_from_token(r.from)?,
            to_table: None,
            to_col: try_column_from_token(r.to)?,
        })),
        Token::ColCuboid(_, r) => Ok(Reference::Col(ColRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            col: try_column_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to_col: try_column_from_token(r.to)?,
        })),
        Token::RowRef(_, r) => Ok(Reference::Row(RowRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            row: try_row_from_token(r.from)?,
            to_table: None,
            to_row: try_row_from_token(r.to)?,
        })),
        Token::RowCuboid(_, r) => Ok(Reference::Row(RowRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            row: try_row_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to_row: try_row_from_token(r.to)?,
        })),
    }
}

/// Parses a space separated list of CellRanges and CellRefs.
pub fn parse_cellrange_vec<'a, I>(i: I) -> Result<Vec<Reference>, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok_vec, _)) = tuple((many1(alt((cellref, cellrange, cellcuboid))), eof))(i)?;

    let mut tvec = Vec::new();
    for tok in tok_vec {
        let reff = match tok {
            Token::CellRef(_, r) => Ok(Reference::Cell(CellRef {
                iri: unquote_opt(r.iri),
                table: unquote_opt2(r.table),
                cell: try_cref_from_token(r.cell)?,
            })),
            Token::RangeRef(_, r) => Ok(Reference::Range(CellRange {
                iri: unquote_opt(r.iri),
                table: unquote_opt2(r.table),
                from: try_cref_from_token(r.from)?,
                to_table: None,
                to: try_cref_from_token(r.to)?,
            })),
            Token::CellCuboid(_, r) => Ok(Reference::Range(CellRange {
                iri: unquote_opt(r.iri),
                table: Some(unquote_str(r.from_table)),
                from: try_cref_from_token(r.from)?,
                to_table: Some(unquote_str(r.to_table)),
                to: try_cref_from_token(r.to)?,
            })),
            _ => Err(OFError::Parse(format!("{:?}", tok))),
        }?;

        tvec.push(reff);
    }

    Ok(tvec)
}

/// Parse a cell range.
pub fn parse_cellrange<'a, I>(i: I) -> Result<CellRange, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok, _)) = tuple((alt((cellrange, cellcuboid)), eof))(i)?;

    match tok {
        Token::RangeRef(_, r) => Ok(CellRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            from: try_cref_from_token(r.from)?,
            to_table: None,
            to: try_cref_from_token(r.to)?,
        }),
        Token::CellCuboid(_, r) => Ok(CellRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            from: try_cref_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to: try_cref_from_token(r.to)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}

/// Parse a cell reference.
pub fn parse_cellref<'a, I>(i: I) -> Result<CellRef, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok, _)) = tuple((cellref, eof))(i)?;

    match tok {
        Token::CellRef(_, r) => Ok(CellRef {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            cell: try_cref_from_token(r.cell)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}

/// Parse a column range reference.
pub fn parse_colrange<'a, I>(i: I) -> Result<ColRange, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok, _)) = tuple((alt((colrange, colcuboid)), eof))(i)?;

    match tok {
        Token::ColRef(_, r) => Ok(ColRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            col: try_column_from_token(r.from)?,
            to_table: None,
            to_col: try_column_from_token(r.to)?,
        }),
        Token::ColCuboid(_, r) => Ok(ColRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            col: try_column_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to_col: try_column_from_token(r.to)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}

/// Parse a row range reference.
pub fn parse_rowrange<'a, I>(i: I) -> Result<RowRange, OFError>
where
    I: Clone
        + Debug
        + ToString
        + PartialEq
        + Compare<&'a str>
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    let (_, (tok, _)) = tuple((alt((rowrange, rowcuboid)), eof))(i)?;

    match tok {
        Token::RowRef(_, r) => Ok(RowRange {
            iri: unquote_opt(r.iri),
            table: unquote_opt2(r.table),
            row: try_row_from_token(r.from)?,
            to_table: None,
            to_row: try_row_from_token(r.to)?,
        }),
        Token::RowCuboid(_, r) => Ok(RowRange {
            iri: unquote_opt(r.iri),
            table: Some(unquote_str(r.from_table)),
            row: try_row_from_token(r.from)?,
            to_table: Some(unquote_str(r.to_table)),
            to_row: try_row_from_token(r.to)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}
