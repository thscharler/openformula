//!
//! Parser
//!

mod conv;
mod parse_core;
mod parse_refs2;
mod token;

pub use self::conv::*;
pub use self::parse_core::*;
pub use self::parse_refs2::*;
pub use self::token::*;
use crate::refs::{CellRange, CellRef};
use crate::OFError;
use nom::combinator::eof;
use nom::lib::std::ops::{RangeFrom, RangeTo};
use nom::sequence::tuple;
use nom::{Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};
use nom_locate::LocatedSpan;
use std::fmt::Debug;

/// Spantype for the parser.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Parse a cell range.
///
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
    let (_, (tok, _)) = tuple((cellrange, eof))(i)?;

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
///
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
