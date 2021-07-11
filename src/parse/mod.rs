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

pub type Span<'a> = LocatedSpan<&'a str>;

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
            table: unquote_opt2(r.sheet),

            row_abs: try_bool_from_abs_flag(r.from.row.0)?,
            row: try_ucell_from_rowname(r.from.row.1)?,
            col_abs: try_bool_from_abs_flag(r.from.col.0)?,
            col: try_ucell_from_colname(r.from.col.1)?,

            to_row_abs: try_bool_from_abs_flag(r.to.row.0)?,
            to_row: try_ucell_from_rowname(r.to.row.1)?,
            to_col_abs: try_bool_from_abs_flag(r.to.col.0)?,
            to_col: try_ucell_from_colname(r.to.col.1)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}

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
            table: unquote_opt2(r.sheet),
            row_abs: try_bool_from_abs_flag(r.cell.row.0)?,
            row: try_ucell_from_rowname(r.cell.row.1)?,
            col_abs: try_bool_from_abs_flag(r.cell.col.0)?,
            col: try_ucell_from_colname(r.cell.col.1)?,
        }),
        _ => Err(OFError::Parse(format!("{:?}", tok))),
    }
}
