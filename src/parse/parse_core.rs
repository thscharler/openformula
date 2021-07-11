use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::char as nom_char;
use nom::combinator::recognize;
use nom::lib::std::ops::{RangeFrom, RangeTo};
use nom::multi::{count, many0};
use nom::sequence::delimited;
use nom::{
    Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice,
};

/// Parse "."
pub fn dot<'a, I>(i: I) -> IResult<I, I>
where
    I: Clone
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
    tag(".")(i)
}

/// Parse ":"
pub fn colon<'a, I>(i: I) -> IResult<I, I>
where
    I: Clone
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
    tag(":")(i)
}

/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn quoted<I>(quote: char) -> impl FnMut(I) -> IResult<I, I, nom::error::Error<I>>
where
    I: Clone
        + PartialEq
        + InputIter<Item = char>
        + InputLength
        + InputTake
        + InputTakeAtPosition<Item = char>
        + Offset
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
{
    move |i: I| {
        let (i, r) = delimited(
            nom_char(quote),
            recognize(many0(alt((
                take_while1(|v| v != quote),
                recognize(count(nom_char(quote), 2)),
            )))),
            nom_char(quote),
        )(i)?;

        Ok((i, r))
    }
}
