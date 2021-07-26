use crate::parse::{
    colon, dot, quoted, CellCuboidToken, CellRangeToken, CellRefToken, CellToken, ColCuboidToken,
    ColRangeToken, RowCuboidToken, RowRangeToken, Token,
};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, none_of, one_of};
use nom::combinator::{consumed, opt, recognize};
use nom::lib::std::ops::{RangeFrom, RangeTo};
use nom::multi::many1;
use nom::sequence::{terminated, tuple};
use nom::{
    Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice,
};

/// Parses all adress kinds.
pub fn range_address<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, r) = alt((
        cellcuboid, rowcuboid, colcuboid, rowrange, colrange, cellrange, cellref,
    ))(i)?;

    Ok((i, r))
}

/// Parse a error reference.
pub fn referr<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, p) = tag("#REF!")(i)?;
    Ok((i, Token::ErrRef(p)))
}

/// Parse a row cuboid or row range.
pub fn rowcuboid<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname_0, _dot_0, row_0, _colon, sheetname_1, _dot_1, row_1))) = consumed(
        tuple((opt(iri), sheetname, dot, row, colon, sheetname, dot, row)),
    )(i)?;

    Ok((
        i,
        Token::RowCuboid(
            p,
            RowCuboidToken {
                iri,
                from_table: sheetname_0.1,
                from: row_0,
                to_table: sheetname_1.1,
                to: row_1,
            },
        ),
    ))
}

/// Parse a col cuboid or col range.
pub fn colcuboid<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname_0, _dot_0, col_0, _colon, sheetname_1, _dot_1, col_1))) =
        consumed(tuple((
            opt(iri),
            sheetname,
            dot,
            column,
            colon,
            sheetname,
            dot,
            column,
        )))(i)?;

    Ok((
        i,
        Token::ColCuboid(
            p,
            ColCuboidToken {
                iri,
                from_table: sheetname_0.1,
                from: col_0,
                to_table: sheetname_1.1,
                to: col_1,
            },
        ),
    ))
}

/// Parse a cell cuboid or cell range.
pub fn cellcuboid<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (
        i,
        (p, (iri, sheetname_0, _dot_0, col_0, row_0, _colon, sheetname_1, _dot_1, col_1, row_1)),
    ) = consumed(tuple((
        opt(iri),
        sheetname,
        dot,
        column,
        row,
        colon,
        sheetname,
        dot,
        column,
        row,
    )))(i)?;

    Ok((
        i,
        Token::CellCuboid(
            p,
            CellCuboidToken {
                iri,
                from_table: sheetname_0.1,
                from: CellToken {
                    col: col_0,
                    row: row_0,
                },
                to_table: sheetname_1.1,
                to: CellToken {
                    col: col_1,
                    row: row_1,
                },
            },
        ),
    ))
}

/// Parse a row range.
pub fn rowrange<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname, _dot_0, row_0, _colon, _dot_1, row_1))) =
        consumed(tuple((opt(iri), opt(sheetname), dot, row, colon, dot, row)))(i)?;

    Ok((
        i,
        Token::RowRef(
            p,
            RowRangeToken {
                iri,
                table: sheetname,
                from: row_0,
                to: row_1,
            },
        ),
    ))
}

/// Parse a col range.
pub fn colrange<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname, _dot_0, col_0, _colon, _dot_1, col_1))) = consumed(tuple((
        opt(iri),
        opt(sheetname),
        dot,
        column,
        colon,
        dot,
        column,
    )))(i)?;

    Ok((
        i,
        Token::ColRef(
            p,
            ColRangeToken {
                iri,
                table: sheetname,
                from: col_0,
                to: col_1,
            },
        ),
    ))
}

/// Parse a cell range.
pub fn cellrange<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname, _dot_0, col_0, row_0, _colon, _dot_1, col_1, row_1))) =
        consumed(tuple((
            opt(iri),
            opt(sheetname),
            dot,
            column,
            row,
            colon,
            dot,
            column,
            row,
        )))(i)?;

    Ok((
        i,
        Token::RangeRef(
            p,
            CellRangeToken {
                iri,
                table: sheetname,
                from: CellToken {
                    col: col_0,
                    row: row_0,
                },
                to: CellToken {
                    col: col_1,
                    row: row_1,
                },
            },
        ),
    ))
}

/// Parse a cell reference.
pub fn cellref<'a, I>(i: I) -> IResult<I, Token<I>>
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
    let (i, (p, (iri, sheetname, _dot, col, row))) =
        consumed(tuple((opt(iri), opt(sheetname), dot, column, row)))(i)?;

    Ok((
        i,
        Token::CellRef(
            p,
            CellRefToken {
                iri,
                table: sheetname,
                cell: CellToken { col, row },
            },
        ),
    ))
}

/// IRI ... i18n something
pub fn iri<'a, I>(i: I) -> IResult<I, I>
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
    let (i, iri) = terminated(quoted('\''), tag("#"))(i)?;
    Ok((i, iri))
}

/// Parse a sheetname.
pub fn sheetname<'a, I>(i: I) -> IResult<I, (Option<I>, I)>
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
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, name) = alt((quoted('\''), recognize(many1(none_of("]. #$'")))))(i)?;

    Ok((i, (abs, name)))
}

/// Parse a row label.
pub fn row<'a, I>(i: I) -> IResult<I, (Option<I>, I)>
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
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, row) = recognize(many1(one_of("0123456789")))(i)?;

    Ok((i, (abs, row)))
}

/// Parse a col label.
pub fn column<'a, I>(i: I) -> IResult<I, (Option<I>, I)>
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
    let (i, abs) = opt(tag("$"))(i)?;
    let (i, col) = alpha1(i)?;
    Ok((i, (abs, col)))
}

#[cfg(test)]
mod tests {

    use crate::parse::parse_refs2::{cellref, column, iri, row, sheetname, CellRefToken};
    use crate::parse::{quoted, CellToken, Span, Token};
    use crate::OFError;
    use nom::error::{Error as NomError, ErrorKind, ParseError};
    use nom::Err as NomErr;

    #[test]
    fn test_cellref2() -> Result<(), OFError> {
        assert_eq!(
            cellref(".A21")?,
            (
                "",
                Token::CellRef(
                    ".A21",
                    CellRefToken {
                        iri: None,
                        table: None,
                        cell: CellToken {
                            col: (None, "A"),
                            row: (None, "21"),
                        }
                    }
                )
            )
        );

        Ok(())
    }

    #[test]
    fn test_cellref() -> Result<(), OFError> {
        unsafe {
            assert_eq!(
                cellref(Span::new(".A21"))?,
                (
                    Span::new_from_raw_offset(4, 1, "", ()),
                    Token::CellRef(
                        Span::from(".A21"),
                        CellRefToken {
                            iri: None,
                            table: None,
                            cell: CellToken {
                                col: (None, Span::new_from_raw_offset(1, 1, "A", ())),
                                row: (None, Span::new_from_raw_offset(2, 1, "21", ()))
                            }
                        }
                    )
                )
            );

            Ok(())
        }
    }

    #[test]
    fn test_quoted() {
        assert_eq!(
            quoted('\'')("'"),
            Err(NomErr::Error(NomError::from_error_kind(
                "",
                ErrorKind::Char
            )))
        );
        assert_eq!(quoted('\'')("''"), Ok(("", "")));
        assert_eq!(
            quoted('\'')("'''"),
            Err(NomErr::Error(NomError::from_error_kind(
                "",
                ErrorKind::Char
            )))
        );
        assert_eq!(quoted('\'')("''''"), Ok(("", "''")));
        assert_eq!(quoted('\'')("'abcd'"), Ok(("", "abcd")));
        assert_eq!(quoted('\'')("'a'bcd'"), Ok(("bcd'", "a")));
        assert_eq!(quoted('\'')("'a''bcd'"), Ok(("", "a''bcd")));
        assert_eq!(quoted('\'')("'a'''bcd'"), Ok(("bcd'", "a''")));
    }

    #[test]
    fn test_column() {
        assert_eq!(column("A"), Ok(("", (None, "A"))));
        assert_eq!(column("AAAA"), Ok(("", (None, "AAAA"))));
        assert_eq!(column("AAAA "), Ok((" ", (None, "AAAA"))));
        assert_eq!(column("AAAA1234"), Ok(("1234", (None, "AAAA"))));
    }

    #[test]
    fn test_column2() {
        unsafe {
            assert_eq!(
                column(Span::new("A")),
                Ok((
                    Span::new_from_raw_offset(1, 1, "", ()),
                    (None, Span::from("A"))
                ))
            );
            assert_eq!(
                column(Span::new("AAAA")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                column(Span::new("AAAA ")),
                Ok((
                    Span::new_from_raw_offset(4, 1, " ", ()),
                    (None, Span::from("AAAA"))
                ))
            );
            assert_eq!(
                column(Span::new("AAAA1234")),
                Ok((
                    Span::new_from_raw_offset(4, 1, "1234", ()),
                    (None, Span::from("AAAA"))
                ))
            );
        }
    }

    #[test]
    fn test_row() {
        assert_eq!(row("1"), Ok(("", (None, "1"))));
        assert_eq!(row("123"), Ok(("", (None, "123"))));
        assert_eq!(row("123 "), Ok((" ", (None, "123"))));
    }

    #[test]
    fn test_sheetname() {
        assert_eq!(sheetname("sheet1"), Ok(("", (None, "sheet1"))));
        assert_eq!(sheetname("sheet1]"), Ok(("]", (None, "sheet1"))));
        assert_eq!(sheetname("sheet1."), Ok((".", (None, "sheet1"))));
        assert_eq!(sheetname("sheet1$"), Ok(("$", (None, "sheet1"))));
        assert_eq!(sheetname("sheet1 "), Ok((" ", (None, "sheet1"))));
        assert_eq!(sheetname("sheet1#"), Ok(("#", (None, "sheet1"))));
        assert_eq!(sheetname("'sheet1'"), Ok(("", (None, "sheet1"))));
    }

    #[test]
    fn test_iri() {
        assert_eq!(iri("'file:c:x.txt'#"), Ok(("", "file:c:x.txt")));
    }
}
