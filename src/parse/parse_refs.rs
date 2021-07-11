use crate::parse::Span;
use crate::refs::{
    CellCuboid, CellRange, CellRef, ColCuboid, ColRange, Reference, RowCuboid, RowRange,
};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, char as nom_char, none_of, one_of};
use nom::combinator::{opt, recognize};
use nom::error::{ErrorKind, ParseError};
use nom::lib::std::ops::{RangeFrom, RangeTo};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, terminated, tuple};
use nom::{IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};

/// Parses any cell reference.
pub fn parse_reference(i: Span) -> IResult<Span, Reference> {
    let (i, _) = tag("[")(i)?;
    let (i, reference) = alt((ref_error, range_address))(i)?;
    let (i, _) = tag("]")(i)?;

    Ok((i, reference))
}

/// Parses a bare cellref.
pub fn parse_cellref(i: Span) -> IResult<Span, CellRef> {
    cellref(i)
}

/// Parses a bare cellrange.
pub fn parse_cellrange(i: Span) -> IResult<Span, CellRange> {
    cellrange(i)
}

/// Parses a bare colrange.
pub fn parse_colrange(i: Span) -> IResult<Span, ColRange> {
    colrange(i)
}

/// Parses a bare rowrange.
pub fn parse_rowrange(i: Span) -> IResult<Span, RowRange> {
    rowrange(i)
}

/// Parses a space separated list of CellRange and CellRef.
pub fn parse_rowrange_vec(i: Span) -> IResult<Span, Vec<Reference>> {
    many1(alt((ref_cellrange, ref_cellref)))(i)
}

/// Parses all adress kinds.
fn range_address(i: Span) -> IResult<Span, Reference> {
    alt((
        ref_cellcuboid,
        ref_rowcuboid,
        ref_colcuboid,
        ref_rowrange,
        ref_colrange,
        ref_cellrange,
        ref_cellref,
    ))(i)
}

/// Parse a error reference.
fn ref_error(i: Span) -> IResult<Span, Reference> {
    let (i, _) = tag("#REF!")(i)?;
    Ok((i, Reference::Err))
}

/// Parse a cell reference.
fn ref_cellcuboid(i: Span) -> IResult<Span, Reference> {
    let (i, cellcuboid) = cellcuboid(i)?;
    Ok((i, Reference::Cuboid(cellcuboid)))
}

/// Parse a cell reference.
fn ref_rowcuboid(i: Span) -> IResult<Span, Reference> {
    let (i, rowcuboid) = rowcuboid(i)?;
    Ok((i, Reference::RowCuboid(rowcuboid)))
}

/// Parse a cell reference.
fn ref_colcuboid(i: Span) -> IResult<Span, Reference> {
    let (i, colcuboid) = colcuboid(i)?;
    Ok((i, Reference::ColCuboid(colcuboid)))
}

/// Parse a cell reference.
fn ref_rowrange(i: Span) -> IResult<Span, Reference> {
    let (i, rowrange) = rowrange(i)?;
    Ok((i, Reference::Row(rowrange)))
}

/// Parse a cell reference.
fn ref_colrange(i: Span) -> IResult<Span, Reference> {
    let (i, colrange) = colrange(i)?;
    Ok((i, Reference::Col(colrange)))
}

/// Parse a cell reference.
fn ref_cellrange(i: Span) -> IResult<Span, Reference> {
    let (i, cellrange) = cellrange(i)?;
    Ok((i, Reference::Range(cellrange)))
}

/// Parse a cell reference.
fn ref_cellref(i: Span) -> IResult<Span, Reference> {
    let (i, cellref) = cellref(i)?;
    Ok((i, Reference::Cell(cellref)))
}

/// Parse a row cuboid or row range.
fn rowcuboid(i: Span) -> IResult<Span, RowCuboid> {
    let (i, (iri, sheetname_0, _dot_0, row_0, _colon, sheetname_1, _dot_1, row_1)) =
        tuple((opt(iri), sheetname, dot, row, colon, sheetname, dot, row))(i)?;

    Ok((
        i,
        RowCuboid {
            iri,
            table: sheetname_0,
            row_abs: row_0.0,
            row: row_0.1,
            to_table: sheetname_1,
            to_row_abs: row_1.0,
            to_row: row_1.1,
        },
    ))
}

/// Parse a col cuboid or col range.
fn colcuboid(i: Span) -> IResult<Span, ColCuboid> {
    let (i, (iri, sheetname_0, _dot_0, col_0, _colon, sheetname_1, _dot_1, col_1)) = tuple((
        opt(iri),
        sheetname,
        dot,
        column,
        colon,
        sheetname,
        dot,
        column,
    ))(i)?;

    Ok((
        i,
        ColCuboid {
            iri,
            table: sheetname_0,
            col_abs: col_0.0,
            col: col_0.1,
            to_table: sheetname_1,
            to_col_abs: col_1.0,
            to_col: col_1.1,
        },
    ))
}

/// Parse a cell cuboid or cell range.
fn cellcuboid(i: Span) -> IResult<Span, CellCuboid> {
    let (i, (iri, sheetname_0, _dot_0, col_0, row_0, _colon, sheetname_1, _dot_1, col_1, row_1)) =
        tuple((
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
        ))(i)?;

    Ok((
        i,
        CellCuboid {
            iri,
            table: sheetname_0,
            row_abs: row_0.0,
            row: row_0.1,
            col_abs: col_0.0,
            col: col_0.1,
            to_table: sheetname_1,
            to_row_abs: row_1.0,
            to_row: row_1.1,
            to_col_abs: col_1.0,
            to_col: col_1.1,
        },
    ))
}

/// Parse a row range.
fn rowrange(i: Span) -> IResult<Span, RowRange> {
    let (i, (iri, sheetname, _dot_0, row_0, _colon, _dot_1, row_1)) =
        tuple((opt(iri), opt(sheetname), dot, row, colon, dot, row))(i)?;

    Ok((
        i,
        RowRange {
            iri,
            table: sheetname,
            row_abs: row_0.0,
            row: row_0.1,
            to_row_abs: row_1.0,
            to_row: row_1.1,
        },
    ))
}

/// Parse a col range.
fn colrange(i: Span) -> IResult<Span, ColRange> {
    let (i, (iri, sheetname, _dot_0, col_0, _colon, _dot_1, col_1)) =
        tuple((opt(iri), opt(sheetname), dot, column, colon, dot, column))(i)?;

    Ok((
        i,
        ColRange {
            iri,
            table: sheetname,
            col_abs: col_0.0,
            col: col_0.1,
            to_col_abs: col_1.0,
            to_col: col_1.1,
        },
    ))
}

/// Parse a cell range.
fn cellrange(i: Span) -> IResult<Span, CellRange> {
    let (i, (iri, sheetname, _dot_0, col_0, row_0, _colon, _dot_1, col_1, row_1)) = tuple((
        opt(iri),
        opt(sheetname),
        dot,
        column,
        row,
        colon,
        dot,
        column,
        row,
    ))(i)?;

    Ok((
        i,
        CellRange {
            iri,
            table: sheetname,
            row_abs: row_0.0,
            row: row_0.1,
            col_abs: col_0.0,
            col: col_0.1,
            to_row_abs: row_1.0,
            to_row: row_1.1,
            to_col_abs: col_1.0,
            to_col: col_1.1,
        },
    ))
}

/// Parse a cell reference.
fn cellref(i: Span) -> IResult<Span, CellRef> {
    let (i, (iri, sheetname, _dot, col, row)) =
        tuple((opt(iri), opt(sheetname), dot, column, row))(i)?;

    Ok((
        i,
        CellRef {
            iri,
            table: sheetname,
            row_abs: row.0,
            row: row.1,
            col_abs: col.0,
            col: col.1,
        },
    ))
}

/// IRI ... i18n something
fn iri(i: Span) -> IResult<Span, String> {
    let (i, iri) = terminated(quoted('\''), tag("#"))(i)?;
    Ok((i, unquote(&iri)))
}

/// Parse a sheetname.
fn sheetname(i: Span) -> IResult<Span, String> {
    let (i, _abs) = match opt(tag("$"))(i)? {
        (i, Some(_)) => (i, true),
        (i, None) => (i, false),
    };

    let (i, name) = alt((quoted('\''), recognize(many1(none_of("]. #$'")))))(i)?;

    Ok((i, unquote(&name)))
}

/// Parse a row label.
fn row(i: Span) -> IResult<Span, (bool, u32)> {
    let (i, abs) = match opt(tag("$"))(i)? {
        (i, Some(_)) => (i, true),
        (i, None) => (i, false),
    };
    let (i, row) = match recognize(many1(one_of("0123456789")))(i)? {
        (i, str) => {
            let row = match str.parse::<u32>() {
                Ok(row) => row,
                Err(_) => return Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Alpha))),
            };
            (i, row)
        }
    };

    Ok((i, (abs, row - 1)))
}

/// Parse a col label.
fn column(i: Span) -> IResult<Span, (bool, u32)> {
    let (i, abs) = match opt(tag("$"))(i)? {
        (i, Some(_)) => (i, true),
        (i, None) => (i, false),
    };
    let (i, col_str) = alpha1(i)?;

    let col = colname_as_ucell(col_str)?;

    Ok((i, (abs, col)))
}

/// Parse "."
fn dot(i: Span) -> IResult<Span, Span> {
    tag(".")(i)
}

/// Parse ":"
fn colon(i: Span) -> IResult<Span, Span> {
    tag(":")(i)
}

/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
fn quoted<I>(quote: char) -> impl FnMut(I) -> IResult<I, I, nom::error::Error<I>>
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

/// Parse a col label to a column index.
fn colname_as_ucell(i: Span) -> Result<u32, nom::Err<nom::error::Error<Span>>> {
    let mut col = 0u32;

    for c in i.chars() {
        assert!(c >= 'A' && c <= 'Z');

        let mut v = c as u32 - b'A' as u32;
        if v == 25 {
            v = 0;
            col = (col + 1) * 26;
        } else {
            v += 1;
            col *= 26;
        }
        col += v as u32;
    }

    if col == 0 {
        Err(nom::Err::Error(nom::error::Error::from_error_kind(
            i,
            ErrorKind::Digit,
        )))
    } else {
        Ok(col - 1)
    }
}

/// Unquotes a quoted table name.
fn unquote(s: &str) -> String {
    s.replace("''", "'")
}

#[cfg(test)]
mod tests {
    use super::{colname_as_ucell, column, ref_cellrange, ref_cellref, row};
    use crate::parse::{parse_reference, Span};
    use crate::refs::{CellRange, CellRef, Reference};
    use crate::str::{push_colname, push_rowname};
    use crate::{ucell, OFError};
    use nom_locate::LocatedSpan;

    #[test]
    fn test_colval() -> Result<(), nom::Err<nom::error::Error<Span<'static>>>> {
        assert_eq!(colname_as_ucell(Span::new("A"))?, 0);
        assert_eq!(colname_as_ucell(Span::new("AA"))?, 26);
        assert_eq!(colname_as_ucell(Span::new("AB"))?, 27);

        Ok(())
    }

    #[test]
    fn test_ref() -> Result<(), nom::Err<nom::error::Error<LocatedSpan<&'static str>>>> {
        let tests = vec![
            ("['file:///C:/Users/stommy/Documents/IdeaProjects/spreadsheet-ods/try/Unbenannt%202.ods'#$Tabelle1.B6]", "['file:///C:/Users/stommy/Documents/IdeaProjects/spreadsheet-ods/try/Unbenannt%202.ods'#$Tabelle1.B6]"),
            ("[#REF!]", "[#REF!]"),
            ("[.A1]", "[.A1]"),
            ("[.$A1]", "[.$A1]"),
            ("[.A$1]", "[.A$1]"),
            ("[.$A$1]", "[.$A$1]"),
            ("[sheet.A1]", "[sheet.A1]"),
            ("['sheet2'.A1]", "[sheet2.A1]"),
            ("['she''et'.A1]", "['she''et'.A1]"),
            ("[.A1:.C3]", "[.A1:.C3]"),
            ("[.$A1:.$C3]", "[.$A1:.$C3]"),
            ("[.A$1:.C$3]", "[.A$1:.C$3]"),
            ("[.$A$1:.$C$3]", "[.$A$1:.$C$3]"),
            (
                "['ref:anywhere'#$sheet.A1:.C3]",
                "['ref:anywhere'#$sheet.A1:.C3]",
            ),
            (
                "['file:anyfile'#$Tabelle1.B6]",
                "['file:anyfile'#$Tabelle1.B6]"
            ),
            ("['sheet'.A1:.C3]", "[sheet.A1:.C3]"),
            ("[sheet.A1:.C3]", "[sheet.A1:.C3]"),
            ("['she''et'.A1:.C3]", "['she''et'.A1:.C3]"),
            ("[sheet.A1:sheet3.C3]", "[sheet.A1:sheet3.C3]"),
            ("['sheet'.A1:sheet3.C3]", "[sheet.A1:sheet3.C3]"),
            ("['she''et'.A1:sheet3.C3]", "['she''et'.A1:sheet3.C3]"),
            ("[.A:.C]", "[.A:.C]"),
            ("[sheet.A:.C]", "[sheet.A:.C]"),
            ("[sheet.A:sheet2.C]", "[sheet.A:sheet2.C]"),
            ("[.1:.8]", "[.1:.8]"),
            ("[sheet.1:.8]", "[sheet.1:.8]"),
            ("[sheet.1:sheet2.8]", "[sheet.1:sheet2.8]"),
        ];
        for (t, r) in tests {
            let (_rest, reference) = parse_reference(Span::new(t))?;
            assert_eq!(r, reference.to_formula());
        }

        Ok(())
    }

    #[test]
    fn test_parse() -> Result<(), OFError> {
        fn rowname(row: ucell) -> String {
            let mut row_str = String::new();
            push_rowname(&mut row_str, row);
            row_str
        }
        fn colname(col: ucell) -> String {
            let mut col_str = String::new();
            push_colname(&mut col_str, col);
            col_str
        }

        for i in 0..704 {
            let cn = colname(i);
            let ccc = colname_as_ucell(Span::new(&cn))?;
            assert_eq!(i, ccc);
        }

        for i in 0..101 {
            let cn = rowname(i);
            let (input, (_abs, cr)) = row(Span::new(&cn))?;
            assert_eq!(i, cr);
            assert_eq!(*input.fragment(), "");
        }

        let cn = "A";
        let cc = colname_as_ucell(Span::new(cn))?;
        assert_eq!(0, cc);

        let cn = "AAAA32 ";
        let (i, (_abs, cc)) = dbg!(column(Span::new(cn)))?;
        assert_eq!(18278, cc);
        assert_eq!("32 ", *i.fragment());
        let (i, (_abs, cr)) = dbg!(row(i))?;
        assert_eq!(31, cr);
        assert_eq!(" ", *i.fragment());

        let cn = ".A3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(cr, Reference::Cell(CellRef::local(2, 0)));

        let cn = ".$A3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: None,
                row: 2,
                row_abs: false,
                col: 0,
                col_abs: true,
            })
        );

        let cn = ".A$3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: None,
                row: 2,
                row_abs: true,
                col: 0,
                col_abs: false,
            })
        );

        let cn = "fufufu.A3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: Some("fufufu".to_string()),
                row: 2,
                row_abs: false,
                col: 0,
                col_abs: false,
            })
        );

        let cn = "'lak.moi'.A3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: Some("lak.moi".to_string()),
                row: 2,
                row_abs: false,
                col: 0,
                col_abs: false,
            })
        );

        let cn = "'lak''moi'.A3";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: Some("lak'moi".to_string()),
                row: 2,
                row_abs: false,
                col: 0,
                col_abs: false,
            })
        );

        let cn = "****.B4";
        let (_i, cr) = ref_cellref(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Cell(CellRef {
                iri: None,
                table: Some("****".to_string()),
                row: 3,
                row_abs: false,
                col: 1,
                col_abs: false,
            })
        );

        let cn = ".A3:.F9";
        let (_i, cr) = ref_cellrange(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Range(CellRange {
                iri: None,
                table: None,
                row_abs: false,
                row: 2,
                col_abs: false,
                col: 0,
                to_row_abs: false,
                to_row: 8,
                to_col_abs: false,
                to_col: 5,
            })
        );

        let cn = "table.A3:.F9";
        let (_i, cr) = ref_cellrange(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Range(CellRange {
                iri: None,
                table: Some("table".to_string()),
                row_abs: false,
                row: 2,
                col_abs: false,
                col: 0,
                to_row_abs: false,
                to_row: 8,
                to_col_abs: false,
                to_col: 5,
            })
        );

        let cn = "table.A3:.F9";
        let (_i, cr) = ref_cellrange(Span::new(cn))?;
        assert_eq!(
            cr,
            Reference::Range(CellRange {
                iri: None,
                table: Some("table".to_string()),
                row_abs: false,
                row: 2,
                col_abs: false,
                col: 0,
                to_row_abs: false,
                to_row: 8,
                to_col_abs: false,
                to_col: 5,
            })
        );

        // TODO:
        // let cn = "table.A3:.F9 table.A4:.F10";
        // let (i, cr) = range_ref(None)(cn)?;
        // assert_eq!(
        //     cr,
        //     Some(vec![
        //         CellRange::remote("table", 2, 0, 8, 5),
        //         CellRange::remote("table", 3, 0, 9, 5),
        //     ])
        // );

        Ok(())
    }
}
