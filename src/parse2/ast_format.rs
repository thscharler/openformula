//!
//! Output formatting for the AST.
//!

//
// /// Appends the cell reference
// pub fn push_rowrange(buf: &mut String, rowrange: &RowRange) {
//     push_iri(buf, rowrange.iri.as_ref());
//     if let Some(table) = rowrange.table.as_ref() {
//         push_tablename(
//             buf,
//             table,
//             rowrange.iri.is_some() || rowrange.row.row_abs() || rowrange.to_row.row_abs(),
//         );
//     }
//     buf.push('.');
//     if rowrange.row.row_abs() {
//         buf.push('$');
//     }
//     push_rowname(buf, rowrange.row.row());
//     buf.push(':');
//     if let Some(to_table) = rowrange.to_table.as_ref() {
//         push_tablename(
//             buf,
//             to_table,
//             rowrange.iri.is_some() || rowrange.row.row_abs() || rowrange.to_row.row_abs(),
//         );
//     }
//     buf.push('.');
//     if rowrange.to_row.row_abs() {
//         buf.push('$');
//     }
//     push_rowname(buf, rowrange.to_row.row());
// }
//
// /// Appends the cell reference
// pub fn push_colrange(buf: &mut String, colrange: &ColRange) {
//     push_iri(buf, colrange.iri.as_ref());
//     if let Some(table) = colrange.table.as_ref() {
//         push_tablename(
//             buf,
//             table,
//             colrange.iri.is_some() || colrange.col.col_abs() || colrange.to_col.col_abs(),
//         );
//     }
//     buf.push('.');
//     if colrange.col.col_abs() {
//         buf.push('$');
//     }
//     push_colname(buf, colrange.col.col());
//     buf.push(':');
//     if let Some(to_table) = colrange.to_table.as_ref() {
//         push_tablename(
//             buf,
//             to_table,
//             colrange.iri.is_some() || colrange.col.col_abs() || colrange.to_col.col_abs(),
//         );
//     }
//     buf.push('.');
//     if colrange.to_col.col_abs() {
//         buf.push('$');
//     }
//     push_colname(buf, colrange.to_col.col());
// }

use crate::parse2::refs::{CRef, CellRange, CellRef};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

/// Helper struct to use any of these outside of Display or Debug.
/// ```
/// use openformula::parse2::ast_format::{Fmt, fmt_colname};
/// format!(Fmt(|f| fmt_colname(f, 32)));
/// ```
pub struct Fmt<F>(pub F)
where
    for<'a> F: Fn(&mut Formatter<'a>) -> fmt::Result;

impl<F> Debug for Fmt<F>
where
    for<'a> F: Fn(&mut Formatter<'a>) -> fmt::Result,
{
    /// Calls f with the given Formatter.
    fn fmt<'a>(&self, f: &mut Formatter<'a>) -> fmt::Result {
        (self.0)(f)
    }
}

impl<F> Display for Fmt<F>
where
    for<'a> F: Fn(&mut Formatter<'a>) -> fmt::Result,
{
    /// Calls f with the given Formatter.
    fn fmt<'a>(&self, f: &mut Formatter<'a>) -> fmt::Result {
        (self.0)(f)
    }
}

/// Appends the cell reference
pub fn fmt_cellref(f: &mut Formatter<'_>, cellref: &CellRef) -> fmt::Result {
    fmt_iri(f, cellref.iri.as_ref())?;
    if let Some(sheet) = cellref.sheet.as_ref() {
        fmt_sheet_name(
            f,
            sheet,
            cellref.iri.is_some() || cellref.cell.abs_col || cellref.cell.abs_row,
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cellref.cell)?;
    Ok(())
}

/// Appends the range reference
pub fn fmt_cellrange(f: &mut Formatter<'_>, cellrange: &CellRange) -> fmt::Result {
    fmt_iri(f, cellrange.iri.as_ref())?;
    if let Some(sheet) = cellrange.sheet.as_ref() {
        fmt_sheet_name(
            f,
            sheet,
            cellrange.iri.is_some()
                || cellrange.from.abs_row
                || cellrange.from.abs_col
                || cellrange.to.abs_row
                || cellrange.to.abs_col,
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cellrange.from)?;
    write!(f, ":")?;
    if let Some(to_sheet) = cellrange.to_sheet.as_ref() {
        fmt_sheet_name(
            f,
            to_sheet,
            cellrange.iri.is_some()
                || cellrange.from.abs_row
                || cellrange.from.abs_col
                || cellrange.to.abs_row
                || cellrange.to.abs_col,
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cellrange.to)?;
    Ok(())
}

/// Appends the IRI
pub fn fmt_iri(f: &mut Formatter<'_>, iri: Option<&String>) -> fmt::Result {
    if let Some(iri) = iri {
        write!(f, "'")?;
        write!(f, "{}", &iri.replace('\'', "''"))?;
        write!(f, "'")?;
        write!(f, "#")?;
    }

    Ok(())
}

/// Appends the table-name
pub fn fmt_sheet_name(f: &mut Formatter<'_>, sheet_name: &str, abs: bool) -> fmt::Result {
    if abs {
        write!(f, "$")?;
    }
    if sheet_name.contains(|c| c == '\'' || c == ' ' || c == '.') {
        write!(f, "'")?;
        write!(f, "{}", &sheet_name.replace('\'', "''"))?;
        write!(f, "'")?;
    } else {
        write!(f, "{}", sheet_name)?;
    }
    Ok(())
}

/// Appends a simple cell reference.
pub fn fmt_cref(f: &mut Formatter<'_>, r: CRef) -> fmt::Result {
    if r.abs_col {
        write!(f, "$")?;
    }
    fmt_colname(f, r.col)?;
    if r.abs_row {
        write!(f, "$")?;
    }
    fmt_rowname(f, r.row)?;
    Ok(())
}

/// Appends the spreadsheet row name
pub fn fmt_rowname(f: &mut Formatter<'_>, row: u32) -> fmt::Result {
    let mut i = 0;
    let mut dbuf = [0u8; 10];

    // temp solution
    let mut row: u64 = row.into();
    row += 1;
    while row > 0 {
        dbuf[i] = (row % 10) as u8;
        row /= 10;

        i += 1;
    }

    // reverse order
    let mut j = i;
    while j > 0 {
        write!(f, "{}", (b'0' + dbuf[j - 1]) as char)?;
        j -= 1;
    }

    Ok(())
}

/// Appends the spreadsheet column name.
pub fn fmt_colname(f: &mut Formatter<'_>, mut col: u32) -> fmt::Result {
    let mut i = 0;
    let mut dbuf = [0u8; 7];

    if col == u32::MAX {
        // unroll first loop because of overflow
        dbuf[0] = 21;
        i += 1;
        col /= 26;
    } else {
        col += 1;
    }

    while col > 0 {
        dbuf[i] = (col % 26) as u8;
        if dbuf[i] == 0 {
            dbuf[i] = 25;
            col = col / 26 - 1;
        } else {
            dbuf[i] -= 1;
            col /= 26;
        }

        i += 1;
    }

    // reverse order
    let mut j = i;
    while j > 0 {
        write!(f, "{}", (b'A' + dbuf[j - 1]) as char)?;
        j -= 1;
    }

    Ok(())
}
