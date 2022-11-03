//!
//! Output formatting for the AST.
//!

use crate::parse2::refs::{CRef, CellRange, CellRef, ColRange, RowRange};
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
    if let Some(sheet) = cellrange.from_sheet.as_ref() {
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

/// Appends the cell reference
pub fn fmt_colrange(f: &mut Formatter<'_>, colrange: &ColRange) -> fmt::Result {
    fmt_iri(f, colrange.iri.as_ref())?;
    if let Some(sheet) = colrange.from_sheet.as_ref() {
        fmt_sheet_name(
            f,
            sheet,
            colrange.iri.is_some() || colrange.abs_from_col || colrange.abs_to_col,
        )?;
    }
    write!(f, ".")?;
    if colrange.abs_from_col {
        write!(f, "$")?;
    }
    fmt_colname(f, colrange.from_col)?;
    write!(f, ":")?;
    if let Some(to_sheet) = colrange.to_sheet.as_ref() {
        fmt_sheet_name(
            f,
            to_sheet,
            colrange.iri.is_some() || colrange.abs_from_col || colrange.abs_to_col,
        )?;
    }
    write!(f, ".")?;
    if colrange.abs_to_col {
        write!(f, "$")?;
    }
    fmt_colname(f, colrange.to_col)?;
    Ok(())
}

/// Appends the cell reference
pub fn fmt_rowrange(f: &mut Formatter<'_>, rowrange: &RowRange) -> fmt::Result {
    fmt_iri(f, rowrange.iri.as_ref())?;
    if let Some(sheet) = rowrange.from_sheet.as_ref() {
        fmt_sheet_name(
            f,
            sheet,
            rowrange.iri.is_some() || rowrange.abs_from_row || rowrange.abs_to_row,
        )?;
    }
    write!(f, ".")?;
    if rowrange.abs_from_row {
        write!(f, "$")?;
    }
    fmt_rowname(f, rowrange.from_row)?;
    write!(f, ":")?;
    if let Some(to_sheet) = rowrange.to_sheet.as_ref() {
        fmt_sheet_name(
            f,
            to_sheet,
            rowrange.iri.is_some() || rowrange.abs_from_row || rowrange.abs_to_row,
        )?;
    }
    write!(f, ".")?;
    if rowrange.abs_to_row {
        write!(f, "$")?;
    }
    fmt_rowname(f, rowrange.to_row)?;
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
