use crate::refnew::refs::{CRef, CellRange, CellRef, ColRange, RowRange};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

pub(crate) struct Fmt<F>(pub F)
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
pub fn fmt_cellref(f: &mut Formatter<'_>, cell_ref: &CellRef) -> fmt::Result {
    fmt_iri(f, cell_ref.iri())?;
    if let Some(sheet) = cell_ref.table().as_ref() {
        fmt_table_name(
            f,
            sheet,
            cell_ref.iri().is_some() || cell_ref.col_abs() || cell_ref.row_abs(),
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cell_ref.cell())?;
    Ok(())
}

/// Appends the range reference
pub fn fmt_cellrange(f: &mut Formatter<'_>, cell_range: &CellRange) -> fmt::Result {
    fmt_iri(f, cell_range.iri())?;
    if let Some(table) = cell_range.table().as_ref() {
        fmt_table_name(
            f,
            table,
            cell_range.iri().is_some()
                || cell_range.from().row_abs()
                || cell_range.from().col_abs()
                || cell_range.to().row_abs()
                || cell_range.to().col_abs(),
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cell_range.from())?;
    write!(f, ":")?;
    if let Some(to_table) = cell_range.to_table().as_ref() {
        fmt_table_name(
            f,
            to_table,
            cell_range.iri().is_some()
                || cell_range.from().row_abs()
                || cell_range.from().col_abs()
                || cell_range.to().row_abs()
                || cell_range.to().col_abs(),
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cell_range.to())?;
    Ok(())
}

/// Appends the cell reference
pub fn fmt_colrange(f: &mut Formatter<'_>, col_range: &ColRange) -> fmt::Result {
    fmt_iri(f, col_range.iri())?;
    if let Some(sheet) = col_range.table().as_ref() {
        fmt_table_name(
            f,
            sheet,
            col_range.iri().is_some() || col_range.col_abs() || col_range.to_col_abs(),
        )?;
    }
    write!(f, ".")?;
    if col_range.col_abs() {
        write!(f, "$")?;
    }
    fmt_colname(f, col_range.col())?;
    write!(f, ":")?;
    if let Some(to_sheet) = col_range.to_table().as_ref() {
        fmt_table_name(
            f,
            to_sheet,
            col_range.iri().is_some() || col_range.col_abs() || col_range.to_col_abs(),
        )?;
    }
    write!(f, ".")?;
    if col_range.to_col_abs() {
        write!(f, "$")?;
    }
    fmt_colname(f, col_range.to_col())?;
    Ok(())
}

/// Appends the cell reference
pub fn fmt_rowrange(f: &mut Formatter<'_>, row_range: &RowRange) -> fmt::Result {
    fmt_iri(f, row_range.iri())?;
    if let Some(table) = row_range.table().as_ref() {
        fmt_table_name(
            f,
            table,
            row_range.iri().is_some() || row_range.row_abs() || row_range.to_row_abs(),
        )?;
    }
    write!(f, ".")?;
    if row_range.row_abs() {
        write!(f, "$")?;
    }
    fmt_rowname(f, row_range.row())?;
    write!(f, ":")?;
    if let Some(to_table) = row_range.to_table().as_ref() {
        fmt_table_name(
            f,
            to_table,
            row_range.iri().is_some() || row_range.row_abs() || row_range.to_row_abs(),
        )?;
    }
    write!(f, ".")?;
    if row_range.to_row_abs() {
        write!(f, "$")?;
    }
    fmt_rowname(f, row_range.to_row())?;
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
pub fn fmt_table_name(f: &mut Formatter<'_>, table_name: &str, abs: bool) -> fmt::Result {
    if abs {
        write!(f, "$")?;
    }
    if table_name.contains(|c| c == '\'' || c == ' ' || c == '.') {
        write!(f, "'")?;
        write!(f, "{}", &table_name.replace('\'', "''"))?;
        write!(f, "'")?;
    } else {
        write!(f, "{}", table_name)?;
    }
    Ok(())
}

/// Appends a simple cell reference.
pub(crate) fn fmt_cref(f: &mut Formatter<'_>, r: &CRef) -> fmt::Result {
    if r.col_abs() {
        write!(f, "$")?;
    }
    fmt_colname(f, r.col())?;
    if r.row_abs() {
        write!(f, "$")?;
    }
    fmt_rowname(f, r.row())?;
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
