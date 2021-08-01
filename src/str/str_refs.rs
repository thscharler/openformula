use crate::refs::{CellRange, CellRef, ColRange, RowRange};
use crate::CRef;

/// Appends the range reference
pub(crate) fn push_cellrange(buf: &mut String, cellrange: &CellRange) {
    push_iri(buf, cellrange.iri.as_ref());
    if let Some(table) = cellrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            cellrange.iri.is_some()
                || cellrange.from.row_abs()
                || cellrange.from.col_abs()
                || cellrange.to.row_abs()
                || cellrange.to.col_abs(),
        );
    }
    buf.push('.');
    push_cref(buf, cellrange.from);
    buf.push(':');
    if let Some(to_table) = cellrange.to_table.as_ref() {
        push_tablename(
            buf,
            to_table,
            cellrange.iri.is_some()
                || cellrange.from.row_abs()
                || cellrange.from.col_abs()
                || cellrange.to.row_abs()
                || cellrange.to.col_abs(),
        );
    }
    buf.push('.');
    push_cref(buf, cellrange.to);
}

/// Appends the cell reference
pub(crate) fn push_rowrange(buf: &mut String, rowrange: &RowRange) {
    push_iri(buf, rowrange.iri.as_ref());
    if let Some(table) = rowrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            rowrange.iri.is_some() || rowrange.row.row_abs() || rowrange.to_row.row_abs(),
        );
    }
    buf.push('.');
    if rowrange.row.row_abs() {
        buf.push('$');
    }
    push_rowname(buf, rowrange.row.row());
    buf.push(':');
    if let Some(to_table) = rowrange.to_table.as_ref() {
        push_tablename(
            buf,
            to_table,
            rowrange.iri.is_some() || rowrange.row.row_abs() || rowrange.to_row.row_abs(),
        );
    }
    buf.push('.');
    if rowrange.to_row.row_abs() {
        buf.push('$');
    }
    push_rowname(buf, rowrange.to_row.row());
}

/// Appends the cell reference
pub(crate) fn push_colrange(buf: &mut String, colrange: &ColRange) {
    push_iri(buf, colrange.iri.as_ref());
    if let Some(table) = colrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            colrange.iri.is_some() || colrange.col.col_abs() || colrange.to_col.col_abs(),
        );
    }
    buf.push('.');
    if colrange.col.col_abs() {
        buf.push('$');
    }
    push_colname(buf, colrange.col.col());
    buf.push(':');
    if let Some(to_table) = colrange.to_table.as_ref() {
        push_tablename(
            buf,
            to_table,
            colrange.iri.is_some() || colrange.col.col_abs() || colrange.to_col.col_abs(),
        );
    }
    buf.push('.');
    if colrange.to_col.col_abs() {
        buf.push('$');
    }
    push_colname(buf, colrange.to_col.col());
}

/// Appends the cell reference
pub(crate) fn push_cellref(buf: &mut String, cellref: &CellRef) {
    push_iri(buf, cellref.iri.as_ref());
    if let Some(table) = cellref.table.as_ref() {
        push_tablename(
            buf,
            table,
            cellref.iri.is_some() || cellref.cell.col_abs() || cellref.cell.row_abs(),
        );
    }
    buf.push('.');
    push_cref(buf, cellref.cell);
}

/// Appends the IRI
pub(crate) fn push_iri(buf: &mut String, iri: Option<&String>) {
    if let Some(iri) = iri {
        buf.push('\'');
        buf.push_str(&iri.replace('\'', "''"));
        buf.push('\'');
        buf.push('#');
    }
}

/// Appends the table-name
pub(crate) fn push_tablename(buf: &mut String, table: &str, abs: bool) {
    if abs {
        buf.push('$');
    }
    if table.contains(|c| c == '\'' || c == ' ' || c == '.') {
        buf.push('\'');
        buf.push_str(&table.replace('\'', "''"));
        buf.push('\'');
    } else {
        buf.push_str(table);
    }
}

pub(crate) fn push_cref(buf: &mut String, r: CRef) {
    if r.col_abs() {
        buf.push('$');
    }
    push_colname(buf, r.col());
    if r.row_abs() {
        buf.push('$');
    }
    push_rowname(buf, r.row());
}

/// Appends the spreadsheet row name
pub(crate) fn push_rowname(buf: &mut String, row: u32) {
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
        buf.push((b'0' + dbuf[j - 1]) as char);
        j -= 1;
    }
}

/// Appends the spreadsheet column name.
pub(crate) fn push_colname(buf: &mut String, mut col: u32) {
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
        buf.push((b'A' + dbuf[j - 1]) as char);
        j -= 1;
    }
}
