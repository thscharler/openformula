use crate::refs::{CellCuboid, CellRange, CellRef, ColCuboid, ColRange, RowCuboid, RowRange};
use crate::ucell;

/// Appends the cell reference
pub(crate) fn push_rowcuboid(buf: &mut String, rowcuboid: &RowCuboid) {
    push_iri(buf, rowcuboid.iri.as_ref());
    push_tablename(
        buf,
        &rowcuboid.table,
        rowcuboid.iri.is_some() || rowcuboid.row_abs || rowcuboid.to_row_abs,
    );
    buf.push('.');
    if rowcuboid.row_abs {
        buf.push('$');
    }
    push_rowname(buf, rowcuboid.row);
    buf.push(':');
    push_tablename(
        buf,
        &rowcuboid.to_table,
        rowcuboid.iri.is_some() || rowcuboid.row_abs || rowcuboid.to_row_abs,
    );
    buf.push('.');
    if rowcuboid.to_row_abs {
        buf.push('$');
    }
    push_rowname(buf, rowcuboid.to_row);
}

/// Appends the cell reference
pub(crate) fn push_colcuboid(buf: &mut String, colcuboid: &ColCuboid) {
    push_iri(buf, colcuboid.iri.as_ref());
    push_tablename(
        buf,
        &colcuboid.table,
        colcuboid.iri.is_some() || colcuboid.col_abs || colcuboid.to_col_abs,
    );
    buf.push('.');
    if colcuboid.col_abs {
        buf.push('$');
    }
    push_colname(buf, colcuboid.col);
    buf.push(':');
    push_tablename(
        buf,
        &colcuboid.to_table,
        colcuboid.iri.is_some() || colcuboid.col_abs || colcuboid.to_col_abs,
    );
    buf.push('.');
    if colcuboid.to_col_abs {
        buf.push('$');
    }
    push_colname(buf, colcuboid.to_col);
}

/// Appends the range reference
pub(crate) fn push_cellcuboid(buf: &mut String, cellcuboid: &CellCuboid) {
    push_iri(buf, cellcuboid.iri.as_ref());
    push_tablename(
        buf,
        &cellcuboid.table,
        cellcuboid.iri.is_some()
            || cellcuboid.row_abs
            || cellcuboid.col_abs
            || cellcuboid.to_row_abs
            || cellcuboid.to_col_abs,
    );
    buf.push('.');
    if cellcuboid.col_abs {
        buf.push('$');
    }
    push_colname(buf, cellcuboid.col);
    if cellcuboid.row_abs {
        buf.push('$');
    }
    push_rowname(buf, cellcuboid.row);
    buf.push(':');
    push_tablename(
        buf,
        &cellcuboid.to_table,
        cellcuboid.iri.is_some()
            || cellcuboid.row_abs
            || cellcuboid.col_abs
            || cellcuboid.to_row_abs
            || cellcuboid.to_col_abs,
    );
    buf.push('.');
    if cellcuboid.to_col_abs {
        buf.push('$');
    }
    push_colname(buf, cellcuboid.to_col);
    if cellcuboid.to_row_abs {
        buf.push('$');
    }
    push_rowname(buf, cellcuboid.to_row);
}

/// Appends the range reference
pub(crate) fn push_cellrange(buf: &mut String, cellrange: &CellRange) {
    push_iri(buf, cellrange.iri.as_ref());
    if let Some(table) = cellrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            cellrange.iri.is_some()
                || cellrange.row_abs
                || cellrange.col_abs
                || cellrange.to_row_abs
                || cellrange.to_col_abs,
        );
    }
    buf.push('.');
    if cellrange.col_abs {
        buf.push('$');
    }
    push_colname(buf, cellrange.col);
    if cellrange.row_abs {
        buf.push('$');
    }
    push_rowname(buf, cellrange.row);
    buf.push(':');
    buf.push('.');
    if cellrange.to_col_abs {
        buf.push('$');
    }
    push_colname(buf, cellrange.to_col);
    if cellrange.to_row_abs {
        buf.push('$');
    }
    push_rowname(buf, cellrange.to_row);
}

/// Appends the cell reference
pub(crate) fn push_rowrange(buf: &mut String, rowrange: &RowRange) {
    push_iri(buf, rowrange.iri.as_ref());
    if let Some(table) = rowrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            rowrange.iri.is_some() || rowrange.row_abs || rowrange.to_row_abs,
        );
    }
    buf.push('.');
    if rowrange.row_abs {
        buf.push('$');
    }
    push_rowname(buf, rowrange.row);
    buf.push(':');
    buf.push('.');
    if rowrange.to_row_abs {
        buf.push('$');
    }
    push_rowname(buf, rowrange.to_row);
}

/// Appends the cell reference
pub(crate) fn push_colrange(buf: &mut String, colrange: &ColRange) {
    push_iri(buf, colrange.iri.as_ref());
    if let Some(table) = colrange.table.as_ref() {
        push_tablename(
            buf,
            table,
            colrange.iri.is_some() || colrange.col_abs || colrange.to_col_abs,
        );
    }
    buf.push('.');
    if colrange.col_abs {
        buf.push('$');
    }
    push_colname(buf, colrange.col);
    buf.push(':');
    buf.push('.');
    if colrange.to_col_abs {
        buf.push('$');
    }
    push_colname(buf, colrange.to_col);
}

/// Appends the cell reference
pub(crate) fn push_cellref(buf: &mut String, cellref: &CellRef) {
    push_iri(buf, cellref.iri.as_ref());
    if let Some(table) = cellref.table.as_ref() {
        push_tablename(
            buf,
            table,
            cellref.iri.is_some() || cellref.col_abs || cellref.row_abs,
        );
    }
    buf.push('.');
    if cellref.col_abs {
        buf.push('$');
    }
    push_colname(buf, cellref.col);
    if cellref.row_abs {
        buf.push('$');
    }
    push_rowname(buf, cellref.row);
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

/// Appends the spreadsheet row name
pub(crate) fn push_rowname(buf: &mut String, row: ucell) {
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
pub(crate) fn push_colname(buf: &mut String, mut col: ucell) {
    let mut i = 0;
    let mut dbuf = [0u8; 7];

    if col == ucell::MAX {
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
