// /// Appends the range reference
// pub fn push_cellrange(buf: &mut String, cellrange: &CellRange) {
//     push_iri(buf, cellrange.iri.as_ref());
//     if let Some(table) = cellrange.table.as_ref() {
//         push_tablename(
//             buf,
//             table,
//             cellrange.iri.is_some()
//                 || cellrange.from.row_abs()
//                 || cellrange.from.col_abs()
//                 || cellrange.to.row_abs()
//                 || cellrange.to.col_abs(),
//         );
//     }
//     buf.push('.');
//     push_cref(buf, cellrange.from);
//     buf.push(':');
//     if let Some(to_table) = cellrange.to_table.as_ref() {
//         push_tablename(
//             buf,
//             to_table,
//             cellrange.iri.is_some()
//                 || cellrange.from.row_abs()
//                 || cellrange.from.col_abs()
//                 || cellrange.to.row_abs()
//                 || cellrange.to.col_abs(),
//         );
//     }
//     buf.push('.');
//     push_cref(buf, cellrange.to);
// }
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

use crate::parse2::refs::{CRef, CellRef};
use std::fmt::Formatter;

/// Appends the cell reference
pub fn push_cellref(f: &mut Formatter<'_>, cellref: &CellRef) -> std::fmt::Result {
    fmt_iri(f, cellref.iri.as_ref())?;
    if let Some(table) = cellref.sheet.as_ref() {
        fmt_tablename(
            f,
            table,
            cellref.iri.is_some() || cellref.cell.abs_col || cellref.cell.abs_row,
        )?;
    }
    write!(f, ".")?;
    fmt_cref(f, cellref.cell)?;
    Ok(())
}

/// Appends the IRI
pub fn fmt_iri(f: &mut Formatter<'_>, iri: Option<&String>) -> std::fmt::Result {
    if let Some(iri) = iri {
        write!(f, "'")?;
        write!(f, "{}", &iri.replace('\'', "''"))?;
        write!(f, "'")?;
        write!(f, "#")?;
    }

    Ok(())
}

/// Appends the table-name
pub fn fmt_tablename(f: &mut Formatter<'_>, table: &str, abs: bool) -> std::fmt::Result {
    if abs {
        write!(f, "$")?;
    }
    if table.contains(|c| c == '\'' || c == ' ' || c == '.') {
        write!(f, "'")?;
        write!(f, "{}", &table.replace('\'', "''"))?;
        write!(f, "'")?;
    } else {
        write!(f, "{}", table)?;
    }
    Ok(())
}

/// Appends a simple cell reference.
pub fn fmt_cref(f: &mut Formatter<'_>, r: CRef) -> std::fmt::Result {
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
pub fn fmt_rowname(f: &mut Formatter<'_>, row: u32) -> std::fmt::Result {
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
pub fn fmt_colname(f: &mut Formatter<'_>, mut col: u32) -> std::fmt::Result {
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
