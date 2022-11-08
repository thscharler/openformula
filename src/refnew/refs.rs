use crate::refnew::error::CellRefError;
use crate::refnew::refs_format::{
    fmt_cellrange, fmt_cellranges, fmt_cellref, fmt_colrange, fmt_cref, fmt_rowrange, Fmt,
};
use crate::refnew::refs_parser::{
    parse_cell_range, parse_cell_range_list, parse_cell_ref, parse_col_range, parse_row_range, Span,
};
use std::fmt;
use std::fmt::{Display, Formatter, Write};

/// Basic cell reference.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CRef {
    /// Row reference is fixed.
    row_abs: bool,
    /// Row.
    row: u32,
    /// Column reference is fixed.
    col_abs: bool,
    /// Column.
    col: u32,
}

impl CRef {
    /// New empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// New reference.
    pub fn local(row: u32, col: u32) -> Self {
        Self {
            row_abs: false,
            row,
            col_abs: false,
            col,
        }
    }

    /// Row
    pub fn set_row(&mut self, row: u32) {
        self.row = row;
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.row
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.row_abs = abs;
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.row_abs
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.col = col;
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.col
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.col_abs = abs;
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.col_abs
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.col_abs = true;
        self.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The column remains relative, the row is fixed.
    pub fn absolute_row(mut self) -> Self {
        self.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The row remains relative, the column is fixed.
    pub fn absolute_col(mut self) -> Self {
        self.col_abs = true;
        self
    }
}

impl Display for CRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_cref(f, self)
    }
}

/// A reference to a cell, possibly in another sheet in another file.
/// ```
/// use openformula::CellRef;
/// let c1 = CellRef::local(5,2);
/// let c2 = CellRef::local(7,4).abs_col();
/// let c3 = CellRef::sheet("spreadsheet-2", 9,6);
/// let c4 = CellRef::try_from(".A6");
/// ```
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CellRef {
    /// External reference.
    iri: Option<String>,
    /// sheet reference.
    table: Option<String>,
    /// Cell reference.
    cell: CRef,
}

impl CellRef {
    pub fn new_all(
        iri: Option<String>,
        table: Option<String>,
        row_abs: bool,
        row: u32,
        col_abs: bool,
        col: u32,
    ) -> Self {
        Self {
            iri,
            table,
            cell: CRef {
                row_abs,
                row,
                col_abs,
                col,
            },
        }
    }

    pub fn new() -> Self {
        Default::default()
    }

    /// Creates a cellref within the same table.
    pub fn local(row: u32, col: u32) -> Self {
        Self {
            iri: None,
            table: None,
            cell: CRef {
                row_abs: false,
                row,
                col_abs: false,
                col,
            },
        }
    }

    /// Creates a cellref that references another table.
    pub fn remote<S: Into<String>>(table: S, row: u32, col: u32) -> Self {
        Self {
            iri: None,
            table: Some(table.into()),
            cell: CRef {
                row_abs: false,
                row,
                col_abs: false,
                col,
            },
        }
    }

    /// External file
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External file
    pub fn iri(&self) -> Option<&String> {
        self.iri.as_ref()
    }

    /// Table name for references into other tables.
    pub fn set_table<S: Into<String>>(&mut self, table: S) {
        self.table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn table(&self) -> Option<&String> {
        self.table.as_ref()
    }

    /// Row
    pub fn set_row(&mut self, row: u32) {
        self.cell.row = row;
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.cell.row
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.cell.row_abs = abs;
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.cell.row_abs
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.cell.col = col;
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.cell.col
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.cell.col_abs = abs;
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.cell.col_abs
    }

    /// Returns a cell reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        let _ = write!(buf, "{}", Fmt(|f| fmt_cellref(f, self)));
        buf.push(']');

        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.cell.col_abs = true;
        self.cell.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The column remains relative, the row is fixed.
    pub fn absolute_row(mut self) -> Self {
        self.cell.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The row remains relative, the column is fixed.
    pub fn absolute_col(mut self) -> Self {
        self.cell.col_abs = true;
        self
    }

    pub(crate) fn cell(&self) -> &CRef {
        &self.cell
    }
}

impl TryFrom<&str> for CellRef {
    type Error = CellRefError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let (rest, (cell_ref, _tok)) = parse_cell_ref(Span::new(s))?;
        if rest.is_empty() {
            Ok(cell_ref)
        } else {
            Err(CellRefError::cell_ref(rest))
        }
    }
}

impl Display for CellRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_cellref(f, self)
    }
}

/// A cell-range.
///
/// As usual for a spreadsheet this is meant as inclusive from and to.
///
/// ```
/// // let r1 = CellRange::local(0, 0, 9, 9);
/// // let r2 = CellRange::origin_span(5, 5, (3, 3));
/// ```
///
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellRange {
    /// URI to an external source for this range.
    iri: Option<String>,
    /// First sheet for the range.
    from_table: Option<String>,
    /// From
    from: CRef,
    /// Second sheet for the range. Can be empty if only one sheet is involved.
    to_table: Option<String>,
    /// To
    to: CRef,
}

impl CellRange {
    pub fn new_all(
        iri: Option<String>,
        from_table: Option<String>,
        from_row_abs: bool,
        from_row: u32,
        from_col_abs: bool,
        from_col: u32,
        to_table: Option<String>,
        to_row_abs: bool,
        to_row: u32,
        to_col_abs: bool,
        to_col: u32,
    ) -> Self {
        Self {
            iri,
            from_table,
            from: CRef {
                row_abs: from_row_abs,
                row: from_row,
                col_abs: from_col_abs,
                col: from_col,
            },
            to_table,
            to: CRef {
                row_abs: to_row_abs,
                row: to_row,
                col_abs: to_col_abs,
                col: to_col,
            },
        }
    }

    /// Empty
    pub fn new() -> Self {
        Default::default()
    }

    /// Creates the cell range from from + to data.
    pub fn local(row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            from_table: None,
            from: CRef {
                row_abs: false,
                row,
                col_abs: false,
                col,
            },
            to_table: None,
            to: CRef {
                row_abs: false,
                row: to_row,
                col_abs: false,
                col: to_col,
            },
        }
    }

    /// Creates the cell range from from + to data.
    pub fn remote<S: Into<String>>(table: S, row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            from_table: Some(table.into()),
            from: CRef {
                row_abs: false,
                row,
                col_abs: false,
                col,
            },
            to_table: None,
            to: CRef {
                row_abs: false,
                row: to_row,
                col_abs: false,
                col: to_col,
            },
        }
    }

    /// Creates the cell range from origin + spanning data.
    pub fn origin_span(row: u32, col: u32, span: (u32, u32)) -> Self {
        assert!(span.0 > 0);
        assert!(span.1 > 0);
        Self {
            iri: None,
            from_table: None,
            from: CRef {
                row_abs: false,
                row,
                col_abs: false,
                col,
            },
            to_table: None,
            to: CRef {
                row_abs: false,
                row: row + span.0 - 1,
                col_abs: false,
                col: col + span.1 - 1,
            },
        }
    }

    /// External file
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External file
    pub fn iri(&self) -> Option<&String> {
        self.iri.as_ref()
    }

    /// Table name for references into other tables.
    pub fn set_table<S: Into<String>>(&mut self, table: S) {
        self.from_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn table(&self) -> Option<&String> {
        self.from_table.as_ref()
    }

    /// Row
    pub fn set_row(&mut self, row: u32) {
        self.from.row = row;
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.from.row
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.from.row_abs = abs;
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.from.row_abs
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.from.col = col;
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.from.col
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.from.col_abs = abs;
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.from.col_abs
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.to_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.to_table.as_ref()
    }

    /// To row
    pub fn set_to_row(&mut self, to_row: u32) {
        self.to.row = to_row;
    }

    /// To row
    pub fn to_row(&self) -> u32 {
        self.to.row
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to.row_abs = abs;
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to.row_abs
    }

    /// To column
    pub fn set_to_col(&mut self, to_col: u32) {
        self.to.col = to_col;
    }

    /// To column
    pub fn to_col(&self) -> u32 {
        self.to.col
    }

    /// "$" column reference
    pub fn set_to_col_abs(&mut self, abs: bool) {
        self.to.col_abs = abs;
    }

    /// "$" column reference
    pub fn to_col_abs(&self) -> bool {
        self.to.col_abs
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        let _ = write!(buf, "{}", Fmt(|f| fmt_cellrange(f, self)));
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.from.col_abs = true;
        self.from.row_abs = true;
        self.to.col_abs = true;
        self.to.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The columns remain relative, the rows are fixed.
    pub fn absolute_rows(mut self) -> Self {
        self.from.row_abs = true;
        self.to.row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The rows remain relative, the columns are fixed.
    pub fn absolute_cols(mut self) -> Self {
        self.from.col_abs = true;
        self.to.col_abs = true;
        self
    }

    /// Does the range contain the cell.
    /// This is inclusive for to_row and to_col!
    pub fn contains(&self, row: u32, col: u32) -> bool {
        row >= self.from.row && row <= self.to.row && col >= self.from.col && col <= self.to.col
    }

    /// Is this range any longer relevant, when looping rows first, then columns?
    pub fn out_looped(&self, row: u32, col: u32) -> bool {
        row > self.to.row || row == self.to.row && col > self.to.col
    }

    pub(crate) fn from(&self) -> &CRef {
        &self.from
    }

    pub(crate) fn to(&self) -> &CRef {
        &self.to
    }
}

impl TryFrom<&str> for CellRange {
    type Error = CellRefError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let (rest, (cell_range, _tok)) = parse_cell_range(Span::new(s))?;
        if rest.is_empty() {
            Ok(cell_range)
        } else {
            Err(CellRefError::cell_ref(rest))
        }
    }
}

impl Display for CellRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_cellrange(f, self)
    }
}

/// A range over columns.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ColRange {
    /// External reference.
    iri: Option<String>,
    /// Refers to another sheet.
    from_table: Option<String>,
    /// Column reference is fixed.
    from_col_abs: bool,
    /// Column.
    from_col: u32,
    /// Second sheet for the range. Can be empty if only one sheet is involved.
    to_table: Option<String>,
    /// Column reference is fixed.
    to_col_abs: bool,
    /// Column.
    to_col: u32,
}

impl ColRange {
    pub fn new_all(
        iri: Option<String>,
        from_table: Option<String>,
        from_col_abs: bool,
        from_col: u32,
        to_table: Option<String>,
        to_col_abs: bool,
        to_col: u32,
    ) -> Self {
        Self {
            iri,
            from_table,
            from_col_abs,
            from_col,
            to_table,
            to_col_abs,
            to_col,
        }
    }

    /// New range.
    pub fn new(from_col: u32, to_col: u32) -> Self {
        assert!(from_col <= to_col);
        Self {
            iri: None,
            from_table: None,
            from_col_abs: false,
            from_col,
            to_table: None,
            to_col_abs: false,
            to_col,
        }
    }

    /// External file
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External file
    pub fn iri(&self) -> Option<&String> {
        self.iri.as_ref()
    }

    /// Table name for references into other tables.
    pub fn set_table<S: Into<String>>(&mut self, table: S) {
        self.from_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn table(&self) -> Option<&String> {
        self.from_table.as_ref()
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.from_col = col;
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.from_col
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.from_col_abs = abs;
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.from_col_abs
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.to_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.to_table.as_ref()
    }

    /// To column
    pub fn set_to_col(&mut self, to_col: u32) {
        self.to_col = to_col;
    }

    /// To column
    pub fn to_col(&self) -> u32 {
        self.to_col
    }

    /// "$" column reference
    pub fn set_to_col_abs(&mut self, abs: bool) {
        self.to_col_abs = abs;
    }

    /// "$" column reference
    pub fn to_col_abs(&self) -> bool {
        self.to_col_abs
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        let _ = write!(buf, "{}", Fmt(|f| fmt_colrange(f, self)));
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.from_col_abs = true;
        self.to_col_abs = true;
        self
    }

    /// Is the column in this range.
    /// The range is inclusive with the to_col.
    pub fn contains(&self, col: u32) -> bool {
        col >= self.from_col && col <= self.to_col
    }
}

impl TryFrom<&str> for ColRange {
    type Error = CellRefError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let (rest, (col_range, _tok)) = parse_col_range(Span::new(s))?;
        if rest.is_empty() {
            Ok(col_range)
        } else {
            Err(CellRefError::cell_ref(rest))
        }
    }
}

impl Display for ColRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_colrange(f, self)
    }
}

/// A range over rows.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RowRange {
    /// External reference
    iri: Option<String>,
    /// Reference to another sheet.
    from_table: Option<String>,
    /// Row reference is fixed.
    from_row_abs: bool,
    /// Row.
    from_row: u32,
    /// Reference to a second sheet. Only needed if it's different than the
    /// first one.
    to_table: Option<String>,
    /// Row reference is fixed.
    to_row_abs: bool,
    /// Row.
    to_row: u32,
}

impl RowRange {
    pub fn new_all(
        iri: Option<String>,
        from_table: Option<String>,
        from_row_abs: bool,
        from_row: u32,
        to_table: Option<String>,
        to_row_abs: bool,
        to_row: u32,
    ) -> Self {
        Self {
            iri,
            from_table,
            from_row_abs,
            from_row,
            to_table,
            to_row_abs,
            to_row,
        }
    }

    /// New range.
    pub fn new(from_row: u32, to_row: u32) -> Self {
        assert!(from_row <= to_row);
        Self {
            iri: None,
            from_table: None,
            from_row_abs: false,
            from_row,
            to_table: None,
            to_row_abs: false,
            to_row,
        }
    }

    /// External file
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External file
    pub fn iri(&self) -> Option<&String> {
        self.iri.as_ref()
    }

    /// Table name for references into other tables.
    pub fn set_table<S: Into<String>>(&mut self, table: S) {
        self.from_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn table(&self) -> Option<&String> {
        self.from_table.as_ref()
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.from_row
    }

    /// Row
    pub fn set_row(&mut self, row: u32) {
        self.from_row = row;
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.from_row_abs = abs;
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.from_row_abs
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.to_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.to_table.as_ref()
    }

    /// To row
    pub fn to_row(&self) -> u32 {
        self.to_row
    }

    /// To row
    pub fn set_to_row(&mut self, row: u32) {
        self.to_row = row;
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to_row_abs = abs;
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to_row_abs
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        let _ = write!(buf, "{}", Fmt(|f| fmt_rowrange(f, self)));
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.from_row_abs = true;
        self.to_row_abs = true;
        self
    }

    /// Is the row in this range.
    /// The range is inclusive with the to_row.
    pub fn contains(&self, row: u32) -> bool {
        row >= self.from_row && row <= self.to_row
    }
}

impl TryFrom<&str> for RowRange {
    type Error = CellRefError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let (rest, (row_range, _tok)) = parse_row_range(Span::new(s))?;
        if rest.is_empty() {
            Ok(row_range)
        } else {
            Err(CellRefError::cell_ref(rest))
        }
    }
}

impl Display for RowRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Fmt(|f| fmt_rowrange(f, self)))
    }
}

/// Parse a cell reference.
#[deprecated]
pub fn parse_cellref(buf: &str, pos: &mut usize) -> Result<CellRef, CellRefError> {
    let (_rest, (cell_ref, _tok)) = parse_cell_ref(Span::new(&buf[*pos..]))?;
    Ok(cell_ref)
}

/// Parse a list of range refs
#[deprecated]
pub fn parse_cellranges(
    buf: &str,
    pos: &mut usize,
) -> Result<Option<Vec<CellRange>>, CellRefError> {
    let (_rest, vec) = parse_cell_range_list(Span::new(&buf[*pos..]))?;
    Ok(vec)
}

/// Returns a list of ranges as string.
#[deprecated]
pub fn cellranges_string(vec: &[CellRange]) -> String {
    let mut buf = String::new();
    let _ = write!(buf, "{}", Fmt(|f| fmt_cellranges(f, vec)));
    buf
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn test_names() {
//         let mut buf = String::new();
//
//         push_colname(&mut buf, 0);
//         assert_eq!(buf, "A");
//         buf.clear();
//
//         push_colname(&mut buf, 1);
//         assert_eq!(buf, "B");
//         buf.clear();
//
//         push_colname(&mut buf, 26);
//         assert_eq!(buf, "AA");
//         buf.clear();
//
//         push_colname(&mut buf, 675);
//         assert_eq!(buf, "YZ");
//         buf.clear();
//
//         push_colname(&mut buf, 676);
//         assert_eq!(buf, "ZA");
//         buf.clear();
//
//         push_colname(&mut buf, u32::MAX - 1);
//         assert_eq!(buf, "MWLQKWU");
//         buf.clear();
//
//         push_colname(&mut buf, u32::MAX);
//         assert_eq!(buf, "MWLQKWV");
//         buf.clear();
//
//         push_rowname(&mut buf, 0);
//         assert_eq!(buf, "1");
//         buf.clear();
//
//         push_rowname(&mut buf, 927);
//         assert_eq!(buf, "928");
//         buf.clear();
//
//         push_rowname(&mut buf, u32::MAX - 1);
//         assert_eq!(buf, "4294967295");
//         buf.clear();
//
//         push_rowname(&mut buf, u32::MAX);
//         assert_eq!(buf, "4294967296");
//         buf.clear();
//
//         push_tablename(&mut buf, Some(&"fable".to_string()), false);
//         assert_eq!(buf, "fable.");
//         buf.clear();
//
//         push_tablename(&mut buf, Some(&"fa le".to_string()), false);
//         assert_eq!(buf, "'fa le'.");
//         buf.clear();
//
//         push_tablename(&mut buf, Some(&"fa'le".to_string()), false);
//         assert_eq!(buf, "'fa''le'.");
//         buf.clear();
//
//         push_tablename(&mut buf, Some(&"fa.le".to_string()), false);
//         assert_eq!(buf, "'fa.le'.");
//         buf.clear();
//
//         push_tablename(&mut buf, None, false);
//         assert_eq!(buf, ".");
//         buf.clear();
//
//         push_cellref(&mut buf, &CellRef::local(5, 6));
//         assert_eq!(buf, ".G6");
//         buf.clear();
//
//         push_cellrange(&mut buf, &CellRange::local(5, 6, 7, 8));
//         assert_eq!(buf, ".G6:.I8");
//         buf.clear();
//
//         push_cellrange(&mut buf, &CellRange::remote("blame", 5, 6, 7, 8));
//         assert_eq!(buf, "blame.G6:.I8");
//         buf.clear();
//     }
//
//     use crate::refnew::refs_parser::Span;
//     use crate::refnew::tokens::{col, row};
//
//     #[test]
//     fn test_parse() -> Result<(), OdsError> {
//         fn rowname(row: u32) -> String {
//             let mut row_str = String::new();
//             push_rowname(&mut row_str, row);
//             row_str
//         }
//         fn colname(col: u32) -> String {
//             let mut col_str = String::new();
//             push_colname(&mut col_str, col);
//             col_str
//         }
//
//         for i in 0..704 {
//             let cn = colname(i);
//             let (_rest, (ccc, _tok)) = col(Span::new(cn.as_str()));
//             assert_eq!(Some(i), ccc);
//             assert_eq!(cn.len(), pos);
//         }
//
//         for i in 0..101 {
//             let mut pos = 0usize;
//             let cn = rowname(i);
//             let (_rest, (cr, _tok)) = row(Span::new(cn.as_str()));
//             assert_eq!(Some(i), cr);
//             assert_eq!(cn.len(), pos);
//         }
//
//         let mut pos = 0usize;
//         let cn = "A32";
//         let cc = parse_colname(cn, &mut pos);
//         assert_eq!(Some(0), cc);
//         assert_eq!(1, pos);
//
//         let mut pos = 0usize;
//         let cn = "AAAA32 ";
//         let cc = parse_colname(cn, &mut pos);
//         assert_eq!(Some(18278), cc);
//         assert_eq!(4, pos);
//         let cr = parse_rowname(cn, &mut pos);
//         assert_eq!(Some(31), cr);
//         assert_eq!(6, pos);
//
//         let mut pos = 0usize;
//         let cn = ".A3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(cr, CellRef::local(2, 0));
//
//         let mut pos = 0usize;
//         let cn = ".$A3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: None,
//                 row: 2,
//                 row_abs: false,
//                 col: 0,
//                 col_abs: true,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = ".A$3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: None,
//                 row: 2,
//                 row_abs: true,
//                 col: 0,
//                 col_abs: false,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "fufufu.A3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: Some("fufufu".to_string()),
//                 row: 2,
//                 row_abs: false,
//                 col: 0,
//                 col_abs: false,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "'lak.moi'.A3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: Some("lak.moi".to_string()),
//                 row: 2,
//                 row_abs: false,
//                 col: 0,
//                 col_abs: false,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "'lak''moi'.A3";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: Some("lak'moi".to_string()),
//                 row: 2,
//                 row_abs: false,
//                 col: 0,
//                 col_abs: false,
//             }
//         );
//
//         let mut pos = 4usize;
//         let cn = "****.B4";
//         let cr = parse_cellref(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRef {
//                 table: None,
//                 row: 3,
//                 row_abs: false,
//                 col: 1,
//                 col_abs: false,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = ".A3:.F9";
//         let cr = parse_cellrange(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRange {
//                 table: None,
//                 row_abs: false,
//                 row: 2,
//                 col_abs: false,
//                 col: 0,
//                 to_row_abs: false,
//                 to_row: 8,
//                 to_col_abs: false,
//                 to_col: 5,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "table.A3:.F9";
//         let cr = parse_cellrange(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRange {
//                 table: Some("table".to_string()),
//                 row_abs: false,
//                 row: 2,
//                 col_abs: false,
//                 col: 0,
//                 to_row_abs: false,
//                 to_row: 8,
//                 to_col_abs: false,
//                 to_col: 5,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "table.A3:.F9";
//         let cr = parse_cellrange(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             CellRange {
//                 table: Some("table".to_string()),
//                 row_abs: false,
//                 row: 2,
//                 col_abs: false,
//                 col: 0,
//                 to_row_abs: false,
//                 to_row: 8,
//                 to_col_abs: false,
//                 to_col: 5,
//             }
//         );
//
//         let mut pos = 0usize;
//         let cn = "table.A3:.F9 table.A4:.F10";
//         let cr = parse_cellranges(cn, &mut pos)?;
//         assert_eq!(
//             cr,
//             Some(vec![
//                 CellRange::remote("table", 2, 0, 8, 5),
//                 CellRange::remote("table", 3, 0, 9, 5),
//             ])
//         );
//
//         Ok(())
//     }
// }
