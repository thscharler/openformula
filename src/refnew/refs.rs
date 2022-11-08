use crate::refnew::refs_format::{
    fmt_cellrange, fmt_cellref, fmt_colrange, fmt_cref, fmt_rowrange, Fmt,
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
    type Error = OdsError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut pos = 0usize;
        parse_cellref(s, &mut pos)
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

// impl TryFrom<&str> for CellRange

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

// TODO: impl TryFrom<&str> for ColRange

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

impl Display for RowRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Fmt(|f| fmt_rowrange(f, self)))
    }
}
