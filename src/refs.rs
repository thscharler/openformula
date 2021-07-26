//!
//! Define all kinds of cell references.
//!

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use crate::parse::{parse_cellrange, parse_cellref, Span};
use crate::str::{push_cellrange, push_cellref, push_colrange, push_rowrange};
use crate::{cref, OFError};

/// There is a plethora of ways to reference cells,
/// and not all of them are always applicable.
/// To be able to put them in one jar this enum exists.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reference {
    /// Error reference.
    Err,
    /// References a single cell.
    Cell(CellRef),
    /// References a cell range.
    Range(CellRange),
    /// References a col range.
    Col(ColRange),
    /// References a row range.
    Row(RowRange),
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Reference::Err => write!(f, "[#REF!]"),
            Reference::Cell(v) => write!(f, "{}", v),
            Reference::Range(v) => write!(f, "{}", v),
            Reference::Col(v) => write!(f, "{}", v),
            Reference::Row(v) => write!(f, "{}", v),
        }
    }
}

impl Reference {
    /// Returns a cell reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        match self {
            Reference::Err => buf.push_str("#REF!"),
            Reference::Cell(v) => push_cellref(&mut buf, v),
            Reference::Range(v) => push_cellrange(&mut buf, v),
            Reference::Col(v) => push_colrange(&mut buf, v),
            Reference::Row(v) => push_rowrange(&mut buf, v),
            // Reference::Cuboid(v) => push_cellcuboid(&mut buf, v),
            // Reference::ColCuboid(v) => push_colcuboid(&mut buf, v),
            // Reference::RowCuboid(v) => push_rowcuboid(&mut buf, v),
        }
        buf.push(']');

        buf
    }
}

/// A reference to a cell, possibly in another table.
/// ```
/// use std::convert::TryFrom;
/// use openformula::refs::CellRef;
///
/// let c1 = CellRef::local(5, 2);
/// let c2 = CellRef::remote("spreadsheet-2", 7, 4);
/// let c3 = CellRef::try_from(".A5");
/// ```
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellRef {
    /// External reference
    pub iri: Option<String>,
    /// Table to reference.
    pub table: Option<String>,
    /// Cell.
    pub cell: cref,
}

impl Display for CellRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_cellref(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl TryFrom<&str> for CellRef {
    type Error = OFError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let cell_ref = parse_cellref(Span::new(s))?;
        Ok(cell_ref)
    }
}

impl CellRef {
    /// Empty
    pub fn new() -> Self {
        Self {
            iri: None,
            table: None,
            cell: cref::new(0, 0),
        }
    }

    /// Creates a cellref within the same table.
    pub fn local(row: u32, col: u32) -> Self {
        Self {
            iri: None,
            table: None,
            cell: cref::new(row, col),
        }
    }

    /// Creates a cellref that references another table.
    pub fn remote<S: Into<String>>(table: S, row: u32, col: u32) -> Self {
        Self {
            iri: None,
            table: Some(table.into()),
            cell: cref::new(row, col),
        }
    }

    /// Returns a cell reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        push_cellref(&mut buf, self);
        buf.push(']');

        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.set_col_abs(true);
        self.set_row_abs(true);
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The column remains relative, the row is fixed.
    pub fn absolute_row(mut self) -> Self {
        self.set_row_abs(true);
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The row remains relative, the column is fixed.
    pub fn absolute_col(mut self) -> Self {
        self.set_col_abs(true);
        self
    }

    /// External reference.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External reference.
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
        self.cell.set_row(row);
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.cell.row()
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.cell.set_row_abs(abs);
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.cell.row_abs()
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.cell.set_col(col);
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.cell.col()
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.cell.set_col_abs(abs);
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.cell.col_abs()
    }
}

/// A reference to a range of cells, possibly in another table.
/// As usual for a spreadsheet this is meant as inclusive from and to.
///
/// ```
/// use openformula::refs::CellRange;
///
/// let r1 = CellRange::local(0, 0, 9, 9);
/// let r2 = CellRange::origin_span(5, 5, (3, 3));
/// ```
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellRange {
    /// URI to an external source for this range.
    pub iri: Option<String>,
    /// First table for the range.
    pub table: Option<String>,
    /// From
    pub from: cref,
    /// Second table for the range. Can be empty if only one table is involved.
    pub to_table: Option<String>,
    /// To
    pub to: cref,
}

impl Display for CellRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_cellrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl CellRange {
    /// Empty
    pub fn new() -> Self {
        Self {
            iri: None,
            table: None,
            to_table: None,
            from: (0, 0).into(),
            to: (0, 0).into(),
        }
    }

    /// Creates the cell range from from + to data.
    pub fn local(row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            table: None,
            from: cref::new(row, col),
            to_table: None,
            to: cref::new(to_row, to_col),
        }
    }

    /// Creates the cell range from from + to data.
    pub fn remote<S: Into<String>>(table: S, row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            table: Some(table.into()),
            from: cref::new(row, col),
            to_table: None,
            to: cref::new(to_row, to_col),
        }
    }

    /// Creates a cell cuboid spanning multiple tables.
    pub fn cuboid<S: Into<String>>(
        table: S,
        row: u32,
        col: u32,
        to_table: S,
        to_row: u32,
        to_col: u32,
    ) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            table: Some(table.into()),
            from: cref::new(row, col),
            to_table: Some(to_table.into()),
            to: cref::new(to_row, to_col),
        }
    }

    /// Creates the cell range from origin + spanning data.
    pub fn origin_span<SP: Into<(u32, u32)>>(row: u32, col: u32, span: SP) -> Self {
        let span = span.into();
        assert!(span.0 > 0);
        assert!(span.1 > 0);
        Self {
            iri: None,
            table: None,
            from: cref::new(row, col),
            to_table: None,
            to: cref::new(row + span.0 - 1, col + span.1 - 1),
        }
    }

    /// External reference.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External reference.
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
        self.from.set_row(row);
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.from.row()
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.from.set_row_abs(abs);
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.from.row_abs()
    }

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.from.set_col(col);
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.from.col()
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.from.set_col_abs(abs);
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.from.col_abs()
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.to_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.to_table.as_ref()
    }

    /// Row
    pub fn set_to_row(&mut self, to_row: u32) {
        self.to.set_row(to_row);
    }

    /// Row
    pub fn to_row(&self) -> u32 {
        self.to.row()
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to.set_row_abs(abs);
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to.row_abs()
    }

    /// Column
    pub fn set_to_col(&mut self, to_col: u32) {
        self.to.set_col(to_col);
    }

    /// Column
    pub fn to_col(&self) -> u32 {
        self.to.col()
    }

    /// "$" column reference
    pub fn set_to_col_abs(&mut self, abs: bool) {
        self.to.set_col_abs(abs);
    }

    /// "$" column reference
    pub fn to_col_abs(&self) -> bool {
        self.to.col_abs()
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        push_cellrange(&mut buf, self);
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.from.set_row_abs(true);
        self.from.set_col_abs(true);
        self.to.set_row_abs(true);
        self.to.set_col_abs(true);
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The columns remain relative, the rows are fixed.
    pub fn absolute_rows(mut self) -> Self {
        self.from.set_row_abs(true);
        self.to.set_row_abs(true);
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The rows remain relative, the columns are fixed.
    pub fn absolute_cols(mut self) -> Self {
        self.from.set_col_abs(true);
        self.to.set_col_abs(true);
        self
    }

    /// Does the range contain the cell.
    /// This is inclusive for to_row and to_col!
    pub fn contains(&self, row: u32, col: u32) -> bool {
        row >= self.from.row()
            && row <= self.to.row()
            && col >= self.from.col()
            && col <= self.to.col()
    }

    /// Is this range any longer relevant, when looping rows first, then columns?
    pub fn out_looped(&self, row: u32, col: u32) -> bool {
        row > self.to.row() || row == self.to.row() && col > self.to.col()
    }
}

impl TryFrom<&str> for CellRange {
    type Error = OFError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let cellrange = parse_cellrange(Span::new(value))?;
        Ok(cellrange)
    }
}

/// A range over columns.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ColRange {
    /// External reference.
    pub iri: Option<String>,
    /// Refers to another table.
    pub table: Option<String>,
    /// cref, but only the column is set. any row is ignored.
    pub col: cref,
    /// Second table for the range. Can be empty if only one table is involved.
    pub to_table: Option<String>,
    /// cref, but only the column is set. any row is ignored.
    pub to_col: cref,
}

impl Display for ColRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_colrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl ColRange {
    /// Range
    pub fn new(col: u32, to_col: u32) -> Self {
        assert!(col <= to_col);
        Self {
            iri: None,
            table: None,
            col: cref::new(0, col),
            to_table: None,
            to_col: cref::new(0, to_col),
        }
    }

    /// IRI locates a different workbook.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// IRI locates a different workbook.
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

    /// Column
    pub fn set_col(&mut self, col: u32) {
        self.col.set_col(col);
    }

    /// Column
    pub fn col(&self) -> u32 {
        self.col.col()
    }

    /// "$" column reference
    pub fn set_col_abs(&mut self, abs: bool) {
        self.col.set_col_abs(abs);
    }

    /// "$" column reference
    pub fn col_abs(&self) -> bool {
        self.col.col_abs()
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.to_table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.to_table.as_ref()
    }

    /// Column
    pub fn set_to_col(&mut self, to_col: u32) {
        self.to_col.set_col(to_col);
    }

    /// Column
    pub fn to_col(&self) -> u32 {
        self.to_col.col()
    }

    /// "$" column reference
    pub fn set_to_col_abs(&mut self, abs: bool) {
        self.to_col.set_col_abs(abs);
    }

    /// "$" column reference
    pub fn to_col_abs(&self) -> bool {
        self.to_col.col_abs()
    }

    /// Is the column in this range.
    /// The range is inclusive with the to_col.
    pub fn contains(&self, col: u32) -> bool {
        col >= self.col.col() && col <= self.to_col.col()
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        push_colrange(&mut buf, self);
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.col.set_col_abs(true);
        self.to_col.set_col_abs(true);
        self
    }
}

/// A range over rows.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RowRange {
    /// External reference
    pub iri: Option<String>,
    /// Reference to another table.
    pub table: Option<String>,
    /// Row as cref, but only the row is used.
    pub row: cref,
    /// Reference to a second table. Only needed if it's different than the
    /// first one.
    pub to_table: Option<String>,
    /// To row as cref, but only the row is used.
    pub to_row: cref,
}

impl Display for RowRange {
    ///
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_rowrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl RowRange {
    /// Range
    pub fn new(row: u32, to_row: u32) -> Self {
        assert!(row <= to_row);
        Self {
            iri: None,
            table: None,
            row: cref::new(row, 0),
            to_table: None,
            to_row: cref::new(to_row, 0),
        }
    }

    /// External reference.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

    /// External reference.
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
        self.row.set_row(row);
    }

    /// Row
    pub fn row(&self) -> u32 {
        self.row.row()
    }

    /// "$" row reference
    pub fn set_row_abs(&mut self, abs: bool) {
        self.row.set_row_abs(abs);
    }

    /// "$" row reference
    pub fn row_abs(&self) -> bool {
        self.row.row_abs()
    }

    /// Table name for references into other tables.
    pub fn set_to_table<S: Into<String>>(&mut self, table: S) {
        self.table = Some(table.into());
    }

    /// Table name for references into other tables.
    pub fn to_table(&self) -> Option<&String> {
        self.table.as_ref()
    }

    /// Row
    pub fn set_to_row(&mut self, to_row: u32) {
        self.to_row.set_row(to_row);
    }

    /// Row
    pub fn to_row(&self) -> u32 {
        self.to_row.row()
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to_row.set_row_abs(abs);
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to_row.row_abs()
    }

    /// Is the row in this range.
    /// The range is inclusive with the to_row.
    pub fn contains(&self, row: u32) -> bool {
        row >= self.row.row() && row <= self.to_row.row()
    }

    /// Returns a range reference for a formula.
    pub fn to_formula(&self) -> String {
        let mut buf = String::new();
        buf.push('[');
        push_rowrange(&mut buf, self);
        buf.push(']');
        buf
    }

    /// Makes this RowRange into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.row.set_row_abs(true);
        self.to_row.set_row_abs(true);
        self
    }
}

#[cfg(test)]
mod tests {
    use crate::refs::{CellRange, CellRef};
    use crate::str::{push_cellrange, push_cellref, push_colname, push_rowname, push_tablename};

    #[test]
    fn test_names() {
        let mut buf = String::new();

        push_colname(&mut buf, 0);
        assert_eq!(buf, "A");
        buf.clear();

        push_colname(&mut buf, 1);
        assert_eq!(buf, "B");
        buf.clear();

        push_colname(&mut buf, 26);
        assert_eq!(buf, "AA");
        buf.clear();

        push_colname(&mut buf, 675);
        assert_eq!(buf, "YZ");
        buf.clear();

        push_colname(&mut buf, 676);
        assert_eq!(buf, "ZA");
        buf.clear();

        push_colname(&mut buf, u32::MAX - 1);
        assert_eq!(buf, "MWLQKWU");
        buf.clear();

        push_colname(&mut buf, u32::MAX);
        assert_eq!(buf, "MWLQKWV");
        buf.clear();

        push_rowname(&mut buf, 0);
        assert_eq!(buf, "1");
        buf.clear();

        push_rowname(&mut buf, 927);
        assert_eq!(buf, "928");
        buf.clear();

        push_rowname(&mut buf, u32::MAX - 1);
        assert_eq!(buf, "4294967295");
        buf.clear();

        push_rowname(&mut buf, u32::MAX);
        assert_eq!(buf, "4294967296");
        buf.clear();

        push_tablename(&mut buf, "fable", false);
        assert_eq!(buf, "fable.");
        buf.clear();

        push_tablename(&mut buf, "fa le", false);
        assert_eq!(buf, "'fa le'.");
        buf.clear();

        push_tablename(&mut buf, "fa'le", false);
        assert_eq!(buf, "'fa''le'.");
        buf.clear();

        push_tablename(&mut buf, "fa.le", false);
        assert_eq!(buf, "'fa.le'.");
        buf.clear();

        push_cellref(&mut buf, &CellRef::local(5, 6));
        assert_eq!(buf, ".G6");
        buf.clear();

        push_cellrange(&mut buf, &CellRange::local(5, 6, 7, 8));
        assert_eq!(buf, ".G6:.I8");
        buf.clear();

        push_cellrange(&mut buf, &CellRange::remote("blame", 5, 6, 7, 8));
        assert_eq!(buf, "blame.G6:.I8");
        buf.clear();
    }
}
