use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use crate::parse::{parse_cellrange, parse_cellref, Span};
use crate::str::{
    push_cellcuboid, push_cellrange, push_cellref, push_colcuboid, push_colrange, push_rowcuboid,
    push_rowrange,
};
use crate::{ucell, OFError};

/// There is a plethora of ways to reference cells,
/// and not all of them are always applicable.
/// To be able to put them in one jar this enum exists.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reference {
    Err,
    /// References a single cell.
    Cell(CellRef),
    /// References a cell range.
    Range(CellRange),
    /// References a col range.
    Col(ColRange),
    /// References a row range.
    Row(RowRange),
    /// References a cell range across multiple sheets.
    Cuboid(CellCuboid),
    /// References a col range across multiple sheets.
    ColCuboid(ColCuboid),
    /// References a row range across multiple sheets.
    RowCuboid(RowCuboid),
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Reference::Err => write!(f, "{}", "[#REF!]"),
            Reference::Cell(v) => write!(f, "{}", v),
            Reference::Range(v) => write!(f, "{}", v),
            Reference::Col(v) => write!(f, "{}", v),
            Reference::Row(v) => write!(f, "{}", v),
            Reference::Cuboid(v) => write!(f, "{}", v),
            Reference::ColCuboid(v) => write!(f, "{}", v),
            Reference::RowCuboid(v) => write!(f, "{}", v),
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
            Reference::Cuboid(v) => push_cellcuboid(&mut buf, v),
            Reference::ColCuboid(v) => push_colcuboid(&mut buf, v),
            Reference::RowCuboid(v) => push_rowcuboid(&mut buf, v),
        }
        buf.push(']');

        buf
    }
}

/// A reference to a cell, possibly in another table.
/// ```
/// use spreadsheet_ods::CellRef;
/// use std::convert::TryFrom;
///
/// let c1 = CellRef::local(5, 2);
/// let c2 = CellRef::remote("spreadsheet-2", 7, 4);
/// let c3 = CellRef::try_from(".A5");
/// ```
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellRef {
    pub iri: Option<String>,
    pub table: Option<String>,
    pub row_abs: bool,
    /* Absolute ($) reference */
    pub row: ucell,
    pub col_abs: bool,
    /* Absolute ($) reference */
    pub col: ucell,
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
        let (_, cell_ref) = parse_cellref(Span::new(s))?;
        Ok(cell_ref)
    }
}

impl CellRef {
    pub fn new() -> Self {
        Self {
            iri: None,
            table: None,
            row: 0,
            col: 0,
            row_abs: false,
            col_abs: false,
        }
    }

    /// Creates a cellref within the same table.
    pub fn local(row: ucell, col: ucell) -> Self {
        Self {
            iri: None,
            table: None,
            row,
            row_abs: false,
            col,
            col_abs: false,
        }
    }

    /// Creates a cellref that references another table.
    pub fn remote<S: Into<String>>(table: S, row: ucell, col: ucell) -> Self {
        Self {
            iri: None,
            table: Some(table.into()),
            row,
            row_abs: false,
            col,
            col_abs: false,
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

    /// IRI locates a different workbook.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

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

    pub fn set_row(&mut self, row: ucell) {
        self.row = row;
    }

    pub fn row(&self) -> ucell {
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

    pub fn set_col(&mut self, col: ucell) {
        self.col = col;
    }

    pub fn col(&self) -> ucell {
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
}

/// A reference to a range of cells, possibly in another table.
/// As usual for a spreadsheet this is meant as inclusive from and to.
///
/// ```
/// use spreadsheet_ods::CellRange;
/// let r1 = CellRange::local(0, 0, 9, 9);
/// let r2 = CellRange::origin_span(5, 5, (3, 3));
/// ```
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellRange {
    pub iri: Option<String>,
    pub table: Option<String>,
    pub row_abs: bool,
    /* Absolute ($) reference */
    pub row: ucell,
    pub col_abs: bool,
    /* Absolute ($) reference */
    pub col: ucell,
    pub to_row_abs: bool,
    /* Absolute ($) reference */
    pub to_row: ucell,
    pub to_col_abs: bool,
    /* Absolute ($) reference */
    pub to_col: ucell,
}

impl Display for CellRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_cellrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl CellRange {
    pub fn new() -> Self {
        Self {
            iri: None,
            table: None,
            row_abs: false,
            row: 0,
            col_abs: false,
            col: 0,
            to_row_abs: false,
            to_row: 0,
            to_col_abs: false,
            to_col: 0,
        }
    }

    /// Creates the cell range from from + to data.
    pub fn local(row: ucell, col: ucell, to_row: ucell, to_col: ucell) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            table: None,
            row_abs: false,
            row,
            col_abs: false,
            col,
            to_row_abs: false,
            to_row,
            to_col_abs: false,
            to_col,
        }
    }

    /// Creates the cell range from from + to data.
    pub fn remote<S: Into<String>>(
        table: S,
        row: ucell,
        col: ucell,
        to_row: ucell,
        to_col: ucell,
    ) -> Self {
        assert!(row <= to_row);
        assert!(col <= to_col);
        Self {
            iri: None,
            table: Some(table.into()),
            row_abs: false,
            row,
            col_abs: false,
            col,
            to_row_abs: false,
            to_row,
            to_col_abs: false,
            to_col,
        }
    }

    /// Creates the cell range from origin + spanning data.
    pub fn origin_span(row: ucell, col: ucell, span: (ucell, ucell)) -> Self {
        assert!(span.0 > 0);
        assert!(span.1 > 0);
        Self {
            iri: None,
            table: None,
            row_abs: false,
            row,
            col_abs: false,
            col,
            to_row_abs: false,
            to_row: row + span.0 - 1,
            to_col_abs: false,
            to_col: col + span.1 - 1,
        }
    }

    /// IRI locates a different workbook.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

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

    pub fn set_row(&mut self, row: ucell) {
        self.row = row;
    }

    pub fn row(&self) -> ucell {
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

    pub fn set_col(&mut self, col: ucell) {
        self.col = col;
    }

    pub fn col(&self) -> ucell {
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

    pub fn set_to_row(&mut self, to_row: ucell) {
        self.to_row = to_row;
    }

    pub fn to_row(&self) -> ucell {
        self.to_row
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to_row_abs = abs;
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to_row_abs
    }

    pub fn set_to_col(&mut self, to_col: ucell) {
        self.to_col = to_col;
    }

    pub fn to_col(&self) -> ucell {
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
        push_cellrange(&mut buf, self);
        buf.push(']');
        buf
    }

    /// Makes this CellReference into an absolute reference.
    pub fn absolute(mut self) -> Self {
        self.col_abs = true;
        self.row_abs = true;
        self.to_col_abs = true;
        self.to_row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The columns remain relative, the rows are fixed.
    pub fn absolute_rows(mut self) -> Self {
        self.row_abs = true;
        self.to_row_abs = true;
        self
    }

    /// Makes this CellReference into an absolute reference.
    /// The rows remain relative, the columns are fixed.
    pub fn absolute_cols(mut self) -> Self {
        self.col_abs = true;
        self.to_col_abs = true;
        self
    }

    /// Does the range contain the cell.
    /// This is inclusive for to_row and to_col!
    pub fn contains(&self, row: ucell, col: ucell) -> bool {
        row >= self.row && row <= self.to_row && col >= self.col && col <= self.to_col
    }

    /// Is this range any longer relevant, when looping rows first, then columns?
    pub fn out_looped(&self, row: ucell, col: ucell) -> bool {
        row > self.to_row || row == self.to_row && col > self.to_col
    }
}

impl TryFrom<&str> for CellRange {
    type Error = OFError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let (_, cellrange) = parse_cellrange(Span::new(value))?;
        Ok(cellrange)
    }
}

/// A range over columns.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ColRange {
    pub iri: Option<String>,
    pub table: Option<String>,
    pub col_abs: bool,
    pub col: ucell,
    pub to_col_abs: bool,
    pub to_col: ucell,
}

impl Display for ColRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_colrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl ColRange {
    pub fn new(col: ucell, to_col: ucell) -> Self {
        assert!(col <= to_col);
        Self {
            iri: None,
            table: None,
            col_abs: false,
            col,
            to_col_abs: false,
            to_col,
        }
    }

    /// IRI locates a different workbook.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

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

    pub fn set_col(&mut self, col: ucell) {
        self.col = col;
    }

    pub fn col(&self) -> ucell {
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

    pub fn set_to_col(&mut self, to_col: ucell) {
        self.to_col = to_col;
    }

    pub fn to_col(&self) -> ucell {
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

    /// Is the column in this range.
    /// The range is inclusive with the to_col.
    pub fn contains(&self, col: ucell) -> bool {
        col >= self.col && col <= self.to_col
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
        self.col_abs = true;
        self.to_col_abs = true;
        self
    }
}

/// A range over rows.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RowRange {
    pub iri: Option<String>,
    pub table: Option<String>,
    pub row_abs: bool,
    pub row: ucell,
    pub to_row_abs: bool,
    pub to_row: ucell,
}

impl Display for RowRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_rowrange(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl RowRange {
    pub fn new(row: ucell, to_row: ucell) -> Self {
        assert!(row <= to_row);
        Self {
            iri: None,
            table: None,
            row_abs: false,
            row,
            to_row_abs: false,
            to_row,
        }
    }

    /// IRI locates a different workbook.
    pub fn set_iri<S: Into<String>>(&mut self, iri: S) {
        self.iri = Some(iri.into());
    }

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

    pub fn set_row(&mut self, row: ucell) {
        self.row = row;
    }

    pub fn row(&self) -> ucell {
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

    pub fn set_to_row(&mut self, to_row: ucell) {
        self.to_row = to_row;
    }

    pub fn to_row(&self) -> ucell {
        self.to_row
    }

    /// "$" row reference
    pub fn set_to_row_abs(&mut self, abs: bool) {
        self.to_row_abs = abs;
    }

    /// "$" row reference
    pub fn to_row_abs(&self) -> bool {
        self.to_row_abs
    }

    /// Is the row in this range.
    /// The range is inclusive with the to_row.
    pub fn contains(&self, row: ucell) -> bool {
        row >= self.row && row <= self.to_row
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
        self.row_abs = true;
        self.to_row_abs = true;
        self
    }
}

/// A cellrange that spans a sequence of tables.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CellCuboid {
    pub iri: Option<String>,
    pub table: String,
    pub row_abs: bool,
    /* Absolute ($) reference */
    pub row: ucell,
    pub col_abs: bool,
    /* Absolute ($) reference */
    pub col: ucell,
    pub to_table: String,
    pub to_row_abs: bool,
    /* Absolute ($) reference */
    pub to_row: ucell,
    pub to_col_abs: bool,
    /* Absolute ($) reference */
    pub to_col: ucell,
}

impl Display for CellCuboid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_cellcuboid(&mut buf, self);

        write!(f, "{}", buf)
    }
}

impl CellCuboid {
    pub fn new() -> Self {
        Self {
            iri: None,
            table: "".to_string(),
            row_abs: false,
            row: 0,
            col_abs: false,
            col: 0,
            to_table: "".to_string(),
            to_row_abs: false,
            to_row: 0,
            to_col_abs: false,
            to_col: 0,
        }
    }
}

/// A col range that spans a sequence of tables.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ColCuboid {
    pub iri: Option<String>,
    pub table: String,
    pub col_abs: bool,
    pub col: ucell,
    pub to_table: String,
    pub to_col_abs: bool,
    pub to_col: ucell,
}

impl Display for ColCuboid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_colcuboid(&mut buf, self);

        write!(f, "{}", buf)
    }
}

/// A row range that spans a sequence of tables.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RowCuboid {
    pub iri: Option<String>,
    pub table: String,
    pub row_abs: bool,
    pub row: ucell,
    pub to_table: String,
    pub to_row_abs: bool,
    pub to_row: ucell,
}

impl Display for RowCuboid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        push_rowcuboid(&mut buf, self);

        write!(f, "{}", buf)
    }
}

#[cfg(test)]
mod tests {
    use crate::refs::{CellRange, CellRef};
    use crate::str::{push_cellrange, push_cellref, push_colname, push_rowname, push_tablename};
    use crate::ucell;

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

        push_colname(&mut buf, ucell::MAX - 1);
        assert_eq!(buf, "MWLQKWU");
        buf.clear();

        push_colname(&mut buf, ucell::MAX);
        assert_eq!(buf, "MWLQKWV");
        buf.clear();

        push_rowname(&mut buf, 0);
        assert_eq!(buf, "1");
        buf.clear();

        push_rowname(&mut buf, 927);
        assert_eq!(buf, "928");
        buf.clear();

        push_rowname(&mut buf, ucell::MAX - 1);
        assert_eq!(buf, "4294967295");
        buf.clear();

        push_rowname(&mut buf, ucell::MAX);
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
