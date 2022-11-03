//! Types for cell references.

use crate::parse2::ast_format::{fmt_cellrange, fmt_cellref, fmt_cref, Fmt};
use crate::parse2::ast_parser::parse_cellrange;
use crate::parse2::tracer::Tracer;
use crate::parse2::{ast_parser, Span, ToFormula};
use crate::OFError;
use std::fmt::{Display, Error, Formatter, Write};
use std::{fmt, mem};

/// Basic cell reference.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CRef {
    /// Row reference is fixed.
    pub abs_row: bool,
    /// Row.
    pub row: u32,
    /// Column reference is fixed.
    pub abs_col: bool,
    /// Column.
    pub col: u32,
}

impl CRef {
    /// New empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// New reference.
    pub fn cref(row: u32, col: u32) -> Self {
        Self {
            abs_row: false,
            row,
            abs_col: false,
            col,
        }
    }

    /// Row.
    pub fn row(mut self, row: u32) -> Self {
        self.row = row;
        self
    }

    /// Column.
    pub fn col(mut self, col: u32) -> Self {
        self.col = col;
        self
    }

    /// Cell
    pub fn cell(mut self, row: u32, col: u32) -> Self {
        self.row = row;
        self.col = col;
        self
    }

    /// Absolute.
    pub fn abs(mut self) -> Self {
        self.abs_col = true;
        self.abs_row = true;
        self
    }

    /// Absolute row.
    pub fn abs_row(mut self) -> Self {
        self.abs_row = true;
        self
    }

    /// Absolute column.
    pub fn abs_col(mut self) -> Self {
        self.abs_col = true;
        self
    }
}

impl Display for CRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_cref(f, *self)
    }
}

/// A cell span.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CSpan {
    /// TODO:
    pub row_span: u32,
    /// TODO:
    pub col_span: u32,
}

impl CSpan {
    /// New.
    pub fn new() -> Self {
        Default::default()
    }

    /// New.
    pub fn span(row_span: u32, col_span: u32) -> Self {
        Self { row_span, col_span }
    }
}

impl From<(u32, u32)> for CSpan {
    fn from(v: (u32, u32)) -> Self {
        CSpan::span(v.0, v.1)
    }
}

impl Display for CSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(C{} R{})", self.col_span, self.row_span)?;
        Ok(())
    }
}

/// A reference to a cell, possibly in another table in another file.
/// ```
/// use openformula::parse2::refs::CellRef;
/// let c1 = CellRef::local(5,2);
/// let c2 = CellRef::local(7,4).abs_col();
/// let c3 = CellRef::sheet("spreadsheet-2", 9,6);
/// let c4 = CellRef::try_from(".A6");
/// ```
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CellRef {
    /// External reference.
    pub iri: Option<String>,
    /// Table reference.
    pub sheet: Option<String>,
    /// Cell reference.
    pub cell: CRef,
}

impl Display for CellRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Fmt(|f| fmt_cellref(f, self)))
    }
}

impl TryFrom<&str> for CellRef {
    type Error = OFError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let trace = Tracer::new();
        let (_rest, cell_ref) = ast_parser::parse_cellref(&trace, Span::new(s))?;
        Ok(cell_ref)
    }
}

impl CellRef {
    /// Empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// Reference within the same sheet.
    pub fn local(row: u32, col: u32) -> Self {
        Self {
            iri: None,
            sheet: None,
            cell: CRef {
                abs_row: false,
                row,
                abs_col: false,
                col,
            },
        }
    }

    /// Reference to another table.
    pub fn sheet<S: Into<String>>(sheet: S, row: u32, col: u32) -> Self {
        Self {
            iri: None,
            sheet: Some(sheet.into()),
            cell: CRef {
                abs_row: false,
                row,
                abs_col: false,
                col,
            },
        }
    }

    /// External reference.
    pub fn iri<S: Into<String>>(mut self, iri: S) -> Self {
        self.iri = Some(iri.into());
        self
    }

    /// Sheet.
    pub fn sheet_<S: Into<String>>(mut self, sheet: S) -> Self {
        self.sheet = Some(sheet.into());
        self
    }

    /// Row.
    pub fn row(mut self, row: u32) -> Self {
        self.cell.row = row;
        self
    }

    /// Column.
    pub fn col(mut self, col: u32) -> Self {
        self.cell.col = col;
        self
    }

    /// Cell
    pub fn cell(mut self, row: u32, col: u32) -> Self {
        self.cell.row = row;
        self.cell.col = col;
        self
    }

    /// Makes this an absolute CellRef.
    pub fn abs(mut self) -> Self {
        self.cell.abs_row = true;
        self.cell.abs_col = true;
        self
    }

    /// Makes this an CellRef with an absolute row.
    pub fn abs_row(mut self) -> Self {
        self.cell.abs_row = true;
        self
    }

    /// Makes this a CellRef with an absolute column.
    pub fn abs_col(mut self) -> Self {
        self.cell.abs_col = true;
        self
    }
}

impl ToFormula for CellRef {
    fn to_formula(&self) -> Result<String, fmt::Error> {
        let mut buf = String::new();
        write!(buf, "[")?;
        write!(buf, "{}", Fmt(|f| fmt_cellref(f, self)))?;
        write!(buf, "]")?;
        Ok(buf)
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
    /// First sheet for the range.
    pub sheet: Option<String>,
    /// From
    pub from: CRef,
    /// Second sheet for the range. Can be empty if only one sheet is involved.
    pub to_sheet: Option<String>,
    /// To
    pub to: CRef,
}

impl Display for CellRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Fmt(|f| fmt_cellrange(f, self)))
    }
}

impl CellRange {
    /// Empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// Local to the sheet.
    pub fn local(row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        let mut row = row;
        let mut col = col;
        let mut to_row = to_row;
        let mut to_col = to_col;

        debug_assert!(row <= to_row);
        debug_assert!(col <= to_col);
        if row > to_row {
            mem::swap(&mut row, &mut to_row);
        }
        if col > to_col {
            mem::swap(&mut col, &mut to_col);
        }

        Self {
            iri: None,
            sheet: None,
            from: CRef {
                abs_row: false,
                row,
                abs_col: false,
                col,
            },
            to_sheet: None,
            to: CRef {
                abs_row: false,
                row: to_row,
                abs_col: false,
                col: to_col,
            },
        }
    }

    /// Creates a CellRange with a span.
    pub fn span(row: u32, col: u32, span: CSpan) -> Self {
        let mut span = span;

        debug_assert!(span.row_span > 0);
        debug_assert!(span.col_span > 0);
        if span.row_span == 0 {
            span.row_span = 1;
        }
        if span.col_span == 0 {
            span.col_span = 1;
        }

        Self {
            iri: None,
            sheet: None,
            from: CRef {
                abs_row: false,
                row,
                abs_col: false,
                col,
            },
            to_sheet: None,
            to: CRef {
                abs_row: false,
                row: row + span.row_span - 1,
                abs_col: false,
                col: col + span.col_span - 1,
            },
        }
    }

    /// Refers a different sheet.
    pub fn sheet<S: Into<String>>(sheet: S, row: u32, col: u32, to_row: u32, to_col: u32) -> Self {
        let mut row = row;
        let mut col = col;
        let mut to_row = to_row;
        let mut to_col = to_col;

        debug_assert!(row <= to_row);
        debug_assert!(col <= to_col);
        if row > to_row {
            mem::swap(&mut row, &mut to_row);
        }
        if col > to_col {
            mem::swap(&mut col, &mut to_col);
        }

        Self {
            iri: None,
            sheet: Some(sheet.into()),
            from: CRef {
                abs_row: false,
                row,
                abs_col: false,
                col,
            },
            to_sheet: None,
            to: CRef {
                abs_row: false,
                row: to_row,
                abs_col: false,
                col: to_col,
            },
        }
    }

    /// External reference.
    pub fn iri<S: Into<String>>(mut self, iri: S) -> Self {
        self.iri = Some(iri.into());
        self
    }

    /// Sheet.
    pub fn from_sheet<S: Into<String>>(mut self, sheet: S) -> Self {
        self.sheet = Some(sheet.into());
        self
    }

    /// Row.
    pub fn from_row(mut self, row: u32) -> Self {
        self.from.row = row;
        self
    }

    /// Column.
    pub fn from_col(mut self, col: u32) -> Self {
        self.from.col = col;
        self
    }

    /// Cell
    pub fn from_cell(mut self, row: u32, col: u32) -> Self {
        self.from.row = row;
        self.from.col = col;
        self
    }

    /// Makes this an absolute CellRef.
    pub fn abs_from(mut self) -> Self {
        self.from.abs_row = true;
        self.from.abs_col = true;
        self
    }

    /// Makes this an CellRef with an absolute row.
    pub fn abs_from_row(mut self) -> Self {
        self.from.abs_row = true;
        self
    }

    /// Makes this a CellRef with an absolute column.
    pub fn abs_from_col(mut self) -> Self {
        self.from.abs_col = true;
        self
    }

    /// Sheet.
    pub fn to_sheet<S: Into<String>>(mut self, sheet: S) -> Self {
        self.to_sheet = Some(sheet.into());
        self
    }

    /// Row.
    pub fn to_row(mut self, row: u32) -> Self {
        self.to.row = row;
        self
    }

    /// Column.
    pub fn to_col(mut self, col: u32) -> Self {
        self.to.col = col;
        self
    }

    /// Cell
    pub fn to_cell(mut self, row: u32, col: u32) -> Self {
        self.to.row = row;
        self.to.col = col;
        self
    }

    /// Makes this an absolute CellRef.
    pub fn abs_to(mut self) -> Self {
        self.to.abs_row = true;
        self.to.abs_col = true;
        self
    }

    /// Makes this an CellRef with an absolute row.
    pub fn abs_to_row(mut self) -> Self {
        self.to.abs_row = true;
        self
    }

    /// Makes this a CellRef with an absolute column.
    pub fn abs_to_col(mut self) -> Self {
        self.to.abs_col = true;
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
}

impl ToFormula for CellRange {
    fn to_formula(&self) -> Result<String, Error> {
        let mut buf = String::new();
        write!(buf, "[")?;
        write!(buf, "{}", Fmt(|f| fmt_cellrange(f, self)))?;
        write!(buf, "]")?;
        Ok(buf)
    }
}

impl TryFrom<&str> for CellRange {
    type Error = OFError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let trace = Tracer::new();
        let (_, cellrange) = parse_cellrange(&trace, Span::new(value))?;
        Ok(cellrange)
    }
}
