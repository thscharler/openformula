use crate::parse2::ast_format::fmt_cref;
use crate::parse2::tracer::Tracer;
use crate::parse2::{ast_parser, Span};
use crate::OFError;
use std::fmt::{Display, Formatter};

/// Basic cell reference.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CRef {
    pub abs_row: bool,
    pub row: u32,
    pub abs_col: bool,
    pub col: u32,
}

impl CRef {
    pub fn new(row: u32, col: u32) -> Self {
        Self {
            abs_row: false,
            row,
            abs_col: false,
            col,
        }
    }

    pub fn new_abs(row: u32, col: u32) -> Self {
        Self {
            abs_row: true,
            row,
            abs_col: true,
            col,
        }
    }

    pub fn new_abs_col(row: u32, col: u32) -> Self {
        Self {
            abs_row: false,
            row,
            abs_col: true,
            col,
        }
    }

    pub fn new_abs_row(row: u32, col: u32) -> Self {
        Self {
            abs_row: true,
            row,
            abs_col: false,
            col,
        }
    }

    pub fn col(&self) -> u32 {
        self.col
    }

    pub fn set_col(&mut self, col: u32) {
        self.col = col;
    }

    pub fn abs_col(&self) -> bool {
        self.abs_col
    }

    pub fn set_abs_col(&mut self, abs: bool) {
        self.abs_col = abs;
    }

    pub fn row(&self) -> u32 {
        self.row
    }

    pub fn set_row(&mut self, row: u32) {
        self.row = row;
    }

    pub fn abs_row(&self) -> bool {
        self.abs_row
    }

    pub fn set_abs_row(&mut self, abs: bool) {
        self.abs_row = abs;
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
    pub row_span: u32,
    pub col_span: u32,
}

impl CSpan {
    pub fn new(row_span: u32, col_span: u32) -> Self {
        Self { row_span, col_span }
    }
}

impl Display for CSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(C{} R{})", self.col_span, self.row_span)?;
        Ok(())
    }
}

/// A reference to a cell, possibly in another table in another file.
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(iri) = &self.iri {
            write!(f, "{}#", iri)?;
        }
        if let Some(sheet) = &self.sheet {
            write!(f, "{}.", sheet)?;
        }
        write!(f, "{}", self.cell)?;
        Ok(())
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
