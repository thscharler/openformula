//!
//! Base types.
//!

use std::fmt::{Debug, Formatter};

/// Basic cell reference.
/// Stores 1048576 rows and 1024 columns + 2 absolute flags in an u32.
#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub struct cref {
    cref: u32,
}

impl Debug for cref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "cref: {} {:032b} ({} {}, {} {})",
            self.cref,
            self.cref,
            self.row_abs(),
            self.row(),
            self.col_abs(),
            self.col()
        )
    }
}

impl Default for cref {
    fn default() -> Self {
        Self { cref: 0 }
    }
}

impl From<cref> for (u32, u32) {
    fn from(cref: cref) -> Self {
        (cref.row(), cref.col())
    }
}

impl From<&cref> for (u32, u32) {
    fn from(cref: &cref) -> Self {
        (cref.row(), cref.col())
    }
}

impl From<(u32, u32)> for cref {
    fn from(v: (u32, u32)) -> Self {
        cref::new(v.0, v.1)
    }
}

#[allow(clippy::unusual_byte_groupings)]
impl cref {
    const BIT_ABS_ROW: u32 = 0b0100_0000__0000_0000__0000_0000__0000_0000;
    const BIT_ABS_COL: u32 = 0b1000_0000__0000_0000__0000_0000__0000_0000;

    const MASK_ROW: u32 = 0b0000_0000__0000_1111__1111_1111__1111_1111;
    const MASK_COL: u32 = 0b0011_1111__1111_0000__0000_0000__0000_0000;
    const SHIFT_COL: u32 = 20;

    /// New cref with relative indexes.
    pub fn new(row: u32, col: u32) -> Self {
        assert!(col < 1024);
        assert!(row < 1048576);

        Self {
            cref: row | col << cref::SHIFT_COL,
        }
    }

    /// New cref.
    pub fn new_abs(abs_row: bool, row: u32, abs_col: bool, col: u32) -> Self {
        assert!(col < 1024);
        assert!(row < 1048576);

        Self {
            cref: if abs_row { cref::BIT_ABS_ROW } else { 0 }
                | row
                | if abs_col { cref::BIT_ABS_COL } else { 0 }
                | col << cref::SHIFT_COL,
        }
    }

    /// Change the value to an absolute reference.
    pub fn absolute(mut self) -> cref {
        self.set_row_abs(true);
        self.set_col_abs(true);
        self
    }

    /// Is an absolute reference
    pub fn col_abs(&self) -> bool {
        self.cref & cref::BIT_ABS_COL == cref::BIT_ABS_COL
    }

    /// Sets to an absolute reference
    pub fn set_col_abs(&mut self, abs: bool) {
        if abs {
            self.cref |= cref::BIT_ABS_COL;
        } else {
            self.cref &= !cref::BIT_ABS_COL;
        }
    }

    /// Is an absolute reference
    pub fn row_abs(&self) -> bool {
        self.cref & cref::BIT_ABS_ROW == cref::BIT_ABS_ROW
    }

    /// Sets to an absolute reference
    pub fn set_row_abs(&mut self, abs: bool) {
        if abs {
            self.cref |= cref::BIT_ABS_ROW;
        } else {
            self.cref &= !cref::BIT_ABS_ROW;
        }
    }

    /// Return the column.
    pub fn col(&self) -> u32 {
        (self.cref & cref::MASK_COL) >> cref::SHIFT_COL
    }

    /// Sets the column.
    ///
    /// Panics
    ///
    /// Panics if the column >= 1024.
    pub fn set_col(&mut self, col: u32) {
        assert!(col < 1024);
        self.cref = (self.cref & !cref::MASK_COL) | (col << cref::SHIFT_COL);
    }

    /// Return the row
    pub fn row(&self) -> u32 {
        self.cref & cref::MASK_ROW
    }

    /// Sets the row.
    ///
    /// Panics
    ///
    /// Panics if the row >= 1048576.
    pub fn set_row(&mut self, row: u32) {
        assert!(row < 1048576);
        self.cref = (self.cref & !cref::MASK_ROW) | row;
    }
}

/// A cell can span multiple rows/columns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub struct cspan {
    row_span: u32,
    col_span: u32,
}

impl cspan {
    /// (1,1) span
    pub fn new() -> Self {
        Self {
            row_span: 1,
            col_span: 1,
        }
    }

    /// Row span
    pub fn row_span(&self) -> u32 {
        self.row_span
    }

    /// Row span
    pub fn set_row_span(&mut self, span: u32) {
        self.row_span = span;
    }

    /// Col span
    pub fn col_span(&self) -> u32 {
        self.col_span
    }

    /// Col span
    pub fn set_col_span(&mut self, span: u32) {
        self.col_span = span;
    }
}

impl Default for cspan {
    fn default() -> Self {
        Self {
            row_span: 1,
            col_span: 1,
        }
    }
}

impl From<cspan> for (u32, u32) {
    fn from(span: cspan) -> Self {
        (span.row_span, span.col_span)
    }
}

impl From<&cspan> for (u32, u32) {
    fn from(span: &cspan) -> Self {
        (span.row_span, span.col_span)
    }
}

impl From<(u32, u32)> for cspan {
    fn from(v: (u32, u32)) -> Self {
        cspan {
            row_span: v.0,
            col_span: v.1,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cref;

    #[test]
    fn test_cref() {
        assert_eq!(
            format!("{:?}", cref::new(7, 7)),
            "cref: 7340039 00000000011100000000000000000111 (false 7, false 7)"
        );
        assert_eq!(
            format!("{:?}", cref::new_abs(true, 7, true, 7)),
            "cref: 3228565511 11000000011100000000000000000111 (true 7, true 7)"
        );
        assert_eq!(
            format!("{:?}", cref::new_abs(false, 7, true, 7)),
            "cref: 2154823687 10000000011100000000000000000111 (false 7, true 7)"
        );
        assert_eq!(
            format!("{:?}", cref::new_abs(true, 7, false, 7)),
            "cref: 1081081863 01000000011100000000000000000111 (true 7, false 7)"
        );

        let mut c = cref::new(0, 0);
        assert_eq!(c.row_abs(), false);
        assert_eq!(c.col_abs(), false);
        assert_eq!(c.row(), 0);
        assert_eq!(c.col(), 0);

        c.set_row(1048575);
        assert_eq!(c.row(), 1048575);
        c.set_col(1023);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048575);
        c.set_row(1048574);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048574);
        c.set_col_abs(true);
        assert_eq!(c.col_abs(), true);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048574);
        c.set_row_abs(true);
        assert_eq!(c.row_abs(), true);
        assert_eq!(c.col_abs(), true);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048574);
        c.set_col_abs(false);
        assert_eq!(c.row_abs(), true);
        assert_eq!(c.col_abs(), false);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048574);
        c.set_row_abs(false);
        assert_eq!(c.row_abs(), false);
        assert_eq!(c.col_abs(), false);
        assert_eq!(c.col(), 1023);
        assert_eq!(c.row(), 1048574);
    }
}
