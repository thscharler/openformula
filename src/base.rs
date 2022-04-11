//!
//! Base types.
//!

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::num::ParseIntError;
use std::str::FromStr;

const BIT_ABS_ROW: u32 = 0b_0100_0000_0000_0000__0000_0000_0000_0000;
const BIT_ABS_COL: u32 = 0b_1000_0000_0000_0000__0000_0000_0000_0000;

const MASK_ROW: u32 = 0b_0000_0000_0000_1111__1111_1111_1111_1111;
const MASK_COL: u32 = 0b_0011_1111_1111_0000__0000_0000_0000_0000;

const SHIFT_COL: u32 = 20;

const CLEAN_ROW: u32 = 0b_0000_0000_0000_1111__1111_1111_1111_1111;
const CLEAN_COL: u32 = 0b_0000_0000_0000_0000__0000_0011_1111_1111;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub struct CCol {
    ccol: u32,
}

impl CCol {
    /// New cref with relative indexes.
    pub fn new(col: u32) -> Self {
        debug_assert!(col < 1024);

        Self {
            ccol: (col & CLEAN_COL) << SHIFT_COL,
        }
    }

    /// New cref.
    pub fn new_abs(abs_col: bool, col: u32) -> Self {
        debug_assert!(col < 1024);

        Self {
            ccol: if abs_col { BIT_ABS_COL } else { 0 } | (col & CLEAN_COL) << SHIFT_COL,
        }
    }

    /// Change the value to an absolute reference.
    pub fn absolute(mut self) -> CCol {
        self.set_col_abs(true);
        self
    }

    /// Is an absolute reference
    pub fn col_abs(&self) -> bool {
        self.ccol & BIT_ABS_COL == BIT_ABS_COL
    }

    /// Sets to an absolute reference
    pub fn set_col_abs(&mut self, abs: bool) {
        if abs {
            self.ccol |= BIT_ABS_COL;
        } else {
            self.ccol &= !BIT_ABS_COL;
        }
    }

    /// Return the column.
    pub fn col(&self) -> u32 {
        (self.ccol & MASK_COL) >> SHIFT_COL
    }

    /// Sets the column.
    ///
    /// Panics
    ///
    /// Panics if the column >= 1024.
    pub fn set_col(&mut self, col: u32) {
        debug_assert!(col < 1024);
        self.ccol = (self.ccol & !MASK_COL) | ((col & CLEAN_COL) << SHIFT_COL);
    }
}

#[derive(Debug)]
pub struct ParseColError(String);

impl Error for ParseColError {}

impl Display for ParseColError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl TryFrom<&str> for CCol {
    type Error = ParseColError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut col = 0u32;

        for c in value.chars() {
            if !('A'..='Z').contains(&c) {
                return Err(ParseColError(value.to_string()));
            }

            let mut v = c as u32 - b'A' as u32;
            if v == 25 {
                v = 0;
                col = (col + 1) * 26;
            } else {
                v += 1;
                col *= 26;
            }
            col += v;
        }

        if col == 0 {
            Err(ParseColError(value.to_string()))
        } else {
            Ok(CCol::new(col - 1))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub struct CRow {
    crow: u32,
}

impl TryFrom<&str> for CRow {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let row = u32::from_str(value)?;
        Ok(CRow::new(row - 1))
    }
}

#[allow(clippy::unusual_byte_groupings)]
impl CRow {
    /// New cref with relative indexes.
    pub fn new(row: u32) -> Self {
        debug_assert!(row < 1048576);

        Self {
            crow: (row & CLEAN_ROW),
        }
    }

    /// New cref.
    pub fn new_abs(abs_row: bool, row: u32) -> Self {
        debug_assert!(row < 1048576);

        Self {
            crow: if abs_row { BIT_ABS_ROW } else { 0 } | (row & CLEAN_ROW),
        }
    }

    /// Change the value to an absolute reference.
    pub fn absolute(mut self) -> CRow {
        self.set_row_abs(true);
        self
    }

    /// Is an absolute reference
    pub fn row_abs(&self) -> bool {
        self.crow & BIT_ABS_ROW == BIT_ABS_ROW
    }

    /// Sets to an absolute reference
    pub fn set_row_abs(&mut self, abs: bool) {
        if abs {
            self.crow |= BIT_ABS_ROW;
        } else {
            self.crow &= !BIT_ABS_ROW;
        }
    }

    /// Return the row
    pub fn row(&self) -> u32 {
        self.crow & MASK_ROW
    }

    /// Sets the row.
    ///
    /// Panics
    ///
    /// Panics if the row >= 1048576.
    pub fn set_row(&mut self, row: u32) {
        debug_assert!(row < 1048576);
        self.crow = (self.crow & !MASK_ROW) | (row & CLEAN_ROW);
    }
}

/// Basic cell reference.
/// Stores 1048576 rows and 1024 columns + 2 absolute flags in an u32.
#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub struct CRef {
    cref: u32,
}

impl Debug for CRef {
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

impl Default for CRef {
    fn default() -> Self {
        Self { cref: 0 }
    }
}

impl From<CRef> for (u32, u32) {
    fn from(cref: CRef) -> Self {
        (cref.row(), cref.col())
    }
}

impl From<&CRef> for (u32, u32) {
    fn from(cref: &CRef) -> Self {
        (cref.row(), cref.col())
    }
}

impl From<(u32, u32)> for CRef {
    fn from(v: (u32, u32)) -> Self {
        CRef::new(v.0, v.1)
    }
}

impl From<(CCol, CRow)> for CRef {
    fn from(v: (CCol, CRow)) -> Self {
        CRef {
            cref: v.0.ccol | v.1.crow,
        }
    }
}

#[allow(clippy::unusual_byte_groupings)]
impl CRef {
    /// New cref with relative indexes.
    pub fn new(row: u32, col: u32) -> Self {
        debug_assert!(col < 1024);
        debug_assert!(row < 1048576);

        Self {
            cref: (row & CLEAN_ROW) | (col & CLEAN_COL) << SHIFT_COL,
        }
    }

    /// New cref.
    pub fn new_abs(abs_row: bool, row: u32, abs_col: bool, col: u32) -> Self {
        debug_assert!(col < 1024);
        debug_assert!(row < 1048576);

        Self {
            cref: if abs_row { BIT_ABS_ROW } else { 0 }
                | (row & CLEAN_ROW)
                | if abs_col { BIT_ABS_COL } else { 0 }
                | (col & CLEAN_COL) << SHIFT_COL,
        }
    }

    /// Change the value to an absolute reference.
    pub fn absolute(mut self) -> CRef {
        self.set_row_abs(true);
        self.set_col_abs(true);
        self
    }

    /// Is an absolute reference
    pub fn col_abs(&self) -> bool {
        self.cref & BIT_ABS_COL == BIT_ABS_COL
    }

    /// Sets to an absolute reference
    pub fn set_col_abs(&mut self, abs: bool) {
        if abs {
            self.cref |= BIT_ABS_COL;
        } else {
            self.cref &= !BIT_ABS_COL;
        }
    }

    /// Is an absolute reference
    pub fn row_abs(&self) -> bool {
        self.cref & BIT_ABS_ROW == BIT_ABS_ROW
    }

    /// Sets to an absolute reference
    pub fn set_row_abs(&mut self, abs: bool) {
        if abs {
            self.cref |= BIT_ABS_ROW;
        } else {
            self.cref &= !BIT_ABS_ROW;
        }
    }

    /// Return the column.
    pub fn col(&self) -> u32 {
        (self.cref & MASK_COL) >> SHIFT_COL
    }

    /// Sets the column.
    ///
    /// Panics
    ///
    /// Panics if the column >= 1024.
    pub fn set_col(&mut self, col: u32) {
        debug_assert!(col < 1024);
        self.cref = (self.cref & !MASK_COL) | ((col & CLEAN_COL) << SHIFT_COL);
    }

    /// Return the row
    pub fn row(&self) -> u32 {
        self.cref & MASK_ROW
    }

    /// Sets the row.
    ///
    /// Panics
    ///
    /// Panics if the row >= 1048576.
    pub fn set_row(&mut self, row: u32) {
        debug_assert!(row < 1048576);
        self.cref = (self.cref & !MASK_ROW) | (row & CLEAN_ROW);
    }
}

/// A cell can span multiple rows/columns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub struct CSpan {
    row_span: u32,
    col_span: u32,
}

impl CSpan {
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

impl Default for CSpan {
    fn default() -> Self {
        Self {
            row_span: 1,
            col_span: 1,
        }
    }
}

impl From<CSpan> for (u32, u32) {
    fn from(span: CSpan) -> Self {
        (span.row_span, span.col_span)
    }
}

impl From<&CSpan> for (u32, u32) {
    fn from(span: &CSpan) -> Self {
        (span.row_span, span.col_span)
    }
}

impl From<(u32, u32)> for CSpan {
    fn from(v: (u32, u32)) -> Self {
        CSpan {
            row_span: v.0,
            col_span: v.1,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{CRef, CSpan};
    use std::mem::size_of;

    #[test]
    fn sizes_cref() {
        dbg!(size_of::<CRef>());
        dbg!(size_of::<CSpan>());
    }

    #[test]
    fn test_cref() {
        assert_eq!(
            format!("{:?}", CRef::new(7, 7)),
            "cref: 7340039 00000000011100000000000000000111 (false 7, false 7)"
        );
        assert_eq!(
            format!("{:?}", CRef::new_abs(true, 7, true, 7)),
            "cref: 3228565511 11000000011100000000000000000111 (true 7, true 7)"
        );
        assert_eq!(
            format!("{:?}", CRef::new_abs(false, 7, true, 7)),
            "cref: 2154823687 10000000011100000000000000000111 (false 7, true 7)"
        );
        assert_eq!(
            format!("{:?}", CRef::new_abs(true, 7, false, 7)),
            "cref: 1081081863 01000000011100000000000000000111 (true 7, false 7)"
        );

        let mut c = CRef::new(0, 0);
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
