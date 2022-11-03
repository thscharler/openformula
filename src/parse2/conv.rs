//!
//! Low level conversions from a Span<'a> to ...
//!

use crate::parse2::{ParseExprError, Span};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

/// Parse a bool if a '$' exists.
pub fn try_bool_from_abs_flag<'a>(i: Option<Span<'a>>) -> bool {
    if let Some(i) = i {
        *i == "$"
    } else {
        false
    }
}

/// Parse a row number to a row index.
pub fn try_u32_from_rowname<'a>(i: Span<'a>) -> Result<u32, ParseExprError> {
    match u32::from_str(*i) {
        Ok(v) => Ok(v),
        Err(e) => Err(ParseExprError::ParseInt(i.into(), e)),
    }
}

/// Error for try_u32_from_colname.
#[allow(variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseColnameError {
    /// TODO:
    InvalidChar(char),
    /// TODO:
    InvalidColname(String),
}

impl Display for ParseColnameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseColnameError::InvalidChar(e) => {
                write!(f, "Invalid char '{}'", e)?;
            }
            ParseColnameError::InvalidColname(e) => {
                write!(f, "Invalid colname {}", e)?;
            }
        }
        Ok(())
    }
}

impl Error for ParseColnameError {}

/// Parse a col label to a column index.
pub fn try_u32_from_colname<'a>(i: Span<'a>) -> Result<u32, ParseExprError> {
    let mut col = 0u32;

    for c in (*i).chars() {
        if !('A'..='Z').contains(&c) {
            return Err(ParseExprError::ParseColname(
                i.into(),
                ParseColnameError::InvalidChar(c),
            ));
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
        Err(ParseExprError::ParseColname(
            i.into(),
            ParseColnameError::InvalidColname(format!("{:?}", i)),
        ))
    } else {
        Ok(col - 1)
    }
}
