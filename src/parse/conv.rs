use crate::parse::CellToken;
use crate::{cref, OFError};
use nom::{Compare, CompareResult, InputIter};
use std::fmt::Debug;

/// Parse a bool if a '$' exists.
pub fn try_bool_from_abs_flag<'a, I>(i: Option<I>) -> Result<bool, OFError>
where
    I: Clone + Debug + Compare<&'a str>,
{
    if let Some(i) = i {
        Ok(i.compare("$") == CompareResult::Ok)
    } else {
        Ok(false)
    }
}

/// Parse a cell reference to a cref.
pub fn try_cref_from_token<'a, I>(i: CellToken<I>) -> Result<cref, OFError>
where
    I: Clone + Debug + InputIter<Item = char> + Compare<&'a str>,
{
    Ok(cref::new_abs(
        try_bool_from_abs_flag(i.row.0)?,
        try_u32_from_rowname(i.row.1)?,
        try_bool_from_abs_flag(i.col.0)?,
        try_u32_from_colname(i.col.1)?,
    ))
}

/// Parse a column reference to a cref.
pub fn try_column_from_token<'a, I>(i: (Option<I>, I)) -> Result<cref, OFError>
where
    I: Clone + Debug + InputIter<Item = char> + Compare<&'a str>,
{
    Ok(cref::new_abs(
        false,
        0,
        try_bool_from_abs_flag(i.0)?,
        try_u32_from_colname(i.1)?,
    ))
}

/// Parse a row reference to a cref.
pub fn try_row_from_token<'a, I>(i: (Option<I>, I)) -> Result<cref, OFError>
where
    I: Clone + Debug + InputIter<Item = char> + Compare<&'a str>,
{
    Ok(cref::new_abs(
        try_bool_from_abs_flag(i.0)?,
        try_u32_from_colname(i.1)?,
        false,
        0,
    ))
}

/// Parse a row number to a row index.
pub fn try_u32_from_rowname<I>(i: I) -> Result<u32, OFError>
where
    I: Clone + Debug + InputIter<Item = char>,
{
    let mut row = 0u32;

    for c in i.iter_elements() {
        assert!(('0'..='9').contains(&c));
        let d = c as u32 - b'0' as u32;
        row = row * 10 + d;
    }

    Ok(row - 1)
}

/// Parse a col label to a column index.
pub fn try_u32_from_colname<I>(i: I) -> Result<u32, OFError>
where
    I: Clone + Debug + InputIter<Item = char>,
{
    let mut col = 0u32;

    for c in i.iter_elements() {
        assert!(('A'..='Z').contains(&c));

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
        Err(OFError::Parse(format!("{:?}", i)))
    } else {
        Ok(col - 1)
    }
}

/// Unquotes a quoted table name.
pub fn unquote_str<I>(i: I) -> String
where
    I: Clone + Debug + ToString,
{
    i.to_string().replace("''", "'")
}

/// Unquotes a quoted table name.
pub fn unquote_opt<I>(i: Option<I>) -> Option<String>
where
    I: Clone + Debug + ToString,
{
    i.map(|i| i.to_string().replace("''", "'"))
}

/// Unquotes a quoted table name.
pub fn unquote_opt2<I>(i: Option<(Option<I>, I)>) -> Option<String>
where
    I: Clone + Debug + ToString,
{
    i.map(|i| i.1.to_string().replace("''", "'"))
}
