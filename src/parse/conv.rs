use crate::OFError;
use nom::{Compare, CompareResult, InputIter};
use std::fmt::Debug;

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

pub fn try_ucell_from_rowname<'a, I>(i: I) -> Result<u32, OFError>
where
    I: Clone + Debug + InputIter<Item = char>,
{
    let mut row = 0u32;

    for c in i.iter_elements() {
        assert!(c >= '0' && c <= '9');
        let d = c as u32 - b'0' as u32;
        row = row * 10 + d;
    }

    Ok(row - 1)
}

/// Parse a col label to a column index.
pub fn try_ucell_from_colname<'a, I>(i: I) -> Result<u32, OFError>
where
    I: Clone + Debug + InputIter<Item = char>,
{
    let mut col = 0u32;

    for c in i.iter_elements() {
        assert!(c >= 'A' && c <= 'Z');

        let mut v = c as u32 - b'A' as u32;
        if v == 25 {
            v = 0;
            col = (col + 1) * 26;
        } else {
            v += 1;
            col *= 26;
        }
        col += v as u32;
    }

    if col == 0 {
        Err(OFError::Parse(format!("{:?}", i)))
    } else {
        Ok(col - 1)
    }
}

/// Unquotes a quoted table name.
pub fn unquote_str<'a, I>(i: I) -> String
where
    I: Clone + Debug + ToString,
{
    i.to_string().replace("''", "'")
}

/// Unquotes a quoted table name.
pub fn unquote_opt<'a, I>(i: Option<I>) -> Option<String>
where
    I: Clone + Debug + ToString,
{
    i.map(|i| i.to_string().replace("''", "'"))
}

/// Unquotes a quoted table name.
pub fn unquote_opt2<'a, I>(i: Option<(Option<I>, I)>) -> Option<String>
where
    I: Clone + Debug + ToString,
{
    i.map(|i| i.1.to_string().replace("''", "'"))
}
