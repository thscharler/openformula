//!
//! Low level conversions from a Span<'a> to ...
//!

use crate::Span;

/// Replaces two quotes (") with a single one.
pub fn unquote_double(i: Span<'_>) -> String {
    (*i).replace("\"\"", "\"")
}

/// Replaces one quote (") with two.
pub fn quote_double(i: &str) -> String {
    (*i).replace("\"", "\"\"")
}

/// Replaces two single quotes (') with a single on.
pub fn unquote_single(i: Span<'_>) -> String {
    (*i).replace("''", "'")
}

/// Replaces one single quote (') with two.
pub fn quote_single(i: &str) -> String {
    (*i).replace("'", "''")
}

/// Is this "$"
pub fn conv_abs(abs: Option<Span<'_>>) -> bool {
    match abs {
        None => false,
        Some(s) => *s == "$",
    }
}
