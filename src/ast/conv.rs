//!
//! Low level conversions from a Span<'a> to ...
//!

use crate::ast::Span;

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
