//!
//! Low level conversions from a Span<'a> to ...
//!

use crate::ast::Span;

/// Replaces two quotes (") with a single one.
/// Strips one leading and one trailing quote.
pub fn unquote_double(i: Span<'_>) -> String {
    let i = match i.strip_prefix('\"') {
        None => *i,
        Some(s) => s,
    };
    let i = match i.strip_suffix('\"') {
        None => i,
        Some(s) => s,
    };

    (*i).replace(r#""""#, r#"""#)
}

/// Replaces one quote (") with two.
pub fn quote_double(i: &str) -> String {
    (*i).replace('"', "\"\"")
}

/// Replaces two single quotes (') with a single on.
/// Strips one leading and one trailing quote.
pub fn unquote_single(i: Span<'_>) -> String {
    let i = match i.strip_prefix('\'') {
        None => *i,
        Some(s) => s,
    };
    let i = match i.strip_suffix('\'') {
        None => i,
        Some(s) => s,
    };

    (*i).replace("''", "'")
}

/// Replaces one single quote (') with two.
pub fn quote_single(i: &str) -> String {
    (*i).replace('\'', "''")
}
