#![allow(dead_code)]

mod parser_error;
mod token_error;

use openformula::ast::Span;

pub use parser_error::*;
pub use token_error::*;

pub fn nul() -> Span<'static> {
    Span::new("")
}
