#![allow(dead_code)]

use openformula::ast::Span;

mod parser_error;
mod token_error;

pub use parser_error::*;
pub use token_error::*;

pub fn nul() -> Span<'static> {
    Span::new("")
}
