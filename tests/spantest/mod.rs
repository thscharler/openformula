#![allow(dead_code)]

mod parser_error;
mod test0;
mod test0_parser;
mod test0_token;
mod token_error;

use openformula::iparse::Span;

pub use parser_error::*;
pub use test0::*;
pub use test0_parser::*;
pub use test0_token::*;
pub use token_error::*;

pub fn nul() -> Span<'static> {
    Span::new("")
}
