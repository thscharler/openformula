//!
//! Parser and AST for OpenFormula.
//!

pub mod ast_format;
pub mod ast_parser;
mod ast_parser2;
pub mod conv;
pub mod tokens;
pub mod tracer;

use crate::error::ParseOFError;
use crate::error2::ParseOFError2;
use crate::parse::tracer::Tracer;
use nom_locate::LocatedSpan;

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseOFError>;

pub type ParseResult2<'s, 't, O> = Result<(Span<'s>, O), ParseOFError2>;
