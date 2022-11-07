//!
//! Parser and AST for OpenFormula.
//!

pub mod ast_format;
pub mod ast_parser;
pub mod conv;
pub mod tokens;
pub mod tracer;

use crate::error::ParseOFError;
use crate::parse::tracer::Tracer;
use nom_locate::LocatedSpan;

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseOFError>;
