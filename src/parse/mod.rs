mod parse_refs2;
mod token;

pub use self::parse_refs2::*;
pub use self::token::*;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;
