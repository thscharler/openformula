//!
//! Parser and AST for OpenFormula.
//!

pub mod ast;
pub mod ast_format;
pub mod ast_parser;
pub mod conv;
pub mod refs;
pub mod tokens;
pub mod tracer;

use crate::parse2::conv::ParseColnameError;
use crate::parse2::tracer::Tracer;
use nom_locate::LocatedSpan;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::num::ParseIntError;

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseExprError>;

/// Converts into a format that can be used in a formula.
pub trait ToFormula {
    /// Converts into a format that can be used in a formula.
    fn to_formula(&self) -> Result<String, std::fmt::Error>;
}

/// For the errors the lifetime is annoying. This is a owning copy of the offending span.
#[derive(Debug)]
pub struct ErrSpan {
    /// Offset from the start of input.
    pub offset: usize,
    /// Line.
    pub line: u32,
    /// Column.
    pub column: usize,
    /// The offending fragment.
    pub fragment: String,
}

impl Display for ErrSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} <{}> '{}'",
            self.offset, self.line, self.column, self.fragment
        )
    }
}

impl<'a> From<Span<'a>> for ErrSpan {
    fn from(s: Span<'a>) -> Self {
        Self {
            offset: s.location_offset(),
            line: s.location_line(),
            column: s.get_column(),
            fragment: s.fragment().to_string(),
        }
    }
}

/// Error type for the parser.
#[allow(variant_size_differences)] // TODO: necessary??
#[derive(Debug)]
pub enum ParseExprError {
    /// TODO:
    NomError,
    /// TODO:
    NomFailure,

    /// TODO:
    Expr(ErrSpan),

    /// TODO:
    Number(ErrSpan),
    /// TODO:
    String,
    /// TODO:
    Parenthesis,

    /// TODO:
    Elementary,

    /// TODO:
    CellRef(ErrSpan),
    /// TODO:
    CellRange(ErrSpan),

    /// TODO:
    ParseInt(ParseIntError),
    /// TODO:
    ParseColname(ParseColnameError),
}

impl ParseExprError {
    /// Expr variant.
    pub fn expr<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::Expr(span.into())
    }

    /// Ref variant.
    pub fn cellref<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::CellRef(span.into())
    }

    /// Range variant.
    pub fn cellrange<'a>(span: Span<'a>) -> ParseExprError {
        ParseExprError::CellRange(span.into())
    }
}

impl Error for ParseExprError {}

impl Display for ParseExprError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseExprError::NomError => write!(f, "NomError"),
            ParseExprError::NomFailure => write!(f, "NomFailure"),
            ParseExprError::Expr(s) => write!(f, "Number {}", s),
            ParseExprError::Number(s) => write!(f, "Number {}", s),
            ParseExprError::String => write!(f, "String"),
            ParseExprError::Parenthesis => write!(f, "Parenthesis"),
            ParseExprError::Elementary => write!(f, "Elementary"),
            ParseExprError::ParseInt(e) => write!(f, "ParseInt {:?}", e),
            ParseExprError::ParseColname(e) => write!(f, "ParseColname {:?}", e),
            ParseExprError::CellRef(s) => write!(f, "Number {}", s),
            ParseExprError::CellRange(s) => write!(f, "Number {}", s),
        }
    }
}

impl From<ParseIntError> for ParseExprError {
    fn from(e: ParseIntError) -> Self {
        ParseExprError::ParseInt(e)
    }
}

impl From<ParseColnameError> for ParseExprError {
    fn from(e: ParseColnameError) -> Self {
        ParseExprError::ParseColname(e)
    }
}

#[cfg(test)]
mod tests {
    use crate::parse2::ast::AstTree;
    use crate::parse2::ast_parser::{elementary, opt_number};
    use crate::parse2::tokens::number;
    use crate::parse2::{ast_parser, ParseResult, Span, Tracer};

    fn run_test<'x>(
        str: &'x str,
        testfn: for<'s, 't> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, 't, Box<AstTree<'s>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, tok)) => {
                    println!("{} | {}", tok, rest);
                }
                Err(e) => {
                    println!("{:?}", e);
                }
            }
            println!("{:?}", &tracer);
        }
    }

    fn run_test2<'x>(
        str: &'x str,
        testfn: for<'s, 't> fn(
            &'t Tracer<'s>,
            Span<'s>,
        ) -> ParseResult<'s, 't, Option<Box<AstTree<'s>>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, Some(tok))) => {
                    println!("{} | {}", tok, rest);
                }
                Ok((rest, None)) => {
                    println!(" | {}", rest);
                }
                Err(e) => {
                    println!("{:?}", e);
                }
            }
            println!("{:?}", &tracer);
        }
    }

    #[test]
    fn test_expr() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)", "X", "4*5+1", "4+5*2"];
        for test in tests {
            run_test(test, ast_parser::expr);
        }
    }

    #[test]
    fn test_elementary() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test(test, elementary);
        }
    }

    #[test]
    fn test_number() {
        let test_ok = ["25", "25e+5", "25.", "25.001", "25.003e-7"];
        for test in test_ok {
            run_test(test, number);
        }

        let test_err = ["invalid", "2x5", "25ex+5", "25.x", "25.x001", "25x.003e-7"];
        for test in test_err {
            run_test(test, number);
        }
    }

    #[test]
    fn test_number2() {
        let test = ["25", "2x5", "inv"];
        for test in test {
            run_test(test, number);
            run_test2(test, opt_number);
        }
    }
}
