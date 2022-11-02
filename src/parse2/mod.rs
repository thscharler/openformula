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
use std::fmt::Debug;
use std::num::ParseIntError;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseExprError>;

#[allow(variant_size_differences)] // TODO: necessary??
#[derive(Debug)]
pub enum ParseExprError {
    NomError,
    NomFailure,

    Number,
    String,
    Parenthesis,

    Elementary,

    ParseInt(ParseIntError),
    ParseColname(ParseColnameError),
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

mod tests {
    use crate::parse2::ast::AstTree;
    use crate::parse2::ast_parser::{number, parse_elem, parse_expr};
    use crate::parse2::{ParseResult, Span, Tracer};

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

    #[test]
    fn test_expr() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test(test, parse_expr);
        }
    }

    #[test]
    fn test_elementary() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test(test, parse_elem);
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
}
