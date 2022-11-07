//!
//! Parser and AST for OpenFormula.
//!

pub mod ast_format;
pub mod ast_parser;
pub mod conv;
pub mod tokens;
pub mod tracer;

use crate::error::ParseExprError;
use crate::parse::tracer::Tracer;
use nom_locate::LocatedSpan;

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseExprError>;

#[cfg(test)]
mod tests {
    use crate::ast::AstTree;
    use crate::ast_parser::ElementaryExpr;
    use crate::parse::ast_parser::opt_number;
    use crate::parse::ast_parser::GeneralExpr;
    use crate::parse::{ast_parser, ParseResult, Span, Tracer};

    fn run_test2<'s>(
        str: &'s str,
        testfn: for<'t> fn(
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
            run_test2(test, ast_parser::opt_expr);
        }
    }

    #[test]
    fn test_elementary() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test2(test, ElementaryExpr::parse);
        }
    }

    #[test]
    fn test_number() {
        let test_ok = ["25", "25e+5", "25.", "25.001", "25.003e-7"];
        for test in test_ok {
            run_test2(test, opt_number);
        }

        let test_err = ["invalid", "2x5", "25ex+5", "25.x", "25.x001", "25x.003e-7"];
        for test in test_err {
            run_test2(test, opt_number);
        }
    }
}
