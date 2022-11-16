use openformula::ast::parser::{GeneralExpr, GeneralTerm, IriTerm, NamedExpr, SheetNameTerm};
use openformula::ast::tracer::Tracer;
use openformula::ast::{OFIri, ParseResult, Span};
use openformula::error::OFError;
use std::fmt::Debug;

trait TestRun<'s> {
    type Output;
    type Test;

    fn run(
        rest: &'s str,
        testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, Self::Output>,
        result: Result<Self::Test, OFError>,
    ) {
        let trace = Tracer::new();

        match testfn(&trace, Span::new(rest)) {
            Ok((rest, tok)) => {
                println!("{:?}", &trace);
                println!("=> '{}' | rest='{}'", Self::out_str(&tok), rest);

                match result {
                    Ok(result) => {
                        Self::check(tok, result);
                    }
                    Err(_) => {
                        println!("!!! Expect error but was ok!");
                    }
                }
            }
            Err(e) => {
                println!("{:?}", &trace);
                println!("=> {}", e);

                match result {
                    Ok(_) => {
                        println!("!!! Expected ok but was error!");
                    }
                    Err(t) => {
                        if e.code != t {
                            println!("!!! Expected {:?} but was {:?}", t, e);
                        }
                    }
                }
            }
        }
        println!();
    }

    fn out_str(out: &Self::Output) -> &str;

    fn check(out: Self::Output, res: Self::Test);
}

fn run_test2<'s, O>(
    str: &'s str,
    testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, O>,
) where
    O: Debug,
{
    let tracer = Tracer::new();
    {
        println!("### parse '{}'", str);
        match testfn(&tracer, Span::new(str)) {
            Ok((rest, tok)) => {
                println!("{:?}", &tracer);
                println!("=> {:?} | rest='{}'", tok, rest);
            }
            Err(e) => {
                println!("{:?}", &tracer);
                println!("=> {}", e);
                println!(
                    "   Found '{}' expected [{}] suggest [{}]",
                    e.span(),
                    tracer.expectations(),
                    tracer.suggestions()
                );
            }
        }
    }
}

// TODO: Expr
// TODO: CompareExpr
// TODO: AddExpr
// TODO: MulExpr
// TODO: PowExpr
// TODO: PostfixExpr
// TODO: PrefixExpr
// TODO: ElementaryExpr
// TODO: NumberExpr
// TODO: StringExpr
// TODO: ReferenceExpr
// TODO: ColTerm
// TODO: RowTerm
// TODO: CellRefExpr
// TODO: CelllRangeExpr
// TODO: ColRangeExpr
// TODO: RowRangeExpr
// TODO: ParenthesisExpr
// TODO: FnCallExpr

struct RunIri;

impl<'s> TestRun<'s> for RunIri {
    type Output = Option<OFIri<'s>>;
    type Test = Option<&'static str>;

    fn out_str(out: &Self::Output) -> &str {
        match out {
            None => "",
            Some(iri) => &iri.iri,
        }
    }

    fn check(out: Self::Output, res: Self::Test) {
        match out {
            None => {
                match res {
                    None => { /* ok */ }
                    Some(res) => {
                        println!("!!! Expected {} but was None", res);
                    }
                }
            }
            Some(out) => match res {
                None => {
                    println!("!!! Expected None but was {:?}", res)
                }
                Some(res) => {
                    if out.iri != res {
                        println!("!!! Expected {} but was {}", res, out.iri);
                    }
                }
            },
        }
    }
}

#[test]
pub fn iri() {
    RunIri::run("'external", IriTerm::parse, Err(OFError::ErrIri));
    RunIri::run("'external'", IriTerm::parse, Ok(None));
    RunIri::run("'external'#", IriTerm::parse, Ok(Some("external")));
    RunIri::run("'external'$", IriTerm::parse, Ok(None));

    run_test2("'external", IriTerm::parse);
    run_test2("'external'", IriTerm::parse);
    run_test2("'external'#", IriTerm::parse);
    run_test2("'external'$", IriTerm::parse);
}

#[test]
pub fn sheet_name() {
    run_test2("'sheetname'.", SheetNameTerm::parse);
    run_test2("'sheet''name'.", SheetNameTerm::parse);
    run_test2("'sheetname'", SheetNameTerm::parse);
    run_test2("'sheetname", SheetNameTerm::parse);
    run_test2("sheetname'.", SheetNameTerm::parse);
    run_test2("$'sheetname'.", SheetNameTerm::parse);
    run_test2("$sheetname'.", SheetNameTerm::parse);
    run_test2("$'sheetname.", SheetNameTerm::parse);
    run_test2("$'sheetname'", SheetNameTerm::parse);
}
// TODO: NamedExpr
#[test]
fn test_named() {
    run_test2("Pi", NamedExpr::parse);
    run_test2("$$Tau", NamedExpr::parse);
    run_test2("'xref'.Rho", NamedExpr::parse);
    run_test2("'xref'Foo", NamedExpr::parse);
    run_test2("$$'nice and clean'", NamedExpr::parse);
}
