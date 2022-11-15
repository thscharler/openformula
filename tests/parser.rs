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
// TODO: IriTerm

use openformula::ast::parser::GeneralTerm;
use openformula::ast::parser::IriTerm;
use openformula::ast::tracer::Tracer;
use openformula::ast::{ParseResult, Span};
use std::fmt::Debug;

fn run_test2<'s, O>(
    str: &'s str,
    testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, 't, O>,
) where
    O: Debug,
{
    let tracer = Tracer::new();
    {
        println!();
        println!("parse='{}'", str);
        match testfn(&tracer, Span::new(str)) {
            Ok((rest, tok)) => {
                println!("{:?}", &tracer);
                println!("=> {:?} | rest='{}'", tok, rest);
            }
            Err(e) => {
                println!("{:?}", &tracer);
                println!("=> {}", e);
                println!(
                    "   Found '{}' expected {} suggest {}",
                    e.span(),
                    tracer.expectations(),
                    tracer.suggestions()
                );
            }
        }
    }
}

#[test]
pub fn iri() {
    run_test2("'external", IriTerm::parse);
    run_test2("'external'", IriTerm::parse);
    run_test2("'external'#", IriTerm::parse);
    run_test2("'external'$", IriTerm::parse);
}

// TODO: SheetNameTerm
// TODO: NamedExpr
