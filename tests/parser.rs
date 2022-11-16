mod spantest;

use openformula::ast::parser::{GeneralExpr, GeneralTerm, IriTerm, NamedExpr, SheetNameTerm};
use openformula::ast::tracer::Suggest::*;
use openformula::ast::{OFIri, OFSheetName};
use openformula::error::OFError::*;

pub use spantest::*;

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

#[test]
pub fn iri() {
    impl<'a> TestResult<OFIri<'a>> for OFIri<'a> {
        type TestValue = &'a str;

        fn equal(result: &OFIri<'a>, testvalue: &Self::TestValue) -> bool {
            result.iri == *testvalue
        }

        fn str(result: &OFIri<'a>) -> String {
            result.iri.to_string()
        }

        fn val_str(value: &Self::TestValue) -> String {
            value.to_string()
        }
    }

    TestRun::parse("'external", IriTerm::parse)
        .err(ErrIri)
        .dump()
        .q();
    TestRun::parse("'external'", IriTerm::parse).ok(None).q();
    TestRun::parse("'external'#", IriTerm::parse)
        .ok(Some("external"))
        .q();
    TestRun::parse("'external'$", IriTerm::parse).ok(None).q()
}

#[test]
pub fn sheet_name() {
    impl<'a> TestResult<OFSheetName<'a>> for OFSheetName<'a> {
        type TestValue = &'a str;

        fn equal(result: &OFSheetName<'a>, testvalue: &Self::TestValue) -> bool {
            result.name == *testvalue
        }

        fn str(result: &OFSheetName<'a>) -> String {
            result.name.clone()
        }

        fn val_str(value: &Self::TestValue) -> String {
            value.to_string()
        }
    }

    TestRun::parse("'sheetname'.", SheetNameTerm::parse)
        .ok(Some("sheetname"))
        .q();
    TestRun::parse("'sheet''name'.", SheetNameTerm::parse)
        .ok(Some("sheet'name"))
        .q();
    TestRun::parse("'sheetname'", SheetNameTerm::parse)
        .err(ErrSheetName)
        .expect(Dot)
        .q();
    TestRun::parse("'sheetname", SheetNameTerm::parse)
        .err(ErrSheetName)
        .expect(EndSingleQuote)
        .q();
    TestRun::parse("sheetname'.", SheetNameTerm::parse)
        .ok(None)
        .q();
    TestRun::parse("$'sheetname'.", SheetNameTerm::parse)
        .ok(Some("sheetname"))
        .q();
    TestRun::parse("$sheetname'.", SheetNameTerm::parse)
        .ok(None)
        .q();
    TestRun::parse("$'sheetname.", SheetNameTerm::parse)
        .err(ErrSheetName)
        .expect(EndSingleQuote)
        .q();
    TestRun::parse("$'sheetname'", SheetNameTerm::parse)
        .err(ErrSheetName)
        .expect(Dot)
        .q();
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
