mod spantest;

use openformula::ast::parser::{GeneralExpr, GeneralTerm, IriTerm, NamedExpr, SheetNameTerm};
use openformula::ast::tracer::Suggest::*;
use openformula::ast::{OFAst, OFIri, OFSheetName};
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
    impl<'a> TestResult<Box<OFAst<'a>>> for Box<OFAst<'a>> {
        type TestValue = &'a str;

        fn equal(result: &Box<OFAst<'a>>, testvalue: &Self::TestValue) -> bool {
            match &**result {
                OFAst::NodeNamed(result) => result.simple.ident == *testvalue,
                _ => false,
            }
        }

        fn str(result: &Box<OFAst<'a>>) -> String {
            result.to_string()
        }

        fn val_str(value: &Self::TestValue) -> String {
            value.to_string()
        }
    }
    #[allow(dead_code)]
    fn iri<'s>(result: &'s Box<OFAst<'s>>) -> &'s str {
        match &**result {
            OFAst::NodeNamed(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri,
            },
            _ => unreachable!(),
        }
    }
    fn sheet_nameg<'s>(result: &'s Box<OFAst<'_>>) -> &'s str {
        match &**result {
            OFAst::NodeNamed(result) => match &result.sheet_name {
                None => unreachable!(),
                Some(sheet_name) => &sheet_name.name,
            },
            _ => unreachable!(),
        }
    }

    fn sheet_named<'s>(result: &'s Box<OFAst<'s>>, testvalue: &'s str) -> bool {
        match &**result {
            OFAst::NodeNamed(result) => match &result.sheet_name {
                None => unreachable!(),
                Some(sheet_name) => &sheet_name.name == testvalue,
            },
            _ => unreachable!(),
        }
    }

    TestRun::parse("Pi", NamedExpr::parse).ok("Pi").q();
    TestRun::parse("$$Tau", NamedExpr::parse).ok("Tau").q();

    TestRun::parse("'xref'#Rho", NamedExpr::parse)
        .okg(iri, "xref")
        .q();
    TestRun::parse("'xref'#'hobo'.Rho", NamedExpr::parse)
        .okd(sheet_named, "hobo")
        .okg(iri, "xref")
        .q();

    TestRun::parse("'xref'.Rho", NamedExpr::parse)
        .ok("Rho")
        .okg(sheet_nameg, "xref")
        .q();
    TestRun::parse("'xref'Foo", NamedExpr::parse)
        .err(ErrSheetName)
        .expect(Dot)
        .q();
    TestRun::parse("$$'nice and clean'", NamedExpr::parse)
        .ok("nice and clean")
        .q();
}
