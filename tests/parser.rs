mod spantest;

use openformula::ast::parser::{
    FnCallExpr, GeneralExpr, GeneralTerm, IriTerm, NamedExpr, ParenthesesExpr, RowRangeExpr,
    SheetNameTerm,
};
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

#[test]
pub fn rowrange() {
    fn iri<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeRowRange(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri == test,
            },
            _ => unreachable!(),
        }
    }
    fn sheet_name<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeRowRange(result) => match &result.table {
                None => unreachable!(),
                Some(table) => &table.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn row_row<'s>(result: &'s Box<OFAst<'s>>, test: &(u32, u32)) -> bool {
        match &**result {
            OFAst::NodeRowRange(result) => result.row.row == test.0 && result.to_row.row == test.1,
            _ => unreachable!(),
        }
    }

    TestRun::parse("", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Digit)
        .q();
    TestRun::parse("'iri'#1:3", RowRangeExpr::parse)
        .okeq(iri, "iri")
        .q();
    TestRun::parse("'sheet'.1:3", RowRangeExpr::parse)
        .okeq(sheet_name, "sheet")
        .q();
    // TODO: continue
    TestRun::parse("1:3", RowRangeExpr::parse)
        .okeq(row_row, &(0, 2))
        .q();
    TestRun::parse("1:", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Digit)
        .q();
    TestRun::parse("1", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Colon)
        .q();
    TestRun::parse(":", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Digit)
        .q();
    TestRun::parse(":1", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Digit)
        .q();
    TestRun::parse("C:E", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Digit)
        .q();
}

#[test]
pub fn parentheses() {
    fn always_eq<'s>(_result: &'s Box<OFAst<'s>>, _test: &'s str) -> bool {
        true
    }

    TestRun::parse("(21)", ParenthesesExpr::parse)
        .okeq(always_eq, "")
        .q();
    TestRun::parse("(21", ParenthesesExpr::parse)
        .err(ErrParentheses)
        .expect(ParenthesesClose)
        .q();
    TestRun::parse("21)", ParenthesesExpr::parse)
        .err(ErrParentheses)
        .expect(ParenthesesOpen)
        .q();
    TestRun::parse("21", ParenthesesExpr::parse)
        .err(ErrParentheses)
        .expect(ParenthesesOpen)
        .q();
}

#[test]
pub fn fncall() {
    fn name<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeFnCall(result) => result.name.name == test,
            _ => unreachable!(),
        }
    }

    TestRun::parse("$FUN()", FnCallExpr::parse)
        .err(ErrFnCall)
        .expect(FnName)
        .q();
    TestRun::parse("FUN ( 77  ", FnCallExpr::parse)
        .err(ErrFnCall)
        .expect(ParenthesesClose)
        .q();
    TestRun::parse("FUN 77 ) ", FnCallExpr::parse)
        .err(ErrFnCall)
        .expect(ParenthesesOpen)
        .q();
    TestRun::parse("FUN(", FnCallExpr::parse)
        .err(ErrFnCall)
        .expect(ParenthesesClose)
        .q();
    TestRun::parse("FUN  )", FnCallExpr::parse)
        .err(ErrFnCall)
        .expect(ParenthesesOpen)
        .q();
    TestRun::parse("FUN(   ;;66)", FnCallExpr::parse)
        .okeq(name, "FUN")
        .q();
    TestRun::parse(" FUN(;;66)", FnCallExpr::parse)
        .okeq(name, "FUN")
        .q();
    TestRun::parse("FUN", FnCallExpr::parse).dump();
}

#[test]
pub fn iri() {
    impl<'a> TestEq<&str> for &OFIri<'a> {
        fn eq(&self, other: &&str) -> bool {
            self.iri == *other
        }
    }

    TestRun::parse("'external", IriTerm::parse).err(ErrIri).q();
    TestRun::parse("'external'", IriTerm::parse).okopt(None).q();
    TestRun::parse("'external'#", IriTerm::parse)
        .okopt(Some("external"))
        .q();
    TestRun::parse("'external'$", IriTerm::parse)
        .okopt(None)
        .q()
}

#[test]
pub fn sheet_name() {
    impl<'a> TestEq<&str> for &OFSheetName<'a> {
        fn eq(&self, other: &&str) -> bool {
            self.name == *other
        }
    }

    TestRun::parse("'sheetname'.", SheetNameTerm::parse)
        .okopt(Some("sheetname"))
        .q();
    TestRun::parse("'sheet''name'.", SheetNameTerm::parse)
        .okopt(Some("sheet'name"))
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
        .okopt(None)
        .q();
    TestRun::parse("$'sheetname'.", SheetNameTerm::parse)
        .okopt(Some("sheetname"))
        .q();
    TestRun::parse("$sheetname'.", SheetNameTerm::parse)
        .okopt(None)
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

#[test]
fn test_named() {
    fn iri<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeNamed(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri == test,
            },
            _ => unreachable!(),
        }
    }
    fn sheet_name<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeNamed(result) => match &result.sheet_name {
                None => unreachable!(),
                Some(sheet_name) => &sheet_name.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn ident<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeNamed(result) => result.simple.ident == test,
            _ => unreachable!(),
        }
    }

    TestRun::parse("Pi", NamedExpr::parse).okeq(ident, "Pi").q();
    TestRun::parse("$$Tau", NamedExpr::parse)
        .okeq(ident, "Tau")
        .q();
    TestRun::parse("'xref'#Rho", NamedExpr::parse)
        .okeq(iri, "xref")
        .q();
    TestRun::parse("'xref'#'hobo'.Rho", NamedExpr::parse)
        .okeq(sheet_name, &"hobo")
        .okeq(iri, "xref")
        .q();
    TestRun::parse("'xref'.Rho", NamedExpr::parse)
        .okeq(ident, "Rho")
        .okeq(sheet_name, "xref")
        .q();
    TestRun::parse("'xref'Foo", NamedExpr::parse)
        .err(ErrNamed)
        .expect(Dot)
        .q();
    TestRun::parse("$$'nice and clean'", NamedExpr::parse)
        .okeq(ident, "nice and clean")
        .q();
}
