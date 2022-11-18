mod spantest;

use openformula::ast::parser::{
    CellRangeExpr, CellRefExpr, ColRangeExpr, ColTerm, FnCallExpr, GeneralExpr, GeneralTerm,
    IriTerm, NamedExpr, ParenthesesExpr, RowRangeExpr, RowTerm, SheetNameTerm,
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

#[test]
pub fn reference() {
    // TestRun::parse("", ReferenceExpr::parse).fail().q();
}

#[test]
pub fn colterm() {
    TestRun::parse("", ColTerm::parse)
        .err(ErrCol)
        .expect(Alpha)
        .expect(Col)
        .q();
    TestRun::parse("$A", ColTerm::parse).okok().q();
    TestRun::parse("$", ColTerm::parse)
        .err(ErrCol)
        .expect(Alpha)
        .expect(Col)
        .q();
    TestRun::parse("A", ColTerm::parse).okok().q();
}

#[test]
pub fn rowterm() {
    TestRun::parse("", RowTerm::parse)
        .err(ErrRow)
        .expect(Digit)
        .expect(Row)
        .q();
    TestRun::parse("$1", RowTerm::parse).okok().q();
    TestRun::parse("$", RowTerm::parse)
        .err(ErrRow)
        .expect(Digit)
        .expect(Row)
        .q();
    TestRun::parse("1", RowTerm::parse).okok().q();
}

#[test]
pub fn celref() {
    fn iri<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeCellRef(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri == test,
            },
            _ => unreachable!(),
        }
    }
    fn table<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeCellRef(result) => match &result.table {
                None => unreachable!(),
                Some(table) => &table.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn row_col<'s>(result: &'s Box<OFAst<'s>>, test: &(u32, u32)) -> bool {
        match &**result {
            OFAst::NodeCellRef(result) => result.col.col == test.1 && result.row.row == test.0,
            _ => unreachable!(),
        }
    }
    fn absolute<'s>(result: &'s Box<OFAst<'s>>, test: &(bool, bool)) -> bool {
        match &**result {
            OFAst::NodeCellRef(result) => result.col.abs == test.1 && result.row.abs == test.0,
            _ => unreachable!(),
        }
    }

    TestRun::parse("", CellRefExpr::parse)
        .err(ErrCellRef)
        .expect(Dot)
        .expect(CellRef)
        .q();
    TestRun::parse("'iri'#.A1", CellRefExpr::parse)
        .okeq(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A1", CellRefExpr::parse)
        .okeq(table, "sheet")
        .q();
    TestRun::parse(".A1", CellRefExpr::parse)
        .okeq(row_col, &(0, 0))
        .okeq(absolute, &(false, false))
        .q();
    TestRun::parse(".A", CellRefExpr::parse)
        .err(ErrCellRef)
        .expect(Digit)
        .q();
    TestRun::parse(".1", CellRefExpr::parse)
        .err(ErrCellRef)
        .expect(Alpha)
        .expect(Col)
        .q();
    TestRun::parse("A1", CellRefExpr::parse)
        .err(ErrCellRef)
        .expect(Dot)
        .q();
    TestRun::parse(".$A$1", CellRefExpr::parse)
        .okeq(row_col, &(0, 0))
        .okeq(absolute, &(true, true))
        .q();
}

#[test]
pub fn cellrange() {
    fn iri<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri == test,
            },
            _ => unreachable!(),
        }
    }
    fn table<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => match &result.table {
                None => unreachable!(),
                Some(table) => &table.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn to_table<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => match &result.to_table {
                None => unreachable!(),
                Some(table) => &table.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn row_col<'s>(result: &'s Box<OFAst<'s>>, test: &(u32, u32)) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => result.col.col == test.1 && result.row.row == test.0,
            _ => unreachable!(),
        }
    }
    fn to_row_col<'s>(result: &'s Box<OFAst<'s>>, test: &(u32, u32)) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => {
                result.to_col.col == test.1 && result.to_row.row == test.0
            }
            _ => unreachable!(),
        }
    }
    fn absolute<'s>(result: &'s Box<OFAst<'s>>, test: &(bool, bool)) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => result.col.abs == test.1 && result.row.abs == test.0,
            _ => unreachable!(),
        }
    }
    fn to_absolute<'s>(result: &'s Box<OFAst<'s>>, test: &(bool, bool)) -> bool {
        match &**result {
            OFAst::NodeCellRange(result) => {
                result.to_col.abs == test.1 && result.to_row.abs == test.0
            }
            _ => unreachable!(),
        }
    }

    TestRun::parse("", CellRangeExpr::parse)
        .err(ErrCellRange)
        .q();
    TestRun::parse("'iri'#.A1:.C3", CellRangeExpr::parse)
        .okeq(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A1:.C3", CellRangeExpr::parse)
        .okeq(table, "sheet")
        .q();
    TestRun::parse(".A1:.C3", CellRangeExpr::parse)
        .okeq(row_col, &(0, 0))
        .okeq(to_row_col, &(2, 2))
        .q();
    TestRun::parse(".$A$1:.$C$3", CellRangeExpr::parse)
        .okeq(row_col, &(0, 0))
        .okeq(absolute, &(true, true))
        .okeq(to_row_col, &(2, 2))
        .okeq(to_absolute, &(true, true))
        .q();
    TestRun::parse("'fun'.$A$1:'nofun'.$C$3", CellRangeExpr::parse)
        .okeq(table, "fun")
        .okeq(to_table, "nofun")
        .q();
    TestRun::parse(".A1:.C3", CellRangeExpr::parse).okok().q();
    TestRun::parse(".A1:.3", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Alpha)
        .expect(Col)
        .expect(CellRange)
        .q();
    TestRun::parse(".A1:.C", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Digit)
        .expect(Row)
        .expect(CellRange)
        .q();
    TestRun::parse(".A:.C3", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Digit)
        .expect(Row)
        .expect(CellRange)
        .q();
    TestRun::parse(".1:.C3", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Alpha)
        .expect(Col)
        .expect(CellRange)
        .q();
    TestRun::parse(":.C3", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Dot)
        .expect(CellRange)
        .q();
    TestRun::parse("A1:C3", CellRangeExpr::parse)
        .err(ErrCellRange)
        .expect(Dot)
        .expect(CellRange)
        .q();
}

#[test]
pub fn colrange() {
    fn iri<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeColRange(result) => match &result.iri {
                None => unreachable!(),
                Some(iri) => &iri.iri == test,
            },
            _ => unreachable!(),
        }
    }
    fn sheet_name<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeColRange(result) => match &result.table {
                None => unreachable!(),
                Some(table) => &table.name == test,
            },
            _ => unreachable!(),
        }
    }
    fn col_col<'s>(result: &'s Box<OFAst<'s>>, test: &(u32, u32)) -> bool {
        match &**result {
            OFAst::NodeColRange(result) => result.col.col == test.0 && result.to_col.col == test.1,
            _ => unreachable!(),
        }
    }

    TestRun::parse("", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Dot)
        .q();
    TestRun::parse("'iri'#.A:.C", ColRangeExpr::parse)
        .okeq(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A:.C", ColRangeExpr::parse)
        .okeq(sheet_name, "sheet")
        .q();
    TestRun::parse(".A:.C", ColRangeExpr::parse)
        .okeq(col_col, &(0, 2))
        .q();
    TestRun::parse(".1:", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Alpha)
        .expect(Col)
        .expect(ColRange)
        .q();
    TestRun::parse(".A", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Colon)
        .q();
    TestRun::parse(":", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Dot)
        .q();
    TestRun::parse(":.A", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Dot)
        .q();
    TestRun::parse(":A", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Dot)
        .q();
    TestRun::parse(".5:.7", ColRangeExpr::parse)
        .err(ErrColRange)
        .expect(Alpha)
        .q();
}

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
        .expect(Dot)
        .q();
    TestRun::parse("'iri'#.1:.3", RowRangeExpr::parse)
        .okeq(iri, "iri")
        .q();
    TestRun::parse("'sheet'.1:.3", RowRangeExpr::parse)
        .okeq(sheet_name, "sheet")
        .q();
    TestRun::parse(".1:.3", RowRangeExpr::parse)
        .okeq(row_row, &(0, 2))
        .q();
    TestRun::parse(".1:", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Dot)
        .q();
    TestRun::parse(".1", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Colon)
        .q();
    TestRun::parse(":", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Dot)
        .q();
    TestRun::parse(":.1", RowRangeExpr::parse)
        .err(ErrRowRange)
        .expect(Dot)
        .q();
    TestRun::parse(".C:.E", RowRangeExpr::parse)
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
        .okopt(Some("sheetname"))
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
        .okopt(Some("sheetname"))
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

    TestRun::parse(".Pi", NamedExpr::parse)
        .okeq(ident, "Pi")
        .q();
    TestRun::parse(".$$Tau", NamedExpr::parse)
        .okeq(ident, "Tau")
        .q();
    TestRun::parse("'xref'#.Rho", NamedExpr::parse)
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
    TestRun::parse(".$$'nice and clean'", NamedExpr::parse)
        .okeq(ident, "nice and clean")
        .q();
}
