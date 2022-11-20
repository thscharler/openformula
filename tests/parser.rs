mod spantest;

use openformula::ast::parser::{
    CellRangeExpr, CellRefExpr, ColRangeExpr, ColTerm, FnCallExpr, GeneralExpr, GeneralTerm,
    IriTerm, NamedExpr, NumberExpr, ParenthesesExpr, ReferenceExpr, RowRangeExpr, RowTerm,
    SheetNameTerm, StringExpr,
};
use openformula::ast::{OFAst, OFIri, OFSheetName};
use openformula::error::OFCode::*;

use spantest::*;

// TODO: Expr
// TODO: CompareExpr
// TODO: AddExpr
// TODO: MulExpr
// TODO: PowExpr
// TODO: PostfixExpr
// TODO: PrefixExpr
// TODO: ElementaryExpr
// TODO: NumberExpr

#[test]
pub fn number() {
    fn number<'s>(result: &'s Box<OFAst<'s>>, test: f64) -> bool {
        match &**result {
            OFAst::NodeNumber(v) => v.num == test,
            _ => {
                println!("FAIL: Node is not a Number");
                false
            }
        }
    }

    TestRun::parse("123", NumberExpr::parse)
        .ok(number, 123f64)
        .q();
}

#[test]
pub fn string() {
    fn string<'s>(result: &'s Box<OFAst<'s>>, test: &str) -> bool {
        match &**result {
            OFAst::NodeString(v) => v.str == test,
            _ => {
                println!("FAIL: Node is not a String");
                false
            }
        }
    }

    TestRun::parse("", StringExpr::parse).err(OFString).q();
    TestRun::parse(r#""""#, StringExpr::parse)
        .ok(string, "")
        .q();
    TestRun::parse(r#""abc""#, StringExpr::parse)
        .ok(string, "abc")
        .q();
    TestRun::parse(r#""abc"#, StringExpr::parse)
        .err(OFString)
        .expect(OFQuoteEnd)
        .expect(OFString)
        .q();
    TestRun::parse(r#"abc""#, StringExpr::parse)
        .err(OFString)
        .expect(OFQuoteStart)
        .expect(OFString)
        .q();
    TestRun::parse(r#""ab""cd""#, StringExpr::parse)
        .ok(string, "ab\"cd")
        .q();
    TestRun::parse(r#""ab"cd""#, StringExpr::parse)
        .ok(string, "ab")
        .q();
}

#[test]
pub fn reference() {
    fn cellrange<'s>(result: &'s Box<OFAst<'s>>, _test: &()) -> bool {
        match &**result {
            OFAst::NodeCellRange(_) => true,
            _ => {
                println!("FAIL: Node is not a CellRange");
                false
            }
        }
    }
    fn cellref<'s>(result: &'s Box<OFAst<'s>>, _test: &()) -> bool {
        match &**result {
            OFAst::NodeCellRef(_) => true,
            _ => {
                println!("FAIL: Node is not a CellRef");
                false
            }
        }
    }
    fn colrange<'s>(result: &'s Box<OFAst<'s>>, _test: &()) -> bool {
        match &**result {
            OFAst::NodeColRange(_) => true,
            _ => {
                println!("FAIL: Node is not a ColRange");
                false
            }
        }
    }
    fn rowrange<'s>(result: &'s Box<OFAst<'s>>, _test: &()) -> bool {
        match &**result {
            OFAst::NodeRowRange(_) => true,
            _ => {
                println!("FAIL: Node is not a RowRange");
                false
            }
        }
    }

    TestRun::parse(".1A", ReferenceExpr::parse)
        .err(OFReference)
        .expect(OFReference)
        .q();
    TestRun::parse(".A1:.B2", ReferenceExpr::parse)
        .ok(cellrange, &())
        .q();
    TestRun::parse(".A1", ReferenceExpr::parse)
        .ok(cellref, &())
        .q();
    TestRun::parse(".1:.3", ReferenceExpr::parse)
        .ok(rowrange, &())
        .q();
    TestRun::parse(".C:.E", ReferenceExpr::parse)
        .ok(colrange, &())
        .q();
}

#[test]
pub fn colterm() {
    TestRun::parse("", ColTerm::parse)
        .err(OFCol)
        .expect(OFAlpha)
        .expect(OFCol)
        .q();
    TestRun::parse("$A", ColTerm::parse).okok().q();
    TestRun::parse("$", ColTerm::parse)
        .err(OFCol)
        .expect(OFAlpha)
        .expect(OFCol)
        .q();
    TestRun::parse("A", ColTerm::parse).okok().q();
    TestRun::parse(" $A ", ColTerm::parse).err(OFCol).q();
}

#[test]
pub fn rowterm() {
    TestRun::parse("", RowTerm::parse)
        .err(OFRow)
        .expect(OFDigit)
        .expect(OFRow)
        .q();
    TestRun::parse("$1", RowTerm::parse).okok().q();
    TestRun::parse("$", RowTerm::parse)
        .err(OFRow)
        .expect(OFDigit)
        .expect(OFRow)
        .q();
    TestRun::parse("1", RowTerm::parse).okok().q();
    TestRun::parse(" $1 ", RowTerm::parse).err(OFRow).q();
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
        .err(OFCellRef)
        .expect(OFDot)
        .expect(OFCellRef)
        .q();
    TestRun::parse("'iri'#.A1", CellRefExpr::parse)
        .ok(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A1", CellRefExpr::parse)
        .ok(table, "sheet")
        .q();
    TestRun::parse(".A1", CellRefExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(false, false))
        .q();
    TestRun::parse(".A", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFDigit)
        .q();
    TestRun::parse(".1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFAlpha)
        .expect(OFCol)
        .q();
    TestRun::parse("A1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFDot)
        .q();
    TestRun::parse(".$A$1", CellRefExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(true, true))
        .q();
    TestRun::parse(".$A $1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFDigit)
        .expect(OFRow)
        .expect(OFCellRef)
        .q();
    TestRun::parse(".$ A$1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFAlpha)
        .expect(OFCol)
        .expect(OFCellRef)
        .q();
    TestRun::parse(".$A$ 1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFDigit)
        .expect(OFRow)
        .expect(OFCellRef)
        .q();
    TestRun::parse(".$A$$1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFDigit)
        .expect(OFRow)
        .expect(OFCellRef)
        .q();
    TestRun::parse(".$$A$$1", CellRefExpr::parse)
        .err(OFCellRef)
        .expect(OFAlpha)
        .expect(OFCol)
        .expect(OFCellRef)
        .q();
    TestRun::parse(" 'iri' # $ 'sheet' . $A$1 ", CellRefExpr::parse)
        .ok(iri, "iri")
        .ok(table, "sheet")
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
        .err(OFCellRange)
        .q();
    TestRun::parse("'iri'#.A1:.C3", CellRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A1:.C3", CellRangeExpr::parse)
        .ok(table, "sheet")
        .q();
    TestRun::parse(".A1:.C3", CellRangeExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(to_row_col, &(2, 2))
        .q();
    TestRun::parse(".$A$1:.$C$3", CellRangeExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(true, true))
        .ok(to_row_col, &(2, 2))
        .ok(to_absolute, &(true, true))
        .q();
    TestRun::parse("'fun'.$A$1:'nofun'.$C$3", CellRangeExpr::parse)
        .ok(table, "fun")
        .ok(to_table, "nofun")
        .q();
    TestRun::parse(".A1:.C3", CellRangeExpr::parse).okok().q();
    TestRun::parse(".A1:.3", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFAlpha)
        .expect(OFCol)
        .expect(OFCellRange)
        .q();
    TestRun::parse(".A1:.C", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFDigit)
        .expect(OFRow)
        .expect(OFCellRange)
        .q();
    TestRun::parse(".A:.C3", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFDigit)
        .expect(OFRow)
        .expect(OFCellRange)
        .q();
    TestRun::parse(".1:.C3", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFAlpha)
        .expect(OFCol)
        .expect(OFCellRange)
        .q();
    TestRun::parse(":.C3", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFDot)
        .expect(OFCellRange)
        .q();
    TestRun::parse("A1:C3", CellRangeExpr::parse)
        .err(OFCellRange)
        .expect(OFDot)
        .expect(OFCellRange)
        .q();
    TestRun::parse(
        " 'external' # 'fun' . $A$1 : 'nofun' . $C$3",
        CellRangeExpr::parse,
    )
    .ok(table, "fun")
    .ok(to_table, "nofun")
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
        .err(OFColRange)
        .expect(OFDot)
        .q();
    TestRun::parse("'iri'#.A:.C", ColRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    TestRun::parse("'sheet'.A:.C", ColRangeExpr::parse)
        .ok(sheet_name, "sheet")
        .q();
    TestRun::parse(".A:.C", ColRangeExpr::parse)
        .ok(col_col, &(0, 2))
        .q();
    TestRun::parse(".1:", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFAlpha)
        .expect(OFCol)
        .expect(OFColRange)
        .q();
    TestRun::parse(".A", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFColon)
        .q();
    TestRun::parse(":", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFDot)
        .q();
    TestRun::parse(":.A", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFDot)
        .q();
    TestRun::parse(":A", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFDot)
        .q();
    TestRun::parse(".5:.7", ColRangeExpr::parse)
        .err(OFColRange)
        .expect(OFAlpha)
        .q();
    TestRun::parse(" 'iri' # 'sheet' . $A : . $C ", ColRangeExpr::parse)
        .ok(iri, "iri")
        .ok(sheet_name, "sheet")
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
        .err(OFRowRange)
        .expect(OFDot)
        .q();
    TestRun::parse("'iri'#.1:.3", RowRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    TestRun::parse("'sheet'.1:.3", RowRangeExpr::parse)
        .ok(sheet_name, "sheet")
        .q();
    TestRun::parse(".1:.3", RowRangeExpr::parse)
        .ok(row_row, &(0, 2))
        .q();
    TestRun::parse(".1:", RowRangeExpr::parse)
        .err(OFRowRange)
        .expect(OFDot)
        .q();
    TestRun::parse(".1", RowRangeExpr::parse)
        .err(OFRowRange)
        .expect(OFColon)
        .q();
    TestRun::parse(":", RowRangeExpr::parse)
        .err(OFRowRange)
        .expect(OFDot)
        .q();
    TestRun::parse(":.1", RowRangeExpr::parse)
        .err(OFRowRange)
        .expect(OFDot)
        .q();
    TestRun::parse(".C:.E", RowRangeExpr::parse)
        .err(OFRowRange)
        .expect(OFDigit)
        .q();
    TestRun::parse(" 'iri' # 'sheet' . $1 : . $3 ", RowRangeExpr::parse)
        .ok(iri, "iri")
        .ok(sheet_name, "sheet")
        .q();
}

#[test]
pub fn parentheses() {
    TestRun::parse("(21)", ParenthesesExpr::parse).okok().q();
    TestRun::parse("(21", ParenthesesExpr::parse)
        .err(OFParentheses)
        .expect(OFParenthesesClose)
        .q();
    TestRun::parse("21)", ParenthesesExpr::parse)
        .err(OFParentheses)
        .expect(OFParenthesesOpen)
        .q();
    TestRun::parse("21", ParenthesesExpr::parse)
        .err(OFParentheses)
        .expect(OFParenthesesOpen)
        .q();
    TestRun::parse(" ( 21 ) ", ParenthesesExpr::parse)
        .okok()
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
        .err(OFFnCall)
        .expect(OFFnName)
        .q();
    TestRun::parse("FUN ( 77  ", FnCallExpr::parse)
        .err(OFFnCall)
        .expect(OFParenthesesClose)
        .q();
    TestRun::parse("FUN 77 ) ", FnCallExpr::parse)
        .err(OFFnCall)
        .expect(OFParenthesesOpen)
        .q();
    TestRun::parse("FUN(", FnCallExpr::parse)
        .err(OFFnCall)
        .expect(OFParenthesesClose)
        .q();
    TestRun::parse("FUN  )", FnCallExpr::parse)
        .err(OFFnCall)
        .expect(OFParenthesesOpen)
        .q();
    TestRun::parse("FUN(   ;;66)", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
    TestRun::parse(" FUN(;;66)", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
    TestRun::parse("FUN", FnCallExpr::parse).dump();
    TestRun::parse(" FUN ( ; ; 66 ) ", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
}

#[test]
pub fn iri() {
    fn iri<'s>(result: &'s OFIri<'s>, test: &'s str) -> bool {
        result.iri == test
    }

    optional!(opt_iri(OFIri<'s>, &'s str), iri);

    TestRun::parse("'external", IriTerm::parse).err(OFIri).q();
    TestRun::parse("'external'", IriTerm::parse)
        .ok(opt_iri, None)
        .q();
    TestRun::parse("'external'#", IriTerm::parse)
        .ok(opt_iri, Some("external"))
        .q();
    TestRun::parse("'external'$", IriTerm::parse)
        .ok(opt_iri, None)
        .q();
    TestRun::parse(" 'external' # ", IriTerm::parse)
        .ok(opt_iri, Some("external"))
        .q();
}

#[test]
pub fn sheet_name() {
    fn sheetname<'s>(result: &'s OFSheetName<'s>, test: &'s str) -> bool {
        result.name == test
    }

    optional!(opt_sheetname(OFSheetName<'s>, &'s str), sheetname);

    TestRun::parse("'sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    TestRun::parse("'sheet''name'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheet'name"))
        .q();
    TestRun::parse("'sheetname'", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    TestRun::parse("'sheetname", SheetNameTerm::parse)
        .err(OFSheetName)
        .expect(OFSingleQuoteEnd)
        .q();
    TestRun::parse("sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, None)
        .q();
    TestRun::parse("$'sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    TestRun::parse("$sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, None)
        .q();
    TestRun::parse("$'sheetname.", SheetNameTerm::parse)
        .err(OFSheetName)
        .expect(OFSingleQuoteEnd)
        .q();
    TestRun::parse("$'sheetname'", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();

    TestRun::parse(" $ 'sheetname'", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
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

    TestRun::parse(".Pi", NamedExpr::parse).ok(ident, "Pi").q();
    TestRun::parse(".$$Tau", NamedExpr::parse)
        .ok(ident, "Tau")
        .q();
    TestRun::parse("'xref'#.Rho", NamedExpr::parse)
        .ok(iri, "xref")
        .q();
    TestRun::parse("'xref'#'hobo'.Rho", NamedExpr::parse)
        .ok(sheet_name, &"hobo")
        .ok(iri, "xref")
        .q();
    TestRun::parse("'xref'.Rho", NamedExpr::parse)
        .ok(ident, "Rho")
        .ok(sheet_name, "xref")
        .q();
    TestRun::parse("'xref'Foo", NamedExpr::parse)
        .err(OFNamed)
        .expect(OFDot)
        .q();
    TestRun::parse(".$$'nice and clean'", NamedExpr::parse)
        .ok(ident, "nice and clean")
        .q();
    TestRun::parse(" 'xref' # 'hobo' . Rho ", NamedExpr::parse)
        .ok(sheet_name, &"hobo")
        .ok(iri, "xref")
        .q();
}
