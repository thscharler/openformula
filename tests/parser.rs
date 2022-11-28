mod spantest;

use openformula::ast::parser::{
    AddExpr, CellRangeExpr, CellRefExpr, ColRangeExpr, ColTerm, CompareExpr, ElementaryExpr, Expr,
    FnCallExpr, IriTerm, MulExpr, NamedExpr, NumberExpr, ParenthesesExpr, PostfixExpr, PowExpr,
    PrefixExpr, ReferenceExpr, RowRangeExpr, RowTerm, SheetNameTerm, StringExpr,
};
use openformula::ast::{OFAst, OFIri, OFSheetName};
use openformula::error::OFCode;
use openformula::error::OFCode::*;
use openformula::iparse::tracer::Track;
use openformula::iparse::Parser;

use spantest::*;

#[allow(dead_code)]
// can be used as filter for some annoying stuff.
fn filter_ops_and_ref(t: &Track<'_, OFCode>) -> bool {
    if [
        OFCPrefix,
        OFCPrefixOp,
        // OFCPostfix,
        // OFCPostfixOp,
        OFCPow,
        OFCPowOp,
        OFCMul,
        OFCMulOp,
        OFCAdd,
        OFCAddOp,
        OFCComp,
        OFCCompOp,
    ]
    .contains(&t.func())
    {
        return false;
    }

    if t.func() != OFCReference && t.parents().contains(&OFCReference) {
        return false;
    }

    return true;
}

#[test]
pub fn expr() {
    Test::parse("471", Expr::parse).okok().q();
    Test::parse(r#""strdata""#, Expr::parse).okok().q();
    Test::parse("1+1", Expr::parse).okok().q();
    Test::parse("(1+1)", Expr::parse).okok().q();
    Test::parse("4*5+1", Expr::parse).okok().q();
    Test::parse("4+5*2", Expr::parse).okok().q();
    Test::parse("22 * FUN ( 77 )", Expr::parse).okok().q();
    Test::parse("17 + FUN(  )", Expr::parse).okok().q();
    Test::parse("1+2*3^4", Expr::parse).okok().q();
    Test::parse("27+(19*.A5)", Expr::parse).okok().q();
}

#[test]
pub fn expr_fail() {
    Test::parse("471X", Expr::parse).okok().rest("X").q();
    Test::parse(r#""strdata"#, Expr::parse).err(OFCExpr).q();
    Test::parse("1+", Expr::parse)
        .err(OFCExpr)
        .expect(OFCNumber)
        .q();
    Test::parse("(1+1", Expr::parse)
        .err(OFCExpr)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("XX", Expr::parse)
        .err(OFCExpr)
        .expect(OFCDot)
        .q();
    Test::parse("4*5+", Expr::parse)
        .err(OFCExpr)
        .expect(OFCNumber)
        .q();
    Test::parse("4+5*", Expr::parse)
        .err(OFCExpr)
        .expect(OFCNumber)
        .q();
    Test::parse("22 * $FUN()", Expr::parse).err(OFCExpr).q();
    Test::parse("22 * FUN ( 77+  ", Expr::parse)
        .err(OFCExpr)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("22 * FUN ( 77  ", Expr::parse)
        .err(OFCExpr)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("22 * FUN 77 ) ", Expr::parse)
        .err(OFCExpr)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse("11 ^ FUN(   ;;.A)", Expr::parse).okok().q();
    Test::parse("11 ^ FUN(   ;;X)", Expr::parse)
        .err(OFCExpr)
        .expect(OFCFnCall)
        .q();
}

#[test]
pub fn comp() {
    Test::parse("", CompareExpr::parse)
        .err(OFCComp)
        .expect(OFCElementary)
        .q();
    Test::parse("123>3", CompareExpr::parse).okok().q();
    Test::parse("123>", CompareExpr::parse)
        .err(OFCComp)
        .expect(OFCElementary)
        .q();
    Test::parse(">3", CompareExpr::parse)
        .err(OFCComp)
        .expect(OFCElementary)
        .q();
    Test::parse(" 123 > 3 ", CompareExpr::parse).okok().q();
}

#[test]
pub fn add() {
    Test::parse("", AddExpr::parse)
        .err(OFCAdd)
        .expect(OFCElementary)
        .q();
    Test::parse("123+3", AddExpr::parse).okok().q();
    Test::parse("123+", AddExpr::parse)
        .err(OFCAdd)
        .expect(OFCElementary)
        .q();
    Test::parse("+3", AddExpr::parse).okok().q();
    Test::parse(" 123 + 3 ", AddExpr::parse).okok().q();
}

#[test]
pub fn mul() {
    Test::parse("", MulExpr::parse)
        .err(OFCMul)
        .expect(OFCElementary)
        .q();
    Test::parse("123*3", MulExpr::parse).okok().q();
    Test::parse("123*", MulExpr::parse)
        .err(OFCMul)
        .expect(OFCElementary)
        .q();
    Test::parse("*3", MulExpr::parse)
        .err(OFCMul)
        .expect(OFCElementary)
        .q();
    Test::parse(" 123 * 3 ", MulExpr::parse).okok().q();
}

#[test]
pub fn pow() {
    Test::parse("", PowExpr::parse)
        .err(OFCPow)
        .expect(OFCElementary)
        .q();
    Test::parse("123^3", PowExpr::parse).okok().q();
    Test::parse("123^", PowExpr::parse)
        .err(OFCPow)
        .expect(OFCElementary)
        .q();
    Test::parse("^3", PowExpr::parse)
        .err(OFCPow)
        .expect(OFCElementary)
        .q();
    Test::parse(" 123 ^ 3 ", PowExpr::parse).okok().q();
}

#[test]
pub fn postfix() {
    Test::parse("", PostfixExpr::parse)
        .err(OFCPostfix)
        .expect(OFCElementary)
        .q();
    Test::parse("5", PostfixExpr::parse)
        .filter(&filter_ops_and_ref)
        .okok()
        .q();
    Test::parse("5%", PostfixExpr::parse)
        .filter(&filter_ops_and_ref)
        .okok()
        .q();
}

#[test]
pub fn prefix() {
    Test::parse("", PrefixExpr::parse).err(OFCPrefix).q();
    Test::parse("--5", PrefixExpr::parse).okok().q();
    Test::parse("+-5", PrefixExpr::parse).okok().q();
    Test::parse(" + - 5", PrefixExpr::parse).okok().q();
}

#[test]
pub fn elementary() {
    Test::parse("", ElementaryExpr::parse)
        .err(OFCElementary)
        .q();
    Test::parse("123", ElementaryExpr::parse).okok().q();
    Test::parse("\"abc\"", ElementaryExpr::parse).okok().q();
    Test::parse("(1)", ElementaryExpr::parse).okok().q();
    Test::parse(".A1", ElementaryExpr::parse).okok().q();
    Test::parse("FUN(2)", ElementaryExpr::parse).okok().q();
    Test::parse(".NAMED", ElementaryExpr::parse).okok().q();
    Test::parse("FUN(RAW(2 X))", ElementaryExpr::parse)
        .err(OFCElementary)
        .q();
}

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

    Test::parse("123", NumberExpr::parse).ok(number, 123f64).q();
    Test::parse("123", NumberExpr::parse).ok(number, 123f64).q();
    Test::parse("123", NumberExpr::parse).ok(number, 123f64).q();
    Test::parse("123", NumberExpr::parse).ok(number, 123f64).q();
    Test::parse("123", NumberExpr::parse).ok(number, 123f64).q();
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

    Test::parse("", StringExpr::parse).err(OFCString).q();
    Test::parse(r#""""#, StringExpr::parse).ok(string, "").q();
    Test::parse(r#""abc""#, StringExpr::parse)
        .ok(string, "abc")
        .q();
    Test::parse(r#""abc"#, StringExpr::parse)
        .err(OFCString)
        .expect(OFCQuoteEnd)
        .q();
    Test::parse(r#"abc""#, StringExpr::parse)
        .err(OFCString)
        .expect(OFCQuoteStart)
        .q();
    Test::parse(r#""ab""cd""#, StringExpr::parse)
        .ok(string, "ab\"cd")
        .q();
    Test::parse(r#""ab"cd""#, StringExpr::parse)
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

    Test::parse(".1A", ReferenceExpr::parse)
        .err(OFCReference)
        .q();
    Test::parse(".A1:.B2", ReferenceExpr::parse)
        .ok(cellrange, &())
        .q();
    Test::parse(".A1", ReferenceExpr::parse)
        .ok(cellref, &())
        .q();
    Test::parse(".1:.3", ReferenceExpr::parse)
        .ok(rowrange, &())
        .q();
    Test::parse(".C:.E", ReferenceExpr::parse)
        .ok(colrange, &())
        .q();
}

#[test]
pub fn colterm() {
    Test::parse("", ColTerm::parse)
        .err(OFCCol)
        .expect(OFCAlpha)
        .q();
    Test::parse("$A", ColTerm::parse).okok().q();
    Test::parse("$", ColTerm::parse)
        .err(OFCCol)
        .expect(OFCAlpha)
        .q();
    Test::parse("A", ColTerm::parse).okok().q();
    Test::parse(" $A ", ColTerm::parse).err(OFCCol).q();
}

#[test]
pub fn rowterm() {
    Test::parse("", RowTerm::parse)
        .err(OFCRow)
        .expect(OFCDigit)
        .q();
    Test::parse("$1", RowTerm::parse).okok().q();
    Test::parse("$", RowTerm::parse)
        .err(OFCRow)
        .expect(OFCDigit)
        .q();
    Test::parse("1", RowTerm::parse).okok().q();
    Test::parse(" $1 ", RowTerm::parse).err(OFCRow).q();
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

    Test::parse("", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect(OFCDot)
        .q();
    Test::parse("'iri'#.A1", CellRefExpr::parse)
        .ok(iri, "iri")
        .q();
    Test::parse("'sheet'.A1", CellRefExpr::parse)
        .ok(table, "sheet")
        .q();
    Test::parse(".A1", CellRefExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(false, false))
        .q();
    Test::parse(".A", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect(OFCDigit)
        .q();
    Test::parse(".1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse("A1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCDot, OFCCellRef)
        .q();
    Test::parse(".$A$1", CellRefExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(true, true))
        .q();
    Test::parse(".$A $1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCDigit, OFCRow)
        .q();
    Test::parse(".$ A$1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse(".$A$ 1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCDigit, OFCRow)
        .q();
    Test::parse(".$A$$1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCDigit, OFCRow)
        .q();
    Test::parse(".$$A$$1", CellRefExpr::parse)
        .err(OFCCellRef)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse(" 'iri' # $ 'sheet' . $A$1 ", CellRefExpr::parse)
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

    Test::parse("", CellRangeExpr::parse).err(OFCCellRange).q();
    Test::parse("'iri'#.A1:.C3", CellRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    Test::parse("'sheet'.A1:.C3", CellRangeExpr::parse)
        .ok(table, "sheet")
        .q();
    Test::parse(".A1:.C3", CellRangeExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(to_row_col, &(2, 2))
        .q();
    Test::parse(".$A$1:.$C$3", CellRangeExpr::parse)
        .ok(row_col, &(0, 0))
        .ok(absolute, &(true, true))
        .ok(to_row_col, &(2, 2))
        .ok(to_absolute, &(true, true))
        .q();
    Test::parse("'fun'.$A$1:'nofun'.$C$3", CellRangeExpr::parse)
        .ok(table, "fun")
        .ok(to_table, "nofun")
        .q();
    Test::parse(".A1:.C3", CellRangeExpr::parse).okok().q();
    Test::parse(".A1:.3", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse(".A1:.C", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect2(OFCDigit, OFCRow)
        .q();
    Test::parse(".A:.C3", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect2(OFCDigit, OFCRow)
        .q();
    Test::parse(".1:.C3", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse(":.C3", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect(OFCDot)
        .q();
    Test::parse("A1:C3", CellRangeExpr::parse)
        .err(OFCCellRange)
        .expect(OFCDot)
        .q();
    Test::parse(
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

    Test::parse("", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCDot)
        .q();
    Test::parse("'iri'#.A:.C", ColRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    Test::parse("'sheet'.A:.C", ColRangeExpr::parse)
        .ok(sheet_name, "sheet")
        .q();
    Test::parse(".A:.C", ColRangeExpr::parse)
        .ok(col_col, &(0, 2))
        .q();
    Test::parse(".1:", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect2(OFCAlpha, OFCCol)
        .q();
    Test::parse(".A", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCColon)
        .q();
    Test::parse(":", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCDot)
        .q();
    Test::parse(":.A", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCDot)
        .q();
    Test::parse(":A", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCDot)
        .q();
    Test::parse(".5:.7", ColRangeExpr::parse)
        .err(OFCColRange)
        .expect(OFCAlpha)
        .q();
    Test::parse(" 'iri' # 'sheet' . $A : . $C ", ColRangeExpr::parse)
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

    Test::parse("", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCDot)
        .q();
    Test::parse("'iri'#.1:.3", RowRangeExpr::parse)
        .ok(iri, "iri")
        .q();
    Test::parse("'sheet'.1:.3", RowRangeExpr::parse)
        .ok(sheet_name, "sheet")
        .q();
    Test::parse(".1:.3", RowRangeExpr::parse)
        .ok(row_row, &(0, 2))
        .q();
    Test::parse(".1:", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCDot)
        .q();
    Test::parse(".1", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCColon)
        .q();
    Test::parse(":", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCDot)
        .q();
    Test::parse(":.1", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCDot)
        .q();
    Test::parse(".C:.E", RowRangeExpr::parse)
        .err(OFCRowRange)
        .expect(OFCDigit)
        .q();
    Test::parse(" 'iri' # 'sheet' . $1 : . $3 ", RowRangeExpr::parse)
        .ok(iri, "iri")
        .ok(sheet_name, "sheet")
        .q();
}

#[test]
pub fn parentheses() {
    Test::parse("(21)", ParenthesesExpr::parse).okok().q();
    Test::parse("(21", ParenthesesExpr::parse)
        .err(OFCParentheses)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("21)", ParenthesesExpr::parse)
        .err(OFCParentheses)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse("21", ParenthesesExpr::parse)
        .err(OFCParentheses)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse(" ( 21 ) ", ParenthesesExpr::parse).okok().q();
}

#[test]
pub fn fncall() {
    fn name<'s>(result: &'s Box<OFAst<'s>>, test: &'s str) -> bool {
        match &**result {
            OFAst::NodeFnCall(result) => result.name.name == test,
            _ => unreachable!(),
        }
    }

    Test::parse("$FUN()", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCFnName)
        .q();
    Test::parse("FUN ( 77  ", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("FUN 77 ) ", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse("FUN(", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCParenthesesClose)
        .q();
    Test::parse("FUN  )", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse("FUN(   ;;66)", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
    Test::parse(" FUN(;;66)", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
    Test::parse("FUN", FnCallExpr::parse)
        .err(OFCFnCall)
        .expect(OFCParenthesesOpen)
        .q();
    Test::parse(" FUN ( ; ; 66 ) ", FnCallExpr::parse)
        .ok(name, "FUN")
        .q();
}

#[test]
pub fn iri() {
    fn iri<'s>(result: &'s OFIri<'s>, test: &'s str) -> bool {
        result.iri == test
    }

    optional!(opt_iri(OFIri<'s>, &'s str), iri);

    Test::parse("'external", IriTerm::parse).err(OFCIri).q();
    Test::parse("'external'", IriTerm::parse)
        .ok(opt_iri, None)
        .q();
    Test::parse("'external'#", IriTerm::parse)
        .ok(opt_iri, Some("external"))
        .q();
    Test::parse("'external'$", IriTerm::parse)
        .ok(opt_iri, None)
        .q();
    Test::parse(" 'external' # ", IriTerm::parse)
        .ok(opt_iri, Some("external"))
        .q();
}

type ReportType = CheckTrace;

#[test]
pub fn sheet_name() {
    fn sheetname<'s>(result: &'s OFSheetName<'s>, test: &'s str) -> bool {
        result.name == test
    }

    optional!(opt_sheetname(OFSheetName<'s>, &'s str), sheetname);

    TestN::parse("'sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q::<ReportType>();

    Test::parse("'sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    Test::parse("'sheet''name'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheet'name"))
        .q();
    Test::parse("'sheetname'", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    Test::parse("'sheetname", SheetNameTerm::parse)
        .err(OFCSheetName)
        .expect(OFCSingleQuoteEnd)
        .q();
    Test::parse("sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, None)
        .q();
    Test::parse("$'sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();
    Test::parse("$sheetname'.", SheetNameTerm::parse)
        .ok(opt_sheetname, None)
        .q();
    Test::parse("$'sheetname.", SheetNameTerm::parse)
        .err(OFCSheetName)
        .expect(OFCSingleQuoteEnd)
        .q();
    Test::parse("$'sheetname'", SheetNameTerm::parse)
        .ok(opt_sheetname, Some("sheetname"))
        .q();

    Test::parse(" $ 'sheetname'", SheetNameTerm::parse)
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

    Test::parse(".Pi", NamedExpr::parse).ok(ident, "Pi").q();
    Test::parse(".$$Tau", NamedExpr::parse).ok(ident, "Tau").q();
    Test::parse("'xref'#.Rho", NamedExpr::parse)
        .ok(iri, "xref")
        .q();
    Test::parse("'xref'#'hobo'.Rho", NamedExpr::parse)
        .ok(sheet_name, &"hobo")
        .ok(iri, "xref")
        .q();
    Test::parse("'xref'.Rho", NamedExpr::parse)
        .ok(ident, "Rho")
        .ok(sheet_name, "xref")
        .q();
    Test::parse("'xref'Foo", NamedExpr::parse)
        .err(OFCNamed)
        .expect(OFCDot)
        .q();
    Test::parse(".$$'nice and clean'", NamedExpr::parse)
        .ok(ident, "nice and clean")
        .q();
    Test::parse(" 'xref' # 'hobo' . Rho ", NamedExpr::parse)
        .ok(sheet_name, &"hobo")
        .ok(iri, "xref")
        .q();
}
