extern crate core;

use crate::spantest::*;
use openformula::ast::tokens::{
    col, comparison_op, empty, fn_name, iri, number, quoted_sheet_name, row, sheet_name,
    single_quoted, string,
};
use openformula::error::OFCode::*;
use openformula::iparse::Span;

mod spantest;

#[test]
pub fn test_empty() {
    let span = Span::new("nice span");
    let empt = empty(span);

    assert!(std::ptr::eq(
        *span.fragment() as *const _ as *const (),
        *empt.fragment() as *const _ as *const ()
    ));
}

type R = CheckDump;

#[test]
pub fn test_number() {
    Test::token(".1", number).ok(span, (0, ".1")).q::<R>();
    Test::token(".1e5", number).ok(span, (0, ".1e5")).q::<R>();
    Test::token(".1e-5", number).ok(span, (0, ".1e-5")).q::<R>();
    Test::token("1", number).ok(span, (0, "1")).q::<R>();
    Test::token("1.", number).ok(span, (0, "1.")).q::<R>();
    Test::token("1.1", number).ok(span, (0, "1.1")).q::<R>();
    Test::token("1.1e5", number).ok(span, (0, "1.1e5")).q::<R>();
    Test::token("1.1e-5", number)
        .ok(span, (0, "1.1e-5"))
        .q::<R>();
    Test::token("1e5", number).ok(span, (0, "1e5")).q::<R>();
    Test::token("1e-5", number).ok(span, (0, "1e-5")).q::<R>();

    Test::token("1f-5", number).ok(span, (0, "1")).q::<R>();
    Test::token("f", number).err(OFCNumber).q::<R>();
    Test::token(".f", number).err(OFCNumber).q::<R>();
}

#[test]
pub fn test_string() {
    Test::token("\"\"", string).ok(span, (0, "\"\"")).q::<R>();
    Test::token("\"A\"", string).ok(span, (0, "\"A\"")).q::<R>();
    Test::token("\"ABC\"", string)
        .ok(span, (0, "\"ABC\""))
        .q::<R>();
    Test::token("\"ABC", string).err(OFCQuoteEnd).q::<R>();
    Test::token("ABC\"", string).err(OFCQuoteStart).q::<R>();
    Test::token("ABC", string).err(OFCQuoteStart).q::<R>();

    Test::token("\"AB\"\"CD\"", string)
        .ok(span, (0, "\"AB\"\"CD\""))
        .q::<R>();
    Test::token("\"AB\"CD\"", string)
        .ok(span, (0, "\"AB\""))
        .q::<R>();
    Test::token("\"AB\"\"\"CD\"", string)
        .ok(span, (0, "\"AB\"\"\""))
        .q::<R>();
}

#[test]
pub fn test_fn_name() {
    Test::token("&", fn_name).err(OFCFnName).q::<R>();
    Test::token("", fn_name).err(OFCFnName).q::<R>();
    Test::token("A", fn_name).ok(span, (0, "A")).q::<R>();
    Test::token("A.B", fn_name).ok(span, (0, "A.B")).q::<R>();
    Test::token("A_B", fn_name).ok(span, (0, "A_B")).q::<R>();
}

#[test]
pub fn test_comparison() {
    Test::token("&", comparison_op).err(OFCCompOp).q::<R>();
    Test::token("=", comparison_op).ok(span, (0, "=")).q::<R>();
}

#[test]
pub fn test_sheet_name() {
    Test::token("", sheet_name).err(OFCSheetName).q::<R>();
    Test::token("$", sheet_name).err(OFCSheetName).q::<R>();
    Test::token("$'funinthesun'", sheet_name)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "'funinthesun'"))
        .q::<R>();
    Test::token("$'funinthesun", sheet_name)
        .err(OFCSingleQuoteEnd)
        .q::<R>();
    Test::token("$funinthesun'", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun]", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun.", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun ", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun$", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
    Test::token("$funinthesun#", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q::<R>();
}

#[test]
pub fn test_quoted_sheet_name() {
    Test::token("", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q::<R>();
    Test::token("$", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q::<R>();
    Test::token("$'funinthesun'", quoted_sheet_name)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "'funinthesun'"))
        .q::<R>();
    Test::token("$'funinthesun", quoted_sheet_name)
        .err(OFCSingleQuoteEnd)
        .q::<R>();
    Test::token("$funinthesun'", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q::<R>();
    Test::token("$funinthesun", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q::<R>();
}

#[test]
pub fn test_iri() {
    Test::token("", iri).err(OFCSingleQuoteStart).q::<R>();
    Test::token("$", iri).err(OFCSingleQuoteStart).q::<R>();
    Test::token("'funinthesun'#", iri)
        .ok(span, (0, "'funinthesun'"))
        .q::<R>();
    Test::token("'funinthesun'", iri).err(OFCHashtag).q::<R>();
    Test::token("'funinthesun", iri)
        .err(OFCSingleQuoteEnd)
        .q::<R>();
    Test::token("funinthesun'", iri)
        .err(OFCSingleQuoteStart)
        .q::<R>();
}

#[test]
pub fn test_row() {
    Test::token("", row).err(OFCDigit).q::<R>();
    Test::token("#5", row).err(OFCDigit).q::<R>();
    Test::token("123", row)
        .ok(span_0_isnone, ())
        .ok(span_1, (0, "123"))
        .q::<R>();
    Test::token("123 ", row).ok(span_1, (0, "123")).q::<R>();
    Test::token("$123", row)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "123"))
        .q::<R>();
    Test::token("$", row).err(OFCDigit).q::<R>();
    Test::token("$a", row).err(OFCDigit).q::<R>();
}

#[test]
pub fn test_col() {
    Test::token("", col).err(OFCAlpha).q::<R>();
    Test::token("#5", col).err(OFCAlpha).q::<R>();
    Test::token("123", col).err(OFCAlpha).q::<R>();
    Test::token("123", col).err(OFCAlpha).q::<R>();
    Test::token("$123", col).err(OFCAlpha).q::<R>();
    Test::token("$123", col).err(OFCAlpha).q::<R>();
    Test::token("$", col).err(OFCAlpha).q::<R>();
    Test::token("#a", col).err(OFCAlpha).q::<R>();
    Test::token("$a", col)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "a"))
        .q::<R>();
    Test::token("ACF", col)
        .ok(span_0_isnone, ())
        .ok(span_1, (0, "ACF"))
        .q::<R>();
    Test::token("ACF ", col).ok(span_1, (0, "ACF")).q::<R>();
    Test::token("ACF123", col).ok(span_1, (0, "ACF")).q::<R>();
}

#[test]
pub fn test_single_quoted() {
    Test::token("", single_quoted)
        .err(OFCSingleQuoteStart)
        .q::<R>();
    Test::token("'aaa'", single_quoted)
        .ok(span, (0, "'aaa'"))
        .q::<R>();
    Test::token("'aaa", single_quoted)
        .err(OFCSingleQuoteEnd)
        .q::<R>();
    Test::token("aaa'", single_quoted)
        .err(OFCSingleQuoteStart)
        .q::<R>();
    Test::token("'aa''aa'", single_quoted)
        .ok(span, (0, "'aa''aa'"))
        .q::<R>();
    Test::token("'aa'''aa'", single_quoted)
        .ok(span, (0, "'aa'''"))
        .q::<R>();
    Test::token("'aa'aa'", single_quoted)
        .ok(span, (0, "'aa'"))
        .q::<R>();
}
