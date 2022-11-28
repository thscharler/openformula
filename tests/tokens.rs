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

// comparison fn
fn span<'a, 'b, 's>(span: &'a Span<'s>, value: (usize, &'b str)) -> bool {
    **span == value.1 && span.location_offset() == value.0
}

// comparison fn for the first part
fn span_0<'a, 'b, 's>(span: &'a (Option<Span<'s>>, Span<'s>), value: (usize, &'b str)) -> bool {
    if let Some(span) = &span.0 {
        **span == value.1 && span.location_offset() == value.0
    } else {
        false
    }
}

// comparison fn for the first part
fn span_0_isnone<'a, 's>(span: &'a (Option<Span<'s>>, Span<'s>), _value: ()) -> bool {
    span.0.is_none()
}

// comparison fn for the second part
fn span_1<'a, 'b, 's>(span: &'a (Option<Span<'s>>, Span<'s>), value: (usize, &'b str)) -> bool {
    *span.1 == value.1 && span.1.location_offset() == value.0
}

#[test]
pub fn test_number() {
    Test::token(".1", number).ok(span, (0, ".1")).q();
    Test::token(".1e5", number).ok(span, (0, ".1e5")).q();
    Test::token(".1e-5", number).ok(span, (0, ".1e-5")).q();
    Test::token("1", number).ok(span, (0, "1")).q();
    Test::token("1.", number).ok(span, (0, "1.")).q();
    Test::token("1.1", number).ok(span, (0, "1.1")).q();
    Test::token("1.1e5", number).ok(span, (0, "1.1e5")).q();
    Test::token("1.1e-5", number).ok(span, (0, "1.1e-5")).q();
    Test::token("1e5", number).ok(span, (0, "1e5")).q();
    Test::token("1e-5", number).ok(span, (0, "1e-5")).q();

    Test::token("1f-5", number).ok(span, (0, "1")).q();
    Test::token("f", number).err(OFCNumber).q();
    Test::token(".f", number).err(OFCNumber).q();
}

#[test]
pub fn test_string() {
    Test::token("\"\"", string).ok(span, (0, "\"\"")).q();
    Test::token("\"A\"", string).ok(span, (0, "\"A\"")).q();
    Test::token("\"ABC\"", string).ok(span, (0, "\"ABC\"")).q();
    Test::token("\"ABC", string).err(OFCQuoteEnd).q();
    Test::token("ABC\"", string).err(OFCQuoteStart).q();
    Test::token("ABC", string).err(OFCQuoteStart).q();

    Test::token("\"AB\"\"CD\"", string)
        .ok(span, (0, "\"AB\"\"CD\""))
        .q();
    Test::token("\"AB\"CD\"", string)
        .ok(span, (0, "\"AB\""))
        .q();
    Test::token("\"AB\"\"\"CD\"", string)
        .ok(span, (0, "\"AB\"\"\""))
        .q();
}

#[test]
pub fn test_fn_name() {
    Test::token("&", fn_name).err(OFCFnName).q();
    Test::token("", fn_name).err(OFCFnName).q();
    Test::token("A", fn_name).ok(span, (0, "A")).q();
    Test::token("A.B", fn_name).ok(span, (0, "A.B")).q();
    Test::token("A_B", fn_name).ok(span, (0, "A_B")).q();
}

#[test]
pub fn test_comparison() {
    Test::token("&", comparison_op).err(OFCCompOp).q();
    Test::token("=", comparison_op).ok(span, (0, "=")).q();
}

#[test]
pub fn test_sheet_name() {
    Test::token("", sheet_name).err(OFCSheetName).q();
    Test::token("$", sheet_name).err(OFCSheetName).q();
    Test::token("$'funinthesun'", sheet_name)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "'funinthesun'"))
        .q();
    Test::token("$'funinthesun", sheet_name)
        .err(OFCSingleQuoteEnd)
        .q();
    Test::token("$funinthesun'", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun]", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun.", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun ", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun$", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
    Test::token("$funinthesun#", sheet_name)
        .ok(span_1, (1, "funinthesun"))
        .q();
}

#[test]
pub fn test_quoted_sheet_name() {
    Test::token("", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q();
    Test::token("$", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q();
    Test::token("$'funinthesun'", quoted_sheet_name)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "'funinthesun'"))
        .q();
    Test::token("$'funinthesun", quoted_sheet_name)
        .err(OFCSingleQuoteEnd)
        .q();
    Test::token("$funinthesun'", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q();
    Test::token("$funinthesun", quoted_sheet_name)
        .err(OFCSingleQuoteStart)
        .q();
}

#[test]
pub fn test_iri() {
    Test::token("", iri).err(OFCSingleQuoteStart).q();
    Test::token("$", iri).err(OFCSingleQuoteStart).q();
    Test::token("'funinthesun'#", iri)
        .ok(span, (0, "'funinthesun'"))
        .q();
    Test::token("'funinthesun'", iri).err(OFCHashtag).q();
    Test::token("'funinthesun", iri).err(OFCSingleQuoteEnd).q();
    Test::token("funinthesun'", iri)
        .err(OFCSingleQuoteStart)
        .q();
}

#[test]
pub fn test_row() {
    Test::token("", row).err(OFCDigit).q();
    Test::token("#5", row).err(OFCDigit).q();
    Test::token("123", row)
        .ok(span_0_isnone, ())
        .ok(span_1, (0, "123"))
        .q();
    Test::token("123 ", row).ok(span_1, (0, "123")).q();
    Test::token("$123", row)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "123"))
        .q();
    Test::token("$", row).err(OFCDigit).q();
    Test::token("$a", row).err(OFCDigit).q();
}

#[test]
pub fn test_col() {
    Test::token("", col).err(OFCAlpha).q();
    Test::token("#5", col).err(OFCAlpha).q();
    Test::token("123", col).err(OFCAlpha).q();
    Test::token("123", col).err(OFCAlpha).q();
    Test::token("$123", col).err(OFCAlpha).q();
    Test::token("$123", col).err(OFCAlpha).q();
    Test::token("$", col).err(OFCAlpha).q();
    Test::token("#a", col).err(OFCAlpha).q();
    Test::token("$a", col)
        .ok(span_0, (0, "$"))
        .ok(span_1, (1, "a"))
        .q();
    Test::token("ACF", col)
        .ok(span_0_isnone, ())
        .ok(span_1, (0, "ACF"))
        .q();
    Test::token("ACF ", col).ok(span_1, (0, "ACF")).q();
    Test::token("ACF123", col).ok(span_1, (0, "ACF")).q();
}

#[test]
pub fn test_single_quoted() {
    Test::token("", single_quoted).err(OFCSingleQuoteStart).q();
    Test::token("'aaa'", single_quoted)
        .ok(span, (0, "'aaa'"))
        .q();
    Test::token("'aaa", single_quoted)
        .err(OFCSingleQuoteEnd)
        .q();
    Test::token("aaa'", single_quoted)
        .err(OFCSingleQuoteStart)
        .q();
    Test::token("'aa''aa'", single_quoted)
        .ok(span, (0, "'aa''aa'"))
        .q();
    Test::token("'aa'''aa'", single_quoted)
        .ok(span, (0, "'aa'''"))
        .q();
    Test::token("'aa'aa'", single_quoted)
        .ok(span, (0, "'aa'"))
        .q();
}
