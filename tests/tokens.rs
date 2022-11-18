extern crate core;

use crate::spantest::CheckFailToken;
use crate::spantest::*;
use openformula::ast::tokens::{
    col, comparison_op, empty, fn_name, iri, number, quoted_sheet_name, row, sheet_name,
    single_quoted, string,
};
use openformula::ast::Span;
use openformula::error::OFCode::*;

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

#[test]
pub fn test_number() {
    number(Span::new("1")).cok(0, "1");
    number(Span::new(".1")).cok(0, ".1");
    number(Span::new(".1e5")).cok(0, ".1e5");
    number(Span::new(".1e-5")).cok(0, ".1e-5");
    number(Span::new("1")).cok(0, "1");
    number(Span::new("1.")).cok(0, "1.");
    number(Span::new("1.1")).cok(0, "1.1");
    number(Span::new("1.1e5")).cok(0, "1.1e5");
    number(Span::new("1.1e-5")).cok(0, "1.1e-5");
    number(Span::new("1e5")).cok(0, "1e5");
    number(Span::new("1e-5")).cok(0, "1e-5");

    number(Span::new("1f-5")).cok(0, "1");
    number(Span::new("f")).ctok(OFNumber);
    number(Span::new(".f")).ctok(OFNumber);
}

#[test]
pub fn test_string() {
    string(Span::new("\"\"")).cok(1, "");
    string(Span::new("\"ABC\"")).cok(1, "ABC");
    string(Span::new("\"ABC")).ctok(OFQuoteEnd);
    string(Span::new("ABC\"")).ctok(OFQuoteStart);
    string(Span::new("ABC")).ctok(OFQuoteStart);

    string(Span::new("\"AB\"\"CD\"")).cok(1, "AB\"\"CD");
    string(Span::new("\"AB\"CD\"")).cok(1, "AB");
    string(Span::new("\"AB\"\"\"CD\"")).cok(1, "AB\"\"");
}

#[test]
pub fn test_fn_name() {
    fn_name(Span::new("&")).ctok(OFFnName);
    fn_name(Span::new("")).ctok(OFFnName);
    fn_name(Span::new("A")).cok(0, "A");
    fn_name(Span::new("A.B")).cok(0, "A.B");
    fn_name(Span::new("A_B")).cok(0, "A_B");
}

#[test]
pub fn test_comparison() {
    comparison_op(Span::new("&")).ctok(OFCompOp);
    comparison_op(Span::new("=")).cok(0, "=");
}

#[test]
pub fn test_sheet_name() {
    sheet_name(Span::new("")).ctok(OFSheetName);
    sheet_name(Span::new("$")).ctok(OFSheetName);
    sheet_name(Span::new("$'funinthesun'")).cok0(0, "$");
    sheet_name(Span::new("$'funinthesun'")).cok1(1, "'funinthesun'");
    sheet_name(Span::new("$'funinthesun")).ctok(OFSingleQuoteEnd);
    sheet_name(Span::new("$funinthesun'")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun]")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun.")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun ")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun$")).cok1(1, "funinthesun");
    sheet_name(Span::new("$funinthesun#")).cok1(1, "funinthesun");
}

#[test]
pub fn test_quoted_sheet_name() {
    quoted_sheet_name(Span::new("")).ctok(OFSingleQuoteStart);
    quoted_sheet_name(Span::new("$")).ctok(OFSingleQuoteStart);
    quoted_sheet_name(Span::new("$'funinthesun'")).cok0(0, "$");
    quoted_sheet_name(Span::new("$'funinthesun'")).cok1(1, "'funinthesun'");
    quoted_sheet_name(Span::new("$'funinthesun")).ctok(OFSingleQuoteEnd);
    quoted_sheet_name(Span::new("$funinthesun'")).ctok(OFSingleQuoteStart);
    quoted_sheet_name(Span::new("$funinthesun")).ctok(OFSingleQuoteStart);
}

#[test]
pub fn test_iri() {
    iri(Span::new("")).ctok(OFSingleQuoteStart);
    iri(Span::new("$")).ctok(OFSingleQuoteStart);
    iri(Span::new("'funinthesun'#")).cok(0, "'funinthesun'");
    iri(Span::new("'funinthesun'")).ctok(OFHashtag);
    iri(Span::new("'funinthesun")).ctok(OFSingleQuoteEnd);
    iri(Span::new("funinthesun'")).ctok(OFSingleQuoteStart);
}

#[test]
pub fn test_row() {
    row(Span::new("")).ctok(OFDigit);
    row(Span::new("#5")).ctok(OFDigit);
    row(Span::new("123")).cnone0();
    row(Span::new("123")).cok1(0, "123");
    row(Span::new("123 ")).cok1(0, "123");
    row(Span::new("$123")).cok0(0, "$");
    row(Span::new("$123")).cok1(1, "123");
    row(Span::new("$")).ctok(OFDigit);
    row(Span::new("$a")).ctok(OFDigit);
}

#[test]
pub fn test_col() {
    col(Span::new("")).ctok(OFAlpha);
    col(Span::new("#5")).ctok(OFAlpha);
    col(Span::new("123")).ctok(OFAlpha);
    col(Span::new("123")).ctok(OFAlpha);
    col(Span::new("$123")).ctok(OFAlpha);
    col(Span::new("$123")).ctok(OFAlpha);
    col(Span::new("$")).ctok(OFAlpha);
    col(Span::new("#a")).ctok(OFAlpha);
    col(Span::new("$a")).cok0(0, "$");
    col(Span::new("$a")).cok1(1, "a");
    col(Span::new("ACF")).cnone0();
    col(Span::new("ACF")).cok1(0, "ACF");
    col(Span::new("ACF ")).cok1(0, "ACF");
    col(Span::new("ACF123")).cok1(0, "ACF");
}

#[test]
pub fn test_single_quoted() {
    single_quoted(Span::new("")).ctok(OFSingleQuoteStart);
    single_quoted(Span::new("'aaa'")).cok(0, "'aaa'");
    single_quoted(Span::new("'aaa")).ctok(OFSingleQuoteEnd);
    single_quoted(Span::new("aaa'")).ctok(OFSingleQuoteStart);
    single_quoted(Span::new("'aa''aa'")).cok(0, "'aa''aa'");
    single_quoted(Span::new("'aa'''aa'")).cok(0, "'aa'''");
    single_quoted(Span::new("'aa'aa'")).cok(0, "'aa'");
}
