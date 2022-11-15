mod spantest;

use crate::spantest::*;
use nom::error::ErrorKind;
use openformula::ast::nomtokens::*;
use openformula::ast::Span;

#[test]
pub fn test_simple<'a>() {
    eat_space(Span::new("  X")).cok(2, "X");
    eat_space(Span::new("X")).cok(0, "X");

    decimal(Span::new("A")).cnom(ErrorKind::OneOf);
    decimal(Span::new("")).cnom(ErrorKind::OneOf);
    decimal(Span::new("12")).cok(0, "12");

    fn_name_nom(Span::new("&")).cnom(ErrorKind::TakeWhile1);
    fn_name_nom(Span::new("")).cnom(ErrorKind::TakeWhile1);
    fn_name_nom(Span::new("A")).cok(0, "A");
    fn_name_nom(Span::new("A.B")).cok(0, "A.B");
    fn_name_nom(Span::new("A_B")).cok(0, "A_B");

    identifier_nom(Span::new("&")).cnom(ErrorKind::TakeWhile1);
    identifier_nom(Span::new("")).cnom(ErrorKind::TakeWhile1);
    identifier_nom(Span::new("A")).cok(0, "A");
    identifier_nom(Span::new("A.B")).cok(0, "A");
    identifier_nom(Span::new("A_B")).cok(0, "A_B");

    sheet_name_nom(Span::new("asdf asdf")).cok(0, "asdf");
    sheet_name_nom(Span::new("asdf]asdf")).cok(0, "asdf");
    sheet_name_nom(Span::new("asdf.asdf")).cok(0, "asdf");
    sheet_name_nom(Span::new("asdf#asdf")).cok(0, "asdf");
    sheet_name_nom(Span::new("asdf$asdf")).cok(0, "asdf");
    sheet_name_nom(Span::new("$asdf")).cnom(ErrorKind::NoneOf);
    sheet_name_nom(Span::new("/asdf")).cok(0, "/asdf");

    row_nom(Span::new("")).cnom(ErrorKind::OneOf);
    row_nom(Span::new("#5")).cnom(ErrorKind::OneOf);
    row_nom(Span::new("$5")).cok0(0, "$");
    row_nom(Span::new("$5")).cok1(1, "5");
    row_nom(Span::new("#A")).cnom(ErrorKind::OneOf);
    row_nom(Span::new("$A")).cnom(ErrorKind::OneOf);
    row_nom(Span::new("$A")).cnom(ErrorKind::OneOf);
    row_nom(Span::new("123456789")).cnone0();
    row_nom(Span::new("123456789")).cok1(0, "123456789");
    row_nom(Span::new("ACF")).cnom(ErrorKind::OneOf);

    col_nom(Span::new("")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("#5")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("$5")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("$5")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("#A")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("$A")).cok0(0, "$");
    col_nom(Span::new("$A")).cok1(1, "A");
    col_nom(Span::new("123456789")).cnom(ErrorKind::Alpha);
    col_nom(Span::new("ACF")).cnone0();
    col_nom(Span::new("ACF")).cok1(0, "ACF");
}
