mod spantest;

use crate::spantest::*;
use nom::error::ErrorKind;
use openformula::ast::tokens::nomtokens::*;
use openformula::iparse::Span;

type R = CheckDump;

#[test]
pub fn test_eat_space<'a>() {
    eat_space(Span::new("  X")).cok(2, "X");
    eat_space(Span::new("X")).cok(0, "X");
}

#[test]
pub fn test_simple<'a>() {
    TestN::nom("A", decimal).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("", decimal).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("12", decimal).ok(span, (0, "12")).q::<R>();

    TestN::nom("&", fn_name_nom)
        .err(ErrorKind::TakeWhile1)
        .q::<R>();
    TestN::nom("", fn_name_nom)
        .err(ErrorKind::TakeWhile1)
        .q::<R>();
    TestN::nom("A", fn_name_nom).ok(span, (0, "A")).q::<R>();
    TestN::nom("A.B", fn_name_nom).ok(span, (0, "A.B")).q::<R>();
    TestN::nom("A_B", fn_name_nom).ok(span, (0, "A_B")).q::<R>();

    TestN::nom("&", identifier_nom)
        .err(ErrorKind::TakeWhile1)
        .q::<R>();
    TestN::nom("", identifier_nom)
        .err(ErrorKind::TakeWhile1)
        .q::<R>();
    TestN::nom("A", identifier_nom).ok(span, (0, "A")).q::<R>();
    TestN::nom("A.B", identifier_nom)
        .ok(span, (0, "A"))
        .q::<R>();
    TestN::nom("A_B", identifier_nom)
        .ok(span, (0, "A_B"))
        .q::<R>();

    TestN::nom("asdf asdf", sheet_name_nom)
        .ok(span, (0, "asdf"))
        .q::<R>();
    TestN::nom("asdf]asdf", sheet_name_nom)
        .ok(span, (0, "asdf"))
        .q::<R>();
    TestN::nom("asdf.asdf", sheet_name_nom)
        .ok(span, (0, "asdf"))
        .q::<R>();
    TestN::nom("asdf#asdf", sheet_name_nom)
        .ok(span, (0, "asdf"))
        .q::<R>();
    TestN::nom("asdf$asdf", sheet_name_nom)
        .ok(span, (0, "asdf"))
        .q::<R>();
    TestN::nom("$asdf", sheet_name_nom)
        .err(ErrorKind::NoneOf)
        .q::<R>();
    TestN::nom("/asdf", sheet_name_nom)
        .ok(span, (0, "/asdf"))
        .q::<R>();

    TestN::nom("", row_nom).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("#5", row_nom).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("$5", row_nom).ok(span_0, (0, "$")).q::<R>();
    TestN::nom("$5", row_nom).ok(span_1, (1, "5")).q::<R>();
    TestN::nom("#A", row_nom).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("$A", row_nom).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("$A", row_nom).err(ErrorKind::OneOf).q::<R>();
    TestN::nom("123456789", row_nom)
        .ok(span_0_isnone, ())
        .q::<R>();
    TestN::nom("123456789", row_nom)
        .ok(span_1, (0, "123456789"))
        .q::<R>();
    TestN::nom("ACF", row_nom).err(ErrorKind::OneOf).q::<R>();

    TestN::nom("", col_nom).err(ErrorKind::Alpha).q::<R>();
    TestN::nom("#5", col_nom).err(ErrorKind::Alpha).q::<R>();
    TestN::nom("$5", col_nom).err(ErrorKind::Alpha).q::<R>();
    TestN::nom("$5", col_nom).err(ErrorKind::Alpha).q::<R>();
    TestN::nom("#A", col_nom).err(ErrorKind::Alpha).q::<R>();
    TestN::nom("$A", col_nom).ok(span_0, (0, "$")).q::<R>();
    TestN::nom("$A", col_nom).ok(span_1, (1, "A")).q::<R>();
    TestN::nom("123456789", col_nom)
        .err(ErrorKind::Alpha)
        .q::<R>();
    TestN::nom("ACF", col_nom).ok(span_0_isnone, ()).q::<R>();
    TestN::nom("ACF", col_nom).ok(span_1, (0, "ACF")).q::<R>();
}
