use crate::spantest::TestN;
use openformula::error::OFCode;
use openformula::iparse::error::ParserError;
use openformula::iparse::{ParseResult, Span};
use std::fmt::Debug;

pub type TokenFn<O> = for<'s> fn(Span<'s>) -> ParseResult<'s, O, OFCode>;

impl<'s, O> TestN<(), Span<'s>, (Span<'s>, O), ParserError<'s, OFCode>>
where
    O: Debug,
{
    /// Runs the token function and records the results.
    /// Use ok(), err(), ... to check specifics.
    ///
    /// Finish the test with q().
    pub fn token(span: &'s str, fn_test: fn(Span<'s>) -> ParseResult<'s, O, OFCode>) -> Self {
        Self::run(span, fn_test)
    }
}

impl<'s, P, O> TestN<P, Span<'s>, O, ParserError<'s, OFCode>>
where
    P: Default,
    O: Debug,
{
    /// Checks for an error.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn err(&self, code: OFCode) -> &Self {
        match &self.result {
            Ok(_) => {
                println!("FAIL: Expected error, but was ok!");
                self.flag_fail();
            }
            Err(e) => {
                if e.code != code {
                    println!("FAIL: {:?} <> {:?}", e.code, code);
                    self.flag_fail();
                }
            }
        }
        self
    }

    /// Checks for an expect value.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn expect(&self, code: OFCode) -> &Self {
        match &self.result {
            Ok(_) => {
                println!("FAIL: {:?} was ok not an error.", code,);
                self.flag_fail();
            }
            Err(e) => {
                if !e.is_expected(code) {
                    println!("FAIL: {:?} is not an expected token. {:?}", code, e.expect);
                    self.flag_fail();
                }
            }
        }

        self
    }

    /// Checks for an expect value.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn expect2(&self, code: OFCode, parent: OFCode) -> &Self {
        match &self.result {
            Ok(_) => {
                println!("FAIL: {:?} was ok not an error.", code,);
                self.flag_fail();
            }
            Err(e) => {
                if !e.is_expected2(code, parent) {
                    println!("FAIL: {:?} is not an expected token. {:?}", code, e.expect);
                    self.flag_fail();
                }
            }
        }

        self
    }
}
