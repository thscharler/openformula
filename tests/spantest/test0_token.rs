use crate::spantest::{CompareFn, Report, TestN};
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

impl<'s, P, O, E> TestN<P, Span<'s>, (Span<'s>, O), E>
where
    P: Default,
    O: Debug,
    E: Debug,
{
    /// Checks for ok.
    /// Uses an extraction function to get the relevant result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn ok<V>(&'s self, eq: CompareFn<O, V>, test: V) -> &Self
    where
        V: Debug + Copy,
        O: Debug,
    {
        match &self.result {
            Ok((_, token)) => {
                if !eq(token, test) {
                    println!("FAIL: Value mismatch: {:?} <> {:?}", token, test);
                    self.flag_fail();
                }
            }
            Err(_) => {
                println!("FAIL: Expect ok, but was an error!");
                self.flag_fail();
            }
        }
        self
    }

    /// Tests the remaining string after parsing.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn rest(&self, test: &str) -> &Self {
        match &self.result {
            Ok((rest, _)) => {
                if **rest != test {
                    println!("FAIL: Rest mismatch {} <> {}", **rest, test);
                    self.flag_fail();
                }
            }
            Err(_) => {
                println!("FAIL: Expect ok, but was an error!");
                self.flag_fail();
            }
        }
        self
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

pub struct CheckDump;

impl<'s, P, O, E> Report<P, Span<'s>, (Span<'s>, O), E> for CheckDump
where
    P: Default,
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<P, Span<'s>, (Span<'s>, O), E>) {
        if testn.fail.get() {
            Dump::report(testn);
            panic!()
        }
    }
}

pub struct Dump;

impl<'s, P, O, E> Report<P, Span<'s>, (Span<'s>, O), E> for Dump
where
    P: Default,
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<P, Span<'s>, (Span<'s>, O), E>) {
        println!();
        println!(
            "when parsing '{}' in {}ns =>",
            testn.span,
            testn.duration.as_nanos()
        );
        match &testn.result {
            Ok((rest, token)) => {
                println!("rest {}:\"{}\"", rest.location_offset(), rest);
                println!("{:0?}", token);
            }
            Err(e) => {
                println!("error");
                println!("{:1?}", e);
            }
        }
    }
}
