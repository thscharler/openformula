use openformula::ast::tracer::{Suggest, Tracer};
use openformula::ast::{ParseResult, Span};
use openformula::error::OFError;
use std::cell::RefCell;
use std::fmt::Display;

pub trait TestResult<O> {
    type TestValue;

    fn equal(result: &O, testvalue: &Self::TestValue) -> bool;
    fn str(result: &O) -> String;
    fn val_str(value: &Self::TestValue) -> String;
}

impl<P> TestResult<Option<P>> for Option<P>
where
    P: TestResult<P>,
{
    type TestValue = Option<P::TestValue>;

    fn equal(result: &Option<P>, testvalue: &Self::TestValue) -> bool {
        match result {
            None => match testvalue {
                None => true,
                Some(_) => false,
            },
            Some(result) => match testvalue {
                None => false,
                Some(testvalue) => P::equal(result, testvalue),
            },
        }
    }

    fn str(result: &Option<P>) -> String {
        match result {
            None => "".to_string(),
            Some(result) => P::str(result),
        }
    }

    fn val_str(value: &Self::TestValue) -> String {
        match value {
            None => "".to_string(),
            Some(value) => P::val_str(value),
        }
    }
}

pub struct TestRun<'s, O>
where
    O: TestResult<O>,
{
    pub trace: Tracer<'s>,
    pub span: Span<'s>,
    pub result: ParseResult<'s, O>,
    pub fail: RefCell<bool>,
}

impl<'s, O> TestRun<'s, O>
where
    O: TestResult<O>,
{
    /// Runs the parser and records the results.
    /// Use ok(), err(), ... to check specifics.
    ///
    /// Finish the test with q().
    #[must_use]
    pub fn parse(
        span: &'s str,
        fn_test: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, O>,
    ) -> Self {
        println!();

        let span = Span::new(span);
        let trace = Tracer::new();
        let result = fn_test(&trace, span);

        Self {
            trace,
            span,
            result,
            fail: RefCell::new(false),
        }
    }

    fn flag_fail(&self) {
        *self.fail.borrow_mut() = true;
    }

    /// Checks for ok.
    /// Takes a test-function to check the result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn okg<V>(&'s self, field: fn(&'s O) -> V, test: V) -> &Self
    where
        V: Display,
        V: PartialEq,
    {
        match &self.result {
            Ok((_, token)) => {
                let token_field = field(token);
                if token_field != test {
                    println!("FAIL: Value mismatch: {} <> {}", token_field, test);
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

    /// Checks for ok.
    /// Uses an extraction function to get the relevant result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn okd(&'s self, field: fn(&'s O, O::TestValue) -> bool, test: O::TestValue) -> &Self {
        match &self.result {
            Ok((_, token)) => {
                let test_str = O::val_str(&test);
                if !field(token, test) {
                    println!("FAIL: Value mismatch: {} <> {}", O::str(token), test_str);
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

    /// Checks for ok.
    /// Uses TestResult::equal to test the result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn ok(&self, test: O::TestValue) -> &Self {
        match &self.result {
            Ok((_, token)) => {
                if !O::equal(token, &test) {
                    println!(
                        "FAIL: Value mismatch: {} <> {}",
                        O::str(token),
                        O::val_str(&test)
                    );
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

    /// Checks for and error.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn err(&self, code: OFError) -> &Self {
        match &self.result {
            Ok((_, _)) => {
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
    pub fn expect(&self, suggest: Suggest) -> &Self {
        let vec = self.trace.expect_vec();
        if !vec.contains(&suggest) {
            println!("FAIL: {:?} is not an expected token. [{:?}]", suggest, vec);
            self.flag_fail();
        }

        self
    }

    /// Fails the test if any of the checks before failed.
    ///
    /// Panic
    ///
    /// Panics if any test failed.
    #[track_caller]
    pub fn q(&self) {
        // self.dump();
        if *self.fail.borrow() {
            self.dump();
            panic!()
        }
    }

    /// Dump the result.
    pub fn dump(&self) -> &Self {
        match &self.result {
            Ok((rest, token)) => {
                println!("{:?}", &self.trace);
                println!("=> '{}' | rest='{}'", O::str(token), rest);
            }
            Err(e) => {
                println!("{:?}", &self.trace);
                println!("=> {}", e);
            }
        }
        self
    }
}
