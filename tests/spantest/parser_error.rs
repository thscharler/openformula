use openformula::ast::tracer::{Suggest, Tracer};
use openformula::ast::{ParseResult, Span};
use openformula::error::OFError;
use std::cell::RefCell;
use std::fmt::{Debug, Display};

pub trait TestEq<Rhs = Self> {
    fn eq(&self, other: &Rhs) -> bool;
}

pub struct TestRun<'s, O> {
    pub trace: Tracer<'s>,
    pub span: Span<'s>,
    pub result: ParseResult<'s, O>,
    pub fail: RefCell<bool>,
}

impl<'s, O> TestRun<'s, Option<O>>
where
    O: Debug,
{
    /// Checks for ok.
    /// Uses TestResult::equal to test the result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn okopt<V>(&self, test: Option<V>) -> &Self
    where
        V: Display,
        for<'x> &'x O: TestEq<V>,
    {
        match &self.result {
            Ok((_, token)) => {
                match token {
                    None => match test {
                        None => {}
                        Some(test) => {
                            println!("FAIL: Value mismatch: None <> {}", test.to_string());
                            self.flag_fail();
                        }
                    },
                    Some(token) => match test {
                        None => {
                            println!("FAIL: Value mismatch: {:?} <> None", token);
                            self.flag_fail();
                        }
                        Some(test) => {
                            if !token.eq(&test) {
                                println!(
                                    "FAIL: Value mismatch: {:?} <> {}",
                                    token,
                                    test.to_string()
                                );
                                self.flag_fail();
                            }
                        }
                    },
                };
            }
            Err(_) => {
                println!("FAIL: Expect ok, but was an error!");
                self.flag_fail();
            }
        }
        self
    }
}

impl<'s, O> TestRun<'s, O>
where
    O: Debug,
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
    /// Uses an extraction function to get the relevant result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn okeq<V>(&'s self, eq: fn(&'s O, &V) -> bool, test: &V) -> &Self
    where
        V: Display + ?Sized,
        O: Display,
    {
        match &self.result {
            Ok((_, token)) => {
                if !eq(token, &test) {
                    println!("FAIL: Value mismatch: {} <> {}", token, test.to_string());
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
    pub fn ok<V>(&self, test: V) -> &Self
    where
        V: Display,
        for<'x> &'x O: TestEq<V>,
    {
        match &self.result {
            Ok((_, token)) => {
                if !token.eq(&test) {
                    println!("FAIL: Value mismatch: {:?} <> {}", token, test.to_string());
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
                println!("=> '{:?}' | rest='{}'", token, rest);
            }
            Err(e) => {
                println!("{:?}", &self.trace);
                println!("=> {}", e);
            }
        }
        self
    }
}
