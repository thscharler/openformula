use openformula::ast::tracer::Tracer;
use openformula::ast::{ParseResult, Span};
use openformula::error::OFCode;
use std::cell::RefCell;
use std::fmt::Debug;

type ParserFn<'s, O> = fn(&'_ Tracer<'s>, Span<'s>) -> ParseResult<'s, O>;
type TestFn<'s, O, V> = fn(&'s O, &'_ V) -> bool;

pub struct TestRun<'s, O> {
    pub trace: Tracer<'s>,
    pub span: Span<'s>,
    pub result: ParseResult<'s, O>,
    pub fail: RefCell<bool>,
}

/// Transform a test-fn so that it can take Option values.
///
/// '''
/// fn sheetname<'s>(result: &'s OFSheetName<'s>, test: &'s str) -> bool {
///     result.name == test
/// }
///
/// optional!(opt_sheetname(OFSheetName<'s>, &'s str), sheetname);
/// '''
///
#[allow(unused_macros)]
macro_rules! optional {
    ($name:ident( $O:ty, $V:ty ), $testfn:ident) => {
        fn $name<'s>(result: &'s Option<$O>, test: &Option<$V>) -> bool {
            match result {
                None => match test {
                    None => true,
                    Some(_v) => false,
                },
                Some(o) => match test {
                    None => false,
                    Some(v) => {
                        if !$testfn(o, v) {
                            false
                        } else {
                            true
                        }
                    }
                },
            }
        }
    };
}

#[allow(unused_imports)]
pub(crate) use optional;

// Change the behaviour of q() globally.
enum RunState {
    CheckDump,
    CheckTrace,
    Dump,
    Trace,
}

impl<'s, O> TestRun<'s, O>
where
    O: Debug,
{
    const STATE: RunState = RunState::CheckTrace;

    /// Runs the parser and records the results.
    /// Use ok(), err(), ... to check specifics.
    ///
    /// Finish the test with q().
    #[must_use]
    pub fn parse(span: &'s str, fn_test: ParserFn<'s, O>) -> Self {
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

    /// Always fails.
    ///
    /// Finish the test with q().
    pub fn fail(&self) -> &Self {
        println!("FAIL: Unconditionally");
        self.flag_fail();
        self
    }

    /// Checks for ok.
    /// Any result that is not Err is ok.
    #[must_use]
    pub fn okok(&'s self) -> &Self {
        match &self.result {
            Ok(_) => {}
            Err(_) => {
                println!("FAIL: Expected ok, but was an error.");
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
    pub fn ok<V>(&'s self, eq: TestFn<'s, O, V>, test: &V) -> &Self
    where
        V: Debug + ?Sized,
        O: Debug,
    {
        match &self.result {
            Ok((_, token)) => {
                if !eq(token, &test) {
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

    /// Checks for any error.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn errerr(&self) -> &Self {
        match &self.result {
            Ok((_, _)) => {
                println!("FAIL: Expected error, but was ok!");
                self.flag_fail();
            }
            Err(_) => {}
        }
        self
    }

    /// Checks for an error.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn err(&self, code: OFCode) -> &Self {
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
    pub fn expect(&self, suggest: OFCode) -> &Self {
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
        match Self::STATE {
            RunState::CheckDump => {
                if *self.fail.borrow() {
                    self.dump();
                    panic!()
                }
            }
            RunState::CheckTrace => {
                if *self.fail.borrow() {
                    self.trace();
                    panic!()
                }
            }
            RunState::Dump => {
                self.dump();
            }
            RunState::Trace => {
                self.trace();
            }
        }
    }

    /// Dump the result.
    pub fn dump(&self) -> &Self {
        println!("when parsing '{}'", self.span);
        match &self.result {
            Ok((rest, token)) => {
                println!("=> token={:?} rest=\"{}\"", token, rest);
            }
            Err(e) => {
                println!("=> {}", e);
            }
        }
        self
    }

    /// Dump the result.
    pub fn trace(&self) -> &Self {
        println!("when parsing '{}'", self.span);
        match &self.result {
            Ok((rest, token)) => {
                println!("{:?}", &self.trace);
                println!("=> token={:?} rest=\"{}\"", token, rest);
            }
            Err(e) => {
                println!("{:?}", &self.trace);
                println!("=> {}", e);
            }
        }
        self
    }
}
