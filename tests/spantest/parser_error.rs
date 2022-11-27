use openformula::ast::tracer::Tracer;
use openformula::ast::tracer3::Track;
use openformula::ast::{ParseResult, Span};
use openformula::error::{DebugWidth, OFCode};
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};

type ParserFn<'s, O> = fn(&'_ Tracer<'s>, Span<'s>) -> ParseResult<'s, O>;
type TestFn<'s, O, V> = fn(&'s O, V) -> bool;

pub struct TestRun<'s, O> {
    pub trace: Tracer<'s>,
    pub span: Span<'s>,
    pub result: ParseResult<'s, O>,
    pub filter: RefCell<&'s dyn Fn(&Track<'_>) -> bool>,
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
        fn $name<'s>(result: &'s Option<$O>, test: Option<$V>) -> bool {
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
    const STATE: RunState = RunState::Dump;

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
            filter: RefCell::new(&|_| true),
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
    pub fn ok<V>(&'s self, eq: TestFn<'s, O, V>, test: V) -> &Self
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

    ///
    #[must_use]
    pub fn filter(&self, filter: &'s dyn Fn(&Track<'_>) -> bool) -> &Self {
        self.filter.replace(filter);
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
    pub fn expect(&self, code: OFCode) -> &Self {
        match &self.result {
            Ok(_) => {
                println!("FAIL: {:?} was ok not an error.", code,);
                self.flag_fail();
            }
            Err(e) => {
                if !e.is_expected(code) {
                    println!(
                        "FAIL: {:?} is not an expected token. [{}]",
                        code,
                        e.expect_str()
                    );
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
                    println!(
                        "FAIL: {:?} is not an expected token. [{}]",
                        code,
                        e.expect_str()
                    );
                    self.flag_fail();
                }
            }
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
        println!();
        println!("when parsing '{}' =>", self.span);
        match &self.result {
            Ok((rest, token)) => {
                println!("rest {}:\"{}\"", rest.location_offset(), rest);
                println!("{:0?}", token);
            }
            Err(e) => {
                println!("error");
                println!("{:1?}", e);
            }
        }
        self
    }

    /// Dump the result.
    pub fn trace(&self) -> &Self {
        struct Q<'s, 'x, O>(&'x TestRun<'s, O>);

        impl<'s, 'x, O> Display for Q<'s, 'x, O> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.0
                    .trace
                    .write_debug(f, DebugWidth::Medium, &*self.0.filter.borrow())
            }
        }

        println!();
        println!("when parsing '{}' =>", self.span);
        match &self.result {
            Ok((rest, token)) => {
                println!("{}", Q(self));
                println!("rest {}:\"{}\"", rest.location_offset(), rest);
                println!("{:0?}", token);
            }
            Err(e) => {
                println!("{}", Q(self));
                println!("error");
                println!("{:1?}", e);
            }
        }
        self
    }
}
