use std::cell::Cell;
use std::fmt::Debug;
use std::time::{Duration, Instant};

pub type TestedFn<I, O, E> = fn(I) -> Result<O, E>;
pub type CompareFn<O, V> = for<'a> fn(&'a O, V) -> bool;

pub struct TestN<P, I, O, E>
where
    P: Default,
    I: Debug,
    O: Debug,
    E: Debug,
{
    pub ext: P,
    pub span: I,

    pub result: Result<O, E>,
    pub duration: Duration,

    pub fail: Cell<bool>,
}

pub trait Report<P, I, O, E>
where
    P: Default,
    I: Debug,
    O: Debug,
    E: Debug,
{
    fn report(testn: &TestN<P, I, O, E>);
}

impl<P, I, O, E> TestN<P, I, O, E>
where
    P: Default,
    I: Clone + Debug,
    O: Debug,
    E: Debug,
{
    pub fn run<'a, T>(span: T, fn_test: TestedFn<I, O, E>) -> Self
    where
        T: Into<I>,
    {
        let span: I = span.into();

        let now = Instant::now();
        let result = fn_test(span.clone());
        let elapsed = now.elapsed();

        Self {
            ext: Default::default(),
            span,
            result,
            duration: elapsed,
            fail: Cell::new(false),
        }
    }

    /// Sets the failed flag.
    pub fn flag_fail(&self) {
        self.fail.set(true);
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
    pub fn okok(&self) -> &Self {
        match &self.result {
            Ok(_) => {}
            Err(_) => {
                println!("FAIL: Expected ok, but was an error.");
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
            Ok(_) => {
                println!("FAIL: Expected error, but was ok!");
                self.flag_fail();
            }
            Err(_) => {}
        }
        self
    }

    /// Fails the test if any of the checks before failed.
    ///
    /// Panic
    ///
    /// Panics if any test failed.
    #[track_caller]
    pub fn q<R: Report<P, I, O, E>>(&self) {
        R::report(self);
    }
}
