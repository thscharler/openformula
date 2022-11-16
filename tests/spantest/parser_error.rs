use openformula::ast::tracer::{Suggest, Tracer};
use openformula::ast::{ParseResult, Span};
use openformula::error::OFError;
use std::cell::RefCell;
use std::fmt::Debug;

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
    pub fn parse(
        span: &'s str,
        fn_test: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, O>,
    ) -> Self {
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

    pub fn ok(&self, test: O::TestValue) -> &Self {
        match &self.result {
            Ok((rest, token)) => {
                if !O::equal(token, &test) {
                    println!("{:?}", &self.trace);
                    println!("=> '{}' | rest='{}'", O::str(token), rest);
                    println!(
                        "FAIL: Value mismatch: {} <> {}",
                        O::str(token),
                        O::val_str(&test)
                    );
                    *self.fail.borrow_mut() = true;
                }
            }
            Err(e) => {
                println!("{:?}", &self.trace);
                println!("=> {}", e);
                println!("FAIL: Expect ok, but was an error!");
                *self.fail.borrow_mut() = true;
            }
        }
        self
    }

    pub fn rest(&self, test: &str) -> &Self {
        match &self.result {
            Ok((rest, token)) => {
                if **rest != test {
                    println!("{:?}", &self.trace);
                    println!("=> '{}' | rest='{}'", O::str(token), rest);
                    println!("FAIL: Rest mismatch {} <> {}", **rest, test);
                    *self.fail.borrow_mut() = true;
                }
            }
            Err(e) => {
                println!("{:?}", &self.trace);
                println!("=> {}", e);
                println!("FAIL: Expect ok, but was an error!");
                *self.fail.borrow_mut() = true;
            }
        }
        self
    }

    pub fn err(&self, code: OFError) -> &Self {
        match &self.result {
            Ok((rest, token)) => {
                println!("{:?}", &self.trace);
                println!("=> '{}' | rest='{}'", O::str(token), rest);
                println!("FAIL: Expected error, but was ok!");
                *self.fail.borrow_mut() = true;
            }
            Err(e) => {
                if e.code != code {
                    println!("{:?}", &self.trace);
                    println!("=> {}", e);
                    println!("FAIL: {:?} <> {:?}", e.code, code);
                    *self.fail.borrow_mut() = true;
                }
            }
        }
        self
    }

    pub fn expect(&self, suggest: Suggest) -> &Self {
        let vec = self.trace.expect_vec();
        if !vec.contains(&suggest) {
            println!("FAIL: {:?} is not an expected token. [{:?}]", suggest, vec);
            *self.fail.borrow_mut() = true;
        }

        self
    }

    #[track_caller]
    pub fn q(&self) {
        if *self.fail.borrow() {
            panic!()
        }
    }

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

pub fn run_test2<'s, O>(
    str: &'s str,
    testfn: for<'t> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, O>,
) where
    O: Debug,
{
    let tracer = Tracer::new();
    {
        println!("### parse '{}'", str);
        match testfn(&tracer, Span::new(str)) {
            Ok((rest, tok)) => {
                println!("{:?}", &tracer);
                println!("=> {:?} | rest='{}'", tok, rest);
            }
            Err(e) => {
                println!("{:?}", &tracer);
                println!("=> {}", e);
                println!(
                    "   Found '{}' expected [{}] suggest [{}]",
                    e.span(),
                    tracer.expect_str(),
                    tracer.suggest_str()
                );
            }
        }
    }
}
