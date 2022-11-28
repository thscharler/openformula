use crate::spantest::{Report, TestN};
use openformula::error::OFCode;
use openformula::iparse::error::{DebugWidth, ParserError};
use openformula::iparse::tracer::{CTracer, Track};
use openformula::iparse::{ParseResult, Span, Tracer};
use std::cell::{Cell, RefCell};
use std::fmt::{Debug, Formatter};
use std::time::Instant;

pub type ParserFn<'s, O> = fn(&'_ CTracer<'s, OFCode>, Span<'s>) -> ParseResult<'s, O, OFCode>;

pub struct TestTracer<'s> {
    pub trace: CTracer<'s, OFCode>,
    pub trace_filter: RefCell<&'s dyn Fn(&Track<'_, OFCode>) -> bool>,
}

impl<'s> Default for TestTracer<'s> {
    fn default() -> Self {
        Self {
            trace: CTracer::new(),
            trace_filter: RefCell::new(&|_| true),
        }
    }
}

impl<'s, O> TestN<TestTracer<'s>, Span<'s>, (Span<'s>, O), ParserError<'s, OFCode>>
where
    O: Debug,
{
    /// Runs the parser and records the results.
    /// Use ok(), err(), ... to check specifics.
    ///
    /// Finish the test with q().
    #[must_use]
    pub fn parse(span: &'s str, fn_test: ParserFn<'s, O>) -> Self {
        let span = Span::new(span);
        let trace = CTracer::new();

        let now = Instant::now();
        let result = fn_test(&trace, span);
        let elapsed = now.elapsed();

        Self {
            ext: TestTracer {
                trace,
                trace_filter: RefCell::new(&|_| true),
            },
            span,
            result,
            duration: elapsed,
            fail: Cell::new(false),
        }
    }

    /// Sets a filter on the trace.
    #[must_use]
    pub fn filter(&self, filter: &'s dyn Fn(&Track<'_, OFCode>) -> bool) -> &Self {
        self.ext.trace_filter.replace(filter);
        self
    }
}

pub struct Trace;

impl<'s, O, E> Report<TestTracer<'s>, Span<'s>, (Span<'s>, O), E> for Trace
where
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<TestTracer<'s>, Span<'s>, (Span<'s>, O), E>) {
        struct TracerDebug<'a, 'b, 's> {
            trace: &'a CTracer<'s, OFCode>,
            track_filter: &'b dyn Fn(&Track<'s, OFCode>) -> bool,
        }

        impl<'a, 'b, 's> Debug for TracerDebug<'a, 'b, 's> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.trace
                    .write_debug(f, DebugWidth::Medium, self.track_filter)
            }
        }

        println!();
        println!(
            "when parsing '{}' in {}ns =>",
            testn.span,
            testn.duration.as_nanos()
        );

        let trace = &testn.ext.trace;
        let track_filter_r = testn.ext.trace_filter.borrow();
        let track_filter = &*track_filter_r;

        println!(
            "{:?}",
            TracerDebug {
                trace,
                track_filter
            }
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

pub struct CheckTrace;

impl<'s, O, E> Report<TestTracer<'s>, Span<'s>, (Span<'s>, O), E> for CheckTrace
where
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<TestTracer<'s>, Span<'s>, (Span<'s>, O), E>) {
        if testn.fail.get() {
            Trace::report(testn);
            panic!()
        }
    }
}
