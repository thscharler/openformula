use crate::ast::{ParseResult, Span};
use crate::error::{Expect2, OFCode, ParseOFError, Suggest2};
use std::cell::RefCell;
use std::fmt::Debug;
use std::marker::PhantomData;

pub mod debug_tracer;

pub struct Tracer<'s> {
    /// Function call stack.
    func: RefCell<Vec<OFCode>>,

    /// Collected tracks.
    track: RefCell<Vec<Track<'s>>>,
    suggest: RefCell<Vec<SuggestTrack<'s>>>,
    expect: RefCell<Vec<ExpectTrack<'s>>>,
}

// public fn
impl<'s> Tracer<'s> {
    /// New one.
    pub fn new() -> Self {
        Self {
            func: RefCell::new(Vec::new()),
            track: RefCell::new(Vec::new()),
            suggest: RefCell::new(Vec::new()),
            expect: RefCell::new(Vec::new()),
        }
    }

    /// Enter a parser function. Absolutely necessary for the rest.
    pub fn enter(&self, func: OFCode, span: Span<'s>) {
        self.push_func(func);
        self.push_suggest(func);
        self.push_expect(func);

        self.track_enter(span);
    }

    /// Keep track of steps in a complicated parser.
    pub fn step(&self, step: &'static str, span: Span<'s>) {
        self.track_step(step, span);
    }

    /// Some detailed debug information.
    pub fn debug<T: Into<String>>(&self, step: T) {
        self.track_debug(step.into());
    }

    /// Adds a suggestion for the current stack frame.
    pub fn suggest(&self, suggest: OFCode, span: Span<'s>) {
        self.debug(format!("suggest {:?}", suggest));
        self.add_suggest(suggest, span);
    }

    /// Keep track of this error.
    pub fn stash(&self, err: ParseOFError<'s>) {
        self.add_expect(err.code, err.span);
        self.append_expect(err.expect2);
        self.append_suggest(err.suggest2);
    }

    /// Write a track for an ok result.
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        self.track_ok(rest, span);

        let expect = self.pop_expect();
        self.track_expect(Usage::Drop, expect.list);
        self.pop_suggest();

        self.track_exit();
        self.pop_func();

        Ok((rest, val))
    }

    /// Write a track for an error.
    pub fn err<'t, T>(&'t self, mut err: ParseOFError<'s>) -> ParseResult<'s, T> {
        // Freshly created error.
        if !err.tracing {
            err.tracing = true;
            self.add_expect(err.code, err.span);
        }

        // when backtracking we always replace the current error code.
        err.code = self.func();

        let mut exp = self.pop_expect();
        self.track_expect(Usage::Use, exp.list.clone());
        err.expect2.append(&mut exp.list);

        let mut sug = self.pop_suggest();
        err.suggest2.append(&mut sug.list);

        self.track_error(&err);

        self.track_exit();
        self.pop_func();

        Err(err)
    }
}

// expect
impl<'s> Tracer<'s> {
    fn push_expect(&self, func: OFCode) {
        self.expect.borrow_mut().push(ExpectTrack {
            func,
            usage: Usage::Track,
            list: Vec::new(),
        })
    }

    fn pop_expect(&self) -> ExpectTrack<'s> {
        self.expect.borrow_mut().pop().unwrap()
    }

    fn add_expect(&self, code: OFCode, span: Span<'s>) {
        self.expect
            .borrow_mut()
            .last_mut()
            .unwrap()
            .list
            .push(Expect2 {
                code,
                span,
                parents: self.parent_vec(),
            })
    }

    fn append_expect(&self, mut expect: Vec<Expect2<'s>>) {
        self.expect
            .borrow_mut()
            .last_mut()
            .unwrap()
            .list
            .append(&mut expect);
    }
}

// suggest
impl<'s> Tracer<'s> {
    fn push_suggest(&self, func: OFCode) {
        self.suggest.borrow_mut().push(SuggestTrack {
            func,
            usage: Usage::Track,
            list: Vec::new(),
        })
    }

    fn pop_suggest(&self) -> SuggestTrack<'s> {
        self.suggest.borrow_mut().pop().unwrap()
    }

    fn add_suggest(&self, code: OFCode, span: Span<'s>) {
        self.suggest
            .borrow_mut()
            .last_mut()
            .unwrap()
            .list
            .push(Suggest2 {
                code,
                span,
                parents: self.parent_vec(),
            })
    }

    fn append_suggest(&self, mut suggest: Vec<Suggest2<'s>>) {
        self.suggest
            .borrow_mut()
            .last_mut()
            .unwrap()
            .list
            .append(&mut suggest);
    }
}

// call frame tracking
impl<'s> Tracer<'s> {
    // enter function
    fn push_func(&self, func: OFCode) {
        self.func.borrow_mut().push(func);
    }

    // leave current function
    fn pop_func(&self) {
        self.func.borrow_mut().pop();
    }

    // current function
    fn func(&self) -> OFCode {
        *self.func.borrow().last().unwrap()
    }

    fn parent_vec(&self) -> Vec<OFCode> {
        let mut f_vec = Vec::new();
        for f in &*self.func.borrow() {
            f_vec.push(*f);
        }
        f_vec
    }
}

// basic tracking
impl<'s> Tracer<'s> {
    fn track_enter(&self, span: Span<'s>) {
        self.track.borrow_mut().push(Track::Enter(EnterTrack {
            func: self.func(),
            span,
            list: self.parent_vec(),
        }));
    }

    fn track_step(&self, step: &'static str, span: Span<'s>) {
        self.track.borrow_mut().push(Track::Step(StepTrack {
            func: self.func(),
            step,
            span,
            list: self.parent_vec(),
        }));
    }

    fn track_debug(&self, dbg: String) {
        self.track.borrow_mut().push(Track::Debug(DebugTrack {
            func: self.func(),
            dbg,
            list: self.parent_vec(),
            _phantom: Default::default(),
        }));
    }

    fn track_suggest(&self, usage: Usage, suggest: Vec<Suggest2<'s>>) {
        self.track.borrow_mut().push(Track::Suggest(SuggestTrack {
            func: self.func(),
            usage,
            list: suggest,
        }))
    }

    fn track_expect(&self, usage: Usage, expect: Vec<Expect2<'s>>) {
        self.track.borrow_mut().push(Track::Expect(ExpectTrack {
            func: self.func(),
            usage,
            list: expect,
        }))
    }

    fn track_ok(&self, rest: Span<'s>, span: Span<'s>) {
        self.track.borrow_mut().push(Track::Ok(OkTrack {
            func: self.func(),
            span,
            rest,
            list: self.parent_vec(),
        }));
    }

    fn track_error(&self, err: &ParseOFError<'s>) {
        self.track.borrow_mut().push(Track::Err(ErrTrack {
            func: self.func(),
            span: err.span,
            err: err.to_string(),
            list: self.parent_vec(),
        }));
    }

    fn track_exit(&self) {
        self.track.borrow_mut().push(Track::Exit(ExitTrack {
            func: self.func(),
            _phantom: Default::default(),
        }));
    }
}

// TrackParseResult ------------------------------------------------------

/// Helps with keeping tracks in the parsers.
///
/// This can be squeezed between the call to another parser and the ?-operator.
///
/// Makes sure the tracer can keep track of the complete parse call tree.
pub trait TrackParseResult<'s, 't, O> {
    /// Translates the error code and adds the standard expect value.
    /// Then tracks the error and marks the current function as finished.
    fn track(self, trace: &'t Tracer<'s>) -> Self;
}

impl<'s, 't, O> TrackParseResult<'s, 't, O> for ParseResult<'s, O> {
    fn track(self, trace: &'t Tracer<'s>) -> Self {
        match self {
            Ok(_) => self,
            Err(e) => trace.err(e),
        }
    }
}

// Track -----------------------------------------------------------------

#[derive(Debug)]
pub enum Usage {
    Track,
    Drop,
    Use,
}

/// One per stack frame.
pub struct ExpectTrack<'s> {
    pub func: OFCode,
    pub usage: Usage,
    pub list: Vec<Expect2<'s>>,
}

/// One per stack frame.
pub struct SuggestTrack<'s> {
    pub func: OFCode,
    pub usage: Usage,
    pub list: Vec<Suggest2<'s>>,
}

pub struct EnterTrack<'s> {
    pub func: OFCode,
    pub span: Span<'s>,
    pub list: Vec<OFCode>,
}

pub struct StepTrack<'s> {
    pub func: OFCode,
    pub step: &'static str,
    pub span: Span<'s>,
    pub list: Vec<OFCode>,
}

pub struct DebugTrack<'s> {
    pub func: OFCode,
    pub dbg: String,
    pub list: Vec<OFCode>,
    pub _phantom: PhantomData<Span<'s>>,
}

pub struct OkTrack<'s> {
    pub func: OFCode,
    pub span: Span<'s>,
    pub rest: Span<'s>,
    pub list: Vec<OFCode>,
}

pub struct ErrTrack<'s> {
    pub func: OFCode,
    pub span: Span<'s>,
    pub err: String,
    pub list: Vec<OFCode>,
}

pub struct ExitTrack<'s> {
    pub func: OFCode,
    pub _phantom: PhantomData<Span<'s>>,
}

/// One track of the parsing trace.
pub enum Track<'s> {
    Enter(EnterTrack<'s>),
    Step(StepTrack<'s>),
    Debug(DebugTrack<'s>),
    Expect(ExpectTrack<'s>),
    Suggest(SuggestTrack<'s>),
    Ok(OkTrack<'s>),
    Err(ErrTrack<'s>),
    Exit(ExitTrack<'s>),
}
