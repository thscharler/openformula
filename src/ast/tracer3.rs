//!
//! Traces the parser and helps generating errors and suggestions.
//!

use crate::ast::tracer3::debug_tracer::debug_tracer;
use crate::ast::{ParseResult, Span};
use crate::error::{DebugWidth, Expect, OFCode, ParseOFError, Suggest};
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;

mod debug_tracer;

/// Tracing and error collection.
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
        self.debug(format!("suggest {}:\"{}\" ...", suggest, span));
        self.add_suggest(suggest, span);
    }

    /// Keep track of this error.
    pub fn stash(&self, err: ParseOFError<'s>) {
        self.debug(format!("expect {}:\"{}\" ...", err.code, err.span));
        self.add_expect(err.code, err.span);
        self.append_expect(err.expect);
        self.append_suggest(err.suggest);
    }

    /// Write a track for an ok result.
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        self.track_ok(rest, span);

        let expect = self.pop_expect();
        self.track_expect(Usage::Drop, expect.list);
        let suggest = self.pop_suggest();
        // Keep suggests, sort them out later.
        // Drop at the toplevel if no error occurs?
        if !self.suggest.borrow().is_empty() {
            self.append_suggest(suggest.list);
            //self.track_suggest(Usage::Drop, suggest.list);
        }

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
        err.expect.append(&mut exp.list);

        let mut sug = self.pop_suggest();
        self.track_suggest(Usage::Use, sug.list.clone());
        err.suggest.append(&mut sug.list);

        self.track_error(&err);

        self.track_exit();
        self.pop_func();

        Err(err)
    }

    /// Write a debug output of the Tracer state.
    pub fn write_debug(
        &self,
        f: &mut Formatter<'_>,
        w: DebugWidth,
        filter: &dyn Fn(&Track<'_>) -> bool,
    ) -> fmt::Result {
        debug_tracer(f, w, self, filter)
    }
}

// expect
impl<'s> Tracer<'s> {
    fn push_expect(&self, func: OFCode) {
        self.expect.borrow_mut().push(ExpectTrack {
            func,
            usage: Usage::Track,
            list: Vec::new(),
            parents: self.parent_vec(),
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
            .push(Expect {
                code,
                span,
                parents: self.parent_vec(),
            })
    }

    fn append_expect(&self, mut expect: Vec<Expect<'s>>) {
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
            parents: self.parent_vec(),
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
            .push(Suggest {
                code,
                span,
                parents: self.parent_vec(),
            })
    }

    fn append_suggest(&self, mut suggest: Vec<Suggest<'s>>) {
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
            parents: self.parent_vec(),
        }));
    }

    fn track_step(&self, step: &'static str, span: Span<'s>) {
        self.track.borrow_mut().push(Track::Step(StepTrack {
            func: self.func(),
            step,
            span,
            parents: self.parent_vec(),
        }));
    }

    fn track_debug(&self, dbg: String) {
        self.track.borrow_mut().push(Track::Debug(DebugTrack {
            func: self.func(),
            dbg,
            parents: self.parent_vec(),
            _phantom: Default::default(),
        }));
    }

    fn track_suggest(&self, usage: Usage, suggest: Vec<Suggest<'s>>) {
        if !suggest.is_empty() {
            self.track.borrow_mut().push(Track::Suggest(SuggestTrack {
                func: self.func(),
                usage,
                list: suggest,
                parents: self.parent_vec(),
            }));
        }
    }

    fn track_expect(&self, usage: Usage, expect: Vec<Expect<'s>>) {
        if !expect.is_empty() {
            self.track.borrow_mut().push(Track::Expect(ExpectTrack {
                func: self.func(),
                usage,
                list: expect,
                parents: self.parent_vec(),
            }));
        }
    }

    fn track_ok(&self, rest: Span<'s>, span: Span<'s>) {
        self.track.borrow_mut().push(Track::Ok(OkTrack {
            func: self.func(),
            span,
            rest,
            parents: self.parent_vec(),
        }));
    }

    fn track_error(&self, err: &ParseOFError<'s>) {
        self.track.borrow_mut().push(Track::Err(ErrTrack {
            func: self.func(),
            span: err.span,
            err: err.to_string(),
            parents: self.parent_vec(),
        }));
    }

    fn track_exit(&self) {
        self.track.borrow_mut().push(Track::Exit(ExitTrack {
            func: self.func(),
            parents: self.parent_vec(),
            _phantom: Default::default(),
        }));
    }
}

impl<'s> Default for Tracer<'s> {
    fn default() -> Self {
        Self::new()
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

/// Hint at how the ExpectTrack and SuggestTrack were used.
#[derive(Debug)]
pub enum Usage {
    /// Newly created, currently in use.
    Track,
    /// Forgotten.
    Drop,
    /// Move to a ParseOFError.
    Use,
}

impl Display for Usage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Usage::Track => write!(f, "track"),
            Usage::Drop => write!(f, "drop"),
            Usage::Use => write!(f, "use"),
        }
    }
}

/// One per stack frame.
pub struct ExpectTrack<'s> {
    /// Function.
    pub func: OFCode,
    /// Usage flag.
    pub usage: Usage,
    /// Collected Expect values.
    pub list: Vec<Expect<'s>>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// One per stack frame.
pub struct SuggestTrack<'s> {
    /// Function
    pub func: OFCode,
    /// Usage flag.
    pub usage: Usage,
    /// Collected Suggest values.
    pub list: Vec<Suggest<'s>>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Track for entering a parser function.
pub struct EnterTrack<'s> {
    /// Function
    pub func: OFCode,
    /// Span
    pub span: Span<'s>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Track for step information.
pub struct StepTrack<'s> {
    /// Function
    pub func: OFCode,
    /// Step info.
    pub step: &'static str,
    /// Span
    pub span: Span<'s>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Track for debug information.
pub struct DebugTrack<'s> {
    /// Function.
    pub func: OFCode,
    /// Debug info.
    pub dbg: String,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
    /// For the lifetime ...
    pub _phantom: PhantomData<Span<'s>>,
}

/// Track for ok results.
pub struct OkTrack<'s> {
    /// Function.
    pub func: OFCode,
    /// Span.
    pub span: Span<'s>,
    /// Remaining span.
    pub rest: Span<'s>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Track for err results.
pub struct ErrTrack<'s> {
    /// Function.
    pub func: OFCode,
    /// Span.
    pub span: Span<'s>,
    /// Error message.
    pub err: String, // TODO: check
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Track for exiting a parser function.
pub struct ExitTrack<'s> {
    /// Function
    pub func: OFCode,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
    /// For the lifetime ...
    pub _phantom: PhantomData<Span<'s>>,
}

/// Helper for Track. Not actively used, but can be helpful in filters.
#[derive(PartialEq, Eq)]
#[allow(missing_docs)]
pub enum TrackType {
    Enter,
    Step,
    Debug,
    Expect,
    Suggest,
    Ok,
    Err,
    Exit,
}

/// One track of the parsing trace.
#[allow(missing_docs)]
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

impl<'s> PartialEq<TrackType> for &Track<'s> {
    fn eq(&self, other: &TrackType) -> bool {
        match self {
            Track::Enter(_) => *other == TrackType::Enter,
            Track::Step(_) => *other == TrackType::Step,
            Track::Debug(_) => *other == TrackType::Debug,
            Track::Expect(_) => *other == TrackType::Expect,
            Track::Suggest(_) => *other == TrackType::Suggest,
            Track::Ok(_) => *other == TrackType::Ok,
            Track::Err(_) => *other == TrackType::Err,
            Track::Exit(_) => *other == TrackType::Exit,
        }
    }
}

impl<'s> Track<'s> {
    /// Returns the func value for each branch.
    pub fn func(&self) -> OFCode {
        match self {
            Track::Enter(v) => v.func,
            Track::Step(v) => v.func,
            Track::Debug(v) => v.func,
            Track::Expect(v) => v.func,
            Track::Suggest(v) => v.func,
            Track::Ok(v) => v.func,
            Track::Err(v) => v.func,
            Track::Exit(v) => v.func,
        }
    }

    /// Returns the parser call stack for each branch.
    pub fn parents(&self) -> &Vec<OFCode> {
        match self {
            Track::Enter(v) => &v.parents,
            Track::Step(v) => &v.parents,
            Track::Debug(v) => &v.parents,
            Track::Expect(v) => &v.parents,
            Track::Suggest(v) => &v.parents,
            Track::Ok(v) => &v.parents,
            Track::Err(v) => &v.parents,
            Track::Exit(v) => &v.parents,
        }
    }
}
