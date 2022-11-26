use crate::ast::{ParseResult, Span};
use crate::error::{Expect2, OFCode, ParseOFError, Suggest2};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};

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
    pub fn detail<T: Into<String>>(&self, step: T) {
        self.track_detail(step.into());
    }

    /// Adds a suggestion for the current stack frame.
    pub fn suggest(&self, suggest: OFCode, span: Span<'s>) {
        self.detail(format!("suggest {:?}", suggest));
        self.add_suggest(suggest, span);
    }

    /// Write a track for an ok result.
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        self.track_ok(rest, span);
        self.track_exit();

        self.pop_expect();
        self.pop_suggest();
        self.pop_func();

        Ok((rest, val))
    }

    /// Keep track of this error.
    pub fn stash(&self, err: ParseOFError<'s>) {
        self.add_expect(err.code, err.span);
        self.append_expect(err.expect2);
        self.append_suggest(err.suggest2);
    }

    /// Write a track for any error that originates from a non-traced function.
    pub fn external(&self, err: ParseOFError<'s>) {
        self.add_expect(err.code, err.span);
        // todo: append expect + suggest
        self.track_error(&err);
    }

    /// Write a track for an error.
    pub fn err<'t, T>(&'t self, mut err: ParseOFError<'s>) -> ParseResult<'s, T> {
        // when backtracking we always replace the current error code.
        err.code = self.func();

        let mut exp = self.pop_expect();
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
        self.track
            .borrow_mut()
            .push(Track::Enter(self.func(), span));
    }

    fn track_step(&self, step: &'static str, span: Span<'s>) {
        self.track
            .borrow_mut()
            .push(Track::Step(self.func(), step, span));
    }

    fn track_detail(&self, step: String) {
        self.track
            .borrow_mut()
            .push(Track::Detail(self.func(), step));
    }

    fn track_ok(&self, rest: Span<'s>, span: Span<'s>) {
        self.track
            .borrow_mut()
            .push(Track::Ok(self.func(), span, rest));
    }

    fn track_error(&self, err: &ParseOFError<'s>) {
        self.track
            .borrow_mut()
            .push(Track::Error(self.func(), err.to_string(), err.span));
    }

    fn track_exit(&self) {
        self.track.borrow_mut().push(Track::Exit(self.func()));
    }
}

impl<'s> Debug for Tracer<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "trace")?;
        let tracks = self.track.borrow();

        let mut indent = String::new();
        for tr in tracks.iter() {
            match tr {
                Track::Enter(_, _) => {
                    indent.push_str("  ");
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Step(_, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Detail(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Ok(_, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Error(_, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Exit(_) => {
                    indent.pop();
                    indent.pop();
                }
            }
        }

        if !self.func.borrow().is_empty() {
            write!(f, "    func=")?;
            for func in &*self.func.borrow() {
                write!(f, "{:?} ", func)?;
            }
            writeln!(f)?;
        }

        if !self.expect.borrow().is_empty() {
            write!(f, "    expect=")?;
            for exp in &*self.expect.borrow() {
                writeln!(f, "{:?} ", exp)?;
            }
            writeln!(f)?;
        }

        if !self.suggest.borrow().is_empty() {
            write!(f, "    suggest=")?;
            for sug in &*self.suggest.borrow() {
                writeln!(f, "{:?} ", sug)?;
            }
            writeln!(f)?;
        }

        Ok(())
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

// ExpectTrack -----------------------------------------------------------

/// One per stack frame.
struct ExpectTrack<'s> {
    pub func: OFCode,
    pub list: Vec<Expect2<'s>>,
}

impl<'s> Debug for ExpectTrack<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.func, self.list)?;
        Ok(())
    }
}

// SuggestTrack ----------------------------------------------------------

/// One per stack frame.
struct SuggestTrack<'s> {
    pub func: OFCode,
    pub list: Vec<Suggest2<'s>>,
}

impl<'s> Debug for SuggestTrack<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.func, self.list)?;
        Ok(())
    }
}

// Track -----------------------------------------------------------------

/// One track of the parsing trace.
enum Track<'s> {
    /// Function where this occurred and the input span.
    Enter(OFCode, Span<'s>),
    /// Function with an extra step.
    Step(OFCode, &'static str, Span<'s>),
    /// Internal tracing.
    Detail(OFCode, String),
    /// Function where this occurred and the remaining span.
    Ok(OFCode, Span<'s>, Span<'s>),
    /// Function where this occurred and some error info.
    Error(OFCode, String, Span<'s>),
    /// Exit the function
    Exit(OFCode),
}

impl<'s> Debug for Track<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Track::Enter(func, input) => {
                write!(f, "{}: '{}'", func, input)?;
            }
            Track::Step(func, step, input) => {
                write!(f, "{}: {} '{}'", func, step, input)?;
            }
            Track::Detail(func, detail) => {
                write!(f, "{}: {}", func, detail)?;
            }
            Track::Ok(func, val, rest) => {
                if !val.is_empty() {
                    write!(f, "{}: -> [ {}, '{}' ]", func, val, rest)?;
                } else {
                    write!(f, "{}: -> no match", func)?;
                }
            }
            Track::Error(func, err_str, _err_span) => {
                write!(f, "{}: err={} ", func, err_str)?;
            }
            Track::Exit(func) => {
                write!(f, "return {}: ", func)?;
            }
        }
        Ok(())
    }
}
