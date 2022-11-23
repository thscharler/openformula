//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::{ParseResult, Span};
use crate::error::{OFCode, ParseOFError};
use std::cell::RefCell;
use std::fmt;
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

/// Follows the parsing.
#[derive(Default)]
pub struct Tracer<'s> {
    /// Function call stack.
    pub func: RefCell<Vec<OFCode>>,
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'s>>>,

    /// Optional flag stack.
    pub optional: RefCell<Vec<OFCode>>,

    /// Expected
    pub expect: RefCell<Vec<Expect<'s>>>,

    /// Suggestions
    pub suggest: RefCell<Vec<Suggest>>,
}

// Functions and Tracks.
impl<'s> Tracer<'s> {
    /// New one.
    pub fn new() -> Self {
        Default::default()
    }

    fn push_func(&self, func: OFCode) {
        self.func.borrow_mut().push(func);
    }

    fn func(&self) -> OFCode {
        *self.func.borrow().last().unwrap()
    }

    fn pop_func(&self) {
        self.func.borrow_mut().pop();
    }

    fn track_enter(&self, span: Span<'s>) {
        self.tracks
            .borrow_mut()
            .push(Track::Enter(self.func(), span));
    }

    fn track_step(&self, step: &'static str, span: Span<'s>) {
        self.tracks
            .borrow_mut()
            .push(Track::Step(self.func(), step, span));
    }

    fn track_detail(&self, step: String) {
        self.tracks
            .borrow_mut()
            .push(Track::Detail(self.func(), step));
    }

    fn track_ok(&self, rest: Span<'s>, span: Span<'s>) {
        self.tracks
            .borrow_mut()
            .push(Track::Ok(self.func(), span, rest));
    }

    fn track_error(&self, err: &ParseOFError<'s>) {
        self.tracks.borrow_mut().push(Track::Error(
            self.func(),
            self.in_optional(),
            err.to_string(),
            err.span,
        ));
    }

    fn track_exit(&self) {
        self.tracks.borrow_mut().push(Track::Exit(self.func()));
    }
}

// Optional machinery.
impl<'s> Tracer<'s> {
    /// About to enter an optional part of the parser.
    ///
    /// All calls to optional are noted in a stack, so recursion
    /// works fine. The stack is cleaned up when a function with
    /// this name is exited via one of the exit functions.
    ///
    /// This can savely be called before and after enter of the function.
    pub fn optional(&self, func: OFCode) {
        self.optional.borrow_mut().push(func);
    }

    /// In an optional branch.
    fn in_optional(&self) -> bool {
        self.optional.borrow().last().is_some()
    }

    /// Looses one optional layer if the function matches.
    /// Called by each exit function.
    fn pop_optional(&self) {
        let mut optional = self.optional.borrow_mut();

        if let Some(code) = optional.last() {
            if *code == self.func() {
                optional.pop();
            }
        }
    }
}

// Suggest machinery
impl<'s> Tracer<'s> {
    /// Suggested tokens.
    ///
    /// Keeps a list of suggestions that can be used for user interaction.
    /// This list accumulates endlessly until clear_suggestions is called.
    pub fn suggest(&self, suggest: OFCode) {
        self.detail(format!("suggest {:?}", suggest));
        self.add_suggest(suggest);
    }

    /// Return the suggestions as a String.
    pub fn suggest_str(&self) -> String {
        let mut buf = String::new();
        for s in &*self.suggest.borrow() {
            let _ = write!(buf, "{:?}, ", s);
        }
        buf
    }

    fn push_suggest(&self, func: OFCode) {
        self.suggest.borrow_mut().push(Suggest {
            func,
            ..Default::default()
        });
    }

    fn add_suggest(&self, suggest: OFCode) {
        match self.suggest.borrow_mut().last_mut() {
            None => unreachable!(),
            Some(su) => {
                if let Some(&last) = su.codes.last() {
                    if last != suggest {
                        su.codes.push(suggest);
                    }
                } else {
                    su.codes.push(suggest);
                }
            }
        }
    }

    // Pops the suggestions for the current function.
    fn pop_suggest(&self) {
        let mut su_vec = self.suggest.borrow_mut();

        match su_vec.pop() {
            None => unreachable!(),
            Some(su) => {
                match su_vec.last_mut() {
                    None => su_vec.push(su), // was the last, keep
                    Some(last) => {
                        // add to last
                        if !su.codes.is_empty() {
                            last.next.push(su);
                        }
                    }
                }
            }
        }
    }
}

// Expect machinery
impl<'s> Tracer<'s> {
    /// Expected tokens.
    ///
    /// Collect information about mandatory expected tokens. There can
    /// be any number of these, if at least one of many options is required.
    pub fn expect(&self, code: OFCode, span: Span<'s>) {
        if self.in_optional() {
            self.suggest(code);
        } else {
            self.add_expect(code, span);
        }
    }

    pub fn is_expected(&self, code: OFCode) -> bool {
        for expect in &*self.expect.borrow() {
            if expect.code == code {
                return true;
            }
        }
        return false;
    }

    /// Expected tokens.
    ///
    /// Returns the list of expected tokens as a String.
    pub fn expect_str(&self) -> String {
        let mut buf = String::new();
        for s in &*self.expect.borrow() {
            let _ = write!(buf, "{:?} ", s);
        }
        buf
    }

    fn add_expect(&self, code: OFCode, span: Span<'s>) {
        // check the last one. if it's the same code in a different
        // function we can deduplicate.
        let duplicate = match self.expect.borrow().last() {
            None => false,
            Some(expect) => expect.code == code,
        };

        if !duplicate {
            self.expect.borrow_mut().push(Expect {
                func: self.func(),
                span,
                code,
            })
        }
    }
}

impl<'s> Tracer<'s> {
    /// Entering a parser.
    ///
    /// For the tracing to work, all exit-points of a function have to
    /// be accounted for.
    ///
    /// That means at each exit point you have to call either:
    /// * ok
    /// * parse
    /// * map_nom
    /// * nom
    /// * map_tok
    /// * tok  
    pub fn enter(&self, func: OFCode, span: Span<'s>) {
        self.push_func(func);
        self.push_suggest(func);

        self.track_enter(span);
    }

    /// Extra step.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn step(&self, step: &'static str, span: Span<'s>) {
        self.track_step(step, span);
    }

    /// Extra information.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn detail<T: Into<String>>(&self, step: T) {
        self.track_detail(step.into());
    }

    /// Ok in a parser.
    ///
    /// Returns the given value for ease of use.
    /// Records the success with function, parsed span and rest span.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        self.track_ok(rest, span);
        self.track_exit();

        self.pop_optional();
        self.pop_suggest();
        self.pop_func();

        Ok((rest, val))
    }

    /// Error in a parser.
    ///
    /// Keeps track of the error.
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn err<'t, T>(&'t self, mut err: ParseOFError<'s>) -> ParseResult<'s, T> {
        // if the error doesn't match the current function
        // we track the error and change the code.
        if err.code != self.func() {
            self.expect(err.code, err.span);
            self.track_error(&err);
            err.code = self.func();
        }

        self.expect(err.code, err.span);
        self.track_error(&err);
        self.track_exit();

        self.pop_optional();
        self.pop_suggest();
        self.pop_func();

        Err(err)
    }
}

impl<'s> Debug for Tracer<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "trace")?;
        let tracks = self.tracks.borrow();

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
                Track::Error(_, _, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                }
                Track::Exit(_) => {
                    indent.pop();
                    indent.pop();
                }
            }
        }

        write!(f, "    func=")?;
        for func in &*self.func.borrow() {
            write!(f, "{:?} ", func)?;
        }
        writeln!(f)?;

        write!(f, "    optional=")?;
        for optional in &*self.optional.borrow() {
            write!(f, "{:?} ", optional)?;
        }
        writeln!(f)?;

        write!(f, "    expect=")?;
        for expect in &*self.expect.borrow() {
            write!(f, "{:?} ", expect)?;
        }
        writeln!(f)?;

        write!(f, "    suggest={:?}", self.suggest.borrow())?;

        Ok(())
    }
}

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

pub struct Expect<'s> {
    pub func: OFCode,
    pub span: Span<'s>,
    pub code: OFCode,
}

impl<'s> Debug for Expect<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[{}: {} \"{}\"]", self.func, self.code, self.span)?;
        Ok(())
    }
}

#[derive(Default)]
pub struct Suggest {
    pub func: OFCode,
    pub codes: Vec<OFCode>,
    pub next: Vec<Suggest>,
}

// debug impl
impl Suggest {
    fn indent(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
        for _ in 0..indent * 4 {
            write!(f, " ")?;
        }
        Ok(())
    }

    pub fn arrow(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
        if indent > 0 {
            for _ in 0..((indent - 1) * 4) {
                write!(f, " ")?;
            }
            for _ in ((indent - 1) * 4)..(indent * 4) - 2 {
                write!(f, "-")?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }

    fn dbg_suggest(f: &mut Formatter<'_>, suggest: &Suggest, indent: u32) -> fmt::Result {
        write!(f, "{} : ", suggest.func)?;
        for code in &suggest.codes {
            write!(f, "{:?} ", code)?;
        }

        for su in &suggest.next {
            writeln!(f)?;
            Self::arrow(f, indent + 1)?;
            Self::dbg_suggest(f, su, indent + 1)?;
        }

        Ok(())
    }
}

impl Debug for Suggest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::dbg_suggest(f, self, 0)?;
        Ok(())
    }
}

/// One track of the parsing trace.
pub enum Track<'s> {
    /// Function where this occurred and the input span.
    Enter(OFCode, Span<'s>),
    /// Function with an extra step.
    Step(OFCode, &'static str, Span<'s>),
    /// Internal tracing.
    Detail(OFCode, String),
    /// Function where this occurred and the remaining span.
    Ok(OFCode, Span<'s>, Span<'s>),
    /// Function where this occurred and some error info.
    Error(OFCode, bool, String, Span<'s>),
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
            Track::Error(func, opt, err_str, err_span) => {
                write!(
                    f,
                    "{}: {} err=({}) span='{}'",
                    func,
                    if *opt { "OPT" } else { "!!!" },
                    err_str,
                    err_span
                )?;
            }
            Track::Exit(func) => {
                write!(f, "return {}: ", func)?;
            }
        }
        Ok(())
    }
}
