//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::{ParseResult, Span};
use crate::error::{debug_error, Expect, OFCode, ParseOFError, Suggest};
use std::cell::RefCell;
use std::fmt::Write;
use std::fmt::{Debug, Formatter};
use std::mem;

/// Follows the parsing.
#[derive(Default)]
pub struct Tracer<'s> {
    /// Function call stack.
    pub func: RefCell<Vec<OFCode>>,
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'s>>>,

    /// Expected
    pub expect: RefCell<Option<Expect<'s>>>,
    pub stashed: RefCell<Vec<Stashed<'s>>>,

    /// Suggestions
    pub suggest: RefCell<Vec<Suggest<'s>>>,
}

pub struct Stashed<'s> {
    pub func: OFCode,
    pub stashed: Vec<Expect<'s>>,
}

impl<'s> Debug for Stashed<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for st in &self.stashed {
            debug_error::debug_expect_multi(f, st, 0)?;
        }
        Ok(())
    }
}

// Functions and Tracks.
impl<'s> Tracer<'s> {
    // enter function
    fn push_func(&self, func: OFCode) {
        self.func.borrow_mut().push(func);
    }

    // current function
    fn func(&self) -> OFCode {
        *self.func.borrow().last().unwrap()
    }

    // parent function
    fn par_func(&self) -> Option<OFCode> {
        let func_vec = &*self.func.borrow();
        if func_vec.len() > 1 {
            func_vec.get(func_vec.len() - 2)
        } else {
            None
        }
        .map(|v| *v)
    }

    // leave current function
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
        self.tracks
            .borrow_mut()
            .push(Track::Error(self.func(), err.to_string(), err.span));
    }

    fn track_exit(&self) {
        self.tracks.borrow_mut().push(Track::Exit(self.func()));
    }
}

// Suggest machinery
impl<'s> Tracer<'s> {
    /// Suggested tokens.
    ///
    /// Keeps a list of suggestions that can be used for user interaction.
    /// This list accumulates endlessly until clear_suggestions is called.
    pub fn suggest(&self, suggest: OFCode, span: Span<'s>) {
        self.detail(format!("suggest {:?}", suggest));
        self.add_suggest(suggest, span);
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
            codes: Vec::new(),
            next: Vec::new(),
        });
    }

    // Adds a suggestion for the current function.
    fn add_suggest(&self, code: OFCode, span: Span<'s>) {
        match self.suggest.borrow_mut().last_mut() {
            None => unreachable!(),
            Some(su) => {
                if !su.same_as_last(code) {
                    su.codes.push((code, span));
                }
            }
        }
    }

    // does what it says.
    fn clone_suggest<'t>(&'t self) -> Suggest<'s> {
        self.suggest.borrow().last().unwrap().clone()
    }

    // pops the suggestions for the current function.
    fn pop_suggest(&self) {
        let mut su_vec = self.suggest.borrow_mut();

        match su_vec.pop() {
            None => unreachable!(),
            Some(su) => {
                match su_vec.last_mut() {
                    None => su_vec.push(su), // was the last, keep
                    Some(last) => {
                        // add to last
                        if !su.codes.is_empty() || !su.next.is_empty() {
                            last.next.push(su);
                        }
                    }
                }
            }
        }
    }
}

#[derive(PartialEq)]
enum ExpectDuplicate {
    New,
    Duplicate,
}

// Expect machinery
impl<'s> Tracer<'s> {
    // Each ParseOFError stands for a failed expectation.
    //
    // We keep track of it through all of the call hierarchy.
    // The point of origin gives the main code, everything up in the call hierarchy
    // are parent hints, every error below that has been marked as potentially interesting
    // and has been stashed away stands for some alternative that could have been met.
    fn expect(&self, err: &mut ParseOFError<'s>, stashed: Option<Stashed<'s>>) -> ExpectDuplicate {
        if let Some(exp) = &mut err.expect {
            // error in the middle of back tracing.

            // add stashed as alternatives
            if let Some(mut stashed) = stashed {
                // add stashed expects as alternatives.
                exp.alt.append(&mut stashed.stashed.clone());

                // same in the tracer.
                if let Some(tr_exp) = &mut *self.expect.borrow_mut() {
                    tr_exp.alt.append(&mut stashed.stashed);
                } else {
                    // should be in sync!?
                    unreachable!();
                }
            }
            // todo: tracer += expect

            // the same error is tracked twice in the caller and the callee.
            // these can be filtered out immediately.
            if !exp.same_as_last_par(err.code) {
                // there is already the original code.
                // all new codes come from higher in the call stack
                // and are added as parent codes.
                exp.add_par(Expect::new(self.func(), err.code, err.span));

                // same in the tracer.
                if let Some(tr_exp) = &mut *self.expect.borrow_mut() {
                    tr_exp.add_par(Expect::new(self.func(), err.code, err.span));
                } else {
                    // should be in sync!?
                    unreachable!();
                }

                ExpectDuplicate::New
            } else {
                ExpectDuplicate::Duplicate
            }
        } else {
            // new error.

            // new original code.
            let mut exp = Expect::new(self.func(), err.code, err.span);
            // add stashed as alternatives
            if let Some(stashed) = &stashed {
                let mut clone = stashed.stashed.clone();
                exp.alt.append(&mut clone);
            }
            err.expect = Some(exp);

            // tracer takes the same, but we reuse instead of cloning.
            let mut trexp = Expect::new(self.func(), err.code, err.span);
            if let Some(mut stashed) = stashed {
                trexp.alt.append(&mut stashed.stashed);
            }
            *self.expect.borrow_mut() = Some(trexp);

            ExpectDuplicate::New
        }
    }

    // returns the stash for the current function, if any.
    fn pop_stash(&'_ self) -> Option<Stashed<'s>> {
        let stash_stack = &mut *self.stashed.borrow_mut();
        let stash = match stash_stack.last() {
            None => None,
            Some(alt) if alt.func == self.func() => stash_stack.pop(),
            Some(_) => None,
        };
        stash
    }

    /// Stashes away all potential errors that may reoccur later als alternative explanations.
    /// Used for sequences of alternatives that are checked. The error for each alternative
    /// is stashed away and used again if none of the alternatives matches.
    pub fn stash_expect(&self) {
        let exp = mem::replace(&mut *self.expect.borrow_mut(), None);

        // use one dummy Expect with the current function code to collect
        // potential alternatives. use a stack of those and cleanup on ok() or err().
        if let Some(exp) = exp {
            let mut stash_stack = self.stashed.borrow_mut();

            match stash_stack.last_mut() {
                None => {
                    let mut stash = Stashed {
                        func: self.func(),
                        stashed: Vec::new(),
                    };
                    stash.stashed.push(exp);
                    stash_stack.push(stash);
                }
                Some(stash) if stash.func == self.func() => {
                    stash.stashed.push(exp);
                }
                Some(_) => {}
            };
        }
    }

    /// Is this one of the expected error codes?
    pub fn is_expected(&self, code: OFCode) -> bool {
        if let Some(exp) = &*self.expect.borrow() {
            exp.is_expected(code)
        } else {
            false
        }
    }

    /// Expected tokens.
    ///
    /// Returns the list of expected tokens as a String.
    pub fn expect_str(&self) -> String {
        let mut buf = String::new();

        if let Some(exp) = &*self.expect.borrow() {
            let _ = write!(buf, "{:?} ", exp);
        }
        buf
    }
}

impl<'s> Tracer<'s> {
    /// New one.
    pub fn new() -> Self {
        Default::default()
    }

    /// Entering a parser.
    ///
    /// For the tracing to work, all exit-points of a function have to be accounted for.
    /// That means at each exit point you have to call either ok() or err().
    pub fn enter(&self, func: OFCode, span: Span<'s>) {
        self.push_func(func);
        self.push_suggest(func);

        self.track_enter(span);
    }

    /// One step in the parser, that is of of interest.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn step(&self, step: &'static str, span: Span<'s>) {
        self.track_step(step, span);
    }

    /// Even more detailed information for the logs.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn detail<T: Into<String>>(&self, step: T) {
        self.track_detail(step.into());
    }

    /// Ok in a parser. Records the success with function, parsed span and rest span.
    /// Returns the given result value wrapped in Result::Ok(). This is in symmetry with err.
    /// And makes it easier to check if all return points of a function are covered by Tracer.
    ///
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        self.track_ok(rest, span);

        self.pop_stash();
        self.pop_suggest();

        self.track_exit();
        self.pop_func();

        Ok((rest, val))
    }

    /// Error in a parser.
    ///
    /// Keeps track of the error and collects the error as a failed expectation.
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn err<'t, T>(&'t self, mut err: ParseOFError<'s>) -> ParseResult<'s, T> {
        // First encounter with this error.
        // Fill in the current suggestions and initialize the error tracking.
        if err.suggest.is_none() {
            err.suggest = Some(self.clone_suggest());
        }

        // if the error doesn't match the current function
        // we track the error and change the code.
        if err.code != self.func() {
            if self.expect(&mut err, None) == ExpectDuplicate::New {
                self.track_error(&err);
            }
            err.code = self.func();
        }

        // add stash if any.
        let st = self.pop_stash();
        if self.expect(&mut err, st) == ExpectDuplicate::New {
            self.track_error(&err);
        }

        self.pop_suggest();

        self.track_exit();
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
                Track::Error(_, _, _) => {
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

        write!(f, "    expect=")?;
        if let Some(exp) = &*self.expect.borrow() {
            writeln!(f, "{:?} ", exp)?;
        }

        write!(f, "    suggest=")?;
        for sug in &*self.suggest.borrow() {
            writeln!(f, "{:?} ", sug)?;
        }

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
