//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::parse2::{ParseExprError, Span};
use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

/// Follows the parsing.
pub struct Tracer<'span> {
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'span>>>,
    /// Last fn tracked via enter.
    pub func: RefCell<Vec<&'static str>>,
}

impl<'span> Debug for Tracer<'span> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Tracer")?;
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
                Track::Ok(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::Error(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::ErrorDyn(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::ErrorStr(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
            }
        }
        Ok(())
    }
}

impl<'span> Tracer<'span> {
    /// New one.
    pub fn new() -> Self {
        Self {
            tracks: Default::default(),
            func: Default::default(),
        }
    }

    /// Entering a parser.
    pub fn enter(&self, func: &'static str, span: Span<'span>) {
        self.func.borrow_mut().push(func);
        self.tracks.borrow_mut().push(Track::Enter(func, span));
    }

    /// Extra step.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn step(&self, step: &'static str, span: Span<'span>) {
        let func = *self.func.borrow().last().unwrap();
        self.tracks.borrow_mut().push(Track::Step(func, step, span));
    }

    /// Ok in a parser.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn ok<T>(&self, rest: Span<'span>, val: T) -> (Span<'span>, T) {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Ok(func, rest));
        (rest, val)
    }

    /// Erring in a parser. Accepts only nom errors.
    ///
    /// Returns to reference to this trace to be used in the final error type.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn err<'a>(&'a self, err: nom::Err<nom::error::Error<Span<'span>>>) -> &'a Tracer<'span> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Error(func, err));
        self
    }

    ///
    /// Notes some error and converts it to a ParseExprError.
    pub fn re_err<'s, 't, V, E>(&'t self, err: Result<V, E>) -> Result<V, ParseExprError<'s, 't>>
    where
        E: Into<ParseExprError<'s, 't>>,
        E: Display,
    {
        match err {
            Ok(v) => Ok(v),
            Err(e) => {
                let func = self.func.borrow_mut().pop().unwrap();
                self.tracks
                    .borrow_mut()
                    .push(Track::ErrorStr(func, e.to_string()));

                Err(e.into())
            }
        }
    }

    /// Erring in a parser. Accepts boxed dyn errors for the rest.
    ///
    /// Returns to reference to this trace to be used in the final error type.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn dyn_err<'a>(&'a self, err: Box<dyn Error>) -> &'a Tracer<'span> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::ErrorDyn(func, err));
        self
    }
}

/// One track of the parsing trace.
pub enum Track<'span> {
    /// Function where this occurred and the input span.
    Enter(&'static str, Span<'span>),
    /// Function with an extra step.
    Step(&'static str, &'static str, Span<'span>),
    /// Function where this occurred and the remaining span.
    Ok(&'static str, Span<'span>),
    /// Function where this occurred and some error.
    Error(&'static str, nom::Err<nom::error::Error<Span<'span>>>),
    /// Function where this occurred and some error.
    ErrorDyn(&'static str, Box<dyn Error>),
    /// Some errors don't impl Error.
    ErrorStr(&'static str, String),
}

impl<'span> Debug for Track<'span> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Track::Enter(func, input) => {
                write!(f, "{}: enter '{}'", func, input)?;
            }
            Track::Step(func, step, input) => {
                write!(f, "{}: {} '{}'", func, step, input)?;
            }
            Track::Ok(func, rest) => {
                write!(f, "{}: exit '{}'", func, rest)?;
            }
            Track::Error(func, err) => {
                write!(f, "{}: error {}", func, err)?;
            }
            Track::ErrorDyn(func, err) => {
                write!(f, "{}: error2 {}", func, err)?;
            }
            Track::ErrorStr(func, err) => {
                write!(f, "{}: error3 {}", func, err)?;
            }
        }
        Ok(())
    }
}
