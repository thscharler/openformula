//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::Span;
use crate::error::ParseOFError;
use spreadsheet_ods_cellref::CellRefError;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};

/// Follows the parsing.
#[derive(Default)]
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
                Track::Ok(_, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::Error(_, _, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::ErrorStr(_, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::ErrorSpan(_, _, _) => {
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
        Default::default()
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
    pub fn ok<T>(&self, span: Span<'span>, rest: Span<'span>, val: T) -> (Span<'span>, T) {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Ok(func, span, rest));
        (rest, val)
    }

    /// Erring in a parser. Handles CellRefError.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn reference(&self, span: Span<'span>, err: CellRefError) -> ParseOFError<'span> {
        // TODO: remove this one
        match err {
            CellRefError::ErrNomError(_, e) => ParseOFError::ErrNomError(span, e),
            CellRefError::ErrNomFailure(_, e) => ParseOFError::ErrNomFailure(span, e),
            //CellRefError::ErrCellRef(_) => ParseOFError::ErrCellRef(span),
            CellRefError::ErrCellRange(_) => ParseOFError::ErrCellRange(span),
            CellRefError::ErrColRange(_) => ParseOFError::ErrColRange(span),
            CellRefError::ErrRowRange(_) => ParseOFError::ErrRowRange(span),
            CellRefError::ErrRowname(_, e) => ParseOFError::ErrRowname(span, e),
            CellRefError::ErrColname(_, e) => ParseOFError::ErrColname(span, e),
            _ => todo!(),
        }
    }

    /// Error in a Parser.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn parse(&self, err: ParseOFError<'span>) -> ParseOFError<'span> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::ErrorSpan(func, err.to_string(), *err.span()));

        err
    }

    /// Extracts the error kind and a flag to differentiate between
    /// ast error and failures+incomplete.
    fn error_kind(
        err: &nom::Err<nom::error::Error<Span<'span>>>,
    ) -> (bool, Span<'span>, nom::error::ErrorKind) {
        match err {
            nom::Err::Error(e) => (false, e.input, e.code),
            nom::Err::Failure(e) => (true, e.input, e.code),
            nom::Err::Incomplete(e) => panic!("{:?}", e),
        }
    }

    /// Erring in a parser. Handles all nom errors.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn nom(
        &self,
        // err_fn: fn(span: Span<'span>, nom: nom::error::ErrorKind) -> ParseOFError,
        nom: nom::Err<nom::error::Error<Span<'span>>>,
    ) -> ParseOFError<'span> {
        //
        // The nom error is just the span + error kind in a way.
        // Plus a few bits to differentiate between parsing error and complete failure.
        let (fail, error_span, error_kind) = Self::error_kind(&nom);
        // Map the error.
        // let err = err_fn(rest, error_kind);
        let err = if fail {
            ParseOFError::fail(error_span, error_kind)
        } else {
            ParseOFError::err(error_span, error_kind)
        };

        // Keep track.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::Error(func, err.to_string(), error_span, error_kind));

        err
    }
}

/// One track of the parsing trace.
pub enum Track<'span> {
    /// Function where this occurred and the input span.
    Enter(&'static str, Span<'span>),
    /// Function with an extra step.
    Step(&'static str, &'static str, Span<'span>),
    /// Function where this occurred and the remaining span.
    Ok(&'static str, Span<'span>, Span<'span>),
    /// Function where this occurred and some error info.
    Error(&'static str, String, Span<'span>, nom::error::ErrorKind),
    /// Function where this occurred and some error info.
    ErrorSpan(&'static str, String, Span<'span>),
    ///  Function where this occurred and some error info.
    ErrorStr(&'static str, String),
}

impl<'span> Debug for Track<'span> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Track::Enter(func, input) => {
                write!(f, "{}: '{}'", func, input)?;
            }
            Track::Step(func, step, input) => {
                write!(f, "{}: {} '{}'", func, step, input)?;
            }
            Track::Ok(func, val, rest) => {
                if !val.is_empty() {
                    write!(f, "{}: -> [ {}, '{}' ]", func, val, rest)?;
                } else {
                    write!(f, "{}: -> no match", func)?;
                }
            }
            Track::Error(func, err_str, err_span, err) => {
                write!(
                    f,
                    "{}: !!! err=({}) kind={:?} span='{}'",
                    func, err_str, err, err_span
                )?;
            }
            Track::ErrorSpan(func, err_str, err_span) => {
                write!(f, "{}: !!! err=({}) span='{}'", func, err_str, err_span)?;
            }
            Track::ErrorStr(func, err_str) => {
                write!(f, "{}: !!! err=({})", func, err_str)?;
            }
        }
        Ok(())
    }
}
