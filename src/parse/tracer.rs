//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::error::ParseOFError;
use crate::parse::Span;
use spreadsheet_ods_cellref::CellRefError;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};

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
                Track::Error(_, _, _) => {
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

    /// Extra step.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn step2(&self, step: &'static str) {
        let func = *self.func.borrow().last().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::Step(func, step, Span::new("")));
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
    pub fn reference(&self, err: CellRefError) -> ParseOFError {
        match err {
            CellRefError::ErrNomError(span, e) => ParseOFError::ErrNomError(span.into(), e),
            CellRefError::ErrNomFailure(span, e) => ParseOFError::ErrNomFailure(span.into(), e),
            //CellRefError::ErrCellRef(span) => ParseOFError::ErrCellRef(span.into()),
            CellRefError::ErrCellRange(span) => ParseOFError::ErrCellRange(span.into()),
            CellRefError::ErrColRange(span) => ParseOFError::ErrColRange(span.into()),
            CellRefError::ErrRowRange(span) => ParseOFError::ErrRowRange(span.into()),
            CellRefError::ErrRowname(span, e) => ParseOFError::ErrRowname(span.into(), e),
            CellRefError::ErrColname(span, e) => ParseOFError::ErrColname(span.into(), e),
            _ => todo!(),
        }
    }

    /// Error in a Parser.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn parse(&self, err: ParseOFError) -> ParseOFError {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::ErrorStr(func, err.to_string()));

        err
    }

    /// Extracts the error kind and a flag to differentiate between
    /// parse error and failures+incomplete.
    fn error_kind(err: &nom::Err<nom::error::Error<Span<'_>>>) -> (bool, nom::error::ErrorKind) {
        match err {
            nom::Err::Error(e) => (false, e.code),
            nom::Err::Incomplete(_e) => (true, nom::error::ErrorKind::Fail),
            nom::Err::Failure(e) => (true, e.code),
        }
    }

    /// Erring in a parser. Handles all nom errors.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn nom(
        &self,
        rest: Span<'span>,
        // err_fn: fn(span: Span<'span>, nom: nom::error::ErrorKind) -> ParseOFError,
        nom: nom::Err<nom::error::Error<Span<'span>>>,
    ) -> ParseOFError {
        // The nom error is just the span + error kind in a way.
        // Plus a few bits to differentiate between parsing error and complete failure.
        let (fail, error_kind) = Self::error_kind(&nom);
        // Map the error.
        let err = if fail {
            ParseOFError::fail(rest, error_kind)
        } else {
            ParseOFError::err(rest, error_kind)
            // err_fn(rest, error_kind)
        };

        // Keep track.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::Error(func, err.to_string(), nom));

        err
    }

    /// Notes some error and converts it to a ParseExprError.
    /// Used for ParseIntError et al.
    pub fn re_err<V, E>(&self, err: Result<V, E>) -> Result<V, ParseOFError>
    where
        E: Into<ParseOFError>,
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
}

/// One track of the parsing trace.
pub enum Track<'span> {
    /// Function where this occurred and the input span.
    Enter(&'static str, Span<'span>),
    /// Function with an extra step.
    Step(&'static str, &'static str, Span<'span>),
    /// Function where this occurred and the remaining span.
    Ok(&'static str, Span<'span>, Span<'span>),
    /// Function where this occurred and some error.
    Error(
        &'static str,
        String,
        nom::Err<nom::error::Error<Span<'span>>>,
    ),
    /// Some errors don't impl Error.
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
            Track::Error(func, err_str, err) => {
                write!(f, "{}: !!! {} : {}", func, err_str, err)?;
            }
            Track::ErrorStr(func, err_str) => {
                write!(f, "{}: !!! {}", func, err_str)?;
            }
        }
        Ok(())
    }
}
