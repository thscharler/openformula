//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::Span;
use crate::error::ParseOFError;
use spreadsheet_ods_cellref::CellRefError;
use std::cell::RefCell;
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

///
#[derive(Debug)]
pub enum Suggest {
    Reference,
    FnCall,
    PostfixOp,
    PrefixOp,
    MulOp,
    AddOp,
    CompOp,
    Number,
    String,
    Parenthesis,
    ParenthesisOpen,
    ParenthesisClose,
    Separator,
    Expr,
}

/// Follows the parsing.
#[derive(Default)]
pub struct Tracer<'span> {
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'span>>>,
    /// In an optional branch.
    pub optional: RefCell<Vec<&'static str>>,
    /// Suggestions
    pub suggest: RefCell<Vec<Suggest>>,
    /// Expected
    pub expect: RefCell<Vec<Suggest>>,
    /// Last fn tracked via enter.
    pub func: RefCell<Vec<&'static str>>,
}

impl<'span> Debug for Tracer<'span> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Tracer")?;
        let tracks = self.tracks.borrow();

        let mut span = None;
        let mut indent = String::new();
        for tr in tracks.iter() {
            span = Some(tr.span());

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
                Track::Error(_, _, _, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
                Track::ErrorSpan(_, _, _, _) => {
                    writeln!(f, "{}{:?}", indent, tr)?;
                    indent.pop();
                    indent.pop();
                }
            }
        }

        writeln!(
            f,
            "Found '{}' expected {} suggest {}",
            span.map(|s| s.fragment()).unwrap_or(&""),
            self.expectations(),
            self.suggestions()
        )?;

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

    /// About to enter an optional part of the parser.
    pub fn optional(&self, func: &'static str) {
        self.optional.borrow_mut().push(func);
    }

    /// In an optional branch
    pub fn in_optional(&self) -> bool {
        match self.optional.borrow().last() {
            None => false,
            Some(_) => true,
        }
    }

    /// Looses one optional layer if the function matches.
    fn maybe_drop_optional(&self, func: &'static str) {
        let mut b_optional = self.optional.borrow_mut();
        if let Some(opt) = b_optional.last() {
            if *opt == func {
                b_optional.pop();
            }
        }
    }

    /// Suggested tokens.
    pub fn suggest(&self, suggest: Suggest) {
        self.suggest.borrow_mut().push(suggest);
    }

    /// Suggestions
    pub fn suggestions(&self) -> String {
        let mut buf = String::new();
        for s in &*self.suggest.borrow() {
            let _ = write!(buf, "{:?}, ", s);
        }
        buf
    }

    /// Expected tokens.
    pub fn expect(&self, suggest: Suggest) {
        self.expect.borrow_mut().push(suggest);
    }

    /// Suggestions
    pub fn expectations(&self) -> String {
        let mut buf = String::new();
        for s in &*self.expect.borrow() {
            let _ = write!(buf, "{:?}, ", s);
        }
        buf
    }

    /// Ok in a parser.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn ok<T>(&self, span: Span<'span>, rest: Span<'span>, val: T) -> (Span<'span>, T) {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Ok(func, span, rest));

        self.maybe_drop_optional(func);

        (rest, val)
    }

    /// Erring in a parser. Handles CellRefError.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn reference(&self, span: Span<'span>, err: CellRefError) -> ParseOFError<'span> {
        // TODO: remove this one
        let err = match err {
            CellRefError::ErrNomError(_, e) => ParseOFError::ErrNomError(span, e),
            CellRefError::ErrNomFailure(_, e) => ParseOFError::ErrNomFailure(span, e),
            //CellRefError::ErrCellRef(_) => ParseOFError::ErrCellRef(span),
            CellRefError::ErrCellRange(_) => ParseOFError::ErrCellRange(span),
            CellRefError::ErrColRange(_) => ParseOFError::ErrColRange(span),
            CellRefError::ErrRowRange(_) => ParseOFError::ErrRowRange(span),
            CellRefError::ErrRowname(_, e) => ParseOFError::ErrRowname(span, e),
            CellRefError::ErrColname(_, e) => ParseOFError::ErrColname(span, e),
            _ => todo!(),
        };

        self.parse(err)
    }

    /// Error in a Parser.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn parse(&self, err: ParseOFError<'span>) -> ParseOFError<'span> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::ErrorSpan(
            func,
            self.in_optional(),
            err.to_string(),
            *err.span(),
        ));

        self.maybe_drop_optional(func);

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
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            err.to_string(),
            error_span,
            error_kind,
        ));

        self.maybe_drop_optional(func);

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
    Error(
        &'static str,
        bool,
        String,
        Span<'span>,
        nom::error::ErrorKind,
    ),
    /// Function where this occurred and some error info.
    ErrorSpan(&'static str, bool, String, Span<'span>),
}

impl<'span> Track<'span> {
    pub fn span(&self) -> &Span<'span> {
        match self {
            Track::Enter(_, s) => s,
            Track::Step(_, _, s) => s,
            Track::Ok(_, _, s) => s,
            Track::Error(_, _, _, s, _) => s,
            Track::ErrorSpan(_, _, _, s) => s,
        }
    }
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
            Track::Error(func, opt, err_str, err_span, err) => {
                write!(
                    f,
                    "{}: {} err=({}) kind={:?} span='{}'",
                    func,
                    if *opt { "OPT" } else { "!!!" },
                    err_str,
                    err,
                    err_span
                )?;
            }
            Track::ErrorSpan(func, opt, err_str, err_span) => {
                write!(
                    f,
                    "{}: {} err=({}) span='{}'",
                    func,
                    if *opt { "OPT" } else { "!!!" },
                    err_str,
                    err_span
                )?;
            }
        }
        Ok(())
    }
}
