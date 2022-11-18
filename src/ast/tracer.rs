//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::tokens::TokenError;
use crate::ast::{ParseResult, Span};
use crate::error::{OFError, ParseOFError};
use std::cell::{Ref, RefCell};
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Suggest {
    AddOp,
    Alpha,
    Colname,
    Elementary,
    RowRange,
    Rowname,
    ColRange,
    Col,
    CompOp,
    Digit,
    Colon,
    CellRef,
    Dollar,
    DollarDollar,
    CellRange,
    Dot,
    EndQuote,
    EndSingleQuote,
    Expr,
    FnCall,
    FnName,
    Identifier,
    Named,
    Iri,
    MulOp,
    Number,
    Parentheses,
    ParenthesesClose,
    ParenthesesOpen,
    PostfixOp,
    PowOp,
    PrefixOp,
    Reference,
    Row,
    Semikolon,
    Separator,
    SheetName,
    SingleQuoted,
    StartQuote,
    StartSingleQuote,
    StringContent,
}

/// Follows the parsing.
#[derive(Default)]
pub struct Tracer<'s> {
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'s>>>,
    /// In an optional branch.
    pub optional: RefCell<Vec<&'static str>>,
    /// Suggestions
    pub suggest: RefCell<Vec<Suggest>>,
    /// Expected
    pub expect: RefCell<Vec<Suggest>>,
    /// Last fn tracked via enter.
    pub func: RefCell<Vec<&'static str>>,
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
                    indent.pop();
                    indent.pop();
                }
                Track::Error(_, _, _, _) => {
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

        write!(f, "    func=")?;
        for func in &*self.func.borrow() {
            write!(f, "{} ", func)?;
        }
        writeln!(f)?;

        write!(f, "    optional=")?;
        for optional in &*self.optional.borrow() {
            write!(f, "{} ", optional)?;
        }
        writeln!(f)?;

        write!(f, "    expect=")?;
        for expect in &*self.expect.borrow() {
            write!(f, "{:?} ", expect)?;
        }
        writeln!(f)?;

        write!(f, "    suggest=")?;
        for suggest in &*self.suggest.borrow() {
            write!(f, "{:?} ", suggest)?;
        }
        // writeln!();

        Ok(())
    }
}

impl<'s> Tracer<'s> {
    /// New one.
    pub fn new() -> Self {
        Default::default()
    }

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
    pub fn enter(&self, func: &'static str, span: Span<'s>) {
        self.func.borrow_mut().push(func);
        self.tracks.borrow_mut().push(Track::Enter(func, span));
    }

    /// Extra step.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn step(&self, step: &'static str, span: Span<'s>) {
        let func = *self.func.borrow().last().unwrap();
        self.tracks.borrow_mut().push(Track::Step(func, step, span));
    }

    /// Extra information.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn detail<T: Into<String>>(&self, step: T) {
        let func = *self.func.borrow().last().unwrap();
        self.tracks
            .borrow_mut()
            .push(Track::Detail(func, step.into()));
    }

    /// About to enter an optional part of the parser.
    ///
    /// All calls to optional are noted in a stack, so recursion
    /// works fine. The stack is cleaned up if a function with
    /// this name is exited via one of the exit functions.
    ///
    /// This can savely be called before and after enter of the function.
    pub fn optional(&self, func: &'static str) {
        self.optional.borrow_mut().push(func);
    }

    /// In an optional branch.
    pub fn in_optional(&self) -> bool {
        self.optional.borrow().last().is_some()
    }

    /// Looses one optional layer if the function matches.
    /// Called by each exit function.
    fn maybe_drop_optional(&self, func: &'static str) {
        let mut b_optional = self.optional.borrow_mut();
        if let Some(opt) = b_optional.last() {
            if *opt == func {
                b_optional.pop();
            }
        }
    }

    /// Suggested tokens.
    ///
    /// Keeps a list of suggestions that can be used for user interaction.
    /// This list accumulates endlessly until clear_suggestions is called.
    pub fn suggest(&self, suggest: Suggest) {
        self.detail(format!("suggest {:?}", suggest));
        self.suggest.borrow_mut().push(suggest);
    }

    /// Returns the suggested tokens.
    /// Leaves the contained vec empty!
    pub fn suggest_vec(&self) -> Ref<'_, Vec<Suggest>> {
        self.suggest.borrow()
    }

    /// Return the suggestions as a String.
    pub fn suggest_str(&self) -> String {
        let mut buf = String::new();
        for s in &*self.suggest.borrow() {
            let _ = write!(buf, "{:?}, ", s);
        }
        buf
    }

    /// Remove all suggestions and expectations.
    ///
    /// Call this any time when the parser reaches a point where the
    /// former suggestions from the parser are voided.
    /// This are usually tokens that prevent backtracking before this point.
    pub fn clear_suggest(&self) {
        self.detail("clear suggestions");
        self.suggest.borrow_mut().clear();
    }

    /// Remove all expectations.
    ///
    /// Call this any time when the parser reaches a point where the
    /// former suggestions from the parser are voided.
    /// This are usually tokens that prevent backtracking before this point.
    pub fn clear_expect(&self) {
        self.detail("clear expectations");
        self.expect.borrow_mut().clear();
    }

    /// Expected tokens.
    ///
    /// Collect information about mandatory expected tokens. There can
    /// be any number of these, if at least one of many options is required.
    pub fn expect(&self, suggest: Suggest) {
        self.detail(format!("expect {:?}", suggest));
        self.expect.borrow_mut().push(suggest);
    }

    /// Returns the expected tokens.
    /// Leaves the contained vec empty!
    pub fn expect_vec(&self) -> Ref<'_, Vec<Suggest>> {
        self.expect.borrow()
    }

    /// Expected tokens.
    ///
    /// Returns the list of expected tokens as a String.
    pub fn expect_str(&self) -> String {
        let mut buf = String::new();
        for s in &*self.expect.borrow() {
            let _ = write!(buf, "{:?}, ", s);
        }
        buf
    }

    /// Ok in a parser.
    ///
    /// Returns the given value for ease of use. Records the success with
    /// function, parsed span and rest span.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    // ParseResult<'s, OFAddOp<'s>>
    pub fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Ok(func, span, rest));

        self.maybe_drop_optional(func);

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
    pub fn err_parse_<'t, T>(&'t self, err: ParseOFError<'s>) -> ParseResult<'s, T> {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::ErrorSpan(
            func,
            self.in_optional(),
            err.to_string(),
            *err.span(),
        ));

        self.maybe_drop_optional(func);

        Err(err)
    }

    /// Error in a parser.
    ///
    /// Translates a ParseError to our own code.
    /// Keeps track of the error.
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn err_parse<'t, T>(
        &'t self,
        mut err: ParseOFError<'s>,
        code: OFError,
    ) -> ParseResult<'s, T> {
        // Translates the code with some exceptions.
        if err.code != OFError::ErrNomError
            && err.code != OFError::ErrNomFailure
            && err.code != OFError::ErrUnexpected
            && err.code != OFError::ErrParseIncomplete
        {
            err.code = code;
        }
        // Auto expect.
        if let Some(expect) = code.expect() {
            self.expect(expect);
        }

        // Cleanup function and keep tracks.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::ErrorSpan(
            func,
            self.in_optional(),
            err.to_string(),
            *err.span(),
        ));

        self.maybe_drop_optional(func);

        Err(err)
    }

    /// Error in a parser.
    ///
    /// Maps a nom::Err::Error to one of the ParseOFErrors.
    /// If the error is a nom::Err::Failure it is mapped to ParseOFError::ErrNomFailure.
    /// Records the information in the error with the current function.
    ///
    /// Panics
    ///
    /// If the error is a nom::Err::Incomplete.
    /// Panics if there was no call to enter() before.
    ///
    pub fn err_map_nom<'t, T>(
        &'t self,
        err_fn: fn(span: Span<'s>) -> ParseOFError<'s>,
        nom: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> ParseResult<'s, T> {
        //
        // The nom error is just the span + error kind in a way.
        // Plus a few bits to differentiate between parsing error and complete failure.
        let (fail, error_span, _error_kind) = Self::error_kind(&nom);
        // Map the error.
        let err = if fail {
            ParseOFError::fail(error_span)
        } else {
            err_fn(error_span)
        };

        // Keep track.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            nom.to_string(),
            error_span,
        ));

        self.maybe_drop_optional(func);

        Err(err)
    }

    /// Extracts the error kind and a flag to differentiate between
    /// ast error and failures+incomplete.
    fn error_kind(
        err: &nom::Err<nom::error::Error<Span<'s>>>,
    ) -> (bool, Span<'s>, nom::error::ErrorKind) {
        match err {
            nom::Err::Error(e) => (false, e.input, e.code),
            nom::Err::Failure(e) => (true, e.input, e.code),
            nom::Err::Incomplete(e) => panic!("{:?}", e),
        }
    }

    /// Error in a parser.
    ///
    /// Maps nom::Err::Error to ParseOFError::ErrNomError and calls it a day.
    /// Uses map_nom, see there.
    ///
    /// Panics
    ///
    /// If the error is a nom::Err::Incomplete.
    /// Panics if there was no call to enter() before.
    pub fn err_nom<'t, T>(
        &'t self,
        nom: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> ParseResult<'s, T> {
        self.err_map_nom(ParseOFError::err, nom)
    }

    /// Notes the error, dumps everything and panics.
    /// Might be useful, if a unexpected TokenError occurs.
    ///
    /// Panics
    ///
    /// Always.
    pub fn panic_tok(&self, tok: TokenError<'s>) -> ! {
        let error_span = *tok.span();

        // Keep track.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            tok.to_string(),
            error_span,
        ));

        eprintln!("{:?}", self);
        eprintln!(
            "found '{}' expected {} suggest {}",
            error_span,
            self.expect_str(),
            self.suggest_str()
        );
        eprintln!();
        eprintln!("=> {}", tok);

        unreachable!();
    }

    /// Error in a parser.
    ///
    /// Maps a TokenError to one of the ParseOFErrors.
    /// If the error is a TokenError::TokNomFailure it is mapped to ParseOFError::ErrNomFailure.
    /// If the error is a TokenError::TokNomError it is mapped to ParseOFError::ErrNomError.
    /// If the error is a TokenError::TokUnexpected it is mapped to ParseOFError::ErrUnexpected.
    /// Anything else uses the conversion provided.
    ///
    /// Records the information in the error with the current function.
    ///
    /// Panics
    ///
    /// If the error is a nom::Err::Incomplete.
    /// Panics if there was no call to enter() before.
    pub fn err_map_tok<'t, T>(
        &'t self,
        err_fn: fn(span: Span<'s>) -> ParseOFError<'s>,
        tok: TokenError<'s>,
    ) -> ParseResult<'s, T> {
        let error_span = *tok.span();
        //
        let mut err = if let TokenError::TokNomFailure(_) = &tok {
            ParseOFError::fail(error_span)
        } else if let TokenError::TokNomError(_) = &tok {
            ParseOFError::err(error_span)
        } else if let TokenError::TokUnexpected(_, e) = &tok {
            // Lost in the detail, but that's a bug anyway.
            self.detail(e.to_string());
            ParseOFError::unexpected(error_span)
        } else {
            err_fn(error_span)
        };

        // Keep track.
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            tok.to_string(),
            error_span,
        ));

        self.maybe_drop_optional(func);

        err.tok.push(tok);
        Err(err)
    }

    /// Erring in a parser. Handles all nom errors.
    ///
    /// Panic
    ///
    /// Panics if there was no call to enter() before.
    pub fn err_tok<'t, T>(&'t self, tok: TokenError<'s>) -> ParseResult<'s, T> {
        self.err_map_tok(ParseOFError::err, tok)
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
    fn trace(self, trace: &'t Tracer<'s>, code: OFError) -> Self;
}

impl<'s, 't, O> TrackParseResult<'s, 't, O> for ParseResult<'s, O> {
    fn trace(self, trace: &'t Tracer<'s>, code: OFError) -> Self {
        match self {
            Ok(_) => self,
            Err(e) => trace.err_parse(e, code),
        }
    }
}

/// One track of the parsing trace.
pub enum Track<'s> {
    /// Function where this occurred and the input span.
    Enter(&'static str, Span<'s>),
    /// Function with an extra step.
    Step(&'static str, &'static str, Span<'s>),
    /// Internal tracing.
    Detail(&'static str, String),
    /// Function where this occurred and the remaining span.
    Ok(&'static str, Span<'s>, Span<'s>),
    /// Function where this occurred and some error info.
    Error(&'static str, bool, String, Span<'s>),
    /// Function where this occurred and some error info.
    ErrorSpan(&'static str, bool, String, Span<'s>),
}

impl<'s> Track<'s> {
    pub fn span(&self) -> Option<&Span<'s>> {
        match self {
            Track::Enter(_, s) => Some(s),
            Track::Step(_, _, s) => Some(s),
            Track::Ok(_, _, s) => Some(s),
            Track::Error(_, _, _, s) => Some(s),
            Track::ErrorSpan(_, _, _, s) => Some(s),
            Track::Detail(_, _) => None,
        }
    }
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
