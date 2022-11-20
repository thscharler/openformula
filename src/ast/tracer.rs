//!
//! Tracing construct for the parser.
//!
//! Doesn't copy any strings, just tracks all function calls in the parser.
//!

use crate::ast::{ParseResult, Span};
use crate::error::{OFCode, ParseOFError};
use std::cell::{Ref, RefCell};
use std::fmt;
use std::fmt::Write;
use std::fmt::{Debug, Formatter};

/// Follows the parsing.
#[derive(Default)]
pub struct Tracer<'s> {
    /// Collected tracks.
    pub tracks: RefCell<Vec<Track<'s>>>,
    /// In an optional branch.
    pub optional: RefCell<Vec<&'static str>>,
    /// Suggestions
    //pub suggest: RefCell<Vec<OFCode>>,
    pub suggest: RefCell<Vec<Suggest>>,
    /// Expected
    pub expect: RefCell<Vec<OFCode>>,
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

        write!(f, "    suggest={:?}", self.suggest.borrow())?;

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
        self.suggest.borrow_mut().push(Suggest {
            func: func.to_string(),
            codes: vec![],
            suggest: vec![],
        });
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
    pub fn suggest(&self, suggest: OFCode) {
        self.detail(format!("suggest {:?}", suggest));

        match self.suggest.borrow_mut().last_mut() {
            None => unreachable!(),
            Some(su) => {
                su.codes.push(suggest);
            }
        }
    }

    /// Returns the suggested tokens.
    /// Leaves the contained vec empty!
    pub fn suggest_vec(&self) -> Ref<'_, Vec<OFCode>> {
        todo!() // self.suggest.borrow()
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
        //self.detail("clear suggestions");
        //self.suggest.borrow_mut().clear();
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
    pub fn expect(&self, suggest: OFCode) {
        let is_opt = self.in_optional();
        self.detail(format!(
            "expect {} {:?}",
            if is_opt { "opt" } else { "" },
            suggest
        ));
        if is_opt {
            self.suggest(suggest);
        } else {
            self.expect.borrow_mut().push(suggest);
        }
    }

    /// Returns the expected tokens.
    /// Leaves the contained vec empty!
    pub fn expect_vec(&self) -> Ref<'_, Vec<OFCode>> {
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

    #[track_caller]
    fn dump_and_panic(&self, result: ParseOFError<'s>) -> ! {
        eprintln!("{:?}", self);
        eprintln!(
            "found '{}' expected {} suggest {}",
            result.span(),
            self.expect_str(),
            self.suggest_str()
        );
        eprintln!();
        eprintln!("=> {}", result);

        unreachable!();
    }

    // translate
    fn map_parse_of_error(&self, mut err: ParseOFError<'s>, code: OFCode) -> ParseOFError<'s> {
        // Translates the code with some exceptions.
        if err.code != OFCode::OFNomError
            && err.code != OFCode::OFNomFailure
            && err.code != OFCode::OFUnexpected
            && err.code != OFCode::OFParseIncomplete
        {
            err.code = code;
        }

        err
    }

    fn auto_expect(&self, err: &ParseOFError<'s>) {
        // Auto expect.
        if err.code != OFCode::OFNomError
            && err.code != OFCode::OFNomFailure
            && err.code != OFCode::OFUnexpected
            && err.code != OFCode::OFParseIncomplete
        {
            self.expect(err.code);
        }
    }

    // Fills the machinery ...
    // Keeps track of the new error.
    // Leaves the current function as is.
    fn track_parseoferror(&self, err: &ParseOFError<'s>) {
        let func_vec = self.func.borrow();
        let func = func_vec.last().unwrap();
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            err.to_string(),
            *err.span(),
        ));
    }

    // Pops the suggestions for the current function.
    fn track_suggest_exit(&self, _func: &str) {
        let mut su_vec = self.suggest.borrow_mut();

        match su_vec.pop() {
            None => unreachable!(),
            Some(su) => {
                match su_vec.last_mut() {
                    None => su_vec.push(su), // was the last, keep
                    Some(last) => {
                        // add to last
                        if !su.codes.is_empty() {
                            last.suggest.push(su);
                        }
                    }
                }
            }
        }
    }

    // Fills the machinery ...
    // Keeps track of the new error.
    // Marks the current function as complete.
    fn err_track_parseoferror_exit(&self, err: &ParseOFError<'s>) {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Error(
            func,
            self.in_optional(),
            err.to_string(),
            *err.span(),
        ));

        self.maybe_drop_optional(func);

        self.track_suggest_exit(func);
    }

    // Records the success of a function.
    fn ok_track_exit(&self, span: Span<'s>, rest: Span<'s>) {
        let func = self.func.borrow_mut().pop().unwrap();
        self.tracks.borrow_mut().push(Track::Ok(func, span, rest));

        self.maybe_drop_optional(func);

        self.track_suggest_exit(func);
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
        self.ok_track_exit(span, rest);
        Ok((rest, val))
    }

    /// Error in a parser.
    ///
    /// Keeps track of the original error.
    /// Translates a ParseError to the new code.
    /// Keeps track of the new error.
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn err_track_map_parse<'t, T>(
        &'t self,
        err: ParseOFError<'s>,
        code: OFCode,
    ) -> ParseResult<'s, T> {
        self.auto_expect(&err);
        self.track_parseoferror(&err);
        let err = self.map_parse_of_error(err, code);
        self.auto_expect(&err);
        self.err_track_parseoferror_exit(&err);

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
    pub fn err_map_parse<'t, T>(
        &'t self,
        err: ParseOFError<'s>,
        code: OFCode,
    ) -> ParseResult<'s, T> {
        let err = self.map_parse_of_error(err, code);
        self.auto_expect(&err);
        self.err_track_parseoferror_exit(&err);
        Err(err)
    }

    /// Error in a parser.
    ///
    /// Keeps track of the error.
    /// Marks the current function as complete.
    ///
    /// Panics
    ///
    /// Panics if there was no call to enter() before.
    pub fn err_parse<'t, T>(&'t self, err: ParseOFError<'s>) -> ParseResult<'s, T> {
        self.err_track_parseoferror_exit(&err);
        Err(err)
    }

    /// Notes the error, dumps everything and panics.
    /// Might be useful, if a unexpected TokenError occurs.
    ///
    /// Panics
    ///
    /// Always.
    #[track_caller]
    pub fn panic_parse<'t>(&'t self, err: ParseOFError<'s>) -> ! {
        self.err_track_parseoferror_exit(&err);
        self.dump_and_panic(err);
    }

    // translate ...
    fn map_nom_error(
        &self,
        err_fn: fn(span: Span<'s>) -> ParseOFError<'s>,
        nom: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> ParseOFError<'s> {
        match nom {
            nom::Err::Error(e) => err_fn(e.input),
            nom::Err::Failure(e) => ParseOFError::new(OFCode::OFNomFailure, e.input),
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }

    /// Error in a parser.
    ///
    /// Maps a nom::Err::Error to one of the ParseOFErrors.
    /// If the error is a nom::Err::Failure it is mapped to ParseOFError::ErrNomFailure.
    /// Records the information in the error with the current function.
    /// Marks the current function as complete.    
    ///
    /// Panics
    ///
    /// If the error is a nom::Err::Incomplete.
    /// Panics if there was no call to enter() before.
    pub fn err_map_nom<'t, T>(
        &'t self,
        err_fn: fn(span: Span<'s>) -> ParseOFError<'s>,
        nom: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> ParseResult<'s, T> {
        let err = self.map_nom_error(err_fn, nom);
        self.err_track_parseoferror_exit(&err);
        Err(err)
    }

    /// Error in a parser.
    ///
    /// Maps a nom::Err::Error to ParseOFError::ErrNomError.
    /// If the error is a nom::Err::Failure it is mapped to ParseOFError::ErrNomFailure.
    /// Records the information in the error with the current function.
    /// Marks the current function as complete.    
    ///
    /// Panics
    ///
    /// If the error is a nom::Err::Incomplete.
    /// Panics if there was no call to enter() before.
    pub fn err_nom<'t, T>(
        &'t self,
        nom: nom::Err<nom::error::Error<Span<'s>>>,
    ) -> ParseResult<'s, T> {
        let err = self.map_nom_error(ParseOFError::err, nom);
        self.err_track_parseoferror_exit(&err);
        Err(err)
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
    fn trace(self, trace: &'t Tracer<'s>, code: OFCode) -> Self;
}

impl<'s, 't, O> TrackParseResult<'s, 't, O> for ParseResult<'s, O> {
    fn trace(self, trace: &'t Tracer<'s>, code: OFCode) -> Self {
        match self {
            Ok(_) => self,
            Err(e) => trace.err_map_parse(e, code),
        }
    }
}

#[derive(Default)]
pub struct Suggest {
    pub func: String,
    pub codes: Vec<OFCode>,
    pub suggest: Vec<Suggest>,
}

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
            write!(f, "{:?}", code)?;
        }

        for su in &suggest.suggest {
            writeln!(f)?;
            Self::arrow(f, indent + 1)?;
            Self::dbg_suggest(f, su, indent + 1)?;
        }

        Ok(())
    }
}

impl Debug for Suggest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::dbg_suggest(f, &self, 0)?;
        Ok(())
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
}

impl<'s> Track<'s> {
    pub fn span(&self) -> Option<&Span<'s>> {
        match self {
            Track::Enter(_, s) => Some(s),
            Track::Step(_, _, s) => Some(s),
            Track::Ok(_, _, s) => Some(s),
            Track::Error(_, _, _, s) => Some(s),
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
        }
        Ok(())
    }
}
