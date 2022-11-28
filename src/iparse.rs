use crate::iparse::error::{DebugWidth, ParserError};
use crate::iparse::tracer::Track;
use nom_locate::LocatedSpan;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

/// Code for parser errors and parser functions.
pub trait Code: Copy + Display + Debug + PartialEq {
    const DEFAULT: Self;
    const NOM_ERROR: Self;
    const NOM_FAILURE: Self;
    const PARSE_INCOMPLETE: Self;

    fn is_special(&self) -> bool {
        *self == Self::DEFAULT
            || *self == Self::NOM_ERROR
            || *self == Self::NOM_FAILURE
            || *self == Self::PARSE_INCOMPLETE
    }
}

/// Input type.
pub type Span<'a> = LocatedSpan<&'a str>;

/// Result type.
pub type ParseResult<'s, O, C> = Result<(Span<'s>, O), ParserError<'s, C>>;

/// Trait for one parser function.
pub trait Parser<'s, C: Code, O> {
    // TODO : Changeordering O C
    /// Function and error code.
    fn id() -> C;

    /// Possible look-ahead.
    fn lah(_: Span<'s>) -> bool {
        true
    }
    // fn lah(_: Span<'s>) -> ControlFlow<(), ()> {
    //     ControlFlow::Continue(())
    // }

    /// Parses the expression.
    fn parse<'t>(trace: &'t impl Tracer<'s, C>, rest: Span<'s>) -> ParseResult<'s, O, C>;
}

///
/// Traces the parser and helps generating errors and suggestions.
///
/// ```
/// use openformula::ast::OFAst;
/// use openformula::ast::tokens::dot;
/// use openformula::error::OFCode;
/// use openformula::iparse::tracer::{ CTracer, TrackParseResult };
/// use openformula::iparse::{Parser, ParseResult, Span, Tracer};
/// use openformula::parser::ReferenceExpr;
///
/// let trace = CTracer::new();
/// simple_parse(&trace, Span::new(".A1.")).unwrap();
///
/// fn simple_parse<'s>(trace: &'_ impl Tracer<'s, OFCode>, span: Span<'s>) -> ParseResult<'s, String, OFCode> {
///     trace.enter(OFCode::OFCDot, span);
///
///     // call trace.err() in case of and Result::Err
///     ReferenceExpr::parse(trace, span).track(trace)?;
///
///     // do some parseing
///     match dot(span) {
///         Ok((rest, token)) => {
///             return trace.ok(token, rest, "DOT".to_string()); // return whatever
///         }
///         Err(e) => {
///             return trace.err(e);
///         }
///     }
/// }
/// ```
///
/// The necessary frameing are the call to trace.enter() to establish the environment, and
/// either a call to ok or err at each exit point of the function.
///
/// TrackParseResult can be useful when calling further parse functions. It's method trace()
/// helps keep track of an early exit via the ? operator.
///
/// Use suggest() for optional parts that should be hinted somewhere.
///
/// Use stash() to store parser errors that might be used later. Eg if none of several
/// alternatives fit. All stashed parser errors will be collected and attach as Expect value
/// to a new summary error.
///
pub trait Tracer<'s, C: Code> {
    fn new() -> Self;

    fn enter(&self, func: C, span: Span<'s>);

    fn step(&self, step: &'static str, span: Span<'s>);

    fn debug<T: Into<String>>(&self, step: T);

    fn suggest(&self, suggest: C, span: Span<'s>);

    fn stash(&self, err: ParserError<'s, C>);

    fn ok<T>(&'_ self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T, C>;

    fn err<T>(&'_ self, err: ParserError<'s, C>) -> ParseResult<'s, T, C>;

    fn write_debug<'a, 'b>(
        &'a self,
        f: &mut Formatter<'_>,
        w: DebugWidth,
        filter: FilterFn<'b, 's, C>,
    ) -> fmt::Result;
}

pub type FilterFn<'a, 's, C> = &'a dyn for<'t> Fn(&'t Track<'s, C>) -> bool;

pub mod span {
    use crate::iparse::Span;
    use nom::Offset;
    use std::slice;
    use std::str::from_utf8_unchecked;

    // See span_union for details.
    pub(crate) unsafe fn span_union_opt<'a>(span0: Option<Span<'a>>, span1: Span<'a>) -> Span<'a> {
        unsafe {
            match span0 {
                None => span1,
                Some(span0) => span_union(span0, span1),
            }
        }
    }

    // Returns a new Span that reaches from the beginning of span0 to the end of span1.
    //
    // If any of the following conditions are violated, the result is Undefined Behavior:
    // * Both the starting and other pointer must be either in bounds or one byte past the end of the same allocated object.
    //      Should be guaranteed if both were obtained from on ast run.
    // * Both pointers must be derived from a pointer to the same object.
    //      Should be guaranteed if both were obtained from on ast run.
    // * The distance between the pointers, in bytes, cannot overflow an isize.
    // * The distance being in bounds cannot rely on “wrapping around” the address space.
    pub(crate) unsafe fn span_union<'a>(span0: Span<'a>, span1: Span<'a>) -> Span<'a> {
        let ptr = span0.as_ptr();
        // offset to the start of span1 and add the length of span1.
        let size = span0.offset(&span1) + span1.len();

        unsafe {
            // The size should be within the original allocation, if both spans are from
            // the same ast run. We must ensure that the ast run doesn't generate
            // Spans out of nothing that end in the ast.
            let slice = slice::from_raw_parts(ptr, size);
            // This is all from a str originally and we never got down to bytes.
            let str = from_utf8_unchecked(slice);

            // As span0 was ok the offset used here is ok too.
            Span::new_from_raw_offset(span0.location_offset(), span0.location_line(), str, ())
        }
    }
}

pub mod error {
    use crate::iparse::{Code, Span};
    use std::error::Error;
    use std::fmt;
    use std::fmt::{Display, Formatter};

    /// Error for the Parser.
    pub struct ParserError<'s, C: Code> {
        /// Error code.
        pub code: C,
        /// Error span.
        pub span: Span<'s>,
        /// Flag for Tracer.
        pub tracing: bool,
        /// Suggest values.
        pub suggest: Vec<Suggest<'s, C>>,
        /// Expect values.
        pub expect: Vec<Expect<'s, C>>,
    }

    impl<'s, C: Code> ParserError<'s, C> {
        /// New error.
        pub fn new(code: C, span: Span<'s>) -> Self {
            Self {
                code,
                span,
                tracing: false,
                suggest: Vec::new(),
                expect: Vec::new(),
            }
        }

        /// Special error code. Encodes errors occurring at the margins.
        pub fn is_special(&self) -> bool {
            self.code.is_special()
        }

        /// Error code of the parser.
        pub fn is_parser(&self) -> bool {
            !self.code.is_special()
        }

        /// Was this one of the expected errors.
        pub fn is_expected(&self, code: C) -> bool {
            for exp in &self.expect {
                if exp.code == code {
                    return true;
                }
            }
            false
        }

        /// Was this one of the expected errors, and is in the call stack of parent?
        pub fn is_expected2(&self, code: C, parent: C) -> bool {
            for exp in &self.expect {
                if exp.code == code {
                    for par in &exp.parents {
                        if *par == parent {
                            return true;
                        }
                    }
                }
            }
            false
        }

        /// Create a ParseOFError from a nom::Err
        pub fn nom(e: nom::Err<nom::error::Error<Span<'s>>>) -> ParserError<'s, C> {
            match e {
                nom::Err::Error(e) => ParserError::new(C::NOM_ERROR, e.input),
                nom::Err::Failure(e) => ParserError::new(C::NOM_FAILURE, e.input),
                nom::Err::Incomplete(_) => unreachable!(),
            }
        }

        /// ParseIncomplete variant.
        pub fn parse_incomplete(span: Span<'s>) -> ParserError<'s, C> {
            ParserError::new(C::PARSE_INCOMPLETE, span)
        }
    }

    impl<'s, C: Code> Display for ParserError<'s, C> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{} expects ", self.code)?;
            for (i, exp) in self.expect.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}:\"{}\"", exp.code, exp.span)?;
            }
            // no suggest
            write!(
                f,
                " for span {} \"{}\"",
                self.span.location_offset(),
                self.span
            )?;
            Ok(())
        }
    }

    impl<'s, C: Code> Error for ParserError<'s, C> {}

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum DebugWidth {
        /// Debug flag, can be set with width=0.
        Short,
        /// Debug flag, can be set with width=1.
        Medium,
        /// Debug flag, can be set with width=2.
        Long,
    }

    /// Suggestions, optional tokens.
    #[derive(Clone)]
    pub struct Suggest<'s, C> {
        /// Code for the token.
        pub code: C,
        /// Span
        pub span: Span<'s>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Expected tokens.
    #[derive(Clone)]
    pub struct Expect<'s, C> {
        /// Code for the token.
        pub code: C,
        /// Span.
        pub span: Span<'s>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    impl From<Option<usize>> for DebugWidth {
        fn from(value: Option<usize>) -> Self {
            match value {
                None | Some(0) => DebugWidth::Short,
                Some(1) => DebugWidth::Medium,
                Some(2) => DebugWidth::Long,
                _ => DebugWidth::Short,
            }
        }
    }

    mod debug {
        use crate::iparse::error::{Expect, ParserError, Suggest};
        use crate::iparse::Code;
        use std::fmt;
        use std::fmt::{Debug, Formatter};

        impl<'s, C: Code> Debug for ParserError<'s, C> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match f.width() {
                    None | Some(0) => debug_parse_of_error_short(f, self),
                    Some(1) => debug_parse_of_error_medium(f, self),
                    Some(2) => debug_parse_of_error_long(f, self),
                    _ => Ok(()),
                }
            }
        }

        impl<'s, C: Code> Debug for Suggest<'s, C> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "{}:\"{}\"", self.code, self.span)?;
                Ok(())
            }
        }

        impl<'s, C: Code> Debug for Expect<'s, C> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "{}:\"{}\"", self.code, self.span)?;
                Ok(())
            }
        }

        fn debug_parse_of_error_short<'s, C: Code>(
            f: &mut Formatter<'_>,
            err: &ParserError<'s, C>,
        ) -> fmt::Result {
            write!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
            if !err.expect.is_empty() {
                write!(f, "expect=")?;
                debug_expect2_short(f, &err.expect, 1)?;
            }
            if !err.suggest.is_empty() {
                write!(f, "suggest=")?;
                debug_suggest2_short(f, &err.suggest, 1)?;
            }

            Ok(())
        }

        fn debug_parse_of_error_medium<'s, C: Code>(
            f: &mut Formatter<'_>,
            err: &ParserError<'s, C>,
        ) -> fmt::Result {
            writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
            if !err.expect.is_empty() {
                let mut sorted = err.expect.clone();
                sorted.reverse();
                sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

                // per offset
                let mut grp_offset = 0;
                let mut grp = Vec::new();
                let mut subgrp = Vec::new();
                for exp in &sorted {
                    if exp.span.location_offset() != grp_offset {
                        if !subgrp.is_empty() {
                            grp.push((grp_offset, subgrp));
                            subgrp = Vec::new();
                        }
                        grp_offset = exp.span.location_offset();
                    }

                    subgrp.push(exp);
                }
                if !subgrp.is_empty() {
                    grp.push((grp_offset, subgrp));
                }

                for (g_off, subgrp) in grp {
                    let first = subgrp.first().unwrap();
                    writeln!(f, "expect {}:\"{}\" ", g_off, first.span)?;
                    debug_expect2_medium(f, &subgrp, 1)?;
                }
            }
            if !err.suggest.is_empty() {
                let mut sorted = err.suggest.clone();
                sorted.reverse();
                sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

                // per offset
                let mut grp_offset = 0;
                let mut grp = Vec::new();
                let mut subgrp = Vec::new();
                for exp in &sorted {
                    if exp.span.location_offset() != grp_offset {
                        if !subgrp.is_empty() {
                            grp.push((grp_offset, subgrp));
                            subgrp = Vec::new();
                        }
                        grp_offset = exp.span.location_offset();
                    }

                    subgrp.push(exp);
                }
                if !subgrp.is_empty() {
                    grp.push((grp_offset, subgrp));
                }

                for (g_off, subgrp) in grp {
                    let first = subgrp.first().unwrap();
                    writeln!(f, "suggest {}:\"{}\"", g_off, first.span)?;
                    debug_suggest2_medium(f, &subgrp, 1)?;
                }
            }

            Ok(())
        }

        fn debug_parse_of_error_long<'s, C: Code>(
            f: &mut Formatter<'_>,
            err: &ParserError<'s, C>,
        ) -> fmt::Result {
            writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
            if !err.expect.is_empty() {
                let mut sorted = err.expect.clone();
                sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

                writeln!(f, "expect=")?;
                debug_expect2_long(f, &sorted, 1)?;
            }
            if !err.suggest.is_empty() {
                writeln!(f, "suggest=")?;
                debug_suggest2_long(f, &err.suggest, 1)?;
            }

            Ok(())
        }

        fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
            write!(f, "{}", " ".repeat(ind * 4))?;
            Ok(())
        }

        // expect2

        fn debug_expect2_long<C: Code>(
            f: &mut Formatter<'_>,
            exp_vec: &Vec<Expect<'_, C>>,
            ind: usize,
        ) -> fmt::Result {
            for exp in exp_vec {
                indent(f, ind)?;
                write!(
                    f,
                    "{}:{}:\"{}\"",
                    exp.code,
                    exp.span.location_offset(),
                    exp.span
                )?;
                if !exp.parents.is_empty() {
                    write!(f, " <")?;
                    for (i, p) in exp.parents.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", p)?;
                    }
                    write!(f, ">")?;
                }
                writeln!(f)?;
            }

            Ok(())
        }

        fn debug_expect2_medium<C: Code>(
            f: &mut Formatter<'_>,
            exp_vec: &Vec<&Expect<'_, C>>,
            ind: usize,
        ) -> fmt::Result {
            let mut prefix: Vec<Vec<C>> = Vec::new();

            for exp in exp_vec {
                indent(f, ind)?;
                write!(f, "{:20}", exp.code)?;

                let (suffix, sigil) = loop {
                    if let Some(last) = prefix.last() {
                        match exp.parents.strip_prefix(last.as_slice()) {
                            None => {
                                match prefix.pop() {
                                    None => {
                                        prefix.push(exp.parents.clone());
                                        break (exp.parents.as_slice(), "<<");
                                    }
                                    Some(_) => {}
                                };
                            }
                            Some(suffix) => {
                                prefix.push(exp.parents.clone());
                                break (suffix, "++");
                            }
                        }
                    } else {
                        prefix.push(exp.parents.clone());
                        break (exp.parents.as_slice(), "<<");
                    }
                };

                write!(f, " {} ", sigil)?;
                for (i, p) in suffix.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", p)?;
                }
                writeln!(f)?;
            }

            Ok(())
        }

        fn debug_expect2_short<C: Code>(
            f: &mut Formatter<'_>,
            exp_vec: &Vec<Expect<'_, C>>,
            _ind: usize,
        ) -> fmt::Result {
            for exp in exp_vec {
                write!(f, "{}:\"{}\"", exp.code, exp.span)?;
                if !exp.parents.is_empty() {
                    write!(f, " <")?;
                    for (i, p) in exp.parents.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", p)?;
                    }
                    write!(f, ">")?;
                }
            }

            Ok(())
        }

        // suggest2

        fn debug_suggest2_long<C: Code>(
            f: &mut Formatter<'_>,
            sug_vec: &Vec<Suggest<'_, C>>,
            ind: usize,
        ) -> fmt::Result {
            for sug in sug_vec {
                indent(f, ind)?;
                write!(
                    f,
                    "{}:{}:\"{}\"",
                    sug.code,
                    sug.span.location_offset(),
                    sug.span
                )?;
                if !sug.parents.is_empty() {
                    write!(f, " <")?;
                    for (i, p) in sug.parents.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", p)?;
                    }
                    write!(f, ">")?;
                }
                writeln!(f)?;
            }

            Ok(())
        }

        fn debug_suggest2_medium<C: Code>(
            f: &mut Formatter<'_>,
            sug_vec: &Vec<&Suggest<'_, C>>,
            ind: usize,
        ) -> fmt::Result {
            let mut prefix: Vec<Vec<C>> = Vec::new();

            for sug in sug_vec {
                indent(f, ind)?;
                write!(f, "{:20}", sug.code)?;

                let (suffix, sigil) = loop {
                    if let Some(last) = prefix.last() {
                        match sug.parents.strip_prefix(last.as_slice()) {
                            None => {
                                match prefix.pop() {
                                    None => {
                                        prefix.push(sug.parents.clone());
                                        break (sug.parents.as_slice(), "<<");
                                    }
                                    Some(_) => {}
                                };
                            }
                            Some(suffix) => {
                                prefix.push(sug.parents.clone());
                                break (suffix, "++");
                            }
                        }
                    } else {
                        prefix.push(sug.parents.clone());
                        break (sug.parents.as_slice(), "<<");
                    }
                };

                write!(f, " {} ", sigil)?;
                for (i, p) in suffix.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", p)?;
                }
                writeln!(f)?;
            }

            Ok(())
        }

        fn debug_suggest2_short<C: Code>(
            f: &mut Formatter<'_>,
            sug_vec: &Vec<Suggest<'_, C>>,
            _ind: usize,
        ) -> fmt::Result {
            for sug in sug_vec {
                write!(f, "{}:\"{}\"", sug.code, sug.span)?;
                if !sug.parents.is_empty() {
                    write!(f, " <")?;
                    for (i, p) in sug.parents.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", p)?;
                    }
                    write!(f, ">")?;
                }
            }

            Ok(())
        }
    }
}

pub mod tracer {
    use crate::iparse::error::{DebugWidth, Expect, Suggest};
    use crate::iparse::tracer::debug::debug_tracer;
    use crate::iparse::{Code, FilterFn, ParseResult, ParserError, Span, Tracer};
    use std::cell::RefCell;
    use std::fmt;
    use std::fmt::{Debug, Display, Formatter};
    use std::marker::PhantomData;

    /// Tracing and error collection.
    pub struct CTracer<'s, C: Code> {
        /// Function call stack.
        func: RefCell<Vec<C>>,

        /// Collected tracks.
        track: RefCell<Vec<Track<'s, C>>>,
        suggest: RefCell<Vec<SuggestTrack<'s, C>>>,
        expect: RefCell<Vec<ExpectTrack<'s, C>>>,
    }

    impl<'s, C: Code> Tracer<'s, C> for CTracer<'s, C> {
        /// New one.
        fn new() -> Self {
            Self {
                func: RefCell::new(Vec::new()),
                track: RefCell::new(Vec::new()),
                suggest: RefCell::new(Vec::new()),
                expect: RefCell::new(Vec::new()),
            }
        }

        /// Enter a parser function. Absolutely necessary for the rest.
        fn enter(&self, func: C, span: Span<'s>) {
            self.push_func(func);
            self.push_suggest(func);
            self.push_expect(func);

            self.track_enter(span);
        }

        /// Keep track of steps in a complicated parser.
        fn step(&self, step: &'static str, span: Span<'s>) {
            self.track_step(step, span);
        }

        /// Some detailed debug information.
        fn debug<T: Into<String>>(&self, step: T) {
            self.track_debug(step.into());
        }

        /// Adds a suggestion for the current stack frame.
        fn suggest(&self, suggest: C, span: Span<'s>) {
            self.debug(format!("suggest {}:\"{}\" ...", suggest, span));
            self.add_suggest(suggest, span);
        }

        /// Keep track of this error.
        fn stash(&self, err: ParserError<'s, C>) {
            self.debug(format!("expect {}:\"{}\" ...", err.code, err.span));
            self.add_expect(err.code, err.span);
            self.append_expect(err.expect);
            self.append_suggest(err.suggest);
        }

        /// Write a track for an ok result.
        fn ok<'t, T>(&'t self, span: Span<'s>, rest: Span<'s>, val: T) -> ParseResult<'s, T, C> {
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
        fn err<'t, T>(&'t self, mut err: ParserError<'s, C>) -> ParseResult<'s, T, C> {
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
        fn write_debug<'a, 'b>(
            &'a self,
            f: &mut Formatter<'_>,
            w: DebugWidth,
            filter: FilterFn<'b, 's, C>,
        ) -> fmt::Result {
            debug_tracer(f, w, self, filter)
        }
    }

    // expect
    impl<'s, C: Code> CTracer<'s, C> {
        fn push_expect(&self, func: C) {
            self.expect.borrow_mut().push(ExpectTrack {
                func,
                usage: Usage::Track,
                list: Vec::new(),
                parents: self.parent_vec(),
            })
        }

        fn pop_expect(&self) -> ExpectTrack<'s, C> {
            self.expect.borrow_mut().pop().unwrap()
        }

        fn add_expect(&self, code: C, span: Span<'s>) {
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

        fn append_expect(&self, mut expect: Vec<Expect<'s, C>>) {
            self.expect
                .borrow_mut()
                .last_mut()
                .unwrap()
                .list
                .append(&mut expect);
        }
    }

    // suggest
    impl<'s, C: Code> CTracer<'s, C> {
        fn push_suggest(&self, func: C) {
            self.suggest.borrow_mut().push(SuggestTrack {
                func,
                usage: Usage::Track,
                list: Vec::new(),
                parents: self.parent_vec(),
            })
        }

        fn pop_suggest(&self) -> SuggestTrack<'s, C> {
            self.suggest.borrow_mut().pop().unwrap()
        }

        fn add_suggest(&self, code: C, span: Span<'s>) {
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

        fn append_suggest(&self, mut suggest: Vec<Suggest<'s, C>>) {
            self.suggest
                .borrow_mut()
                .last_mut()
                .unwrap()
                .list
                .append(&mut suggest);
        }
    }

    // call frame tracking
    impl<'s, C: Code> CTracer<'s, C> {
        // enter function
        fn push_func(&self, func: C) {
            self.func.borrow_mut().push(func);
        }

        // leave current function
        fn pop_func(&self) {
            self.func.borrow_mut().pop();
        }

        // current function
        fn func(&self) -> C {
            *self.func.borrow().last().unwrap()
        }

        fn parent_vec(&self) -> Vec<C> {
            self.func.borrow().clone()
        }
    }

    // basic tracking
    impl<'s, C: Code> CTracer<'s, C> {
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

        fn track_suggest(&self, usage: Usage, suggest: Vec<Suggest<'s, C>>) {
            if !suggest.is_empty() {
                self.track.borrow_mut().push(Track::Suggest(SuggestTrack {
                    func: self.func(),
                    usage,
                    list: suggest,
                    parents: self.parent_vec(),
                }));
            }
        }

        fn track_expect(&self, usage: Usage, expect: Vec<Expect<'s, C>>) {
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

        fn track_error(&self, err: &ParserError<'s, C>) {
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

    impl<'s, C: Code> Default for CTracer<'s, C> {
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
    pub trait TrackParseResult<'s, 't, O, C: Code> {
        /// Translates the error code and adds the standard expect value.
        /// Then tracks the error and marks the current function as finished.
        fn track(self, trace: &'t impl Tracer<'s, C>) -> Self;
    }

    impl<'s, 't, O, C: Code> TrackParseResult<'s, 't, O, C> for ParseResult<'s, O, C> {
        fn track(self, trace: &'t impl Tracer<'s, C>) -> Self {
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
    pub struct ExpectTrack<'s, C: Code> {
        /// Function.
        pub func: C,
        /// Usage flag.
        pub usage: Usage,
        /// Collected Expect values.
        pub list: Vec<Expect<'s, C>>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// One per stack frame.
    pub struct SuggestTrack<'s, C: Code> {
        /// Function
        pub func: C,
        /// Usage flag.
        pub usage: Usage,
        /// Collected Suggest values.
        pub list: Vec<Suggest<'s, C>>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Track for entering a parser function.
    pub struct EnterTrack<'s, C> {
        /// Function
        pub func: C,
        /// Span
        pub span: Span<'s>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Track for step information.
    pub struct StepTrack<'s, C> {
        /// Function
        pub func: C,
        /// Step info.
        pub step: &'static str,
        /// Span
        pub span: Span<'s>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Track for debug information.
    pub struct DebugTrack<'s, C> {
        /// Function.
        pub func: C,
        /// Debug info.
        pub dbg: String,
        /// Parser call stack.
        pub parents: Vec<C>,
        /// For the lifetime ...
        pub _phantom: PhantomData<Span<'s>>,
    }

    /// Track for ok results.
    pub struct OkTrack<'s, C> {
        /// Function.
        pub func: C,
        /// Span.
        pub span: Span<'s>,
        /// Remaining span.
        pub rest: Span<'s>,
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Track for err results.
    pub struct ErrTrack<'s, C> {
        /// Function.
        pub func: C,
        /// Span.
        pub span: Span<'s>,
        /// Error message.
        pub err: String, // TODO: check
        /// Parser call stack.
        pub parents: Vec<C>,
    }

    /// Track for exiting a parser function.
    pub struct ExitTrack<'s, C> {
        /// Function
        pub func: C,
        /// Parser call stack.
        pub parents: Vec<C>,
        /// For the lifetime ...
        pub _phantom: PhantomData<Span<'s>>,
    }

    /// One track of the parsing trace.
    #[allow(missing_docs)]
    pub enum Track<'s, C: Code> {
        Enter(EnterTrack<'s, C>),
        Step(StepTrack<'s, C>),
        Debug(DebugTrack<'s, C>),
        Expect(ExpectTrack<'s, C>),
        Suggest(SuggestTrack<'s, C>),
        Ok(OkTrack<'s, C>),
        Err(ErrTrack<'s, C>),
        Exit(ExitTrack<'s, C>),
    }

    impl<'s, C: Code> Track<'s, C> {
        /// Returns the func value for each branch.
        pub fn func(&self) -> C {
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
        pub fn parents(&self) -> &Vec<C> {
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

    mod debug {
        use crate::iparse::error::DebugWidth;
        use crate::iparse::tracer::{
            CTracer, DebugTrack, EnterTrack, ErrTrack, ExitTrack, ExpectTrack, OkTrack, StepTrack,
            SuggestTrack, Track,
        };
        use crate::iparse::{Code, FilterFn};
        use std::fmt;
        use std::fmt::Formatter;

        fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
            write!(f, "{}", " ".repeat(ind * 2))?;
            Ok(())
        }

        pub(crate) fn debug_tracer<'a, 'b, 's, C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            trace: &'a CTracer<'s, C>,
            filter: FilterFn<'b, 's, C>,
        ) -> fmt::Result {
            let mut ind = 0;

            writeln!(f, "trace")?;

            for t in &*trace.track.borrow() {
                match t {
                    Track::Enter(_) => {
                        if filter(t) {
                            ind += 1;
                            indent(f, ind)?;
                            debug_track(f, w, t)?;
                            writeln!(f)?;
                        }
                    }
                    Track::Step(_)
                    | Track::Debug(_)
                    | Track::Expect(_)
                    | Track::Suggest(_)
                    | Track::Ok(_)
                    | Track::Err(_) => {
                        if filter(t) {
                            indent(f, ind)?;
                            debug_track(f, w, t)?;
                            writeln!(f)?;
                        }
                    }
                    Track::Exit(_) => {
                        if filter(t) {
                            // indent(f, ind)?;
                            // debug_track(f, w, t)?;
                            // writeln!(f)?;
                            ind -= 1;
                        }
                    }
                }
            }

            if !trace.func.borrow().is_empty() {
                write!(f, "    func=")?;
                for func in &*trace.func.borrow() {
                    write!(f, "{:?} ", func)?;
                }
                writeln!(f)?;
            }

            if !trace.expect.borrow().is_empty() {
                write!(f, "    expect=")?;
                for exp in &*trace.expect.borrow() {
                    writeln!(f, "{}: {:?}", exp.func, exp.list)?;
                }
                writeln!(f)?;
            }

            if !trace.suggest.borrow().is_empty() {
                write!(f, "    suggest=")?;
                for sug in &*trace.suggest.borrow() {
                    writeln!(f, "{}: {:?}", sug.func, sug.list)?;
                }
                writeln!(f)?;
            }

            Ok(())
        }

        fn debug_track<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &Track<'_, C>,
        ) -> fmt::Result {
            match v {
                Track::Enter(v) => debug_enter(f, w, v),
                Track::Step(v) => debug_step(f, w, v),
                Track::Debug(v) => debug_debug(f, w, v),
                Track::Expect(v) => debug_expect(f, w, v),
                Track::Suggest(v) => debug_suggest(f, w, v),
                Track::Ok(v) => debug_ok(f, w, v),
                Track::Err(v) => debug_err(f, w, v),
                Track::Exit(v) => debug_exit(f, w, v),
            }
        }

        fn debug_enter<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &EnterTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium => {
                    write!(f, "{}: parse \"{}\"", v.func, v.span)
                }
                DebugWidth::Long => write!(f, "{}: parse \"{}\" <<{:?}", v.func, v.span, v.parents),
            }
        }

        fn debug_step<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &StepTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium => {
                    write!(f, "{}: {} \"{}\"", v.func, v.step, v.span)
                }
                DebugWidth::Long => {
                    write!(f, "{}: {} \"{}\" <<{:?}", v.func, v.step, v.span, v.parents)
                }
            }
        }

        fn debug_debug<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &DebugTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: {}", v.func, v.dbg),
                DebugWidth::Long => write!(f, "{}: {} <<{:?}", v.func, v.dbg, v.parents),
            }
        }

        fn debug_expect<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &ExpectTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
                DebugWidth::Medium => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
                DebugWidth::Long => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
            }
        }

        fn debug_suggest<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &SuggestTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
                DebugWidth::Medium => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
                DebugWidth::Long => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
            }
        }
        fn debug_ok<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &OkTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium | DebugWidth::Long => {
                    if !v.span.is_empty() {
                        write!(f, "{}: -> [ {}, '{}' ]", v.func, v.span, v.rest)?;
                    } else {
                        write!(f, "{}: -> no match", v.func)?;
                    }
                }
            }
            Ok(())
        }

        fn debug_err<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &ErrTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: {} ", v.func, v.err),
                DebugWidth::Long => write!(f, "{}: {} <<{:?}", v.func, v.err, v.parents),
            }
        }

        fn debug_exit<C: Code>(
            f: &mut Formatter<'_>,
            w: DebugWidth,
            v: &ExitTrack<'_, C>,
        ) -> fmt::Result {
            match w {
                DebugWidth::Short | DebugWidth::Medium | DebugWidth::Long => {
                    write!(f, "return {}: ", v.func)
                }
            }
        }
    }
}
