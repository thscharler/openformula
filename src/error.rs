use crate::ast::conv::{ParseColnameError, ParseRownameError};
use crate::ast::Span;
use crate::error::OFCode::*;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};

pub struct ParseOFError<'s> {
    pub code: OFCode,
    pub span: Span<'s>,
    pub expect: Option<Vec<Expect<'s>>>,
    pub suggest: Option<Suggest<'s>>,
}

impl<'s> ParseOFError<'s> {
    pub fn new(code: OFCode, span: Span<'s>) -> Self {
        Self {
            code,
            span,
            expect: None,
            suggest: None,
        }
    }

    pub fn is_special(&self) -> bool {
        self.code.is_special()
    }

    pub fn is_parser(&self) -> bool {
        !self.code.is_special()
    }

    /// Create a ParseOFError from a nom::Err
    pub fn nom(e: nom::Err<nom::error::Error<Span<'s>>>) -> ParseOFError<'s> {
        match e {
            nom::Err::Error(e) => ParseOFError::new(OFCNomError, e.input),
            nom::Err::Failure(e) => ParseOFError::new(OFCNomFailure, e.input),
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }

    /// ParseIncomplete variant.
    pub fn parse_incomplete(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParseIncomplete, span)
    }
}

// Simple mappings
impl<'s> ParseOFError<'s> {
    pub fn parens(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParentheses, span)
    }

    pub fn fn_call(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCFnCall, span)
    }

    pub fn elementary(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCElementary, span)
    }

    pub fn string_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCStringOp, span)
    }

    pub fn ref_intersect_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefIntersectOp, span)
    }

    pub fn ref_concat_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefConcatOp, span)
    }

    pub fn ref_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRefOp, span)
    }

    pub fn identifier(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCIdentifier, span)
    }

    pub fn start_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCQuoteStart, span)
    }

    pub fn end_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCQuoteEnd, span)
    }

    pub fn start_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoteStart, span)
    }

    pub fn end_single_quote(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoteEnd, span)
    }

    pub fn reference(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCReference, span)
    }

    pub fn iri(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCIri, span)
    }

    pub fn sheet_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSheetName, span)
    }

    pub fn hashtag(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCHashtag, span)
    }

    pub fn dot(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDot, span)
    }

    pub fn parens_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParenthesesOpen, span)
    }

    pub fn parens_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCParenthesesClose, span)
    }

    pub fn brackets_open(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCBracketsOpen, span)
    }

    pub fn brackets_close(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCBracketsClose, span)
    }

    pub fn semikolon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSemikolon, span)
    }

    pub fn cell_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCellRange, span)
    }

    pub fn col_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCColRange, span)
    }

    pub fn row_range(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRowRange, span)
    }

    pub fn string(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCString, span)
    }

    pub fn number(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCNumber, span)
    }

    pub fn fn_name(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCFnName, span)
    }

    pub fn comp_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCompOp, span)
    }

    pub fn prefix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPrefixOp, span)
    }

    pub fn postfix_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPostfixOp, span)
    }

    pub fn add_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCAddOp, span)
    }

    pub fn mul_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCMulOp, span)
    }

    pub fn pow_op(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCPowOp, span)
    }

    pub fn dollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDollar, span)
    }

    pub fn dollardollar(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDollarDollar, span)
    }

    pub fn single_quoted(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCSingleQuoted, span)
    }

    pub fn alpha(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCAlpha, span)
    }

    pub fn col(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCCol, span)
    }

    pub fn row(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCRow, span)
    }

    pub fn digit(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCDigit, span)
    }

    pub fn colon(span: Span<'s>) -> ParseOFError<'s> {
        ParseOFError::new(OFCColon, span)
    }
}

impl<'s> Debug for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "ParseOFError: {} for \"{}\"", self.code, self.span)?;
        write!(f, "[")?;
        if let Some(expect) = &self.expect {
            for expect in expect {
                write!(f, "{} ", expect)?;
            }
        }
        write!(f, "]")?;
        writeln!(f)?;
        writeln!(f)?;
        if let Some(suggest) = &self.suggest {
            writeln!(f, "{:?}", suggest)?;
            suggest.write_depth_1(f, self.span)?;
        }
        writeln!(f)?;
        writeln!(f)?;
        if let Some(suggest) = &self.suggest {
            writeln!(f, "{:?}", suggest)?;
            suggest.write_depth_0(f, self.span)?;
        }
        Ok(())
    }
}

impl<'s> Display for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.code)?;
        if let Some(expect) = &self.expect {
            write!(f, "[")?;
            for expect in expect {
                write!(f, "{} ", expect.expect)?;
            }
            write!(f, "] ")?;
        }
        // no suggest
        write!(
            f,
            "for span={}::{}:{} '{}'",
            self.span.location_offset(),
            self.span.location_line(),
            self.span.get_column(),
            self.span.fragment()
        )?;
        Ok(())
    }
}

impl<'s> Error for ParseOFError<'s> {}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub enum OFCode {
    #[default]
    OFCDefault,

    /// Nom error.
    OFCNomError,
    /// Nom failure.
    OFCNomFailure,
    /// Parsing didn't parse all of the string.
    OFCParseIncomplete,

    OFCAdd,
    OFCAddOp,
    OFCAlpha,
    OFCBracketsClose,
    OFCBracketsOpen,
    OFCCellRange,
    OFCCellRef,
    OFCCol,
    OFCColRange,
    OFCColname,
    OFCColon,
    OFCComp,
    OFCCompOp,
    OFCDigit,
    OFCDollar,
    OFCDollarDollar,
    OFCDot,
    OFCElementary,
    OFCExpr,
    OFCFnCall,
    OFCFnName,
    OFCHashtag,
    OFCIdentifier,
    OFCIri,
    OFCMul,
    OFCMulOp,
    OFCNamed,
    OFCNumber,
    OFCParentheses,
    OFCParenthesesClose,
    OFCParenthesesOpen,
    OFCPostfix,
    OFCPostfixOp,
    OFCPow,
    OFCPowOp,
    OFCPrefix,
    OFCPrefixOp,
    OFCQuoteEnd,
    OFCQuoteStart,
    OFCRefConcatOp,
    OFCRefIntersectOp,
    OFCRefOp,
    OFCReference,
    OFCRow,
    OFCRowRange,
    OFCRowname,
    OFCSemikolon,
    OFCSeparator,
    OFCSheetName,
    OFCSingleQuoteEnd,
    OFCSingleQuoteStart,
    OFCSingleQuoted,
    OFCString,
    OFCStringOp,
}

impl OFCode {
    pub fn is_special(&self) -> bool {
        match self {
            OFCDefault | OFCNomError | OFCNomFailure | OFCParseIncomplete => true,
            _ => false,
        }
    }
}

impl Display for OFCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        write!(buf, "{:?}", self)?;
        write!(f, "{}", buf.strip_prefix("OFC").unwrap())?;
        Ok(())
    }
}

/// Adds a span as location and converts the error to our own type..
pub trait LocateError<'s, T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>>;
}

impl<'s, T> LocateError<'s, T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(OFCRowname, span)),
        }
    }
}

impl<'s, T> LocateError<'s, T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, ParseOFError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(ParseOFError::new(OFCColname, span)),
        }
    }
}

#[derive(Clone, Copy)]
pub struct OFSpan<'s> {
    pub span: Span<'s>,
    pub code: OFCode,
}

impl<'s> Debug for OFSpan<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} \"{}\"", self.code, self.span)?;
        Ok(())
    }
}

impl<'s> Display for OFSpan<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} \"{}\"", self.code, self.span)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct Expect<'s> {
    pub func: OFCode,
    pub expect: OFSpan<'s>,
}

pub trait AddExpect<'s> {
    fn add_expect(&mut self, func: OFCode, span: Span<'s>, code: OFCode);
}

impl<'s> AddExpect<'s> for Vec<Expect<'s>> {
    fn add_expect(&mut self, func: OFCode, span: Span<'s>, code: OFCode) {
        // check the last one. if it's the same code in a different
        // function we can deduplicate.
        let duplicate = match self.last() {
            None => false,
            Some(expect) => expect.expect.code == code,
        };

        if !duplicate {
            self.push(Expect {
                func,
                expect: OFSpan { span, code },
            })
        }
    }
}

impl<'s> Display for Expect<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.func, self.expect)?;
        Ok(())
    }
}

impl<'s> Debug for Expect<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.expect.code, self.expect.span)?;
        Ok(())
    }
}

#[derive(Default, Clone)]
pub struct Suggest<'s> {
    pub func: OFCode,
    pub codes: Vec<OFSpan<'s>>,
    pub next: Vec<Suggest<'s>>,
}

// debug impl
impl<'s> Suggest<'s> {
    fn write_indent_depth_0(
        &self,
        f: &mut Formatter<'_>,
        full_span: Span<'s>,
        indent: u32,
    ) -> fmt::Result {
        for OFSpan { span, code } in &self.codes {
            dbg::span(f, full_span, indent)?;
            dbg::tick(f, span.location_offset(), indent)?;
            dbg::hint(f, span.location_offset(), *code, indent)?;
        }
        Ok(())
    }

    pub fn write_depth_1(&self, f: &mut Formatter<'_>, full_span: Span<'s>) -> fmt::Result {
        for suggest in &self.next {
            suggest.write_indent_depth_0(f, full_span, 0)?;
            writeln!(f)?;
        }
        Ok(())
    }

    pub fn write_depth_0(&self, f: &mut Formatter<'_>, full_span: Span<'s>) -> fmt::Result {
        self.write_indent_depth_0(f, full_span, 0)
    }

    fn dbg_suggest(f: &mut Formatter<'_>, suggest: &Suggest<'s>, indent: u32) -> fmt::Result {
        write!(f, "{} : ", suggest.func)?;
        for OFSpan { span, code } in &suggest.codes {
            write!(f, "{} {}:\"{}\" ", code, span.location_offset(), span)?;
        }

        for su in &suggest.next {
            writeln!(f)?;
            dbg::arrow(f, indent + 1)?;
            Self::dbg_suggest(f, su, indent + 1)?;
        }

        Ok(())
    }
}

impl<'s> Debug for Suggest<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::dbg_suggest(f, self, 0)?;
        Ok(())
    }
}

mod dbg {
    use crate::ast::Span;
    use crate::error::OFCode;
    use std::fmt;
    use std::fmt::Formatter;

    // pub struct FormatHint<'s> {
    //     pub base: &'s str,
    //     pub hints: Vec<(u32, String)>,
    // }
    //
    // impl<'s> Display for FormatHint<'s> {
    //     fn fmt(&mut self, f: &mut Formatter<'_>) -> fmt::Result {
    //         self.hints.sort();
    //
    //         writeln!(f, "{}", self.base)?;
    //
    //         let mut posmap = HashMap::new();
    //         for (i, (pos, hint)) in self.hints.iter().enumerate() {
    //             posmap.insert(pos as usize, (i, hint));
    //         }
    //
    //         for r in 0..self.hints.len() {
    //             for c in 0..self.base.len() {
    //                 match posmap.get(&c) {
    //                     None => write!(f, " ")?,
    //                     Some((in_r, hint)) => {
    //                         if r < *in_r {
    //                             write!(f, "|")?;
    //                         } else if r == *in_r {
    //                             write!(f, hint)?;
    //                         } else {
    //                             write!(f, " ")?;
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //
    //         Ok(())
    //     }
    // }

    pub fn indent(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
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

    pub fn span(f: &mut Formatter<'_>, span: Span<'_>, ind: u32) -> fmt::Result {
        indent(f, ind)?;
        write!(f, "{}", span)?;
        writeln!(f)?;
        Ok(())
    }

    pub fn tick(f: &mut Formatter<'_>, offset: usize, ind: u32) -> fmt::Result {
        indent(f, ind)?;
        for _ in 0..offset {
            write!(f, " ")?;
        }
        write!(f, "^")?;
        writeln!(f)?;
        Ok(())
    }

    pub fn hint(f: &mut Formatter<'_>, offset: usize, hint: OFCode, ind: u32) -> fmt::Result {
        indent(f, ind)?;
        for _ in 0..offset {
            write!(f, " ")?;
        }
        write!(f, "{}", hint)?;
        writeln!(f)?;
        Ok(())
    }
}
