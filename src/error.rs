//!
//! Contains the ParseOFError and related structs.
//!

mod debug_error;

use crate::ast::conv::{ParseColnameError, ParseRownameError};
use crate::ast::Span;
use crate::error::OFCode::*;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write};

/// Error for the Parser.
pub struct ParseOFError<'s> {
    /// Error code.
    pub code: OFCode,
    /// Error span.
    pub span: Span<'s>,
    /// Flag for Tracer.
    pub tracing: bool,
    /// Suggest values.
    pub suggest: Vec<Suggest<'s>>,
    /// Expect values.
    pub expect: Vec<Expect<'s>>,
}

impl<'s> ParseOFError<'s> {
    /// New error.
    pub fn new(code: OFCode, span: Span<'s>) -> Self {
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
    pub fn is_expected(&self, code: OFCode) -> bool {
        for exp in &self.expect {
            if exp.code == code {
                return true;
            }
        }
        false
    }

    /// Was this one of the expected errors, and is in the call stack of parent?
    pub fn is_expected2(&self, code: OFCode, parent: OFCode) -> bool {
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
#[allow(missing_docs)]
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

impl<'s> Display for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

impl<'s> Error for ParseOFError<'s> {}

#[allow(missing_docs)]
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
    /// Is a 'special' code.
    /// These are the general codes OFCDefault, OFCNomError, OFCNomFailure, OFCParseIncomplete.
    pub fn is_special(&self) -> bool {
        matches!(
            self,
            OFCDefault | OFCNomError | OFCNomFailure | OFCParseIncomplete
        )
    }
}

impl Display for OFCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let code = match self {
            OFCDefault => "Default",
            OFCNomError => "NomError",
            OFCNomFailure => "NomFailure",
            OFCParseIncomplete => "ParseIncomplete",
            OFCAdd => "Add",
            OFCAddOp => "AddOp",
            OFCAlpha => "Alpha",
            OFCBracketsClose => "BracketsClose",
            OFCBracketsOpen => "BracketsOpen",
            OFCCellRange => "CellRange",
            OFCCellRef => "CellRef",
            OFCCol => "Col",
            OFCColRange => "ColRange",
            OFCColname => "Colname",
            OFCColon => "Colon",
            OFCComp => "Comp",
            OFCCompOp => "CompOp",
            OFCDigit => "Digit",
            OFCDollar => "Dollar",
            OFCDollarDollar => "DollarDollar",
            OFCDot => "Dot",
            OFCElementary => "Elementary",
            OFCExpr => "Expr",
            OFCFnCall => "FnCall",
            OFCFnName => "FnName",
            OFCHashtag => "Hashtag",
            OFCIdentifier => "Identifier",
            OFCIri => "Iri",
            OFCMul => "Mul",
            OFCMulOp => "MulOp",
            OFCNamed => "Named",
            OFCNumber => "Number",
            OFCParentheses => "Parentheses",
            OFCParenthesesClose => "ParenthesesClose",
            OFCParenthesesOpen => "ParenthesesOpen",
            OFCPostfix => "Postfix",
            OFCPostfixOp => "PostfixOp",
            OFCPow => "Pow",
            OFCPowOp => "PowOp",
            OFCPrefix => "Prefix",
            OFCPrefixOp => "PrefixOp",
            OFCQuoteEnd => "QuoteEnd",
            OFCQuoteStart => "QuoteStart",
            OFCRefConcatOp => "RefConcatOp",
            OFCRefIntersectOp => "RefIntersectOp",
            OFCRefOp => "RefOp",
            OFCReference => "Reference",
            OFCRow => "Row",
            OFCRowRange => "RowRange",
            OFCRowname => "Rowname",
            OFCSemikolon => "Semikolon",
            OFCSeparator => "Separator",
            OFCSheetName => "SheetName",
            OFCSingleQuoteEnd => "SingleQuoteEnd",
            OFCSingleQuoteStart => "SingleQuoteStart",
            OFCSingleQuoted => "SingleQuoted",
            OFCString => "String",
            OFCStringOp => "StringOp",
        };
        write!(buf, "{:?}", code)?;
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

/// Suggestions, optional tokens.
#[derive(Clone)]
pub struct Suggest<'s> {
    /// Code for the token.
    pub code: OFCode,
    /// Span
    pub span: Span<'s>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Expected tokens.
#[derive(Clone)]
pub struct Expect<'s> {
    /// Code for the token.
    pub code: OFCode,
    /// Span.
    pub span: Span<'s>,
    /// Parser call stack.
    pub parents: Vec<OFCode>,
}

/// Variants of the debug output.
/// Can be set by giving a width:
///
/// '''
///     write!(f, "{:2?}", out)?;
/// '''
///
///
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DebugWidth {
    /// Debug flag, can be set with width=0.
    Short,
    /// Debug flag, can be set with width=1.
    Medium,
    /// Debug flag, can be set with width=2.
    Long,
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
