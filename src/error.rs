//!
//! Contains the ParseOFError and related structs.
//!

mod debug_error;

use crate::ast::conv::{ParseColnameError, ParseRownameError};
use crate::error::OFCode::*;
use crate::iparse::error::ParserError;
use crate::iparse::{Code, Span};
use std::fmt::{Debug, Display, Formatter};

/// Standard parser error.
pub type OFParserError<'s> = ParserError<'s, OFCode>;

// Simple mappings
#[allow(missing_docs)]
impl<'s> OFParserError<'s> {
    pub fn parens(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCParentheses, span)
    }

    pub fn fn_call(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCFnCall, span)
    }

    pub fn elementary(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCElementary, span)
    }

    pub fn string_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCStringOp, span)
    }

    pub fn ref_intersect_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCRefIntersectOp, span)
    }

    pub fn ref_concat_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCRefConcatOp, span)
    }

    pub fn ref_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCRefOp, span)
    }

    pub fn identifier(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCIdentifier, span)
    }

    pub fn start_quote(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCQuoteStart, span)
    }

    pub fn end_quote(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCQuoteEnd, span)
    }

    pub fn start_single_quote(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCSingleQuoteStart, span)
    }

    pub fn end_single_quote(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCSingleQuoteEnd, span)
    }

    pub fn reference(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCReference, span)
    }

    pub fn iri(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCIri, span)
    }

    pub fn sheet_name(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCSheetName, span)
    }

    pub fn hashtag(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCHashtag, span)
    }

    pub fn dot(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCDot, span)
    }

    pub fn parens_open(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCParenthesesOpen, span)
    }

    pub fn parens_close(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCParenthesesClose, span)
    }

    pub fn brackets_open(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCBracketsOpen, span)
    }

    pub fn brackets_close(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCBracketsClose, span)
    }

    pub fn semikolon(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCSemikolon, span)
    }

    pub fn cell_range(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCCellRange, span)
    }

    pub fn col_range(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCColRange, span)
    }

    pub fn row_range(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCRowRange, span)
    }

    pub fn string(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCString, span)
    }

    pub fn number(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCNumber, span)
    }

    pub fn fn_name(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCFnName, span)
    }

    pub fn comp_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCCompOp, span)
    }

    pub fn prefix_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCPrefixOp, span)
    }

    pub fn postfix_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCPostfixOp, span)
    }

    pub fn add_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCAddOp, span)
    }

    pub fn mul_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCMulOp, span)
    }

    pub fn pow_op(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCPowOp, span)
    }

    pub fn dollar(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCDollar, span)
    }

    pub fn dollardollar(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCDollarDollar, span)
    }

    pub fn single_quoted(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCSingleQuoted, span)
    }

    pub fn alpha(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCAlpha, span)
    }

    pub fn col(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCCol, span)
    }

    pub fn row(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCRow, span)
    }

    pub fn digit(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCDigit, span)
    }

    pub fn colon(span: Span<'s>) -> OFParserError<'s> {
        OFParserError::new(OFCColon, span)
    }
}

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

impl Code for OFCode {
    const DEFAULT: Self = OFCDefault;
    const NOM_ERROR: Self = OFCNomError;
    const NOM_FAILURE: Self = OFCNomFailure;
    const PARSE_INCOMPLETE: Self = OFCParseIncomplete;
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
        write!(f, "{}", code)?;
        Ok(())
    }
}

// TODO: better name
/// Adds a span as location and converts the error to our own type..
pub trait LocateError<'s, T, E> {
    /// Maps some error and adds the information of the span where the error occured.
    fn locate_err(self, span: Span<'s>) -> Result<T, OFParserError<'s>>;
}

impl<'s, T> LocateError<'s, T, ParseRownameError> for Result<T, ParseRownameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, OFParserError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(OFParserError::new(OFCRowname, span)),
        }
    }
}

impl<'s, T> LocateError<'s, T, ParseColnameError> for Result<T, ParseColnameError> {
    fn locate_err(self, span: Span<'s>) -> Result<T, OFParserError<'s>> {
        match self {
            Ok(v) => Ok(v),
            Err(_) => Err(OFParserError::new(OFCColname, span)),
        }
    }
}
