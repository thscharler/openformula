use crate::refs::CellRef;
use crate::{CCol, CRow};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, char as nom_char, none_of, one_of};
use nom::combinator::{consumed, eof, opt, recognize};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, terminated, tuple};
use nom::{Err, IResult, Offset, Slice};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use std::rc::Rc;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type ParseResult<'a, O> = Result<(Span<'a>, O), ParseExprError<'a>>;

#[derive(Debug)]
pub enum ParseExprError<'a> {
    NomError(Span<'a>, nom::error::ErrorKind),
    NomFailure(Span<'a>, nom::error::ErrorKind),

    Backtracking(Tracking<'a>),
    Failure(Tracking<'a>),

    Number(Tracking<'a>),
    String(Tracking<'a>),
    CellRef(Tracking<'a>),
    Elementary(Tracking<'a>),

    Parenthesis(Tracking<'a>),

    PostfixOp(Tracking<'a>),
    PrefixOp(Tracking<'a>),
}

impl<'a> PartialEq for ParseExprError<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl<'a> Display for ParseExprError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl<'a> Error for ParseExprError<'a> {}

impl<'a> From<nom::Err<nom::error::Error<Span<'a>>>> for ParseExprError<'a> {
    fn from(e: Err<nom::error::Error<Span<'a>>>) -> Self {
        match e {
            Err::Error(e) => ParseExprError::NomError(e.input, e.code),
            Err::Failure(e) => ParseExprError::NomFailure(e.input, e.code),
            Err::Incomplete(_n) => unreachable!(),
        }
    }
}

pub struct ParseTracer<'trace, 'source> {
    prior: Option<&'trace ParseTracer<'trace, 'source>>,
    leafs: Rc<RefCell<Vec<String>>>,
    trace: &'trace str,
    fragment: Span<'source>,
}

impl<'trace, 'source> ParseTracer<'trace, 'source> {
    pub fn new(trace: &'trace str, fragment: Span<'source>) -> ParseTracer<'trace, 'source> {
        ParseTracer {
            prior: None,
            leafs: Rc::new(RefCell::new(Vec::new())),
            trace: trace.into(),
            fragment,
        }
    }

    pub fn push(
        &'trace self,
        trace: &'trace str,
        fragment: Span<'source>,
    ) -> ParseTracer<'trace, 'source> {
        println!("{} {}", trace, fragment);
        ParseTracer {
            prior: Some(self),
            leafs: Rc::clone(&self.leafs),
            trace: trace.into(),
            fragment,
        }
    }

    pub fn leaf(&self, leaf: &str) {
        let mut v = self.leafs.borrow_mut();
        v.push(leaf.to_string());
    }

    pub fn clear_leafs(&self) {
        let mut v = self.leafs.borrow_mut();
        v.clear();
    }

    pub fn collect(&'trace self) -> Tracking<'source> {
        self.i_collect(None)
    }

    pub fn collect_nom(
        &'trace self,
        nom: nom::Err<nom::error::Error<Span<'source>>>,
    ) -> Tracking<'source> {
        self.i_collect(Some(nom))
    }

    fn i_collect(
        &'trace self,
        nom: Option<nom::Err<nom::error::Error<Span<'source>>>>,
    ) -> Tracking<'source> {
        let mut c = Tracking {
            nom,
            trace: vec![],
            leafs: vec![],
        };

        let mut m = self;
        loop {
            for l in &*m.leafs.borrow() {
                c.leafs.push(l.clone());
            }
            c.trace.push((m.trace.to_string(), m.fragment));

            m = if let Some(prior) = m.prior {
                prior
            } else {
                break;
            };
        }

        c
    }
}

#[derive(Debug)]
pub struct Tracking<'source> {
    nom: Option<nom::Err<nom::error::Error<Span<'source>>>>,
    trace: Vec<(String, Span<'source>)>,
    leafs: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum ParseTree<'a> {
    Number(OFNumber<'a>),
    String(OFString<'a>),
    CellRef(OFCellRef<'a>),
    Parenthesis(Box<ParseTree<'a>>),
    PrefixOp(PrefixToken<'a>, Box<ParseTree<'a>>),
    InfixOp(Box<ParseTree<'a>>, InfixToken<'a>, Box<ParseTree<'a>>),
    PostfixOp(Box<ParseTree<'a>>, PostfixToken<'a>),
}

#[derive(Debug)]
pub struct OFNumber<'a>(f64, Span<'a>);

impl<'a> PartialEq for OFNumber<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
pub struct OFString<'a>(String, Span<'a>);

impl<'a> PartialEq for OFString<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
pub struct OFCellRef<'a>(CellRef, Span<'a>);

impl<'a> PartialEq for OFCellRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
pub enum InfixToken<'a> {
    InfixAdd(Span<'a>),
    InfixSubtract(Span<'a>),
    InfixMultiply(Span<'a>),
    InfixDivide(Span<'a>),
    InfixPower(Span<'a>),
}

impl<'a> PartialEq for InfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum PrefixToken<'a> {
    PrefixPlus(Span<'a>),
    PrefixMinus(Span<'a>),
}

impl<'a> PartialEq for PrefixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum PostfixToken<'a> {
    PostfixPercent(Span<'a>),
}

impl<'a> PartialEq for PostfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

pub fn parse<'a>(i: Span<'a>) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = ParseTracer::new("parse", i.clone());
    let (i, expr) = parse_expression(&trace, i)?;

    let trace = trace.push("eof", i);
    let eof: IResult<Span<'a>, Span<'a>> = eof(i);
    match eof {
        Ok((i, _eof)) => Ok((i, expr)),
        Err(e @ nom::Err::Error(_)) => Err(ParseExprError::Backtracking(trace.collect_nom(e))),
        Err(e @ nom::Err::Failure(_)) => Err(ParseExprError::Failure(trace.collect_nom(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

// Expression ::=
// Whitespace* (
// Array |
// FunctionName '(' ParameterList ')' |
// Reference |
// QuotedLabel |
// AutomaticIntersection |
// NamedExpression |
// Error
// ) Whitespace*
// SingleQuoted ::= "'" ([^'] | "''")+ "'"
//
//
// Number |
// String |
// PrefixOp Expression |
// Expression InfixOp Expression |
// Expression PostfixOp |
// '(' Expression ')' |

pub fn parse_expression<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = trace.push("parse_expression", i.clone());

    match parse_infix(&trace, i) {
        Ok((i, o)) => Ok((i, o)),
        Err(e) => Err(e),
    }
}

pub fn parse_infix<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i0: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = trace.push("parse_infix", i0.clone());

    match parse_postfix(&trace, i0) {
        Ok((mut i1, mut exp0)) =>
        //
        {
            loop {
                match infix_op(&trace, i1) {
                    Ok((i2, Some(o))) => match parse_postfix(&trace, i2) {
                        Ok((i3, exp1)) => {
                            i1 = i3;
                            exp0 = Box::new(ParseTree::InfixOp(exp0, o, exp1));
                        }
                        Err(e) => break Err(e),
                    },
                    Ok((i2, None)) => break Ok((i2, exp0)),
                    Err(e) => break Err(e),
                }
            }
        }
        Err(e) => Err(e),
    }
}

pub fn parse_postfix<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i0: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = trace.push("parse_postfix", i0.clone());

    match parse_prefix(&trace, i0) {
        Ok((mut i1, mut expr)) =>
        //
        {
            loop {
                match postfix_op(&trace, i1) {
                    Ok((i2, Some(tok))) => {
                        i1 = i2;
                        expr = Box::new(ParseTree::PostfixOp(expr, tok));
                    }
                    Ok((i2, None)) => break Ok((i2, expr)),
                    Err(e) => break Err(e),
                }
            }
        }
        Err(e) => Err(e),
    }
}

pub fn parse_prefix<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = trace.push("parse_prefix", i.clone());

    match prefix_op(&trace, i) {
        Ok((i, Some(prefix))) => match parse_prefix(&trace, i) {
            Ok((i, expr)) => Ok((i, Box::new(ParseTree::PrefixOp(prefix, expr)))),
            Err(e) => Err(e),
        },
        Ok((i, None)) => match parse_elementary(&trace, i) {
            Ok((i, expr)) => Ok((i, expr)),
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    }
}

// Number |
// '(' Expression ')' |
pub fn parse_elementary<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    let trace = trace.push("parse_elementary", i.clone());

    match opt_number(&trace, i) {
        Ok((i, Some(o))) => {
            return Ok((i, Box::new(ParseTree::Number(o))));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    match opt_string(&trace, i) {
        Ok((i, Some(o))) => {
            return Ok((i, Box::new(ParseTree::String(o))));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    match cellref(&trace, i) {
        Ok((i, o)) => return Ok((i, Box::new(ParseTree::CellRef(o)))),
        Err(_) => { /* skip */ }
    }

    trace.leaf("(");
    match parenthesis_open(i) {
        Ok((i, _)) => {
            trace.clear_leafs();

            let (i, expr) = parse_expression(&trace, i)?;

            trace.leaf(")");
            match parenthesis_close(i) {
                Ok((i, _)) => {
                    trace.clear_leafs();
                    return Ok((i, Box::new(ParseTree::Parenthesis(expr))));
                }
                Err(e) => return Err(ParseExprError::Parenthesis(trace.collect_nom(e))),
            }
        }
        Err(nom::Err::Error(_)) => { /* skip */ }
        Err(e @ nom::Err::Failure(_)) => {
            return Err(ParseExprError::Parenthesis(trace.collect_nom(e)))
        }
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }

    match parse_expression(&trace, i) {
        Ok((i, o)) => {
            return Ok((i, o));
        }
        Err(ParseExprError::Backtracking(_)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    Err(ParseExprError::Elementary(trace.collect()))
}

/// Parse a cell reference.
pub fn cellref<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    input: Span<'a>,
) -> ParseResult<'a, OFCellRef<'a>> {
    trace.leaf("CELLREF");

    let i = input.clone();

    let (i, _) = match brackets_open(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, iri) = match opt(iri)(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, table) = match opt(sheetname)(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, _) = match dot(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, col) = match column(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, row) = match row(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };
    let (i, _) = match brackets_close(i) {
        Ok((i, o)) => (i, o),
        Err(e) => return Err(ParseExprError::CellRef(trace.collect_nom(e))),
    };

    trace.clear_leafs();

    let index = input.offset(&i);
    let p = input.slice(..index);

    Ok((
        i,
        OFCellRef(
            CellRef {
                iri: iri.map(|v| v.to_string()),
                table: table.map(|v| v.to_string()),
                cell: (col.0, row.0).into(),
            },
            p,
        ),
    ))
}

/// Parse a sheetname.
pub fn sheetname<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    let (i, _) = opt(tag("$"))(i)?;
    let (i, name) = alt((quoted('\''), recognize(many1(none_of("]. #$'")))))(i)?;

    Ok((i, name))
}

/// IRI ... i18n something
pub fn iri<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    let (i, iri) = terminated(quoted('\''), tag("#"))(i)?;
    Ok((i, iri))
}

/// Parse a col label.
pub fn column<'a>(i: Span<'a>) -> IResult<Span<'a>, (CCol, Span<'a>)> {
    let (i, (p, (abs, col))) = consumed(tuple((opt(tag("$")), alpha1)))(i)?;

    let mut ccol = match CCol::try_from(*col.fragment()) {
        Ok(v) => v,
        Err(_) => {
            return Err(Err::Error(nom::error::Error::new(
                p,
                nom::error::ErrorKind::Char,
            )))
        }
    };
    if abs.is_some() {
        ccol.set_col_abs(true);
    }

    Ok((i, (ccol, p)))
}

/// Parse a row label.
pub fn row<'a>(i: Span<'a>) -> IResult<Span<'a>, (CRow, Span<'a>)> {
    let (i, (p, (abs, row))) = consumed(tuple((
        opt(tag("$")),
        recognize(many1(one_of("0123456789"))),
    )))(i)?;

    let mut crow = match CRow::try_from(*row.fragment()) {
        Ok(v) => v,
        Err(_) => {
            return Err(Err::Error(nom::error::Error::new(
                p,
                nom::error::ErrorKind::Char,
            )))
        }
    };
    if abs.is_some() {
        crow.set_row_abs(true);
    }
    Ok((i, (crow, p)))
}

pub fn opt_number<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<OFNumber<'a>>> {
    trace.leaf("NUMBER");

    match opt(pn_number)(i) {
        Ok((i, Some(o))) => {
            trace.clear_leafs();
            match (*o).parse::<f64>() {
                Ok(v) => Ok((i, Some(OFNumber(v, o)))),
                Err(_) => unreachable!(),
            }
        }
        Ok((i, None)) => {
            trace.clear_leafs();
            Ok((i, None))
        }
        Err(e) => Err(ParseExprError::Number(trace.collect_nom(e))),
    }
}

/// Standard number.
///
/// Number ::= StandardNumber |
/// '.' [0-9]+ ([eE] [-+]? [0-9]+)?
/// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
pub fn pn_number<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    alt((
        // Case one: .42
        recognize(tuple((
            nom_char('.'),
            decimal,
            opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
        ))),
        // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(tuple((nom_char('.'), opt(decimal)))),
            one_of("eE"),
            opt(one_of("+-")),
            decimal,
        ))),
        // Case three: 42 and 42. and 42.42
        recognize(tuple((
            decimal, //
            opt(tuple((
                nom_char('.'), //
                opt(decimal),
            ))), //
        ))),
    ))(i)
}

///
/// Any number of digits.
///
pub fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(recognize(many1(one_of("0123456789")))(input)?)
}

pub fn opt_string<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<OFString<'a>>> {
    trace.leaf("STRING");

    match opt(pn_string)(i) {
        Ok((i, Some(o))) => {
            trace.clear_leafs();
            Ok((i, Some(OFString(o.to_string(), o))))
        }
        Ok((i, None)) => {
            trace.clear_leafs();
            Ok((i, None))
        }
        Err(e) => Err(ParseExprError::String(trace.collect_nom(e))),
    }
}

pub fn pn_string<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(quoted('"')(i)?)
}

pub fn prefix_op<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<PrefixToken<'a>>> {
    trace.leaf("+");
    trace.leaf("-");

    let exp: IResult<Span<'a>, (Span<'a>, Option<Span<'a>>)> =
        consumed(opt(alt((tag("+"), tag("-")))))(i);

    match exp {
        Ok((i, (p, Some(tok)))) => {
            trace.clear_leafs();
            match *tok.fragment() {
                "+" => Ok((i, Some(PrefixToken::PrefixPlus(p)))),
                "-" => Ok((i, Some(PrefixToken::PrefixMinus(p)))),
                _ => unreachable!(),
            }
        }
        Ok((i, (_, None))) => {
            trace.clear_leafs();
            Ok((i, None))
        }
        Err(e @ nom::Err::Error(_)) => Err(ParseExprError::PrefixOp(trace.collect_nom(e))),
        Err(e @ nom::Err::Failure(_)) => Err(ParseExprError::Failure(trace.collect_nom(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn postfix_op<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<PostfixToken<'a>>> {
    trace.leaf("%");

    let exp: IResult<Span<'a>, (Span<'a>, Option<Span<'a>>)> = consumed(opt(tag("%")))(i);
    match exp {
        Ok((i, (p, Some(tok)))) => {
            trace.clear_leafs();
            match *tok.fragment() {
                "%" => Ok((i, Some(PostfixToken::PostfixPercent(p)))),
                _ => unreachable!(),
            }
        }
        Ok((i, (_, None))) => {
            trace.clear_leafs();
            Ok((i, None))
        }
        Err(e @ nom::Err::Error(_)) => Err(ParseExprError::PostfixOp(trace.collect_nom(e))),
        Err(e @ nom::Err::Failure(_)) => Err(ParseExprError::Failure(trace.collect_nom(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn infix_op<'a, 'b>(
    trace: &'b ParseTracer<'b, 'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<InfixToken<'a>>> {
    trace.leaf("+");
    trace.leaf("-");
    trace.leaf("*");
    trace.leaf("/");
    trace.leaf("^");

    let exp: IResult<Span<'a>, Option<Span<'a>>> =
        opt(alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("^"))))(i);
    match exp {
        Ok((i, Some(tok))) => {
            trace.clear_leafs();
            match *tok {
                "+" => Ok((i, Some(InfixToken::InfixAdd(tok)))),
                "-" => Ok((i, Some(InfixToken::InfixSubtract(tok)))),
                "*" => Ok((i, Some(InfixToken::InfixMultiply(tok)))),
                "/" => Ok((i, Some(InfixToken::InfixDivide(tok)))),
                "^" => Ok((i, Some(InfixToken::InfixPower(tok)))),
                _ => unreachable!(),
            }
        }
        Ok((i, None)) => Ok((i, None)),
        Err(e @ nom::Err::Error(_)) => Err(ParseExprError::Backtracking(trace.collect_nom(e))),
        Err(e @ nom::Err::Failure(_)) => Err(ParseExprError::Failure(trace.collect_nom(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn comparison_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(alt((
        tag("="),
        tag("<>"),
        tag("<"),
        tag(">"),
        tag("<="),
        tag(">="),
    ))(i)?)
}

pub fn string_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(tag("&")(i)?)
}

pub fn reference_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(tag("&")(i)?)
}

pub fn ref_intersection_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(tag("!")(i)?)
}

pub fn ref_concatenation_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(tag("~")(i)?)
}

pub fn separator<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    Ok(tag(";")(i)?)
}

/// Parse "."
pub fn dot<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(".")(i)
}

pub fn parenthesis_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("(")(i)
}

pub fn parenthesis_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag(")")(i)
}

pub fn brackets_open<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("[")(i)
}

pub fn brackets_close<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    tag("]")(i)
}

/// Parse a quoted string. A double quote within is an escaped quote.
/// Returns the string within the outer quotes. The double quotes are not
/// reduced.
pub fn quoted<'a>(quote: char) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    move |i| {
        let (i, r) = delimited(
            nom_char(quote),
            recognize(many0(alt((
                take_while1(|v| v != quote),
                recognize(count(nom_char(quote), 2)),
            )))),
            nom_char(quote),
        )(i)?;

        Ok((i, r))
    }
}

#[allow(unused_imports)]
mod tests {
    use crate::parse2::parse_expression::{
        parse, parse_expression, ParseExprError, ParseResult, ParseTree, Span,
    };
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::char as nom_char;
    use nom::character::complete::one_of;
    use nom::combinator::{opt, recognize};
    use nom::error::ParseError;
    use nom::number::complete::double;
    use nom::sequence::{preceded, tuple};
    use nom::{
        AsBytes, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset,
        Slice,
    };
    use std::ops::{RangeFrom, RangeTo};

    fn doparse<'a>(str: &'a str) -> ParseResult<'a, Box<ParseTree<'a>>> {
        parse(Span::new(str))
    }

    fn assert_parse<'a, 'b>(
        str: &'a str,
        test_expr: Box<ParseTree<'b>>,
        test_err: ParseExprError<'b>,
    ) {
        match parse(Span::new(str)) {
            Ok((_i, expr)) => {
                assert_eq!(expr, test_expr);
            }
            Err(err) => {
                assert_eq!(err, test_err);
            }
        }
    }

    #[test]
    fn test_number() {
        let test_expr = ["25ex"];

        for e in test_expr {
            println!("{}", e);
            match parse(Span::new(e)) {
                Ok((_i, expr)) => {
                    println!("{:#?}", expr);
                }
                Err(e) => {
                    println!("{:#?}", e);
                }
            };
        }
    }

    #[test]
    fn test1() {
        let test_expr = ["--1"];

        for e in test_expr {
            println!("{}", e);
            match parse(Span::new(e)) {
                Ok((_i, expr)) => {
                    println!("{:#?}", expr);
                }
                Err(e) => {
                    println!("{:#?}", e);
                }
            };
        }
    }
}
