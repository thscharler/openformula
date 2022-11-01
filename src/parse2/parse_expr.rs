pub use coretypes::*;
use nom::combinator::opt;
use nom_locate::LocatedSpan;
use std::fmt::Debug;
use std::mem;
pub use tracer::*;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type ParseResult<'a, 'b, O> = Result<(Span<'a>, O), ParseExprError<'b>>;

#[derive(Debug)]
pub enum ParseTree<'a> {
    Number(OFNumber<'a>),
    String(OFString<'a>),
    Parenthesis(Box<ParseTree<'a>>),
    PrefixOp(PrefixToken<'a>, Box<ParseTree<'a>>),
    InfixOp(Box<ParseTree<'a>>, InfixToken<'a>, Box<ParseTree<'a>>),
    PostfixOp(Box<ParseTree<'a>>, PostfixToken<'a>),
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
pub enum PostfixToken<'a> {
    PostfixPercent(Span<'a>),
}

impl<'a> PartialEq for PostfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum ParseExprError<'span> {
    NomError(&'span Tracer<'span>),
    NomFailure(&'span Tracer<'span>),

    Number(&'span Tracer<'span>),
    String(&'span Tracer<'span>),

    Parenthesis(&'span Tracer<'span>),

    Elementary(&'span Tracer<'span>),
}

pub fn opt_number<'a, 'b>(
    trace: &'b Tracer<'b>,
    i: Span<'a>,
) -> ParseResult<'a, 'b, Option<OFNumber<'a>>> {
    trace.enter("opt_number", i);

    match opt(tokens::number)(i) {
        Ok((rest, Some(tok))) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Some(OFNumber(val, tok)))),
            Err(_) => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, None)),
        Err(e) => Err(ParseExprError::Number(trace.err(e))),
    }
}

pub fn opt_string<'a>(trace: &'a Tracer<'a>, i: Span<'a>) -> ParseResult<'a, Option<OFString<'a>>> {
    trace.enter("opt_string", i);

    match opt(tokens::string)(i) {
        Ok((rest, Some(tok))) => Ok(trace.ok(rest, Some(OFString(tok.to_string(), tok)))),
        Ok((rest, None)) => Ok(trace.ok(rest, None)),
        Err(e) => Err(ParseExprError::String(trace.err(e))),
    }
}

pub fn opt_expr_parentheses<'a>(
    trace: &'a Tracer<'a>,
    i: Span<'a>,
) -> ParseResult<'a, Option<Box<ParseTree<'a>>>> {
    trace.enter("opt_expr_parentheses", i);

    match tokens::parenthesis_open(i) {
        Ok((rest1, _tok)) => {
            let (rest2, expr) = parse_expr(&trace, rest1)?;
            return match tokens::parenthesis_close(rest2) {
                Ok((i, _tok)) => Ok(trace.ok(i, Some(Box::new(ParseTree::Parenthesis(expr))))),
                Err(e) => Err(ParseExprError::Parenthesis(trace.err(e))),
            };
        }
        Err(nom::Err::Error(_)) => return Ok(trace.ok(i, None)),
        Err(e @ nom::Err::Failure(_)) => return Err(ParseExprError::Parenthesis(trace.err(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn parse_expr<'a>(trace: &'a Tracer<'a>, i: Span<'a>) -> ParseResult<'a, Box<ParseTree<'a>>> {
    trace.enter("parse_expression", i);

    match parse_infix_expr(&trace, i) {
        Ok((rest, expr)) => Ok(trace.ok(rest, expr)),
        Err(e) => Err(e),
    }
}

pub fn parse_infix_expr<'a>(
    trace: &'a Tracer<'a>,
    i: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    trace.enter("parse_infix", i);

    match parse_postfix_expr(&trace, i) {
        Ok((mut rest1, mut expr1)) => loop {
            match tokens::infix_op_mapped(rest1) {
                Ok((rest2, Some(tok))) => match parse_postfix_expr(trace, rest2) {
                    Ok((rest3, expr2)) => {
                        rest1 = rest3;
                        expr1 = Box::new(ParseTree::InfixOp(expr1, tok, expr2));
                    }
                    Err(e) => break Err(e),
                },
                Ok((rest2, None)) => {
                    break Ok(trace.ok(rest2, expr1));
                }
                Err(e @ nom::Err::Error(_)) => return Err(ParseExprError::NomError(trace.err(e))),
                Err(e @ nom::Err::Failure(_)) => {
                    return Err(ParseExprError::NomFailure(trace.err(e)))
                }
                Err(nom::Err::Incomplete(_)) => unreachable!(),
            }
        },
        Err(e) => Err(e),
    }
}

pub fn parse_postfix_expr<'a>(
    trace: &'a Tracer<'a>,
    i0: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    trace.enter("parse_postfix", i0);

    match parse_prefix(&trace, i0) {
        Ok((mut rest1, mut expr)) => loop {
            match tokens::postfix_op_mapped(rest1) {
                Ok((rest2, Some(tok))) => {
                    rest1 = rest2;
                    expr = Box::new(ParseTree::PostfixOp(expr, tok));
                }
                Ok((i2, None)) => break Ok(trace.ok(i2, expr)),
                Err(e @ nom::Err::Error(_)) => break Err(ParseExprError::NomError(trace.err(e))),
                Err(e @ nom::Err::Failure(_)) => {
                    break Err(ParseExprError::NomFailure(trace.err(e)))
                }
                Err(nom::Err::Incomplete(_)) => unreachable!(),
            }
        },
        Err(e) => Err(e),
    }
}

pub fn parse_prefix<'a>(trace: &'a Tracer<'a>, i: Span<'a>) -> ParseResult<'a, Box<ParseTree<'a>>> {
    trace.enter("parse_prefix", i);

    match tokens::prefix_op_mapped(i) {
        Ok((rest1, Some(prefix))) => match parse_prefix(&trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, Box::new(ParseTree::PrefixOp(prefix, expr)))),
            Err(e) => Err(e),
        },
        Ok((rest1, None)) => match parse_elementary(&trace, rest1) {
            Ok((rest2, expr)) => Ok(trace.ok(rest2, expr)),
            Err(e) => Err(e),
        },
        Err(e @ nom::Err::Error(_)) => Err(ParseExprError::NomError(trace.err(e))),
        Err(e @ nom::Err::Failure(_)) => Err(ParseExprError::NomFailure(trace.err(e))),
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

// Number |
// '(' Expression ')' |
pub fn parse_elementary<'a, 'b>(
    trace: &'b Tracer<'b>,
    i: Span<'a>,
) -> ParseResult<'a, Box<ParseTree<'a>>> {
    trace.enter("parse_elementary", i);

    match opt_number(&trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, Box::new(ParseTree::Number(tok))));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    match opt_string(&trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, Box::new(ParseTree::String(tok))));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    // match opt_cellref(&trace, i) {
    //     Ok((i, Some(o))) => {
    //         trace.clear_leafs();
    //         return Ok((i, Box::new(ParseTree::CellRef(o))));
    //     }
    //     Ok((_, None)) => { /* skip */ }
    //     Err(e) => return Err(e),
    // }

    match opt_expr_parentheses(&trace, i) {
        Ok((rest, Some(tok))) => {
            return Ok(trace.ok(rest, tok));
        }
        Ok((_, None)) => { /* skip */ }
        Err(e) => return Err(e),
    }

    // match parse_expr(&trace, i) {
    //     Ok((rest, tok)) => {
    //         return Ok(trace.ok(rest, tok));
    //     }
    //     Err(e) => return Err(e),
    // }

    Err(ParseExprError::Elementary(trace))
}

mod tests {
    use crate::parse2::parse_expr::{parse_elementary, ParseResult, ParseTree, Span, Tracer};
    use nom_locate::LocatedSpan;

    fn run_test<'b>(
        str: &'b str,
        testfn: for<'a> fn(&'a Tracer<'a>, Span<'b>) -> ParseResult<'a, Box<ParseTree<'b>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, tok)) => {
                    dbg!(rest);
                    dbg!(tok);
                }
                Err(e) => {
                    dbg!(e);
                }
            }
            dbg!(&tracer);
        }
    }

    #[test]
    fn test_expr() {
        let tests = ["471", r#""strdata""#, "1+1"];
        for test in tests {
            run_test(test, parse_elementary);
        }
    }

    #[test]
    fn test_number() {
        // let test_ok = ["25", "25e+5", "25.", "25.001", "25.003e-7"];
        //
        // for e in test_ok {
        //     println!("{}", e);
        //     let mut tracer = Tracer::new();
        //     match opt_number(&mut tracer, Span::new(e)) {
        //         Ok((_i, expr)) => {
        //             println!("{:#?}", expr);
        //         }
        //         Err(e) => {
        //             println!("{:#?}", e);
        //         }
        //     };
        // }

        // let test_err = ["invalid", "2x5", "25ex+5", "25.x", "25.x001", "25x.003e-7"];
        //
        // for e in test_err {
        //     println!("{}", e);
        //     let tracer = Tracer::new();
        //
        //     dbg!(opt_number(&tracer, Span::new(e)).unwrap());
        //
        //     dbg!(&tracer);
        //
        //     // match opt_number(&mut tracer, Span::new(e)) {
        //     //     Ok((_i, expr)) => {
        //     //         println!("{:#?}", expr);
        //     //     }
        //     //     Err(e) => {
        //     //         println!("{:#?}", e);
        //     //     }
        //     // };
        // }
    }
}

mod tracer {
    use crate::parse2::parse_expr::Span;
    use nom::Err;
    use std::cell::RefCell;
    use std::fmt::{Debug, Formatter};

    /// Follows the parsing.
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
                    Track::Track(_, _) => {
                        indent.push_str("  ");
                        writeln!(f, "{}{:?}", indent, tr)?;
                    }
                    Track::Ok(_, _) => {
                        writeln!(f, "{}{:?}", indent, tr)?;
                        indent.pop();
                        indent.pop();
                    }
                    Track::Error(_, _) => {
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
            Self {
                tracks: Default::default(),
                func: Default::default(),
            }
        }

        /// Entering a parser.
        pub fn enter(&self, func: &'static str, span: Span<'span>) {
            self.func.borrow_mut().push(func);
            self.tracks.borrow_mut().push(Track::Track(func, span));
        }

        /// Ok in a parser.
        pub fn ok<T>(&self, rest: Span<'span>, val: T) -> (Span<'span>, T) {
            let func = self.func.borrow_mut().pop().unwrap();
            self.tracks.borrow_mut().push(Track::Ok(func, rest));
            (rest, val)
        }

        /// Erring in a parser. Assumes the error type contains a clone of this Tracer which is returned.
        pub fn err<'a>(
            &'a self,
            err: nom::Err<nom::error::Error<Span<'span>>>,
        ) -> &'a Tracer<'span> {
            let func = self.func.borrow_mut().pop().unwrap();
            self.tracks.borrow_mut().push(Track::Error(func, err));
            self
        }
    }

    /// One track of the parsing trace.
    pub enum Track<'span> {
        /// Function where this occurred and the input span.
        Track(&'static str, Span<'span>),
        /// Function where this occurred and the remaining span.
        Ok(&'static str, Span<'span>),
        /// Function where this occurred and some resulting error.
        Error(&'static str, nom::Err<nom::error::Error<Span<'span>>>),
    }

    impl<'span> Debug for Track<'span> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Track::Track(func, input) => {
                    write!(f, "{}: enter '{}'", func, input)?;
                }
                Track::Error(func, err) => {
                    write!(f, "{}: error {}", func, err)?;
                }
                Track::Ok(func, rest) => {
                    write!(f, "{}: exit '{}'", func, rest)?;
                }
            }
            Ok(())
        }
    }

    impl<'span> Clone for Track<'span> {
        fn clone(&self) -> Self {
            match self {
                Track::Track(func, input) => Self::Track(func, *input),
                Track::Error(func, err) => Self::Error(
                    func,
                    match err {
                        Err::Incomplete(e) => nom::Err::Incomplete(*e),
                        Err::Error(e) => nom::Err::Error(nom::error::Error::new(e.input, e.code)),
                        Err::Failure(e) => {
                            nom::Err::Failure(nom::error::Error::new(e.input, e.code))
                        }
                    },
                ),
                Track::Ok(func, rest) => Self::Ok(func, *rest),
            }
        }
    }
}

mod tokens {
    use crate::parse2::parse_expr::{InfixToken, PostfixToken, PrefixToken, Span};
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while1};
    use nom::character::complete::{char as nchar, one_of};
    use nom::combinator::{opt, recognize};
    use nom::multi::{count, many0, many1};
    use nom::sequence::{delimited, tuple};
    use nom::IResult;

    #[allow(unused_imports)]

    /// Standard number.
    ///
    /// Number ::= StandardNumber |
    /// '.' [0-9]+ ([eE] [-+]? [0-9]+)?
    /// StandardNumber ::= [0-9]+ ('.' [0-9]+)? ([eE] [-+]? [0-9]+)?
    pub fn number<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        alt((
            // Case one: .42
            recognize(tuple((
                nchar('.'),
                decimal,
                opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
            ))),
            // Case two: 42e42 and 42.42e42
            recognize(tuple((
                decimal,
                opt(tuple((nchar('.'), opt(decimal)))),
                one_of("eE"),
                opt(one_of("+-")),
                decimal,
            ))),
            // Case three: 42 and 42. and 42.42
            recognize(tuple((
                decimal, //
                opt(tuple((
                    nchar('.'), //
                    opt(decimal),
                ))), //
            ))),
        ))(i)
    }

    /// Standard string
    pub fn string<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        quoted('"')(i)
    }

    fn decimal<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        recognize(many1(one_of("0123456789")))(input)
    }

    pub fn comparison_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        alt((
            tag("="),
            tag("<>"),
            tag("<"),
            tag(">"),
            tag("<="),
            tag(">="),
        ))(i)
    }

    pub fn string_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag("&")(i)
    }

    pub fn reference_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag("&")(i)
    }

    pub fn ref_intersection_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag("!")(i)
    }

    pub fn ref_concatenation_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag("~")(i)
    }

    pub fn separator<'a>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        tag(";")(i)
    }

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

    pub fn infix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
        opt(alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("^"))))(i)
    }

    pub fn infix_op_mapped<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<InfixToken<'a>>> {
        match infix_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "+" => Ok((rest, Some(InfixToken::InfixAdd(tok)))),
                "-" => Ok((rest, Some(InfixToken::InfixSubtract(tok)))),
                "*" => Ok((rest, Some(InfixToken::InfixMultiply(tok)))),
                "/" => Ok((rest, Some(InfixToken::InfixDivide(tok)))),
                "^" => Ok((rest, Some(InfixToken::InfixPower(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok((rest, None)),
            Err(e) => Err(e),
        }
    }

    pub fn prefix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
        opt(alt((tag("+"), tag("-"))))(i)
    }

    pub fn prefix_op_mapped<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<PrefixToken<'a>>> {
        match prefix_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "+" => Ok((rest, Some(PrefixToken::PrefixPlus(tok)))),
                "-" => Ok((rest, Some(PrefixToken::PrefixMinus(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok((rest, None)),
            Err(e) => Err(e),
        }
    }

    pub fn postfix_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<Span<'a>>> {
        opt(tag("%"))(i)
    }

    pub fn postfix_op_mapped<'a>(i: Span<'a>) -> IResult<Span<'a>, Option<PostfixToken<'a>>> {
        match postfix_op(i) {
            Ok((rest, Some(tok))) => match *tok {
                "%" => Ok((rest, Some(PostfixToken::PostfixPercent(tok)))),
                _ => unreachable!(),
            },
            Ok((rest, None)) => Ok((rest, None)),
            Err(e) => Err(e),
        }
    }

    /// Parse a quoted string. A double quote within is an escaped quote.
    /// Returns the string within the outer quotes. The double quotes are not
    /// reduced.
    pub fn quoted<'a>(quote: char) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Span<'a>> {
        move |i| {
            let (i, r) = delimited(
                nchar(quote),
                recognize(many0(alt((
                    take_while1(|v| v != quote),
                    recognize(count(nchar(quote), 2)),
                )))),
                nchar(quote),
            )(i)?;

            Ok((i, r))
        }
    }
}

mod coretypes {
    use crate::parse2::parse_expr::Span;

    #[derive(Debug)]
    pub struct OFNumber<'a>(pub f64, pub Span<'a>);

    impl<'a> PartialEq for OFNumber<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }

    #[derive(Debug)]
    pub struct OFString<'a>(pub String, pub Span<'a>);

    impl<'a> PartialEq for OFString<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
}