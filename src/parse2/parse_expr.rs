pub use coretypes::*;
use nom::combinator::opt;
use nom_locate::LocatedSpan;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
pub use tracer::*;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type ParseResult<'s, 't, O> = Result<(Span<'s>, O), ParseExprError<'s, 't>>;

#[derive(Debug)]
pub enum ParseTree<'a> {
    Empty(),
    Number(OFNumber<'a>),
    String(OFString<'a>),
    Parenthesis(Box<ParseTree<'a>>),
    PrefixOp(PrefixToken<'a>, Box<ParseTree<'a>>),
    InfixOp(Box<ParseTree<'a>>, InfixToken<'a>, Box<ParseTree<'a>>),
    PostfixOp(Box<ParseTree<'a>>, PostfixToken<'a>),
}

impl<'a> Display for ParseTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseTree::Empty() => Ok(()),
            ParseTree::Number(v) => {
                write!(f, "{}", v)
            }
            ParseTree::String(v) => {
                write!(f, "{}", v)
            }
            ParseTree::Parenthesis(expr) => {
                write!(f, "({})", expr)
            }
            ParseTree::PrefixOp(op, expr) => {
                write!(f, "{}{}", op, expr)
            }
            ParseTree::InfixOp(expr1, op, expr2) => {
                write!(f, "{} {} {}", expr1, op, expr2)
            }
            ParseTree::PostfixOp(expr, op) => {
                write!(f, "{}{}", expr, op)
            }
        }
    }
}

#[derive(Debug)]
pub enum PrefixToken<'a> {
    Plus(Span<'a>),
    Minus(Span<'a>),
}

impl<'a> PrefixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            PrefixToken::Plus(span) => *span,
            PrefixToken::Minus(span) => *span,
        }
    }
}

impl<'a> Display for PrefixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixToken::Plus(_) => write!(f, "+"),
            PrefixToken::Minus(_) => write!(f, "-"),
        }
    }
}

impl<'a> PartialEq for PrefixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum InfixToken<'a> {
    Add(Span<'a>),
    Subtract(Span<'a>),
    Multiply(Span<'a>),
    Divide(Span<'a>),
    Power(Span<'a>),
}

impl<'a> InfixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            InfixToken::Add(span) => *span,
            InfixToken::Subtract(span) => *span,
            InfixToken::Multiply(span) => *span,
            InfixToken::Divide(span) => *span,
            InfixToken::Power(span) => *span,
        }
    }
}

impl<'a> Display for InfixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixToken::Add(_) => write!(f, "+"),
            InfixToken::Subtract(_) => write!(f, "-"),
            InfixToken::Multiply(_) => write!(f, "*"),
            InfixToken::Divide(_) => write!(f, "/"),
            InfixToken::Power(_) => write!(f, "^"),
        }
    }
}

impl<'a> PartialEq for InfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum PostfixToken<'a> {
    Percent(Span<'a>),
}

impl<'a> PostfixToken<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            PostfixToken::Percent(span) => *span,
        }
    }
}

impl<'a> Display for PostfixToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PostfixToken::Percent(_) => write!(f, "%"),
        }
    }
}

impl<'a> PartialEq for PostfixToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }
}

#[derive(Debug)]
pub enum ParseExprError<'s, 't> {
    NomError(&'t Tracer<'s>),
    NomFailure(&'t Tracer<'s>),

    Number(&'t Tracer<'s>),
    String(&'t Tracer<'s>),
    Parenthesis(&'t Tracer<'s>),

    Elementary(&'t Tracer<'s>),
}

pub fn number<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("opt_number", i);

    match tokens::number(i) {
        Ok((rest, tok)) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(ParseTree::Number(OFNumber(val, tok))))),
            Err(e) => Err(ParseExprError::Number(trace.dyn_err(Box::new(e)))),
        },
        Err(e) => Err(ParseExprError::Number(trace.err(e))),
    }
}

pub fn opt_number<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("opt_number", i);

    match opt(tokens::number)(i) {
        Ok((rest, Some(tok))) => match (*tok).parse::<f64>() {
            Ok(val) => Ok(trace.ok(rest, Box::new(ParseTree::Number(OFNumber(val, tok))))),
            Err(_) => unreachable!(),
        },
        Ok((rest, None)) => Ok(trace.ok(rest, Box::new(ParseTree::Empty()))),
        Err(e) => Err(ParseExprError::Number(trace.err(e))),
    }
}

pub fn opt_string<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("opt_string", i);

    match opt(tokens::string)(i) {
        Ok((rest, Some(tok))) => Ok(trace.ok(
            rest,
            Box::new(ParseTree::String(OFString(tok.to_string(), tok))),
        )),
        Ok((rest, None)) => Ok(trace.ok(rest, Box::new(ParseTree::Empty()))),
        Err(e) => Err(ParseExprError::String(trace.err(e))),
    }
}

pub fn opt_expr_parentheses<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Option<Box<ParseTree<'s>>>> {
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

pub fn parse_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("parse_expression", i);

    match parse_infix_expr(&trace, i) {
        Ok((rest, expr)) => Ok(trace.ok(rest, expr)),
        Err(e) => Err(e),
    }
}

pub fn parse_infix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("parse_infix", i);

    match parse_postfix_expr(&trace, i) {
        Ok((mut rest1, mut expr1)) => loop {
            match tokens::infix_op_mapped(rest1) {
                Ok((rest2, Some(tok))) => {
                    trace.step("op", tok.span());
                    match parse_postfix_expr(trace, rest2) {
                        Ok((rest3, expr2)) => {
                            rest1 = rest3;
                            expr1 = Box::new(ParseTree::InfixOp(expr1, tok, expr2));
                        }
                        Err(e) => break Err(e),
                    }
                }
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

pub fn parse_postfix_expr<'s, 't>(
    trace: &'t Tracer<'s>,
    i0: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("parse_postfix", i0);

    match parse_prefix(&trace, i0) {
        Ok((mut rest1, mut expr)) => loop {
            match tokens::postfix_op_mapped(rest1) {
                Ok((rest2, Some(tok))) => {
                    trace.step("op", tok.span());
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

pub fn parse_prefix<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("parse_prefix", i);

    match tokens::prefix_op_mapped(i) {
        Ok((rest1, Some(tok))) => {
            trace.step("op", tok.span());
            match parse_prefix(&trace, rest1) {
                Ok((rest2, expr)) => Ok(trace.ok(rest2, Box::new(ParseTree::PrefixOp(tok, expr)))),
                Err(e) => Err(e),
            }
        }
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
pub fn parse_elementary<'s, 't>(
    trace: &'t Tracer<'s>,
    i: Span<'s>,
) -> ParseResult<'s, 't, Box<ParseTree<'s>>> {
    trace.enter("parse_elementary", i);

    match opt_number(&trace, i) {
        Ok((rest, expr)) => {
            match &*expr {
                ParseTree::Empty() => { /* skip */ }
                ParseTree::Number(_) => return Ok(trace.ok(rest, expr)),
                _ => unreachable!(),
            }
        }
        Err(e) => return Err(e),
    }

    match opt_string(&trace, i) {
        Ok((rest, expr)) => {
            match &*expr {
                ParseTree::Empty() => { /* skip */ }
                ParseTree::String(_) => return Ok(trace.ok(rest, expr)),
                _ => unreachable!(),
            }
        }
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

    Err(ParseExprError::Elementary(trace))
}

mod tests {
    use crate::parse2::parse_expr::{
        number, parse_elementary, parse_expr, ParseResult, ParseTree, Span, Tracer,
    };

    fn run_test<'x>(
        str: &'x str,
        testfn: for<'s, 't> fn(&'t Tracer<'s>, Span<'s>) -> ParseResult<'s, 't, Box<ParseTree<'s>>>,
    ) {
        let tracer = Tracer::new();
        {
            println!();
            println!("{}", str);
            match testfn(&tracer, Span::new(str)) {
                Ok((rest, tok)) => {
                    println!("{} | {}", tok, rest);
                }
                Err(e) => {
                    println!("{:?}", e);
                }
            }
            println!("{:?}", &tracer);
        }
    }

    #[test]
    fn test_expr() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test(test, parse_expr);
        }
    }

    #[test]
    fn test_elementary() {
        let tests = ["471", r#""strdata""#, "1+1", "(1+1)"];
        for test in tests {
            run_test(test, parse_elementary);
        }
    }

    #[test]
    fn test_number() {
        let test_ok = ["25", "25e+5", "25.", "25.001", "25.003e-7"];
        for test in test_ok {
            run_test(test, number);
        }

        let test_err = ["invalid", "2x5", "25ex+5", "25.x", "25.x001", "25x.003e-7"];
        for test in test_err {
            run_test(test, number);
        }
    }
}

mod tracer {
    use crate::parse2::parse_expr::Span;
    use std::cell::RefCell;
    use std::error::Error;
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
                    Track::Enter(_, _) => {
                        indent.push_str("  ");
                        writeln!(f, "{}{:?}", indent, tr)?;
                    }
                    Track::Step(_, _, _) => {
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
                    Track::ErrorDyn(_, _) => {
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

        /// Ok in a parser.
        ///
        /// Panic
        ///
        /// Panics if there was no call to enter() before.
        pub fn ok<T>(&self, rest: Span<'span>, val: T) -> (Span<'span>, T) {
            let func = self.func.borrow_mut().pop().unwrap();
            self.tracks.borrow_mut().push(Track::Ok(func, rest));
            (rest, val)
        }

        /// Erring in a parser. Accepts only nom errors.
        ///
        /// Returns to reference to this trace to be used in the final error type.
        ///
        /// Panic
        ///
        /// Panics if there was no call to enter() before.
        pub fn err<'a>(
            &'a self,
            err: nom::Err<nom::error::Error<Span<'span>>>,
        ) -> &'a Tracer<'span> {
            let func = self.func.borrow_mut().pop().unwrap();
            self.tracks.borrow_mut().push(Track::Error(func, err));
            self
        }

        /// Erring in a parser. Accepts boxed dyn errors for the rest.
        ///
        /// Returns to reference to this trace to be used in the final error type.
        ///
        /// Panic
        ///
        /// Panics if there was no call to enter() before.
        pub fn dyn_err<'a>(&'a self, err: Box<dyn Error>) -> &'a Tracer<'span> {
            let func = self.func.borrow_mut().pop().unwrap();
            self.tracks.borrow_mut().push(Track::ErrorDyn(func, err));
            self
        }
    }

    /// One track of the parsing trace.
    pub enum Track<'span> {
        /// Function where this occurred and the input span.
        Enter(&'static str, Span<'span>),
        /// Function with an extra step.
        Step(&'static str, &'static str, Span<'span>),
        /// Function where this occurred and the remaining span.
        Ok(&'static str, Span<'span>),
        /// Function where this occurred and some error.
        Error(&'static str, nom::Err<nom::error::Error<Span<'span>>>),
        /// Function where this occurred and some error.
        ErrorDyn(&'static str, Box<dyn Error>),
    }

    impl<'span> Debug for Track<'span> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Track::Enter(func, input) => {
                    write!(f, "{}: enter '{}'", func, input)?;
                }
                Track::Step(func, step, input) => {
                    write!(f, "{}: {} '{}'", func, step, input)?;
                }
                Track::Ok(func, rest) => {
                    write!(f, "{}: exit '{}'", func, rest)?;
                }
                Track::Error(func, err) => {
                    write!(f, "{}: error {}", func, err)?;
                }
                Track::ErrorDyn(func, err) => {
                    write!(f, "{}: error2 {}", func, err)?;
                }
            }
            Ok(())
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
                "+" => Ok((rest, Some(InfixToken::Add(tok)))),
                "-" => Ok((rest, Some(InfixToken::Subtract(tok)))),
                "*" => Ok((rest, Some(InfixToken::Multiply(tok)))),
                "/" => Ok((rest, Some(InfixToken::Divide(tok)))),
                "^" => Ok((rest, Some(InfixToken::Power(tok)))),
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
                "+" => Ok((rest, Some(PrefixToken::Plus(tok)))),
                "-" => Ok((rest, Some(PrefixToken::Minus(tok)))),
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
                "%" => Ok((rest, Some(PostfixToken::Percent(tok)))),
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
    use std::fmt::{Display, Formatter};

    #[derive(Debug)]
    pub struct OFNumber<'a>(pub f64, pub Span<'a>);

    impl<'a> Display for OFNumber<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<'a> PartialEq for OFNumber<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }

    #[derive(Debug)]
    pub struct OFString<'a>(pub String, pub Span<'a>);

    impl<'a> Display for OFString<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<'a> PartialEq for OFString<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
}
