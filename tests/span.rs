// use nom::bytes::complete::tag;
// use nom::sequence::tuple;
// use nom::IResult;
// use openformula::iparse::Span;
//
// #[test]
// pub fn test_span<'s>() {
//     let span = Span::new("ABC");
//
//     let (_rest, (x, y, z)) = nom_p(span).unwrap();
//
//     dbg!(x.fragment());
//
//
//     dbg!(x);
//     dbg!(y);
//     dbg!(z);
// }
//
// fn nom_p<'s>(span: Span<'s>) -> IResult<Span<'s>, (Span<'s>, Span<'s>, Span<'s>)> {
//     tuple((tag("A"), tag("B"), tag("C")))(span)
// }
