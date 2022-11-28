use crate::spantest::TestN;
use nom::error::ErrorKind;
use nom::IResult;
use openformula::iparse::Span;
use std::fmt::Debug;

impl<'s, O> TestN<(), Span<'s>, (Span<'s>, O), nom::Err<nom::error::Error<Span<'s>>>>
where
    O: Debug,
{
    pub fn nom(span: &'s str, fn_test: fn(Span<'s>) -> IResult<Span<'s>, O>) -> Self {
        Self::run(span, fn_test)
    }
}

impl<'s, P, O> TestN<P, Span<'s>, (Span<'s>, O), nom::Err<nom::error::Error<Span<'s>>>>
where
    P: Default,
    O: Debug,
{
    #[must_use]
    pub fn err(&self, kind: ErrorKind) -> &Self {
        match &self.result {
            Ok(_) => {
                println!("FAIL: Expected error, but was ok!");
                self.flag_fail();
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                if e.code != kind {
                    println!("FAIL: {:?} <> {:?}", e.code, kind);
                    self.flag_fail();
                }
            }
            Err(nom::Err::Incomplete(_)) => {
                println!("FAIL: nom::Err::Incomplete");
                self.flag_fail();
            }
        }
        self
    }
}
