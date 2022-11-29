use nom::IResult;
use openformula::error::OFCode;
use openformula::iparse::{ParseResult, Span};

pub trait TestSpan {
    fn ok(&self, offset: usize, fragment: &str) -> &Self;
}

pub trait TestSpanPair {
    fn ok_0(&self, offset: usize, fragment: &str) -> &Self;
    fn ok_0_isnone(&self) -> &Self;
    fn ok_1(&self, offset: usize, fragment: &str) -> &Self;
}

pub trait TestFail<C> {
    fn err(&self, code: C) -> &Self;
    fn dump(&self) -> &Self;
}

impl<'a> TestSpan for Span<'a> {
    #[track_caller]
    fn ok(&self, offset: usize, fragment: &str) -> &Self {
        if *self.fragment() != fragment {
            println!("Fragment fails:");
            println!("    result='{}'", self.fragment());
            println!("    test  ='{}'", fragment);
            assert!(false);
        }
        if self.location_offset() != offset {
            println!("Offset fails for '{}'", self.fragment());
            println!("    offset={}", self.location_offset());
            println!("    test  ={}", offset);
            assert!(false);
        }
        self
    }
}

impl<'a> TestSpan for ParseResult<'a, Span<'a>, OFCode> {
    #[track_caller]
    fn ok(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_rest, token)) => {
                token.ok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }
}

impl<'a> TestSpan for (Span<'a>, Span<'a>) {
    #[track_caller]
    fn ok(&self, offset: usize, fragment: &str) -> &Self {
        self.1.ok(offset, fragment);
        self
    }
}

impl<'a> TestSpan for IResult<Span<'a>, Span<'a>> {
    #[track_caller]
    fn ok(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_rest, token)) => {
                token.ok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }
}

impl<'a> TestSpanPair for IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn ok_0(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    test.ok(offset, fragment);
                } else {
                    println!("Was None, should be {} '{}'", offset, fragment);
                    assert!(false);
                }
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn ok_0_isnone(&self) -> &Self {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    println!(
                        "Was something {} '{}', should be None",
                        test.location_offset(),
                        test.fragment()
                    );
                }
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn ok_1(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_, (_, test))) => {
                test.ok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }
}

impl<'a> TestSpanPair for ParseResult<'a, (Option<Span<'a>>, Span<'a>), OFCode> {
    #[track_caller]
    fn ok_0(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    test.ok(offset, fragment);
                } else {
                    println!("Was None, should be {} '{}'", offset, fragment);
                    assert!(false);
                }
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn ok_0_isnone(&self) -> &Self {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    println!(
                        "Was something {} '{}', should be None",
                        test.location_offset(),
                        test.fragment()
                    );
                }
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn ok_1(&self, offset: usize, fragment: &str) -> &Self {
        match self {
            Ok((_, (_, test))) => {
                test.ok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }
}

impl<'a> TestFail<nom::error::ErrorKind> for IResult<Span<'a>, Span<'a>> {
    #[track_caller]
    fn err(&self, kind: nom::error::ErrorKind) -> &Self {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{}'", rest, token);
                assert!(false);
            }
            Err(nom::Err::Error(e)) => {
                if e.code != kind {
                    println!("Failed with the wrong ErrorKind:");
                    println!(
                        "    '{}' => result={:?} <> kind={:?}",
                        e.input.fragment(),
                        e.code,
                        kind
                    );
                    assert!(false);
                }
            }
            Err(e @ nom::Err::Failure(_)) => {
                println!("Failed with Err:Failure");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e @ nom::Err::Incomplete(_)) => {
                println!("Failed with Err:Incomplete");
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn dump(&self) -> &Self {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
        self
    }
}

impl<'a> TestFail<nom::error::ErrorKind> for IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn err(&self, kind: nom::error::ErrorKind) -> &Self {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{:?}'", rest, token);
                assert!(false);
            }
            Err(nom::Err::Error(e)) => {
                if e.code != kind {
                    println!("Failed with the wrong ErrorKind:");
                    println!(
                        "    '{}' => result={:?} <> kind={:?}",
                        e.input.fragment(),
                        e.code,
                        kind
                    );
                    assert!(false);
                }
            }
            Err(e @ nom::Err::Failure(_)) => {
                println!("Failed with Err:Failure");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e @ nom::Err::Incomplete(_)) => {
                println!("Failed with Err:Incomplete");
                println!("{:?}", e);
                assert!(false);
            }
        }
        self
    }

    #[track_caller]
    fn dump(&self) -> &Self {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
        self
    }
}

impl<'a> TestFail<OFCode> for ParseResult<'a, Span<'a>, OFCode> {
    #[track_caller]
    fn err(&self, kind: OFCode) -> &Self {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{}'", rest, token);
                assert!(false);
            }
            Err(e) if e.code == OFCode::OFCNomError => {
                println!("Failed with ErrNomError. To unspecified.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) if e.code == OFCode::OFCNomFailure => {
                println!("Failed with ErrNomFailure.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) => {
                if e.code != kind {
                    println!("Failed with the wrong ErrorKind:");
                    println!("    '{}' => result={} <> kind={:?}", e.span, e, kind);
                    assert!(false);
                }
            }
        }
        self
    }

    #[track_caller]
    fn dump(&self) -> &Self {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
        self
    }
}

impl<'a> TestFail<OFCode> for ParseResult<'a, (Option<Span<'a>>, Span<'a>), OFCode> {
    #[track_caller]
    fn err(&self, kind: OFCode) -> &Self {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{:?}'", rest, token);
                assert!(false);
            }
            Err(e) if e.code == OFCode::OFCNomError => {
                println!("Failed with ErrNomError. To unspecified.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) if e.code == OFCode::OFCNomFailure => {
                println!("Failed with ErrNomFailure.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) => {
                if e.code != kind {
                    println!("Failed with the wrong ErrorKind:");
                    println!("    '{}' => result={} <> kind={:?}", e.span, e, kind);
                    assert!(false);
                }
            }
        }
        self
    }

    #[track_caller]
    fn dump(&self) -> &Self {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
        self
    }
}
