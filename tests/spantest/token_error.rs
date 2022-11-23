use nom::error::ErrorKind;
use nom::IResult;
use openformula::ast::{ParseResult, Span};
use openformula::error::OFCode;
use openformula::error::OFCode::{OFCNomError, OFCNomFailure};

pub trait CheckOk {
    fn cok(&self, offset: usize, fragment: &str);
}

pub trait CheckOk2 {
    fn cok0(&self, offset: usize, fragment: &str);
    fn cok1(&self, offset: usize, fragment: &str);
}

pub trait CheckNone {
    fn cnone0(&self);
}

pub trait CheckFailNom {
    fn dump(&self);
    fn cnom(&self, kind: ErrorKind);
}

pub trait CheckFailToken {
    fn dump(&self);
    fn ctok(&self, kind: OFCode);
}

impl<'a> CheckOk for Span<'a> {
    #[track_caller]
    fn cok(&self, offset: usize, fragment: &str) {
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
    }
}

impl<'a> CheckOk for (Span<'a>, Span<'a>) {
    #[track_caller]
    fn cok(&self, offset: usize, fragment: &str) {
        self.1.cok(offset, fragment);
    }
}

impl<'a> CheckOk for IResult<Span<'a>, Span<'a>> {
    #[track_caller]
    fn cok(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_rest, token)) => {
                token.cok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}

impl<'a> CheckFailNom for IResult<Span<'a>, Span<'a>> {
    #[track_caller]
    fn dump(&self) {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
    }

    #[track_caller]
    fn cnom(&self, kind: ErrorKind) {
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
    }
}

impl<'a> CheckOk2 for IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn cok0(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    test.cok(offset, fragment);
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
    }

    #[track_caller]
    fn cok1(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_, (_, test))) => {
                test.cok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}

impl<'a> CheckFailNom for IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn dump(&self) {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
    }

    #[track_caller]
    fn cnom(&self, kind: ErrorKind) {
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
    }
}

impl<'a> CheckNone for IResult<Span<'a>, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn cnone0(&self) {
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
    }
}

impl<'a> CheckOk for ParseResult<'a, Span<'a>> {
    #[track_caller]
    fn cok(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_rest, token)) => {
                token.cok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}

impl<'a> CheckFailToken for ParseResult<'a, Span<'a>> {
    #[track_caller]
    fn dump(&self) {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
    }

    #[track_caller]
    fn ctok(&self, kind: OFCode) {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{}'", rest, token);
                assert!(false);
            }
            Err(e) if e.code == OFCNomError => {
                println!("Failed with ErrNomError. To unspecified.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) if e.code == OFCNomFailure => {
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
    }
}

impl<'a> CheckOk2 for ParseResult<'a, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn cok0(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_, (test, _))) => {
                if let Some(test) = test {
                    test.cok(offset, fragment);
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
    }

    #[track_caller]
    fn cok1(&self, offset: usize, fragment: &str) {
        match self {
            Ok((_, (_, test))) => {
                test.cok(offset, fragment);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}

impl<'a> CheckFailToken for ParseResult<'a, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn dump(&self) {
        match self {
            Ok(v) => {
                println!("Always fail: {:?}", v);
            }
            Err(e) => {
                println!("Always fail: {:?}", e);
            }
        }
    }

    #[track_caller]
    fn ctok(&self, kind: OFCode) {
        match self {
            Ok((rest, token)) => {
                println!("Ok, but should have failed:");
                println!("    rest='{}' token='{:?}'", rest, token);
                assert!(false);
            }
            Err(e) if e.code == OFCNomError => {
                println!("Failed with ErrNomError. To unspecified.");
                println!("{:?}", e);
                assert!(false);
            }
            Err(e) if e.code == OFCNomFailure => {
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
    }
}

impl<'a> CheckNone for ParseResult<'a, (Option<Span<'a>>, Span<'a>)> {
    #[track_caller]
    fn cnone0(&self) {
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
    }
}
