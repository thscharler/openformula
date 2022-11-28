use crate::spantest::{CompareFn, Report, TestN};
use openformula::iparse::Span;
use std::fmt::Debug;

impl<'s, P, O, E> TestN<P, Span<'s>, (Span<'s>, O), E>
where
    P: Default,
    O: Debug,
    E: Debug,
{
    /// Checks for ok.
    /// Uses an extraction function to get the relevant result.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn ok<V>(&'s self, eq: CompareFn<O, V>, test: V) -> &Self
    where
        V: Debug + Copy,
        O: Debug,
    {
        match &self.result {
            Ok((_, token)) => {
                if !eq(token, test) {
                    println!("FAIL: Value mismatch: {:?} <> {:?}", token, test);
                    self.flag_fail();
                }
            }
            Err(_) => {
                println!("FAIL: Expect ok, but was an error!");
                self.flag_fail();
            }
        }
        self
    }

    /// Tests the remaining string after parsing.
    ///
    /// Finish the test with q()
    #[must_use]
    pub fn rest(&self, test: &str) -> &Self {
        match &self.result {
            Ok((rest, _)) => {
                if **rest != test {
                    println!("FAIL: Rest mismatch {} <> {}", **rest, test);
                    self.flag_fail();
                }
            }
            Err(_) => {
                println!("FAIL: Expect ok, but was an error!");
                self.flag_fail();
            }
        }
        self
    }
}

pub struct CheckDump;

impl<'s, P, O, E> Report<P, Span<'s>, (Span<'s>, O), E> for CheckDump
where
    P: Default,
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<P, Span<'s>, (Span<'s>, O), E>) {
        if testn.fail.get() {
            Dump::report(testn);
            panic!()
        }
    }
}

pub struct Dump;

impl<'s, P, O, E> Report<P, Span<'s>, (Span<'s>, O), E> for Dump
where
    P: Default,
    E: Debug,
    O: Debug,
{
    fn report(testn: &TestN<P, Span<'s>, (Span<'s>, O), E>) {
        println!();
        println!(
            "when parsing '{}' in {}ns =>",
            testn.span,
            testn.duration.as_nanos()
        );
        match &testn.result {
            Ok((rest, token)) => {
                println!("rest {}:\"{}\"", rest.location_offset(), rest);
                println!("{:0?}", token);
            }
            Err(e) => {
                println!("error");
                println!("{:1?}", e);
            }
        }
    }
}

// Compare fn

// comparison fn
#[allow(dead_code)]
pub fn span<'a, 'b, 's>(span: &'a Span<'s>, value: (usize, &'b str)) -> bool {
    **span == value.1 && span.location_offset() == value.0
}

// comparison fn for the first part
#[allow(dead_code)]
pub fn span_0<'a, 'b, 's>(span: &'a (Option<Span<'s>>, Span<'s>), value: (usize, &'b str)) -> bool {
    if let Some(span) = &span.0 {
        **span == value.1 && span.location_offset() == value.0
    } else {
        false
    }
}

// comparison fn for the first part
#[allow(dead_code)]
pub fn span_0_isnone<'a, 's>(span: &'a (Option<Span<'s>>, Span<'s>), _value: ()) -> bool {
    span.0.is_none()
}

// comparison fn for the second part
#[allow(dead_code)]
pub fn span_1<'a, 'b, 's>(span: &'a (Option<Span<'s>>, Span<'s>), value: (usize, &'b str)) -> bool {
    *span.1 == value.1 && span.1.location_offset() == value.0
}
