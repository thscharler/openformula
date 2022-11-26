use crate::error::{Expect, ParseOFError, Suggest};
use std::fmt;
use std::fmt::Formatter;

pub fn debug_parse_of_error<'s>(f: &mut Formatter<'_>, err: &ParseOFError<'s>) -> std::fmt::Result {
    writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if let Some(expect) = &err.expect {
        writeln!(f, "expect=")?;
        debug_expect_multi(f, expect, 1)?;
    }
    if let Some(sug) = &err.suggest {
        writeln!(f, "suggest=")?;
        debug_suggest_multi(f, sug, 1)?;
    }

    Ok(())
}

pub fn display_parse_of_error<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "OFError {} ", err.code)?;
    if let Some(expect) = &err.expect {
        display_expect_single(f, expect)?;
    }
    // no suggest
    write!(
        f,
        "for span={} \"{}\"",
        err.span.location_offset(),
        err.span
    )?;
    Ok(())
}

fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
    write!(f, "{}", " ".repeat(ind * 4))?;
    Ok(())
}

// Expect

pub fn display_expect_single(f: &mut Formatter<'_>, exp: &Expect<'_>) -> fmt::Result {
    write!(f, " {}: {}", exp.func, exp.code)?;
    for alt_exp in &exp.alt {
        write!(f, " +")?;
        display_expect_single(f, alt_exp)?;
    }
    for par_exp in &exp.par {
        write!(f, " ?")?;
        display_expect_single(f, par_exp)?;
    }

    Ok(())
}

fn rec_expect_multi_2(f: &mut Formatter<'_>, exp: &Expect<'_>, ind: usize) -> fmt::Result {
    indent(f, ind)?;

    let plausible = if exp.span.location_offset() > 0 {
        "*"
    } else {
        " "
    };

    write!(
        f,
        " <- {}{}: {} {}:\"{}\"",
        plausible,
        exp.func,
        exp.code,
        exp.span.location_offset(),
        exp.span
    )?;

    write!(f, " << ")?;
    for par_exp in &exp.par {
        write!(f, "{} ", par_exp.func)?;
    }
    writeln!(f)?;

    let mut alt = exp.alt.clone();
    alt.sort_by(|v1, v2| v2.span.location_offset().cmp(&v1.span.location_offset()));

    for alt_exp in &alt {
        rec_expect_multi_2(f, alt_exp, ind + 1)?;
    }

    Ok(())
}

fn rec_expect_multi(f: &mut Formatter<'_>, exp: &Expect<'_>, ind: usize) -> fmt::Result {
    indent(f, ind)?;
    write!(
        f,
        "{}: {} {}:\"{}\"",
        exp.func,
        exp.code,
        exp.span.location_offset(),
        exp.span
    )?;

    write!(f, " << ")?;
    for par_exp in &exp.par {
        write!(f, "{} ", par_exp.func)?;
    }
    writeln!(f)?;

    let mut alt = exp.alt.clone();
    alt.sort_by(|v1, v2| v2.span.location_offset().cmp(&v1.span.location_offset()));

    for alt_exp in &alt {
        rec_expect_multi_2(f, alt_exp, ind + 1)?;
    }

    Ok(())
}

pub fn debug_expect_multi(f: &mut Formatter<'_>, exp: &Expect<'_>, ind: usize) -> fmt::Result {
    rec_expect_multi(f, exp, ind)
}

fn debug_expect_single_rec(f: &mut Formatter<'_>, exp: &Expect<'_>, _ind: usize) -> fmt::Result {
    write!(
        f,
        " + {} {}:\"{}\"",
        exp.code,
        exp.span.location_offset(),
        exp.span
    )?;
    Ok(())
}

pub fn debug_expect_single(f: &mut Formatter<'_>, exp: &Expect<'_>, ind: usize) -> fmt::Result {
    write!(
        f,
        "{}: {} {}:\"{}\"",
        exp.func,
        exp.code,
        exp.span.location_offset(),
        exp.span
    )?;

    let mut alt = exp.alt.clone();
    alt.sort_by(|v1, v2| v2.span.location_offset().cmp(&v1.span.location_offset()));

    for exp in &alt {
        debug_expect_single_rec(f, exp, ind)?;
    }

    Ok(())
}

// Suggest

fn rec_suggest_multi_2(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    if sug.codes.len() == 0 {
        for next in &sug.next {
            rec_suggest_multi(f, next, ind)?;
        }
    } else {
        indent(f, ind)?;
        write!(f, " <- {}: ", sug.func)?;
        for (code, span) in &sug.codes {
            writeln!(f, "{} {}:\"{}\" ", code, span.location_offset(), span)?;
        }

        for next in &sug.next {
            rec_suggest_multi(f, next, ind + 1)?;
        }
    }
    Ok(())
}

fn rec_suggest_multi(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    if sug.codes.len() == 0 {
        for next in &sug.next {
            rec_suggest_multi(f, next, ind)?;
        }
    } else {
        indent(f, ind)?;
        write!(f, "{}: ", sug.func)?;
        for (code, span) in &sug.codes {
            writeln!(f, "{} {}:\"{}\" ", code, span.location_offset(), span)?;
        }

        for next in &sug.next {
            rec_suggest_multi_2(f, next, ind + 1)?;
        }
    }
    Ok(())
}

pub fn debug_suggest_multi(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    rec_suggest_multi(f, sug, ind)
}

fn rec_suggest_single_2(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    if sug.codes.len() == 0 {
        for next in &sug.next {
            rec_suggest_single_2(f, next, ind)?;
        }
    } else {
        for (code, span) in &sug.codes {
            write!(f, " <- {} {}:\"{}\"", code, span.location_offset(), span)?;
        }
        for next in &sug.next {
            rec_suggest_single_2(f, next, ind + 1)?;
        }
    }
    Ok(())
}

fn rec_suggest_single(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    if sug.codes.len() == 0 {
        for next in &sug.next {
            rec_suggest_single(f, next, ind)?;
        }
    } else {
        for (code, span) in &sug.codes {
            write!(f, "{} {}:\"{}\"", code, span.location_offset(), span)?;
        }
        for next in &sug.next {
            rec_suggest_single_2(f, next, ind + 1)?;
        }
    }
    Ok(())
}

pub fn debug_suggest_single(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    rec_suggest_single(f, sug, ind)
}
