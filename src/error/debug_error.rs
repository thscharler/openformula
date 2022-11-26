use crate::error::{Expect2, ParseOFError, Suggest2};
use std::fmt;
use std::fmt::Formatter;

pub fn debug_parse_of_error_single<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect2.is_empty() {
        writeln!(f, "expect2=")?;
        debug_expect2_single(f, &err.expect2, 1)?;
    }
    if !err.suggest2.is_empty() {
        write!(f, "suggest2=")?;
        debug_suggest2_single(f, &err.suggest2, 1)?;
    }

    Ok(())
}

pub fn debug_parse_of_error_multi<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect2.is_empty() {
        writeln!(f, "expect2=")?;
        debug_expect2_multi(f, &err.expect2, 1)?;
    }
    if !err.suggest2.is_empty() {
        writeln!(f, "suggest2=")?;
        debug_suggest2_multi(f, &err.suggest2, 1)?;
    }

    Ok(())
}

pub fn display_parse_of_error<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "OFError {} ", err.code)?;
    for exp in &err.expect2 {
        display_expect2_single(f, exp, 0)?;
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

// expect2

pub fn debug_expect2_multi(
    f: &mut Formatter<'_>,
    exp_vec: &Vec<Expect2<'_>>,
    ind: usize,
) -> fmt::Result {
    for exp in exp_vec {
        indent(f, ind)?;
        write!(f, "{}:\"{}\"", exp.code, exp.span)?;
        if !exp.parents.is_empty() {
            write!(f, " <")?;
            for p in &exp.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        writeln!(f)?;
    }

    Ok(())
}

pub fn debug_expect2_single(
    f: &mut Formatter<'_>,
    exp_vec: &Vec<Expect2<'_>>,
    _ind: usize,
) -> fmt::Result {
    for exp in exp_vec {
        write!(f, "{}:\"{}\"", exp.code, exp.span)?;
        if !exp.parents.is_empty() {
            write!(f, " <")?;
            for p in &exp.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
    }

    Ok(())
}

pub fn display_expect2_single(
    f: &mut Formatter<'_>,
    exp: &Expect2<'_>,
    _ind: usize,
) -> fmt::Result {
    write!(f, "{}:\"{}\"", exp.code, exp.span)?;
    Ok(())
}

// suggest2

pub fn debug_suggest2_multi(
    f: &mut Formatter<'_>,
    sug_vec: &Vec<Suggest2<'_>>,
    ind: usize,
) -> fmt::Result {
    for sug in sug_vec {
        indent(f, ind)?;
        write!(f, "{}:\"{}\"", sug.code, sug.span)?;
        if !sug.parents.is_empty() {
            write!(f, " <")?;
            for p in &sug.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        writeln!(f)?;
    }

    Ok(())
}

pub fn debug_suggest2_single(
    f: &mut Formatter<'_>,
    sug_vec: &Vec<Suggest2<'_>>,
    _ind: usize,
) -> fmt::Result {
    for sug in sug_vec {
        write!(f, "{}:\"{}\"", sug.code, sug.span)?;
        if !sug.parents.is_empty() {
            write!(f, " <")?;
            for p in &sug.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
    }

    Ok(())
}
