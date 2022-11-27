use crate::error::{Expect2, ParseOFError, Suggest2};
use std::fmt;
use std::fmt::{Debug, Formatter};

impl<'s> Debug for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match f.width() {
            None | Some(0) => debug_parse_of_error_single(f, self),
            Some(1) => debug_parse_of_error_multi(f, self),
            Some(2) => debug_parse_of_error_multi(f, self),
            _ => Ok(()),
        }
    }
}

impl<'s> Debug for Suggest2<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        if !self.parents.is_empty() {
            write!(f, "<")?;
            for p in &self.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }
}

impl<'s> Debug for Expect2<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        if !self.parents.is_empty() {
            write!(f, "<")?;
            for p in &self.parents {
                write!(f, "{} ", p)?;
            }
            write!(f, "> ")?;
        }
        Ok(())
    }
}

pub fn debug_parse_of_error_single<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect2.is_empty() {
        write!(f, "expect=")?;
        debug_expect2_single(f, &err.expect2, 1)?;
    }
    if !err.suggest2.is_empty() {
        write!(f, "suggest=")?;
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
        let mut sorted = err.expect2.clone();
        sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

        writeln!(f, "expect=")?;
        debug_expect2_multi(f, &sorted, 1)?;
    }
    if !err.suggest2.is_empty() {
        writeln!(f, "suggest=")?;
        debug_suggest2_multi(f, &err.suggest2, 1)?;
    }

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
        write!(
            f,
            "{}:{}:\"{}\"",
            exp.code,
            exp.span.location_offset(),
            exp.span
        )?;
        if !exp.parents.is_empty() {
            write!(f, " <")?;
            for (i, p) in exp.parents.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ">")?;
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
            for (i, p) in exp.parents.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ">")?;
        }
    }

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
        write!(
            f,
            "{}:{}:\"{}\"",
            sug.code,
            sug.span.location_offset(),
            sug.span
        )?;
        if !sug.parents.is_empty() {
            write!(f, " <")?;
            for (i, p) in sug.parents.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ">")?;
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
            for (i, p) in sug.parents.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ">")?;
        }
    }

    Ok(())
}
