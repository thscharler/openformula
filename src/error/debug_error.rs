use crate::error::{Expect, OFCode, ParseOFError, Suggest};
use std::fmt;
use std::fmt::{Debug, Formatter};

impl<'s> Debug for ParseOFError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match f.width() {
            None | Some(0) => debug_parse_of_error_short(f, self),
            Some(1) => debug_parse_of_error_medium(f, self),
            Some(2) => debug_parse_of_error_long(f, self),
            _ => Ok(()),
        }
    }
}

impl<'s> Debug for Suggest<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        Ok(())
    }
}

impl<'s> Debug for Expect<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\"{}\"", self.code, self.span)?;
        Ok(())
    }
}

fn debug_parse_of_error_short<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect.is_empty() {
        write!(f, "expect=")?;
        debug_expect2_short(f, &err.expect, 1)?;
    }
    if !err.suggest.is_empty() {
        write!(f, "suggest=")?;
        debug_suggest2_short(f, &err.suggest, 1)?;
    }

    Ok(())
}

fn debug_parse_of_error_medium<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect.is_empty() {
        let mut sorted = err.expect.clone();
        sorted.reverse();
        sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

        // per offset
        let mut grp_offset = 0;
        let mut grp = Vec::new();
        let mut subgrp = Vec::new();
        for exp in &sorted {
            if exp.span.location_offset() != grp_offset {
                if !subgrp.is_empty() {
                    grp.push((grp_offset, subgrp));
                    subgrp = Vec::new();
                }
                grp_offset = exp.span.location_offset();
            }

            subgrp.push(exp);
        }
        if !subgrp.is_empty() {
            grp.push((grp_offset, subgrp));
        }

        for (g_off, subgrp) in grp {
            let first = subgrp.first().unwrap();
            writeln!(f, "expect {}:\"{}\" ", g_off, first.span)?;
            debug_expect2_medium(f, &subgrp, 1)?;
        }
    }
    if !err.suggest.is_empty() {
        let mut sorted = err.suggest.clone();
        sorted.reverse();
        sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

        // per offset
        let mut grp_offset = 0;
        let mut grp = Vec::new();
        let mut subgrp = Vec::new();
        for exp in &sorted {
            if exp.span.location_offset() != grp_offset {
                if !subgrp.is_empty() {
                    grp.push((grp_offset, subgrp));
                    subgrp = Vec::new();
                }
                grp_offset = exp.span.location_offset();
            }

            subgrp.push(exp);
        }
        if !subgrp.is_empty() {
            grp.push((grp_offset, subgrp));
        }

        for (g_off, subgrp) in grp {
            let first = subgrp.first().unwrap();
            writeln!(f, "suggest {}:\"{}\"", g_off, first.span)?;
            debug_suggest2_medium(f, &subgrp, 1)?;
        }
    }

    Ok(())
}

fn debug_parse_of_error_long<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    writeln!(f, "ParseOFError {} \"{}\"", err.code, err.span)?;
    if !err.expect.is_empty() {
        let mut sorted = err.expect.clone();
        sorted.sort_by(|a, b| b.span.location_offset().cmp(&a.span.location_offset()));

        writeln!(f, "expect=")?;
        debug_expect2_long(f, &sorted, 1)?;
    }
    if !err.suggest.is_empty() {
        writeln!(f, "suggest=")?;
        debug_suggest2_long(f, &err.suggest, 1)?;
    }

    Ok(())
}

fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
    write!(f, "{}", " ".repeat(ind * 4))?;
    Ok(())
}

// expect2

fn debug_expect2_long(f: &mut Formatter<'_>, exp_vec: &Vec<Expect<'_>>, ind: usize) -> fmt::Result {
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

fn debug_expect2_medium(
    f: &mut Formatter<'_>,
    exp_vec: &Vec<&Expect<'_>>,
    ind: usize,
) -> fmt::Result {
    let mut prefix: Vec<Vec<OFCode>> = Vec::new();

    for exp in exp_vec {
        indent(f, ind)?;
        write!(f, "{:20}", exp.code)?;

        let (suffix, sigil) = loop {
            if let Some(last) = prefix.last() {
                match exp.parents.strip_prefix(last.as_slice()) {
                    None => {
                        match prefix.pop() {
                            None => {
                                prefix.push(exp.parents.clone());
                                break (exp.parents.as_slice(), "<<");
                            }
                            Some(_) => {}
                        };
                    }
                    Some(suffix) => {
                        prefix.push(exp.parents.clone());
                        break (suffix, "++");
                    }
                }
            } else {
                prefix.push(exp.parents.clone());
                break (exp.parents.as_slice(), "<<");
            }
        };

        write!(f, " {} ", sigil)?;
        for (i, p) in suffix.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", p)?;
        }
        writeln!(f)?;
    }

    Ok(())
}

fn debug_expect2_short(
    f: &mut Formatter<'_>,
    exp_vec: &Vec<Expect<'_>>,
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

fn debug_suggest2_long(
    f: &mut Formatter<'_>,
    sug_vec: &Vec<Suggest<'_>>,
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

fn debug_suggest2_medium(
    f: &mut Formatter<'_>,
    sug_vec: &Vec<&Suggest<'_>>,
    ind: usize,
) -> fmt::Result {
    let mut prefix: Vec<Vec<OFCode>> = Vec::new();

    for sug in sug_vec {
        indent(f, ind)?;
        write!(f, "{:20}", sug.code)?;

        let (suffix, sigil) = loop {
            if let Some(last) = prefix.last() {
                match sug.parents.strip_prefix(last.as_slice()) {
                    None => {
                        match prefix.pop() {
                            None => {
                                prefix.push(sug.parents.clone());
                                break (sug.parents.as_slice(), "<<");
                            }
                            Some(_) => {}
                        };
                    }
                    Some(suffix) => {
                        prefix.push(sug.parents.clone());
                        break (suffix, "++");
                    }
                }
            } else {
                prefix.push(sug.parents.clone());
                break (sug.parents.as_slice(), "<<");
            }
        };

        write!(f, " {} ", sigil)?;
        for (i, p) in suffix.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", p)?;
        }
        writeln!(f)?;
    }

    Ok(())
}

fn debug_suggest2_short(
    f: &mut Formatter<'_>,
    sug_vec: &Vec<Suggest<'_>>,
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
