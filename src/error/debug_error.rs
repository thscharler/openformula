use crate::error::{ParseOFError, Suggest};
use std::fmt;
use std::fmt::Formatter;

pub fn debug_parse_of_error<'s>(f: &mut Formatter<'_>, err: &ParseOFError<'s>) -> std::fmt::Result {
    writeln!(f, "DEBUG_PARSE: {} ", err.code)?;
    if let Some(expect) = &err.expect {
        writeln!(f, "  [")?;
        for exp in expect {
            writeln!(f, "    {:?}", exp)?;
        }
        writeln!(f, "  ]")?;
    }

    // todo: suggest ...

    Ok(())
}

pub fn display_parse_of_error<'s>(
    f: &mut Formatter<'_>,
    err: &ParseOFError<'s>,
) -> std::fmt::Result {
    write!(f, "DISPLAY_PARSE: {} ", err.code)?;
    if let Some(expect) = &err.expect {
        write!(f, "[")?;
        for exp in expect {
            write!(f, " {}", exp)?;
        }
        write!(f, "]")?;
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

fn rec_suggest(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    if sug.codes.len() == 0 {
        for next in &sug.next {
            rec_suggest(f, next, ind)?;
        }
    } else {
        writeln!(f)?;
        indent(f, ind)?;
        write!(f, " <- {}: ", sug.func)?;
        for (code, span) in &sug.codes {
            write!(f, "{} {}:\"{}\" ", code, span.location_offset(), span)?;
        }

        for next in &sug.next {
            rec_suggest(f, next, ind + 1)?;
        }
    }

    Ok(())
}

pub fn debug_suggest(f: &mut Formatter<'_>, sug: &Suggest<'_>, ind: usize) -> fmt::Result {
    indent(f, ind)?;
    write!(f, "DEBUG_SUGGEST:")?;
    rec_suggest(f, sug, ind)
}
