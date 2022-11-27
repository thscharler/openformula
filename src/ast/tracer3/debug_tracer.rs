use crate::ast::tracer3::{
    DebugTrack, EnterTrack, ErrTrack, ExitTrack, ExpectTrack, OkTrack, StepTrack, SuggestTrack,
    Tracer, Track,
};
use crate::error::DebugWidth;
use std::fmt;
use std::fmt::{Debug, Formatter};

impl<'s> Debug for ExpectTrack<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.func, self.list)?;
        Ok(())
    }
}

impl<'s> Debug for SuggestTrack<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.func, self.list)?;
        Ok(())
    }
}

impl<'s> Debug for Tracer<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match f.width() {
            None | Some(0) => debug_tracer(f, DebugWidth::Short, self),
            Some(1) => debug_tracer(f, DebugWidth::Medium, self),
            Some(2) => debug_tracer(f, DebugWidth::Long, self),
            _ => Ok(()),
        }
    }
}

impl<'s> Debug for Track<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        debug_track(f, DebugWidth::Short, self)
    }
}

fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
    write!(f, "{}", " ".repeat(ind * 2))?;
    Ok(())
}

pub fn debug_tracer(f: &mut Formatter<'_>, w: DebugWidth, v: &Tracer<'_>) -> fmt::Result {
    let mut ind = 0;

    writeln!(f, "trace")?;

    for t in &*v.track.borrow() {
        match t {
            Track::Enter(_) => {
                ind += 1;
                indent(f, ind)?;
                debug_track(f, w, t)?;
                writeln!(f)?;
            }
            Track::Step(_)
            | Track::Debug(_)
            | Track::Expect(_)
            | Track::Suggest(_)
            | Track::Ok(_)
            | Track::Err(_) => {
                indent(f, ind)?;
                debug_track(f, w, t)?;
                writeln!(f)?;
            }
            Track::Exit(_) => {
                indent(f, ind)?;
                debug_track(f, w, t)?;
                writeln!(f)?;
                ind -= 1;
            }
        }
    }

    if !v.func.borrow().is_empty() {
        write!(f, "    func=")?;
        for func in &*v.func.borrow() {
            write!(f, "{:?} ", func)?;
        }
        writeln!(f)?;
    }

    if !v.expect.borrow().is_empty() {
        write!(f, "    expect=")?;
        for exp in &*v.expect.borrow() {
            writeln!(f, "{:?} ", exp)?;
        }
        writeln!(f)?;
    }

    if !v.suggest.borrow().is_empty() {
        write!(f, "    suggest=")?;
        for sug in &*v.suggest.borrow() {
            writeln!(f, "{:?} ", sug)?;
        }
        writeln!(f)?;
    }

    Ok(())
}

pub fn debug_track(f: &mut Formatter<'_>, w: DebugWidth, v: &Track<'_>) -> fmt::Result {
    match v {
        Track::Enter(v) => debug_enter(f, w, v),
        Track::Step(v) => debug_step(f, w, v),
        Track::Debug(v) => debug_debug(f, w, v),
        Track::Expect(v) => debug_expect(f, w, v),
        Track::Suggest(v) => debug_suggest(f, w, v),
        Track::Ok(v) => debug_ok(f, w, v),
        Track::Err(v) => debug_err(f, w, v),
        Track::Exit(v) => debug_exit(f, w, v),
    }
}

pub fn debug_enter(f: &mut Formatter<'_>, w: DebugWidth, v: &EnterTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: \"{}\"", v.func, v.span),
        DebugWidth::Long => write!(f, "{}: \"{}\" <<{:?}", v.func, v.span, v.list),
    }
}

pub fn debug_step(f: &mut Formatter<'_>, w: DebugWidth, v: &StepTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => {
            write!(f, "{}: {} \"{}\"", v.func, v.step, v.span)
        }
        DebugWidth::Long => write!(f, "{}: {} \"{}\" <<{:?}", v.func, v.step, v.span, v.list),
    }
}

pub fn debug_debug(f: &mut Formatter<'_>, w: DebugWidth, v: &DebugTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: {}", v.func, v.dbg),
        DebugWidth::Long => write!(f, "{}: {} <<{:?}", v.func, v.dbg, v.list),
    }
}

pub fn debug_expect(f: &mut Formatter<'_>, w: DebugWidth, v: &ExpectTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
        DebugWidth::Medium => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
        DebugWidth::Long => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
    }
}

pub fn debug_suggest(f: &mut Formatter<'_>, w: DebugWidth, v: &SuggestTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
        DebugWidth::Medium => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
        DebugWidth::Long => write!(f, "{}:{:?} {:?}", v.func, v.usage, v.list),
    }
}
pub fn debug_ok(f: &mut Formatter<'_>, w: DebugWidth, v: &OkTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => {
            if !v.span.is_empty() {
                write!(f, "{}: -> [ {}, '{}' ]", v.func, v.span, v.rest)?;
            } else {
                write!(f, "{}: -> no match", v.func)?;
            }
        }
        DebugWidth::Long => {
            if !v.span.is_empty() {
                write!(f, "{}: -> [ {}, '{}' ]", v.func, v.span, v.rest)?;
            } else {
                write!(f, "{}: -> no match", v.func)?;
            }
        }
    }
    Ok(())
}

pub fn debug_err(f: &mut Formatter<'_>, w: DebugWidth, v: &ErrTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: err={} ", v.func, v.err),
        DebugWidth::Long => write!(f, "{}: err={} <<{:?}", v.func, v.err, v.list),
    }
}

pub fn debug_exit(f: &mut Formatter<'_>, w: DebugWidth, v: &ExitTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium | DebugWidth::Long => {
            write!(f, "return {}: ", v.func)
        }
    }
}
