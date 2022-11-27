use crate::ast::tracer::{
    DebugTrack, EnterTrack, ErrTrack, ExitTrack, ExpectTrack, OkTrack, StepTrack, SuggestTrack,
    Tracer, Track,
};
use crate::error::DebugWidth;
use std::fmt;
use std::fmt::{Debug, Formatter};

impl<'s> Debug for Tracer<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match f.width() {
            None | Some(0) => debug_tracer(f, DebugWidth::Short, self, &|_| true),
            Some(1) => debug_tracer(f, DebugWidth::Medium, self, &|_| true),
            Some(2) => debug_tracer(f, DebugWidth::Long, self, &|_| true),
            _ => Ok(()),
        }
    }
}

fn indent(f: &mut Formatter<'_>, ind: usize) -> fmt::Result {
    write!(f, "{}", " ".repeat(ind * 2))?;
    Ok(())
}

pub(crate) fn debug_tracer(
    f: &mut Formatter<'_>,
    w: DebugWidth,
    v: &Tracer<'_>,
    filter: &dyn Fn(&Track<'_>) -> bool,
) -> fmt::Result {
    let mut ind = 0;

    writeln!(f, "trace")?;

    for t in &*v.track.borrow() {
        match t {
            Track::Enter(_) => {
                if filter(t) {
                    ind += 1;
                    indent(f, ind)?;
                    debug_track(f, w, t)?;
                    writeln!(f)?;
                }
            }
            Track::Step(_)
            | Track::Debug(_)
            | Track::Expect(_)
            | Track::Suggest(_)
            | Track::Ok(_)
            | Track::Err(_) => {
                if filter(t) {
                    indent(f, ind)?;
                    debug_track(f, w, t)?;
                    writeln!(f)?;
                }
            }
            Track::Exit(_) => {
                if filter(t) {
                    // indent(f, ind)?;
                    // debug_track(f, w, t)?;
                    // writeln!(f)?;
                    ind -= 1;
                }
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
            writeln!(f, "{}: {:?}", exp.func, exp.list)?;
        }
        writeln!(f)?;
    }

    if !v.suggest.borrow().is_empty() {
        write!(f, "    suggest=")?;
        for sug in &*v.suggest.borrow() {
            writeln!(f, "{}: {:?}", sug.func, sug.list)?;
        }
        writeln!(f)?;
    }

    Ok(())
}

fn debug_track(f: &mut Formatter<'_>, w: DebugWidth, v: &Track<'_>) -> fmt::Result {
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

fn debug_enter(f: &mut Formatter<'_>, w: DebugWidth, v: &EnterTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: parse \"{}\"", v.func, v.span),
        DebugWidth::Long => write!(f, "{}: parse \"{}\" <<{:?}", v.func, v.span, v.parents),
    }
}

fn debug_step(f: &mut Formatter<'_>, w: DebugWidth, v: &StepTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => {
            write!(f, "{}: {} \"{}\"", v.func, v.step, v.span)
        }
        DebugWidth::Long => write!(f, "{}: {} \"{}\" <<{:?}", v.func, v.step, v.span, v.parents),
    }
}

fn debug_debug(f: &mut Formatter<'_>, w: DebugWidth, v: &DebugTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: {}", v.func, v.dbg),
        DebugWidth::Long => write!(f, "{}: {} <<{:?}", v.func, v.dbg, v.parents),
    }
}

fn debug_expect(f: &mut Formatter<'_>, w: DebugWidth, v: &ExpectTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
        DebugWidth::Medium => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
        DebugWidth::Long => write!(f, "{}: {} expect {:?}", v.func, v.usage, v.list),
    }
}

fn debug_suggest(f: &mut Formatter<'_>, w: DebugWidth, v: &SuggestTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
        DebugWidth::Medium => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
        DebugWidth::Long => write!(f, "{}: {} suggest {:?}", v.func, v.usage, v.list),
    }
}
fn debug_ok(f: &mut Formatter<'_>, w: DebugWidth, v: &OkTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium | DebugWidth::Long => {
            if !v.span.is_empty() {
                write!(f, "{}: -> [ {}, '{}' ]", v.func, v.span, v.rest)?;
            } else {
                write!(f, "{}: -> no match", v.func)?;
            }
        }
    }
    Ok(())
}

fn debug_err(f: &mut Formatter<'_>, w: DebugWidth, v: &ErrTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium => write!(f, "{}: {} ", v.func, v.err),
        DebugWidth::Long => write!(f, "{}: {} <<{:?}", v.func, v.err, v.parents),
    }
}

fn debug_exit(f: &mut Formatter<'_>, w: DebugWidth, v: &ExitTrack<'_>) -> fmt::Result {
    match w {
        DebugWidth::Short | DebugWidth::Medium | DebugWidth::Long => {
            write!(f, "return {}: ", v.func)
        }
    }
}
