use crate::{CompilerError, Span};
use fastrand::Rng;
use std::cell::RefCell;
use std::fmt::Debug;
use std::io;
use std::io::Write;
use std::rc::Rc;
use vfpl_global::GlobalCtx;

/// A wrapper around a color that is not Display
struct ColorWrapper(&'static str);

impl ColorWrapper {
    fn display(&self, with_color: bool) -> &'static str {
        if with_color {
            self.0
        } else {
            ""
        }
    }
}

pub fn display_error<E, W>(
    source: &str,
    error: E,
    mut w: W,
    with_color: bool,
    global_ctx: Rc<RefCell<GlobalCtx>>,
) -> io::Result<()>
where
    E: CompilerError + Debug,
    W: Write,
{
    let global_ctx = global_ctx.borrow();
    let rng = global_ctx.sess().rng();

    let span = if error.span() == Span::eof() {
        // todo this should be handled better
        Span::single(source.len() - 1)
    } else {
        error.span()
    };

    let mut chars = 0;
    let lines = source.split_inclusive('\n').enumerate();
    for (idx, line) in lines {
        if chars + line.len() + 1 > span.start {
            let offset_on_line = span.start - chars;

            writeln!(
                w,
                "{}error: {}{}",
                RED.display(with_color),
                error.message(),
                RESET.display(with_color)
            )?;
            writeln!(
                w,
                "      {}|{}",
                CYAN.display(with_color),
                RESET.display(with_color)
            )?;
            writeln!(
                w,
                "{}{:>5} |{} {}",
                CYAN.display(with_color),
                idx + 1,
                RESET.display(with_color),
                &line[..line.len() - 1]
            )?;
            write!(
                w,
                "      {}|{} ",
                CYAN.display(with_color),
                RESET.display(with_color)
            )?;
            writeln!(
                w,
                "{}{}{}{}",
                " ".repeat(offset_on_line),
                RED.display(with_color),
                "^".repeat(span.len()),
                RESET.display(with_color),
            )?;
            if let Some(note) = error.note() {
                writeln!(
                    w,
                    "      {}|{}",
                    CYAN.display(with_color),
                    RESET.display(with_color)
                )?;
                writeln!(
                    w,
                    "\n{}{}{}",
                    GREEN.display(with_color),
                    note,
                    RESET.display(with_color)
                )?;
            }
            if let Some(suggestion) = error.suggestion() {
                write_suggestion(w, suggestion, rng)?;
            }
            break;
        }
        chars += line.len();
    }

    Ok(())
}

fn write_suggestion<W: Write>(mut w: W, suggestion: String, rng: &Rng) -> io::Result<()> {
    const SUGGEST_LEN: usize = 6;
    // bias it
    const SUGGEST_START: [&str; SUGGEST_LEN] = [
        "You could",
        "You could",
        "You could",
        "I have thought about it and came to the conclusion that you should be able to",
        "I think you should",
        "I think you should",
    ];

    let index = rng.usize(0..SUGGEST_LEN);
    let start = SUGGEST_START[index];

    writeln!(w, "{} {}", start, suggestion)
}

macro_rules! color {
    ($name:ident: $value:literal) => {
        const $name: ColorWrapper = ColorWrapper(concat!("\x1B[", $value));
    };
}

color!(RED: "0;31m");
color!(RESET: "0m");
color!(CYAN: "0;36m");
color!(GREEN: "0;32m");
