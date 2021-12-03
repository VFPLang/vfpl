mod span;

pub use span::Span;
use std::fmt::Debug;

pub trait CompilerError {
    fn span(&self) -> Span;
    fn message(&self) -> String;
    fn note(&self) -> Option<String>;
}

pub fn display_error<E>(source: &str, error: E)
where
    E: CompilerError + Debug,
{
    let mut chars = 0;
    let lines = source.split_inclusive('\n').enumerate();
    for (idx, line) in lines {
        if chars + line.len() + 1 > error.span().start {
            let offset_on_line = error.span().start - chars;

            println!("{}error: {}{}", RED, error.message(), RESET);
            println!("      {}|{}", CYAN, RESET);
            println!(
                "{}{:>5} |{} {}",
                CYAN,
                idx + 1,
                RESET,
                &line[..line.len() - 1]
            );
            print!("      {}|{} ", CYAN, RESET);
            println!(
                "{}{}{}{}",
                " ".repeat(offset_on_line),
                RED,
                "^".repeat(error.span().len()),
                RESET,
            );
            if let Some(note) = error.note() {
                println!("      {}|{}", CYAN, RESET);
                println!(
                    "      {}|{}   {}note: {}{}",
                    CYAN, RESET, GREEN, note, RESET
                );
            }
            break;
        }
        chars += line.len();
    }
}

macro_rules! color {
    ($name:ident: $value:literal) => {
        const $name: &str = concat!("\x1B[", $value);
    };
}

color!(RED: "0;31m");
color!(RESET: "0m");
color!(CYAN: "0;36m");
color!(GREEN: "0;32m");
