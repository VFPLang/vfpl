mod display;
mod random;
mod span;

pub use display::display_error;
pub use random::{random_ident, random_number};
pub use span::Span;

/// A trait for any error emitted during process
pub trait CompilerError {
    /// Return the span where the error occurred, for nicer error reporting. If no span is found,
    /// use [Span::dummy].
    fn span(&self) -> Span;

    /// The main message of the compiler error. It should be polite and not too long, but doesn't
    /// need extra verbosity.
    ///
    /// This needs to be a full sentence.
    ///
    /// # Examples
    ///
    /// `You have tried to add a String to a function.`
    fn message(&self) -> String;

    /// An additional note to give more context to the message. It can be long, but also needs to be
    /// polite.
    ///
    /// This needs to be a full sentence.
    ///
    /// # Examples
    /// `Due to the constraints of our spacetime, we have not found a sensible
    /// way to add a String to a function.`
    fn note(&self) -> Option<String> {
        None
    }

    /// A suggestion about what the programmer could do to resolve the error. It must be polite,
    /// and is allowed to be very wrong and not what the programmer intended, but it should fix the problem.
    ///
    /// This needs to be appendable to the phrase `You could `
    ///
    /// # Example
    /// `add another String to the String, for example "Function".`
    fn suggestion(&self) -> Option<String> {
        None
    }
}

#[derive(Debug)]
pub struct VfplError {
    pub span: Span,
    pub message: String,
    pub note: Option<String>,
    pub suggestion: Option<String>,
}

impl CompilerError for VfplError {
    fn span(&self) -> Span {
        self.span
    }

    fn message(&self) -> String {
        self.message.clone()
    }

    fn note(&self) -> Option<String> {
        self.note.clone()
    }

    fn suggestion(&self) -> Option<String> {
        self.suggestion.clone()
    }
}
