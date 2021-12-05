use crate::error::{CompilerError, Span};
use crate::VfplError;

#[derive(Debug)]
pub(super) struct ParseError {
    span: Span,
    message: String,
    note: Option<String>,
    suggestion: Option<String>,
}

impl ParseError {
    pub(super) fn simple(span: Span, message: String) -> Self {
        Self {
            span,
            message,
            note: None,
            suggestion: None,
        }
    }

    pub(super) fn full(span: Span, message: String, note: String, suggestion: String) -> Self {
        Self {
            span,
            message,
            note: Some(note),
            suggestion: Some(suggestion),
        }
    }
}

impl CompilerError for ParseError {
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

impl From<ParseError> for VfplError {
    fn from(error: ParseError) -> Self {
        Self {
            span: error.span(),
            message: error.message(),
            note: error.note(),
            suggestion: error.suggestion(),
        }
    }
}
