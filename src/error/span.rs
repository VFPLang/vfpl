#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Create a span with a start and length
    pub fn start_len(start: usize, len: usize) -> Self {
        Self {
            start,
            end: start + len,
        }
    }

    /// Create a span with a start and end
    pub fn start_end(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Create a span for a single character starting at `start`
    pub fn single(start: usize) -> Self {
        Self {
            start,
            end: start + 1,
        }
    }

    /// Create an empty span, useful for testing
    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }

    /// Extends the span by the second one
    /// The other one has to be after the current one
    pub fn extend(&self, other: Span) -> Span {
        debug_assert!(self.start <= other.start);
        debug_assert!(self.end <= other.end);
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Extends the span by the second one, if it exists
    /// The other one has to be after the current one, if it exists
    pub fn option_extend(&self, other: Option<Span>) -> Span {
        match other {
            None => *self,
            Some(span) => self.extend(span),
        }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}
