use crate::error::Span;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parse::{ParseError, ParseResult, Parser};

impl Parser {
    const MAX_DEPTH: usize = 200;

    pub(super) fn parse_rule<F, R>(&mut self, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        self.enter_parse_rule()?;
        let result = f(self);
        self.leave_parse_rule();
        result
    }

    /// Returns the next token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub(super) fn next(&mut self) -> Token {
        self.tokens.next().expect("Stepped beyond EOF")
    }

    /// Returns peeked token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't peek
    /// more after it gets an EOF
    pub(super) fn peek(&mut self) -> &Token {
        self.tokens.peek().expect("Peeked beyond EOF")
    }

    /// Won't panic if peeked beyond EOF
    pub(super) fn maybe_peek_nth_kind(&mut self, n: usize) -> Option<&TokenKind> {
        self.tokens.peek_nth(n).map(|token| &token.kind)
    }

    /// Returns the kind of a peeked token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't peek
    /// more after it gets an EOF
    pub(super) fn peek_kind(&mut self) -> &TokenKind {
        &self.peek().kind
    }

    /// Returns the next token if it matches the expected kind
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub(super) fn try_consume_kind(&mut self, expected_kind: TokenKind) -> Option<Token> {
        if self.peek_kind() == &expected_kind {
            Some(self.next())
        } else {
            None
        }
    }

    /// Returns the span of the next token, and an error if it doesn't match
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub(super) fn expect_kind(&mut self, expected_kind: TokenKind) -> ParseResult<Span> {
        let next = self.next();
        if next.kind == expected_kind {
            Ok(next.span)
        } else {
            Err(ParseError::full(
                next.span,
                format!("expected {}, found {}", expected_kind, next.kind),
                "Although I do know what the next token must be, I cannot just make the assumption and treat the next token like it was the token I want.".to_string(),
                "replace the token with the one I expected.".to_string(),
            ))
        }
    }

    pub(super) fn expect_kinds<const N: usize>(
        &mut self,
        expected_kinds: [TokenKind; N],
    ) -> ParseResult<()> {
        for kind in expected_kinds {
            self.expect_kind(kind)?;
        }
        Ok(())
    }

    fn enter_parse_rule(&mut self) -> ParseResult<()> {
        self.depth += 1;
        if self.depth > Self::MAX_DEPTH {
            Err(ParseError::full(
                self.tokens
                    .peek()
                    .map_or_else(Span::dummy, |token| token.span),
                "Nesting too deep".to_string(),
                "I can only nest myself so deep, more is not possible, because I don't want to have a stack overflow!".to_string(),
                "extract the very nested structure to a variable or function".to_string(),
            ))
        } else {
            Ok(())
        }
    }

    fn leave_parse_rule(&mut self) {
        self.depth -= 1;
    }
}
