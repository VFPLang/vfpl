use crate::{ParseError, ParseResult, Parser};
use fastrand::Rng;
use vfpl_error::Span;
use vfpl_lexer::tokens::{CondKeyword, Token, TokenKind};

impl Parser {
    const MAX_DEPTH: usize = 200;

    pub fn display_kind(&self, kind: &TokenKind) -> String {
        let global_ctx = self.global_ctx.borrow();

        kind.display(&global_ctx)
    }

    pub fn rng(&self) -> Rng {
        self.global_ctx.borrow().sess().rng().clone()
    }

    pub fn parse_rule<F, R>(&mut self, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        self.enter_parse_rule()?;
        let result = f(self);
        self.leave_parse_rule();
        result
    }

    /// Parses a list of things
    /// The signature got out of hand
    pub fn list<R>(
        &mut self,
        single_indicator: TokenKind,
        multi_indicator: TokenKind,
        mut single_parser: impl FnMut(&mut Parser) -> ParseResult<R>,
        mut get_span: impl FnMut(&R) -> Span,
        inner_error: impl FnOnce(&Token, &Parser) -> ParseError,
        outer_error: impl FnOnce(&Token, &Parser) -> ParseError,
    ) -> ParseResult<(Vec<R>, Span)> {
        self.parse_rule(|parser| {
            if let Some(token) = parser.try_consume_kind(TokenKind::CondKw(CondKeyword::No)) {
                let multi_span = parser.expect_kind(multi_indicator.clone())?;
                Ok((vec![], token.span.extend(multi_span)))
            } else if let Some(the_token) =
                parser.try_consume_kind(TokenKind::CondKw(CondKeyword::The))
            {
                if parser.try_consume_kind(single_indicator.clone()).is_some() {
                    // only a single element

                    let value = single_parser(parser)?;
                    let value_span = get_span(&value);

                    Ok((vec![value], the_token.span.extend(value_span)))
                } else if parser.try_consume_kind(multi_indicator.clone()).is_some() {
                    // multiple elements

                    let value = single_parser(parser)?;

                    let mut values = vec![value];

                    while parser.try_consume_kind(TokenKind::Comma).is_some() {
                        let value = single_parser(parser)?;
                        values.push(value);
                    }

                    parser.expect_kind(TokenKind::And)?;

                    let last_value = single_parser(parser)?;
                    let last_arg_span = get_span(&last_value);
                    values.push(last_value);

                    Ok((values, the_token.span.extend(last_arg_span)))
                } else {
                    // thank you borrow checker
                    let next = parser.peek().clone();
                    Err(inner_error(&next, parser))
                }
            } else {
                // thank you borrow checker
                let next = parser.peek().clone();
                Err(outer_error(&next, parser))
            }
        })
    }

    /// Returns the next token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub fn next(&mut self) -> Token {
        self.tokens.next().expect("Stepped beyond EOF")
    }

    /// Returns peeked token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't peek
    /// more after it gets an EOF
    pub fn peek(&mut self) -> &Token {
        self.tokens.peek().expect("Peeked beyond EOF")
    }

    /// Won't panic if peeked beyond EOF
    pub fn maybe_peek_nth_kind(&mut self, n: usize) -> Option<&TokenKind> {
        self.tokens.peek_nth(n).map(|token| &token.kind)
    }

    /// Returns the kind of a peeked token
    /// This panics if it doesn't have any more tokens, since the parser shouldn't peek
    /// more after it gets an EOF
    pub fn peek_kind(&mut self) -> &TokenKind {
        &self.peek().kind
    }

    /// Returns the next token if it matches the expected kind
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub fn try_consume_kind(&mut self, expected_kind: TokenKind) -> Option<Token> {
        if self.peek_kind() == &expected_kind {
            Some(self.next())
        } else {
            None
        }
    }

    /// Returns the span of the next token, and an error if it doesn't match
    /// This panics if it doesn't have any more tokens, since the parser shouldn't advance
    /// more after it gets an EOF
    pub fn expect_kind(&mut self, expected_kind: TokenKind) -> ParseResult<Span> {
        let next = self.next();
        if next.kind == expected_kind {
            Ok(next.span)
        } else {
            Err(ParseError::full(
                next.span,
                format!("expected {}, found {}", self.display_kind(&expected_kind), self.display_kind(&next.kind)),
                "Although I do know what the next token must be, I cannot just make the assumption and treat the next token like it was the token I want.".to_string(),
                "replace the token with the one I expected.".to_string(),
            ))
        }
    }

    pub fn expect_kinds<const N: usize>(
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
