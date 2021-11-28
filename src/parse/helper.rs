use crate::error::Span;
use crate::lexer::tokens::{Token, TokenType};
use crate::parse::{ParseError, ParseResult, Parser};

impl Parser {
    const MAX_DEPTH: usize = 500;

    pub(super) fn parse_rule<F, R>(&mut self, f: F) -> ParseResult<R>
    where
        F: FnOnce(&mut Self) -> ParseResult<R>,
    {
        self.enter_parse_rule()?;
        let result = f(self);
        self.leave_parse_rule();
        result
    }

    pub(super) fn next(&mut self) -> ParseResult<Token> {
        self.tokens.next().ok_or_else(|| ParseError {
            span: Span::dummy(),
            message: "reached end of file".to_string(),
        })
    }

    pub(super) fn peek_kind(&mut self) -> ParseResult<&TokenType> {
        self.tokens
            .peek()
            .map(|token| &token.kind)
            .ok_or_else(|| ParseError {
                span: Span::dummy(),
                message: "reached end of file".to_string(),
            })
    }

    pub(super) fn try_consume_kind(
        &mut self,
        expected_kind: TokenType,
    ) -> ParseResult<Option<Token>> {
        if self.peek_kind()? == &expected_kind {
            Ok(Some(self.next()?))
        } else {
            Ok(None)
        }
    }

    pub(super) fn expect_kind(&mut self, expected_kind: TokenType) -> ParseResult<Span> {
        let next = self.next()?;
        if next.kind == expected_kind {
            Ok(next.span)
        } else {
            Err(ParseError {
                span: next.span,
                message: format!("expected {}, found {}", expected_kind, next.kind),
            })
        }
    }

    pub(super) fn expect_kinds<const N: usize>(
        &mut self,
        expected_kinds: [TokenType; N],
    ) -> ParseResult<()> {
        for kind in expected_kinds {
            self.expect_kind(kind)?;
        }
        Ok(())
    }

    fn enter_parse_rule(&mut self) -> ParseResult<()> {
        self.depth += 1;
        if self.depth > Self::MAX_DEPTH {
            Err(ParseError {
                span: self
                    .tokens
                    .peek()
                    .map(|token| token.span)
                    .unwrap_or_else(Span::dummy),
                message: "Nesting too deep".to_string(),
            })
        } else {
            Ok(())
        }
    }

    fn leave_parse_rule(&mut self) {
        self.depth -= 1;
    }
}
