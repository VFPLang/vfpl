use std::iter::Peekable;
use std::num::ParseFloatError;
use std::option::Option::Some;
use std::str::CharIndices;

use unicode_xid::UnicodeXID;

use tokens::Token;

use crate::error::Span;
use crate::lexer::tokens::TokenKind;

mod helper;
pub mod tokens;

type LexerResult<T> = Result<T, LexerError>;

enum LexerError {
    UnexpectedEOF,
    StringNotClosed(Span),
    IntParseError(Span),
    FloatParseError(Span),
}

struct Lexer<'a> {
    char_indices: Peekable<CharIndices<'a>>,
}

impl Lexer<'_> {
    pub fn next(&mut self) -> Option<(usize, char)> {
        self.char_indices.next()
    }

    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.char_indices.peek().cloned()
    }

    pub fn new(str: &str) -> Lexer {
        Lexer {
            char_indices: str.char_indices().peekable(),
        }
    }

    pub fn compute_tokens(&mut self) -> LexerResult<Vec<Token>> {
        let mut tokens = Vec::new();
        while let Some((idx, char)) = self.next() {
            match char {
                '.' => tokens.push(Token::new_from_single(TokenKind::Dot, idx)),
                '(' => tokens.push(Token::new_from_single(TokenKind::ParenOpen, idx)),
                ')' => tokens.push(Token::new_from_single(TokenKind::ParenClose, idx)),
                ',' => tokens.push(Token::new_from_single(TokenKind::Comma, idx)),
                '0'..='9' | '-' => self.compute_number(&mut tokens, char, idx)?,
                other if other.is_whitespace() => todo!(),
                other if other.is_xid_start() => todo!(),
                _ => todo!(),
            }
        }
        Ok(tokens)
    }

    fn compute_number(
        &mut self,
        tokens: &mut Vec<Token>,
        char: char,
        idx: usize,
    ) -> LexerResult<()> {
        let mut number = String::from(char);
        let mut is_decimal = false;
        while let Some((i, n)) = self.peek() {
            match n {
                '.' if is_decimal => {
                    let number = number
                        .parse::<f64>()
                        .map_err(|_| LexerError::FloatParseError(Span::start_end(idx, i)))?;
                    tokens.push(Token::new(TokenKind::Float(number), idx, i));
                    break;
                }
                '.' => {
                    self.next();
                    if self.peek().map_or(false, |(_, c)| c.is_ascii_digit()) {
                        number.push(n);
                        is_decimal = true;
                    } else {
                        let number = number
                            .parse::<i64>()
                            .map_err(|_| LexerError::IntParseError(Span::start_end(idx, i)))?;
                        tokens.push(Token::new(TokenKind::Int(number), idx, i));
                        tokens.push(Token::new_from_single(TokenKind::Dot, i));
                        break;
                    }
                }
                '0'..='9' => {
                    self.next();
                    number.push(n);
                }
                other if other.is_xid_start() => {
                    self.next();
                    return if is_decimal {
                        Err(LexerError::FloatParseError(Span::start_end(idx, i)))
                    } else {
                        Err(LexerError::IntParseError(Span::start_end(idx, i)))
                    };
                }
                _ => {
                    if is_decimal {
                        tokens.push(Token::new(
                            TokenKind::Float(number.parse().map_err(|_| {
                                LexerError::FloatParseError(Span::start_end(idx, i))
                            })?),
                            idx,
                            i,
                        ))
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Int(
                                number.parse().map_err(|_| {
                                    LexerError::IntParseError(Span::start_end(idx, i))
                                })?,
                            ),
                            idx,
                            i,
                        ))
                    }
                }
            }
        }
        Ok(())
    }

    fn compute_string(&mut self, start: usize) -> LexerResult<Token> {
        let mut str = String::new();
        while let Some((idx, char)) = self.next() {
            if char != '"' {
                str.push(char);
            } else {
                return Ok(Token {
                    span: Span::start_end(start, idx),
                    kind: TokenKind::Please,
                });
            }
        }
        Err(LexerError::StringNotClosed(Span::single(start)))
    }
}
