#![allow(dead_code)]

use std::iter::Peekable;
use std::option::Option::Some;
use std::str::CharIndices;

use unicode_xid::UnicodeXID;

use tokens::Token;

use crate::error::Span;
use crate::lexer::helper::compute_keyword;
use crate::lexer::tokens::TokenKind;

mod helper;
#[cfg(test)]
mod test;
pub mod tokens;

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug)]
pub enum LexerError {
    UnexpectedEOF,
    IntParseError(Span),
    FloatParseError(Span),
    InvalidCharacter(Span),
    InvalidEscapeCharacter(Span),
}

#[derive(Debug)]
pub struct Lexer<'a> {
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
                '0'..='9' | '-' => tokens.push(self.compute_number(char, idx)?),
                other if other.is_whitespace() => {}
                other if other.is_xid_start() => tokens.push(self.compute_identifier(char, idx)?),
                _ => todo!(),
            }
        }
        Ok(tokens)
    }

    fn compute_identifier(&mut self, char: char, idx: usize) -> LexerResult<Token> {
        let mut identifier = String::from(char);
        while let Some((_, char)) = self.peek() {
            if char.is_xid_continue() {
                self.next();
                identifier.push(char);
            } else {
                break;
            }
        }
        let end = identifier.len();
        let kind = compute_keyword(&identifier).unwrap_or(TokenKind::Ident(identifier));
        Ok(Token::new_from_len(kind, idx, end))
    }

    fn compute_number(&mut self, char: char, idx: usize) -> LexerResult<Token> {
        let mut number = String::from(char);
        let mut is_decimal = false;
        while let Some((i, n)) = self.peek() {
            match n {
                '.' if is_decimal => {
                    let number = number
                        .parse::<f64>()
                        .map_err(|_| LexerError::FloatParseError(Span::start_end(idx, i)))?;
                    return Ok(Token::new(TokenKind::Float(number), idx, i));
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
                        return Ok(Token::new(TokenKind::Int(number), idx, i));
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
                _ => break,
            }
        }

        let len = number.len();
        if is_decimal {
            Ok(Token::new_from_len(
                TokenKind::Float(
                    number
                        .parse()
                        .map_err(|_| LexerError::FloatParseError(Span::start_len(idx, len)))?,
                ),
                idx,
                number.len(),
            ))
        } else {
            Ok(Token::new_from_len(
                TokenKind::Int(
                    number
                        .parse()
                        .map_err(|_| LexerError::IntParseError(Span::start_len(idx, len)))?,
                ),
                idx,
                len,
            ))
        }
    }

    fn compute_string(&mut self, start: usize) -> LexerResult<Token> {
        let mut str = String::new();
        while let Some((idx, char)) = self.next() {
            match char {
                '\\' => {
                    let (_, next_char) = self.next().ok_or(LexerError::UnexpectedEOF)?;
                    match next_char {
                        '"' => str.push('\"'),
                        'n' => str.push('\n'),
                        'r' => str.push('\r'),
                        't' => str.push('\t'),
                        '0' => str.push('\0'),
                        '\\' => str.push('\\'),
                        _ => {
                            return Err(LexerError::InvalidEscapeCharacter(Span::start_end(
                                idx,
                                idx + 1,
                            )))
                        }
                    }
                }
                '"' => {
                    str.push(char);
                    return Ok(Token::new(TokenKind::String(str), start, idx));
                }
                other => str.push(other),
            }
        }
        Err(LexerError::UnexpectedEOF)
    }
}
