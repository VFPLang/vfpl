use fastrand::Rng;
use std::option::Option::Some;
use std::rc::Rc;
use std::str::CharIndices;

use peekmore::{PeekMore, PeekMoreIterator};
use unicode_xid::UnicodeXID;

use crate::error;
use tokens::Token;

use crate::error::{CompilerError, Span};
use crate::global::Session;
use crate::lexer::helper::compute_keyword;
use crate::lexer::tokens::TokenKind;

mod helper;
#[cfg(test)]
mod test;
pub mod tokens;

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug)]
pub struct Lexer<'a> {
    char_indices: PeekMoreIterator<CharIndices<'a>>,
    session: Rc<Session>,
}

impl Lexer<'_> {
    pub fn next(&mut self) -> Option<(usize, char)> {
        self.char_indices.next()
    }

    /// Consume elements n times
    pub fn consume_elements(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.char_indices.peek().copied()
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<(usize, char)> {
        self.char_indices.peek_nth(n).copied()
    }

    pub fn new(str: &str, session: Rc<Session>) -> Lexer {
        Lexer {
            char_indices: str.char_indices().peekmore(),
            session,
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
                '\"' => tokens.push(self.compute_string(idx)?),
                '<' => {
                    if self.peek_nth(0).ok_or(LexerError::UnexpectedEOF)?.1 == '!'
                        && self.peek_nth(1).ok_or(LexerError::UnexpectedEOF)?.1 == '-'
                        && self.peek_nth(2).ok_or(LexerError::UnexpectedEOF)?.1 == '-'
                    {
                        while let Some((_, char)) = self.next() {
                            if char == '-' {
                                if self.peek_nth(0).ok_or(LexerError::UnexpectedEOF)?.1 == '-'
                                    && self.peek_nth(1).ok_or(LexerError::UnexpectedEOF)?.1 == '>'
                                {
                                    self.consume_elements(2);
                                    break;
                                }
                            } else {
                                continue;
                            }
                        }
                    } else {
                        return Err(LexerError::InvalidCharacter(Span::single(idx)));
                    }
                }
                '0'..='9' | '-' => tokens.push(self.compute_number(char, idx)?),
                other if other.is_whitespace() => continue,
                other if other.is_xid_start() => tokens.push(self.compute_identifier(char, idx)),
                _ => return Err(LexerError::InvalidCharacter(Span::single(idx))),
            }
        }

        tokens.push(Token::eof());
        Ok(tokens)
    }

    fn compute_identifier(&mut self, char: char, idx: usize) -> Token {
        let mut identifier = String::from(char);
        while let Some((_, char)) = self.peek() {
            if char.is_xid_continue() {
                self.next();
                identifier.push(char);
            } else {
                break;
            }
        }
        identifier = identifier.to_lowercase();
        let end = identifier.len();
        let kind = compute_keyword(&identifier).unwrap_or(TokenKind::Ident(identifier));
        Token::new_from_len(kind, idx, end)
    }

    fn compute_number(&mut self, char: char, idx: usize) -> LexerResult<Token> {
        let mut number = String::from(char);
        let mut end = loop {
            if let Some((i, char)) = self.peek() {
                match char {
                    '0'..='9' => {
                        self.next();
                        number.push(char);
                    }
                    other if other.is_xid_start() => {
                        return Err(LexerError::InvalidCharacter(Span::single(i)))
                    }
                    _ => {
                        // This works because the character before it will be 1 byte long (0..9 || -)
                        break i;
                    }
                }
            } else {
                break idx + number.len();
            }
        };

        if let Some((i, char)) = self.peek() {
            if char == '.' {
                if let Some((i, char)) = self.peek_nth(1) {
                    println!("{}", char);
                    if char.is_ascii_digit() {
                        number.push('.');
                        self.next();
                        number.push(char);
                        self.next();

                        end = loop {
                            if let Some((i, char)) = self.peek() {
                                match char {
                                    '0'..='9' => {
                                        self.next();
                                        number.push(char)
                                    }
                                    other if other.is_xid_start() => {
                                        return Err(LexerError::InvalidCharacter(Span::single(i)));
                                    }
                                    _ => {
                                        // This works because the character before it will be 1 byte long (0..9 || -)
                                        break i;
                                    }
                                }
                            } else {
                                break idx + number.len();
                            }
                        }
                    } else if char.is_xid_start() {
                        return Err(LexerError::InvalidCharacter(Span::single(i)));
                    }
                }
            } else if char.is_ascii_alphabetic() {
                return Err(LexerError::LetterInNumber(Span::start_end(idx, i)));
            }
        }

        if number.contains('.') {
            let number = number.parse::<f64>().map_err(|_| {
                LexerError::FloatParseError(Span::start_end(idx, end), self.session.rng().clone())
            })?;
            Ok(Token::new(TokenKind::Float(number), idx, end))
        } else {
            let number = number.parse::<i64>().map_err(|_| {
                LexerError::IntParseError(Span::start_end(idx, end), self.session.rng().clone())
            })?;
            Ok(Token::new(TokenKind::Int(number), idx, end))
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
                            )));
                        }
                    }
                }
                '"' => {
                    return Ok(Token::new(TokenKind::String(str), start, idx));
                }
                other => str.push(other),
            }
        }
        Err(LexerError::UnexpectedEOF)
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedEOF,
    IntParseError(Span, Rng),
    FloatParseError(Span, Rng),
    InvalidCharacter(Span),
    InvalidEscapeCharacter(Span),
    LetterInNumber(Span),
}

impl CompilerError for LexerError {
    fn span(&self) -> Span {
        match self {
            LexerError::UnexpectedEOF => Span::dummy(),
            LexerError::IntParseError(span, _) => *span,
            LexerError::FloatParseError(span, _) => *span,
            LexerError::InvalidCharacter(span) => *span,
            LexerError::InvalidEscapeCharacter(span) => *span,
            LexerError::LetterInNumber(span) => *span,
        }
    }

    fn message(&self) -> String {
        match self {
            LexerError::UnexpectedEOF => "Unexpected EOF.".to_string(),
            LexerError::IntParseError(_, _) => "Error parsing integer literal.".to_string(),
            LexerError::FloatParseError(_, _) => "Error parsing float literal.".to_string(),
            LexerError::InvalidCharacter(_) => "Invalid character.".to_string(),
            LexerError::InvalidEscapeCharacter(_) => "Invalid escape character.".to_string(),
            LexerError::LetterInNumber(_) => "Invalid letter in number.".to_string(),
        }
    }

    fn note(&self) -> Option<String> {
        Some(match self {
            LexerError::UnexpectedEOF => "I still need something, but I just can't find it.".to_string(),
            LexerError::IntParseError(_, _) => "Maybe the number was too big or small? I can only handle a limited amount of your number power.".to_string(),
            LexerError::FloatParseError(_, _) => "Maybe the number was too big or small? I can only handle a limited amount of your number power.".to_string(),
            LexerError::InvalidCharacter(_) => "I tried really hard to understand what you mean, but I am not capable of understanding it. Thank you for your understanding.".to_string(),
            LexerError::InvalidEscapeCharacter(_) => r#"I am only able to process the following escape sequences: (\", \n, \r, \t, \0, \\)."#.to_string(),
            LexerError::LetterInNumber(_) => "You have a very nice number, I like it. But sadly there is a letter in there, and I don't want to just assume that it was not there. I am a firm believer of only doing what I'm explicitly told to.".to_string(),
        })
    }

    fn suggestion(&self) -> Option<String> {
        Some(match self {
            LexerError::UnexpectedEOF => r#"need to add a " somewhere."#.to_string(),
            LexerError::IntParseError(_, rng) => format!("use {} instead.", error::random_number(rng)),
            LexerError::FloatParseError(_, rng) => format!("try to use {} instead.", error::random_number(rng)),
            LexerError::InvalidCharacter(_) => "delete that character. I won't miss it for sure.".to_string(),
            LexerError::InvalidEscapeCharacter(_) => "open a pull request to https://github.com/VFPLang/vfpl to add the escape character.".to_string(),
            LexerError::LetterInNumber(_) => "remove the letter from the number. Numbers are quite introverted and like being alone, it's ok. If you really want to, you could add some special character like , or ) next to it as a friend.".to_string()
        })
    }
}
