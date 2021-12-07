use std::fmt::{Display, Formatter};
use vfpl_error::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    CondKw(CondKeyword),
    Ident(String),
    // Keywords
    Absent,
    And,
    As,
    Break,
    Call,
    Check,
    Create,
    Define,
    Do,
    End,
    False,
    Function,
    Initialize,
    NoValue,
    Not,
    Null,
    Or,
    Otherwise,
    Please,
    Repeat,
    Return,
    Structure,
    Then,
    This,
    True,
    Undefined,
    Variable,
    Whether,
    While,

    // literals
    String(String),
    Int(i64),
    Float(f64),

    // symbols
    /// .
    Dot,
    /// (
    ParenOpen,
    /// )
    ParenClose,
    /// ,
    Comma,

    /// The last token
    Eof,
}

/// A conditional keyword that can be used as an identifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CondKeyword {
    Add,
    Argument,
    Arguments,
    By,
    Div,
    Does,
    Equal,
    Field,
    Fields,
    From,
    Go,
    Greater,
    Has,
    Have,
    Is,
    Less,
    Mod,
    Mul,
    No,
    Of,
    Out,
    Parameter,
    Parameters,
    Returns,
    Set,
    Sleep,
    Sub,
    Take,
    Than,
    That,
    The,
    To,
    Value,
    With,
}

impl AsRef<str> for CondKeyword {
    fn as_ref(&self) -> &'static str {
        match self {
            CondKeyword::Add => "add",
            CondKeyword::Argument => "argument",
            CondKeyword::Arguments => "arguments",
            CondKeyword::By => "by",
            CondKeyword::Div => "div",
            CondKeyword::Does => "does",
            CondKeyword::Equal => "equal",
            CondKeyword::From => "from",
            CondKeyword::Go => "go",
            CondKeyword::Greater => "greater",
            CondKeyword::Has => "has",
            CondKeyword::Have => "have",
            CondKeyword::Is => "is",
            CondKeyword::Less => "less",
            CondKeyword::Mod => "mod",
            CondKeyword::Mul => "mul",
            CondKeyword::No => "no",
            CondKeyword::Of => "of",
            CondKeyword::Out => "out",
            CondKeyword::Parameter => "parameter",
            CondKeyword::Parameters => "parameters",
            CondKeyword::Returns => "returns",
            CondKeyword::Set => "set",
            CondKeyword::Sleep => "sleep",
            CondKeyword::Sub => "sub",
            CondKeyword::Take => "take",
            CondKeyword::Than => "than",
            CondKeyword::That => "that",
            CondKeyword::The => "the",
            CondKeyword::To => "to",
            CondKeyword::Value => "value",
            CondKeyword::With => "with",
            CondKeyword::Field => "field",
            CondKeyword::Fields => "fields",
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Absent => f.write_str("keyword `absent`"),
            TokenKind::And => f.write_str("keyword `and`"),
            TokenKind::As => f.write_str("keyword `as`"),
            TokenKind::Break => f.write_str("keyword `break`"),
            TokenKind::Call => f.write_str("keyword `call`"),
            TokenKind::Check => f.write_str("keyword `check`"),
            TokenKind::Comma => f.write_str("`,`"),
            TokenKind::CondKw(cond) => f.write_str(&format!("keyword `{}`", cond.as_ref())),
            TokenKind::Create => f.write_str("keyword `create`"),
            TokenKind::Define => f.write_str("keyword `define`"),
            TokenKind::Do => f.write_str("keyword `do`"),
            TokenKind::Dot => f.write_str("`.`"),
            TokenKind::End => f.write_str("keyword `end`"),
            TokenKind::Eof => f.write_str("end of file"),
            TokenKind::False => f.write_str("keyword `false`"),
            TokenKind::Float(value) => f.write_str(&format!("`{}`", value)),
            TokenKind::Function => f.write_str("keyword `function`"),
            TokenKind::Ident(name) => f.write_str(&format!("`{}`", name)),
            TokenKind::Initialize => f.write_str("keyword `initialize`"),
            TokenKind::Int(value) => f.write_str(&format!("`{}`", value)),
            TokenKind::NoValue => f.write_str("keyword `novalue` )"),
            TokenKind::Not => f.write_str("keyword `not`"),
            TokenKind::Null => f.write_str("keyword `null`"),
            TokenKind::Or => f.write_str("keyword `or`"),
            TokenKind::Otherwise => f.write_str("keyword `otherwise`"),
            TokenKind::ParenClose => f.write_str("`)`"),
            TokenKind::ParenOpen => f.write_str("`(`"),
            TokenKind::Please => f.write_str("keyword `please`"),
            TokenKind::Repeat => f.write_str("keyword `repeat`"),
            TokenKind::Return => f.write_str("keyword `return`"),
            TokenKind::String(value) => f.write_str(&format!("`\"{}\"`", value)),
            TokenKind::Structure => f.write_str("keyword `structure`"),
            TokenKind::Then => f.write_str("keyword `then`"),
            TokenKind::This => f.write_str("keyword `this`"),
            TokenKind::True => f.write_str("keyword `true`"),
            TokenKind::Undefined => f.write_str("keyword `undefined`"),
            TokenKind::Variable => f.write_str("keyword `variable`"),
            TokenKind::Whether => f.write_str("keyword `whether`"),
            TokenKind::While => f.write_str("keyword `while`"),
        }
    }
}

impl Token {
    /// Token with a Span of one character
    pub fn new_from_single(kind: TokenKind, idx: usize) -> Self {
        Token {
            kind,
            span: Span::single(idx),
        }
    }

    pub fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            span: Span::start_end(start, end),
        }
    }

    pub fn new_from_len(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            span: Span::start_len(start, end),
        }
    }

    pub fn eof() -> Self {
        Token {
            span: Span::eof(),
            kind: TokenKind::Eof,
        }
    }
}
