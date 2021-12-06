use std::fmt::{Display, Formatter};
use vfpl_error::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    CondKeyword(CondKeyword),
    Ident(String),
    // Keywords
    Please,
    As,
    Initialize,
    Variable,
    End,
    Check,
    Whether,
    Then,
    Do,
    Otherwise,
    Break,
    This,
    Create,
    Function,
    Call,
    And,
    Absent,
    Null,
    NoValue,
    Undefined,
    True,
    False,
    Not,
    Or,
    Repeat,
    Return,
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
    Sub,
    Mul,
    Div,
    Mod,
    With,
    The,
    Value,
    Of,
    Set,
    To,
    From,
    By,
    Take,
    Out,
    Parameter,
    Parameters,
    That,
    Returns,
    No,
    Argument,
    Arguments,
    Go,
    Sleep,
    Does,
    Has,
    Is,
    Have,
    Greater,
    Less,
    Than,
    Equal,
}

impl CondKeyword {
    fn as_str(&self) -> &'static str {
        match self {
            CondKeyword::Add => "add",
            CondKeyword::Sub => "sub",
            CondKeyword::Mul => "mul",
            CondKeyword::Div => "div",
            CondKeyword::Mod => "mod",
            CondKeyword::With => "with",
            CondKeyword::The => "the",
            CondKeyword::Value => "value",
            CondKeyword::Of => "of",
            CondKeyword::Set => "set",
            CondKeyword::To => "to",
            CondKeyword::From => "from",
            CondKeyword::By => "by",
            CondKeyword::Take => "take",
            CondKeyword::Out => "out",
            CondKeyword::Parameter => "parameter",
            CondKeyword::Parameters => "parameters",
            CondKeyword::That => "that",
            CondKeyword::Returns => "returns",
            CondKeyword::No => "no",
            CondKeyword::Argument => "argument",
            CondKeyword::Arguments => "arguments",
            CondKeyword::Go => "go",
            CondKeyword::Sleep => "sleep",
            CondKeyword::Does => "does",
            CondKeyword::Has => "has",
            CondKeyword::Is => "is",
            CondKeyword::Have => "have",
            CondKeyword::Greater => "greater",
            CondKeyword::Less => "less",
            CondKeyword::Than => "than",
            CondKeyword::Equal => "equal",
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Please => f.write_str("keyword `please`"),
            TokenKind::Ident(name) => f.write_str(&format!("`{}`", name)),
            TokenKind::As => f.write_str("keyword `as`"),
            TokenKind::Initialize => f.write_str("keyword `initialize`"),
            TokenKind::Variable => f.write_str("keyword `variable`"),
            TokenKind::End => f.write_str("keyword `end`"),
            TokenKind::Check => f.write_str("keyword `check`"),
            TokenKind::Whether => f.write_str("keyword `whether`"),
            TokenKind::Then => f.write_str("keyword `then`"),
            TokenKind::Do => f.write_str("keyword `do`"),
            TokenKind::Otherwise => f.write_str("keyword `otherwise`"),
            TokenKind::Break => f.write_str("keyword `break`"),
            TokenKind::This => f.write_str("keyword `this`"),
            TokenKind::Create => f.write_str("keyword `create`"),
            TokenKind::Function => f.write_str("keyword `function`"),
            TokenKind::Absent => f.write_str("keyword `absent`"),
            TokenKind::Null => f.write_str("keyword `null`"),
            TokenKind::NoValue => f.write_str("keyword `novalue` )"),
            TokenKind::Undefined => f.write_str("keyword `undefined`"),
            TokenKind::True => f.write_str("keyword `true`"),
            TokenKind::False => f.write_str("keyword `false`"),
            TokenKind::Not => f.write_str("keyword `not`"),
            TokenKind::Or => f.write_str("keyword `or`"),
            TokenKind::Call => f.write_str("keyword `call`"),
            TokenKind::Repeat => f.write_str("keyword `repeat`"),
            TokenKind::Return => f.write_str("keyword `return`"),
            TokenKind::String(value) => f.write_str(&format!("`\"{}\"`", value)),
            TokenKind::Int(value) => f.write_str(&format!("`{}`", value)),
            TokenKind::Float(value) => f.write_str(&format!("`{}`", value)),
            TokenKind::Dot => f.write_str("`.`"),
            TokenKind::ParenOpen => f.write_str("`(`"),
            TokenKind::ParenClose => f.write_str("`)`"),
            TokenKind::Comma => f.write_str("`,`"),
            TokenKind::And => f.write_str("keyword `and`"),
            TokenKind::While => f.write_str("keyword `while`"),
            TokenKind::Eof => f.write_str("end of file"),
            TokenKind::CondKeyword(cond) => f.write_str(&format!("keyword `{}`", cond.as_str())),
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
