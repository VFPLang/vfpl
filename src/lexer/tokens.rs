use std::fmt::{Display, Formatter};

use crate::error::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Please,

    Ident(String),
    // Keywords
    As,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Initialize,
    Variable,
    With,
    The,
    Value,
    Of,
    Set,
    To,
    From,
    By,
    Take,
    End,
    Check,
    Whether,
    Then,
    Do,
    Otherwise,
    Break,
    Out,
    This,
    Create,
    Function,
    Parameter,
    Parameters,
    That,
    Returns,
    Call,
    No,
    Argument,
    Arguments,
    And,
    Go,
    Sleep,
    Absent,
    Null,
    NoValue,
    Undefined,
    True,
    False,
    Does,
    Has,
    Is,
    Not,
    Have,
    Greater,
    Less,
    Than,
    Equal,
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
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Please => f.write_str("keyword `please`"),
            TokenKind::Ident(name) => f.write_str(&format!("`{}`", name)),
            TokenKind::As => f.write_str("keyword `as`"),
            TokenKind::Add => f.write_str("keyword `add`"),
            TokenKind::Sub => f.write_str("keyword `subtract`"),
            TokenKind::Mul => f.write_str("keyword `multiply`"),
            TokenKind::Div => f.write_str("keyword `divide`"),
            TokenKind::Mod => f.write_str("keyword `modulo`"),
            TokenKind::Initialize => f.write_str("keyword `initialize`"),
            TokenKind::Variable => f.write_str("keyword `variable`"),
            TokenKind::With => f.write_str("keyword `with`"),
            TokenKind::The => f.write_str("keyword `the`"),
            TokenKind::Value => f.write_str("keyword `value`"),
            TokenKind::Of => f.write_str("keyword `of`"),
            TokenKind::Set => f.write_str("keyword `set`"),
            TokenKind::To => f.write_str("keyword `to`"),
            TokenKind::From => f.write_str("keyword `from`"),
            TokenKind::By => f.write_str("keyword `by`"),
            TokenKind::Take => f.write_str("keyword `take`"),
            TokenKind::End => f.write_str("keyword `end`"),
            TokenKind::Check => f.write_str("keyword `check`"),
            TokenKind::Whether => f.write_str("keyword `whether`"),
            TokenKind::Then => f.write_str("keyword `then`"),
            TokenKind::Do => f.write_str("keyword `do`"),
            TokenKind::Otherwise => f.write_str("keyword `otherwise`"),
            TokenKind::Break => f.write_str("keyword `break`"),
            TokenKind::Out => f.write_str("keyword `out`"),
            TokenKind::This => f.write_str("keyword `this`"),
            TokenKind::Create => f.write_str("keyword `create`"),
            TokenKind::Function => f.write_str("keyword `function`"),
            TokenKind::Parameter => f.write_str("keyword `parameter`"),
            TokenKind::Parameters => f.write_str("keyword `parameters`"),
            TokenKind::That => f.write_str("keyword `that`"),
            TokenKind::Returns => f.write_str("keyword `returns`"),
            TokenKind::Go => f.write_str("keyword `go`"),
            TokenKind::Sleep => f.write_str("keyword `sleep`"),
            TokenKind::Absent => f.write_str("keyword `absent`"),
            TokenKind::Null => f.write_str("keyword `null`"),
            TokenKind::NoValue => f.write_str("keyword `no value` )"),
            TokenKind::Undefined => f.write_str("keyword `undefined`"),
            TokenKind::True => f.write_str("keyword `true`"),
            TokenKind::False => f.write_str("keyword `false`"),
            TokenKind::No => f.write_str("keyword `no`"),
            TokenKind::Argument => f.write_str("keyword `argument`"),
            TokenKind::Arguments => f.write_str("keyword `arguments`"),
            TokenKind::Does => f.write_str("keyword `does`"),
            TokenKind::Has => f.write_str("keyword `has`"),
            TokenKind::Is => f.write_str("keyword `is`"),
            TokenKind::Not => f.write_str("keyword `not`"),
            TokenKind::Have => f.write_str("keyword `have`"),
            TokenKind::Greater => f.write_str("keyword `greater`"),
            TokenKind::Less => f.write_str("keyword `less`"),
            TokenKind::Than => f.write_str("keyword `than`"),
            TokenKind::Equal => f.write_str("keyword `equal`"),
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
}
