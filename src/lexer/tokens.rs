use crate::error::Span;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Please,

    Ident(String),
    // Keywords
    As,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
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

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Please => f.write_str("keyword `please`"),
            TokenType::Ident(name) => f.write_str(&format!("`{}`", name)),
            TokenType::As => f.write_str("keyword `as`"),
            TokenType::Add => f.write_str("keyword `add`"),
            TokenType::Subtract => f.write_str("keyword `subtract`"),
            TokenType::Multiply => f.write_str("keyword `multiply`"),
            TokenType::Divide => f.write_str("keyword `divide`"),
            TokenType::Modulo => f.write_str("keyword `modulo`"),
            TokenType::Initialize => f.write_str("keyword `initialize`"),
            TokenType::Variable => f.write_str("keyword `variable`"),
            TokenType::With => f.write_str("keyword `with`"),
            TokenType::The => f.write_str("keyword `the`"),
            TokenType::Value => f.write_str("keyword `value`"),
            TokenType::Of => f.write_str("keyword `of`"),
            TokenType::Set => f.write_str("keyword `set`"),
            TokenType::To => f.write_str("keyword `to`"),
            TokenType::From => f.write_str("keyword `from`"),
            TokenType::By => f.write_str("keyword `by`"),
            TokenType::Take => f.write_str("keyword `take`"),
            TokenType::End => f.write_str("keyword `end`"),
            TokenType::Check => f.write_str("keyword `check`"),
            TokenType::Whether => f.write_str("keyword `whether`"),
            TokenType::Then => f.write_str("keyword `then`"),
            TokenType::Do => f.write_str("keyword `do`"),
            TokenType::Otherwise => f.write_str("keyword `otherwise`"),
            TokenType::Break => f.write_str("keyword `break`"),
            TokenType::Out => f.write_str("keyword `out`"),
            TokenType::This => f.write_str("keyword `this`"),
            TokenType::Create => f.write_str("keyword `create`"),
            TokenType::Function => f.write_str("keyword `function`"),
            TokenType::Parameter => f.write_str("keyword `parameter`"),
            TokenType::Parameters => f.write_str("keyword `parameters`"),
            TokenType::That => f.write_str("keyword `that`"),
            TokenType::Returns => f.write_str("keyword `returns`"),
            TokenType::Go => f.write_str("keyword `go`"),
            TokenType::Sleep => f.write_str("keyword `sleep`"),
            TokenType::Absent => f.write_str("keyword `absent`"),
            TokenType::Null => f.write_str("keyword `null`"),
            TokenType::NoValue => f.write_str("keyword `no value` )"),
            TokenType::Undefined => f.write_str("keyword `undefined`"),
            TokenType::True => f.write_str("keyword `true`"),
            TokenType::False => f.write_str("keyword `false`"),
            TokenType::String(value) => f.write_str(&format!("`\"{}\"`", value)),
            TokenType::Int(value) => f.write_str(&format!("`{}`", value)),
            TokenType::Float(value) => f.write_str(&format!("`{}`", value)),
            TokenType::Dot => f.write_str("`.`"),
            TokenType::ParenOpen => f.write_str("`(`"),
            TokenType::ParenClose => f.write_str("`)`"),
            TokenType::Comma => f.write_str("`,`"),
        }
    }
}
