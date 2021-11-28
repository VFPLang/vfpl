use crate::error::Span;

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
    Go,
    Sleep,
    Absent,
    Null,
    NoValue,
    Undefined,
    True,
    False,

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
