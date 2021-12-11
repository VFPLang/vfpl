use vfpl_error::Span;
use vfpl_global::Spur;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

// todo: manual debug impl with a static testing context or something
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    CondKw(CondKeyword),
    Ident(Spur),
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
    /// The contents of the string literal are not interned, because only identifiers are
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
