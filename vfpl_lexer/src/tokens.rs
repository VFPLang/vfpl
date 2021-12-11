use vfpl_error::Span;
use vfpl_global::{GlobalCtx, Spur};

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

impl TokenKind {
    pub fn display(&self, global_ctx: &GlobalCtx) -> String {
        match self {
            TokenKind::Absent => "keyword `absent`".to_string(),
            TokenKind::And => "keyword `and`".to_string(),
            TokenKind::As => "keyword `as`".to_string(),
            TokenKind::Break => "keyword `break`".to_string(),
            TokenKind::Call => "keyword `call`".to_string(),
            TokenKind::Check => "keyword `check`".to_string(),
            TokenKind::Comma => "`,`".to_string(),
            TokenKind::CondKw(cond) => format!("keyword `{}`", cond.as_ref()),
            TokenKind::Create => "keyword `create`".to_string(),
            TokenKind::Define => "keyword `define`".to_string(),
            TokenKind::Do => "keyword `do`".to_string(),
            TokenKind::Dot => "`.`".to_string(),
            TokenKind::End => "keyword `end`".to_string(),
            TokenKind::Eof => "end of file".to_string(),
            TokenKind::False => "keyword `false`".to_string(),
            TokenKind::Float(value) => format!("`{}`", value),
            TokenKind::Function => "keyword `function`".to_string(),
            TokenKind::Ident(name) => {
                format!("`{}`", global_ctx.resolve_string(name))
            }
            TokenKind::Initialize => "keyword `initialize`".to_string(),
            TokenKind::Int(value) => format!("`{}`", value),
            TokenKind::NoValue => "keyword `novalue` )".to_string(),
            TokenKind::Not => "keyword `not`".to_string(),
            TokenKind::Null => "keyword `null`".to_string(),
            TokenKind::Or => "keyword `or`".to_string(),
            TokenKind::Otherwise => "keyword `otherwise`".to_string(),
            TokenKind::ParenClose => "`)`".to_string(),
            TokenKind::ParenOpen => "`(`".to_string(),
            TokenKind::Please => "keyword `please`".to_string(),
            TokenKind::Repeat => "keyword `repeat`".to_string(),
            TokenKind::Return => "keyword `return`".to_string(),
            TokenKind::String(value) => format!("`\"{}\"`", value),
            TokenKind::Structure => "keyword `structure`".to_string(),
            TokenKind::Then => "keyword `then`".to_string(),
            TokenKind::This => "keyword `this`".to_string(),
            TokenKind::True => "keyword `true`".to_string(),
            TokenKind::Undefined => "keyword `undefined`".to_string(),
            TokenKind::Variable => "keyword `variable`".to_string(),
            TokenKind::Whether => "keyword `whether`".to_string(),
            TokenKind::While => "keyword `while`".to_string(),
        }
    }
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

impl CondKeyword {
    pub fn intern(&self, global_ctx: &mut GlobalCtx) -> Spur {
        global_ctx.intern_string(self.as_ref())
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
