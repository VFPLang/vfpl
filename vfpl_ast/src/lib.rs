use std::fmt::{Display, Formatter, Write};
use vfpl_error::Span;

type Ident = String;

pub type Program = Body;

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedIdent {
    pub span: Span,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    FnDecl(FnDecl),
    Struct(Struct),
    Break(Break),
    Expr(Expr),
    If(If),
    Return(Return),
    Terminate(Terminate),
    VarInit(VarInit),
    VarSet(VarSet),
    While(While),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::FnDecl(inner) => inner.span,
            Stmt::Struct(inner) => inner.span,
            Stmt::Break(inner) => inner.span,
            Stmt::Expr(expr) => expr.span(),
            Stmt::If(inner) => inner.span,
            Stmt::Return(inner) => inner.span,
            Stmt::Terminate(inner) => inner.span,
            Stmt::VarInit(inner) => inner.span,
            Stmt::VarSet(inner) => inner.span,
            Stmt::While(inner) => inner.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarInit {
    pub span: Span,
    pub name: TypedIdent,
    pub init: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarSet {
    pub span: Span,
    pub name: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub span: Span,
    pub if_part: IfPart,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfPart {
    pub span: Span,
    pub cond: Box<Expr>,
    pub body: Body,
    pub else_part: Option<Else>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Else {
    pub span: Span,
    pub kind: ElseKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseKind {
    ElseIf(Box<IfPart>),
    Else(Body),
}

impl ElseKind {
    pub fn span(&self) -> Span {
        match self {
            ElseKind::Else(body) => body.span,
            ElseKind::ElseIf(if_) => if_.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub span: Span,
    pub cond: Expr,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub span: Span,
    pub name: Ident,
    pub params: FnParams,
    pub fn_return: FnReturn,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub span: Span,
    pub name: Ident,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub span: Span,
    pub ty_ident: TypedIdent,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParams {
    pub span: Span,
    pub params: Vec<TypedIdent>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnReturn {
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub span: Span,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Terminate {
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Any,
    Name(Ident),
    Integer,
    Float,
    Boolean,
    String,
    Absent,
    Null,
    NoValue,
    Undefined,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Comparison(Comparison),
    ArithmeticOp(ArithmeticOp),
    Call(Call),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(lit) => lit.span,
            Expr::Comparison(comp) => comp.span,
            Expr::Call(call) => call.span,
            Expr::ArithmeticOp(op) => op.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub span: Span,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Absent,
    Null,
    NoValue,
    Undefined,
    String(String),
    Int(i64),
    Float(f64),
    True,
    False,
    Ident(Ident),
    Struct(StructLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comparison {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub kind: ComparisonKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticOp {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub kind: ArithmeticOpKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithmeticOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonKind {
    NotEq,
    Eq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub span: Span,
    pub fn_name: Ident,
    pub args: CallArgs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallArgs {
    pub span: Span,
    pub args: Vec<ValueIdent>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueIdent {
    pub span: Span,
    pub expr: Box<Expr>,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: Ident,
    pub fields: Vec<ValueIdent>,
}

impl Display for ComparisonKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonKind::NotEq => f.write_str("!="),
            ComparisonKind::Eq => f.write_str("=="),
            ComparisonKind::Greater => f.write_char('>'),
            ComparisonKind::Less => f.write_char('<'),
            ComparisonKind::GreaterEq => f.write_str(">="),
            ComparisonKind::LessEq => f.write_str("<="),
        }
    }
}
