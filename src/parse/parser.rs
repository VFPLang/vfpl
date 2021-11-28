use super::{ParseError, ParseResult, Parser};
use crate::error::Span;
use crate::lexer::tokens::TokenType;
use crate::parse::ast::*;

type Todo = ();

impl Parser {
    pub fn program(&mut self) -> ParseResult<Program> {
        self.parse_rule(|parser| parser.body())
    }

    pub fn body(&mut self) -> ParseResult<Body> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn typed_ident(&mut self) -> ParseResult<TypedIdent> {
        self.parse_rule(|parser| {
            let (ident, ident_span) = parser.ident()?;
            parser.expect_kind(TokenType::As)?;
            let ty = parser.ty()?;

            Ok(TypedIdent {
                span: ident_span.extend(ty.span),
                name: ident,
                ty,
            })
        })
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn var_init(&mut self) -> ParseResult<VarInit> {
        self.parse_rule(|parser| {
            let init_span = parser.expect_kind(TokenType::Initialize)?;
            parser.expect_kind(TokenType::Variable)?;
            let name = parser.typed_ident()?;
            parser.expect_kinds([
                TokenType::With,
                TokenType::The,
                TokenType::Value,
                TokenType::Of,
            ])?;
            let init = parser.expr()?;

            Ok(VarInit {
                span: init_span.extend(init.span()),
                name,
                init,
            })
        })
    }

    pub fn var_set(&mut self) -> ParseResult<VarSet> {
        self.parse_rule(|parser| {
            let set_span = parser.expect_kind(TokenType::Set)?;
            parser.expect_kinds([TokenType::The, TokenType::Variable])?;
            let (name, _) = parser.ident()?;

            parser.expect_kinds([
                TokenType::To,
                TokenType::The,
                TokenType::Value,
                TokenType::Of,
            ])?;

            let expr = parser.expr()?;

            Ok(VarSet {
                span: set_span.extend(expr.span()),
                name,
                expr,
            })
        })
    }

    pub fn add(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn subtract(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn multiply(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn divide(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn modulo(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn if_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn if_parserart(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn else_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn while_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn break_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn fn_decl(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn params(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn no_parserarams(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn single_parseraram(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn multi_param(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn fn_return(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn return_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn terminate(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn call(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn call_args(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn no_arg(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn single_arg(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn multi_arg(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn ty(&mut self) -> ParseResult<Ty> {
        self.parse_rule(|parser| {
            let token = parser.next()?;

            let ty_kind = match token.kind {
                TokenType::Absent => TyKind::Absent,
                TokenType::Null => TyKind::Null,
                TokenType::NoValue => TyKind::NoValue,
                TokenType::Undefined => TyKind::Undefined,
                TokenType::Ident(value) => TyKind::Name(value),
                _ => {
                    return Err(ParseError {
                        span: token.span,
                        message: format!("Expected type, found {}", &token.kind),
                    })
                }
            };

            Ok(Ty {
                span: token.span,
                kind: ty_kind,
            })
        })
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| parser.comparison())
    }

    pub fn comparison(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let lhs = parser.call_expr()?;

            let (rhs, kind) = match parser.peek_kind() {
                Ok(&TokenType::Does) => {
                    parser.expect_kinds([
                        TokenType::Does,
                        TokenType::Not,
                        TokenType::Have,
                        TokenType::The,
                        TokenType::Value,
                    ])?;
                    (parser.comparison()?, ComparisonKind::NotEq)
                }
                Ok(&TokenType::Has) => {
                    parser.expect_kinds([TokenType::Has, TokenType::The, TokenType::Value])?;
                    (parser.comparison()?, ComparisonKind::Eq)
                }
                Ok(&TokenType::Is) => {
                    let is_span = parser.expect_kind(TokenType::Is)?;

                    let comp_kind = if parser.try_consume_kind(TokenType::Greater)?.is_some() {
                        if parser.try_consume_kind(TokenType::Or)?.is_some() {
                            parser.expect_kind(TokenType::Equal)?;
                            ComparisonKind::GreaterEq
                        } else {
                            ComparisonKind::Greater
                        }
                    } else if parser.try_consume_kind(TokenType::Less)?.is_some() {
                        if parser.try_consume_kind(TokenType::Or)?.is_some() {
                            parser.expect_kind(TokenType::Equal)?;
                            ComparisonKind::LessEq
                        } else {
                            ComparisonKind::Less
                        }
                    } else {
                        return Err(ParseError {
                            span: is_span,
                            message: "expected `greater` or `less` after `is`".to_string(),
                        });
                    };

                    parser.expect_kind(TokenType::Than)?;

                    (parser.comparison()?, comp_kind)
                }
                _ => return Ok(lhs),
            };

            Ok(Expr::Comparison(Comparison {
                span: lhs.span().extend(rhs.span()),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind,
            }))
        })
    }

    pub fn call_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match *parser.peek_kind()? {
            TokenType::Call => parser.call_expr(),
            _ => parser.primary_expr(),
        })
    }

    pub fn primary_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let expr = match parser.peek_kind()? {
                TokenType::ParenOpen => {
                    let expr = parser.expr()?;
                    parser.expect_kind(TokenType::ParenClose)?;
                    expr
                }
                _ => Expr::Literal(parser.literal()?),
            };

            Ok(expr)
        })
    }

    pub fn literal(&mut self) -> ParseResult<Literal> {
        self.parse_rule(|parser| {
            let token = parser.next()?;

            let literal_kind = match token.kind {
                TokenType::Absent => LiteralKind::Absent,
                TokenType::Null => LiteralKind::Null,
                TokenType::NoValue => LiteralKind::NoValue,
                TokenType::Undefined => LiteralKind::Undefined,
                TokenType::True => LiteralKind::True,
                TokenType::False => LiteralKind::False,
                TokenType::String(value) => LiteralKind::String(value),
                TokenType::Int(value) => LiteralKind::Int(value),
                TokenType::Float(value) => LiteralKind::Float(value),
                _ => {
                    return Err(ParseError {
                        span: token.span,
                        message: format!("Expected literal, found {}", &token.kind),
                    })
                }
            };

            Ok(Literal {
                span: token.span,
                kind: literal_kind,
            })
        })
    }

    pub fn ident(&mut self) -> ParseResult<(String, Span)> {
        self.parse_rule(|parser| {
            let next = parser.next()?;

            if let TokenType::Ident(name) = next.kind {
                Ok((name, next.span))
            } else {
                Err(ParseError {
                    span: next.span,
                    message: format!("Expected identifier, found {}", next.kind),
                })
            }
        })
    }
}
