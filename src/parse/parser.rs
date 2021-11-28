use super::{ParseError, ParseResult, Parser};
use crate::error::Span;
use crate::lexer::tokens::{Token, TokenType};
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

    pub fn add(&mut self) -> ParseResult<ArithmeticOp> {
        self.parse_rule(|parser| {
            let op_span = parser.expect_kind(TokenType::Add)?;
            let expr = parser.expr()?;
            parser.expect_kind(TokenType::To)?;
            let (var, var_span) = parser.ident()?;

            Ok(ArithmeticOp {
                span: op_span.extend(var_span),
                expr,
                var,
                kind: ArithmeticOpKind::Add,
            })
        })
    }

    pub fn subtract(&mut self) -> ParseResult<ArithmeticOp> {
        self.parse_rule(|parser| {
            let op_span = parser.expect_kind(TokenType::Sub)?;
            let expr = parser.expr()?;
            parser.expect_kind(TokenType::From)?;
            let (var, var_span) = parser.ident()?;

            Ok(ArithmeticOp {
                span: op_span.extend(var_span),
                expr,
                var,
                kind: ArithmeticOpKind::Sub,
            })
        })
    }

    pub fn multiply(&mut self) -> ParseResult<ArithmeticOp> {
        self.parse_rule(|parser| {
            let op_span = parser.expect_kind(TokenType::Mul)?;
            let (var, var_span) = parser.ident()?;
            parser.expect_kind(TokenType::With)?;
            let expr = parser.expr()?;

            Ok(ArithmeticOp {
                span: op_span.extend(var_span),
                expr,
                var,
                kind: ArithmeticOpKind::Mul,
            })
        })
    }

    pub fn divide(&mut self) -> ParseResult<ArithmeticOp> {
        self.parse_rule(|parser| {
            let op_span = parser.expect_kind(TokenType::Div)?;
            let (var, var_span) = parser.ident()?;
            parser.expect_kind(TokenType::By)?;
            let expr = parser.expr()?;

            Ok(ArithmeticOp {
                span: op_span.extend(var_span),
                expr,
                var,
                kind: ArithmeticOpKind::Div,
            })
        })
    }

    pub fn modulo(&mut self) -> ParseResult<ArithmeticOp> {
        self.parse_rule(|parser| {
            let op_span = parser.expect_kind(TokenType::Take)?;
            let (var, var_span) = parser.ident()?;
            parser.expect_kind(TokenType::Mod)?;
            let expr = parser.expr()?;

            Ok(ArithmeticOp {
                span: op_span.extend(var_span),
                expr,
                var,
                kind: ArithmeticOpKind::Mod,
            })
        })
    }

    pub fn if_stmt(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn if_part(&mut self) -> ParseResult<Todo> {
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

    pub fn no_params(&mut self) -> ParseResult<Todo> {
        self.parse_rule(|_parser| todo!())
    }

    pub fn single_param(&mut self) -> ParseResult<Todo> {
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

    pub fn terminate(&mut self) -> ParseResult<Terminate> {
        self.parse_rule(|parser| {
            let go_span = parser.expect_kind(TokenType::Go)?;
            parser.expect_kind(TokenType::To)?;
            let sleep_span = parser.expect_kind(TokenType::Sleep)?;

            Ok(Terminate {
                span: go_span.extend(sleep_span),
            })
        })
    }

    pub fn call(&mut self) -> ParseResult<Call> {
        self.parse_rule(|parser| {
            let call_span = parser.expect_kind(TokenType::Call)?;
            let (fn_name, _) = parser.ident()?;

            parser.expect_kind(TokenType::With)?;

            let args = parser.call_args()?;

            Ok(Call {
                span: call_span.extend(args.span),
                fn_name,
                args,
            })
        })
    }

    pub fn call_args(&mut self) -> ParseResult<CallArgs> {
        self.parse_rule(|parser| {
            if let Some(token) = parser.try_consume_kind(TokenType::No)? {
                let arguments_span = parser.expect_kind(TokenType::Arguments)?;
                Ok(CallArgs {
                    span: token.span.extend(arguments_span),
                    args: vec![],
                })
            } else if let Some(the_token) = parser.try_consume_kind(TokenType::The)? {
                if parser.try_consume_kind(TokenType::Argument)?.is_some() {
                    let arg = parser.call_arg()?;

                    Ok(CallArgs {
                        span: the_token.span.extend(arg.span),
                        args: vec![arg],
                    })
                } else if let Some(arg_token) = parser.try_consume_kind(TokenType::Arguments)? {
                    parser.multi_args(the_token, arg_token)
                } else {
                    let next = parser.peek()?;
                    Err(ParseError {
                        span: next.span,
                        message: format!(
                            "Expected `argument(s)` after `the` in function call, got {}",
                            next.kind
                        ),
                    })
                }
            } else {
                let next = parser.peek()?;
                Err(ParseError {
                    span: next.span,
                    message: format!(
                        "Expected `no` or `the` after `with` in function call, got {}",
                        next.kind
                    ),
                })
            }
        })
    }

    pub fn multi_args(&mut self, the_token: Token, _arg_token: Token) -> ParseResult<CallArgs> {
        self.parse_rule(|parser| {
            let arg = parser.call_arg()?;

            let mut call_args = vec![arg];

            while parser.try_consume_kind(TokenType::Comma)?.is_some() {
                let arg = parser.call_arg()?;
                call_args.push(arg);
            }

            parser.expect_kind(TokenType::And)?;

            let last_arg = parser.call_arg()?;
            let last_arg_span = last_arg.span;
            call_args.push(last_arg);

            Ok(CallArgs {
                span: the_token.span.extend(last_arg_span),
                args: call_args,
            })
        })
    }

    pub fn call_arg(&mut self) -> ParseResult<CallArg> {
        self.parse_rule(|parser| {
            let first_arg_val = parser.expr()?;
            parser.expect_kind(TokenType::As)?;
            let (first_name, first_name_span) = parser.ident()?;

            Ok(CallArg {
                span: first_arg_val.span().extend(first_name_span),
                expr: first_arg_val,
                name: first_name,
            })
        })
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
