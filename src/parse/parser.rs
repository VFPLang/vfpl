use crate::error::Span;
use crate::lexer::tokens::TokenKind;
use crate::parse::ast::*;

use super::{ParseError, Parser, ParseResult};

impl Parser {
    pub fn program(&mut self) -> ParseResult<Program> {
        self.parse_rule(Parser::body)
    }

    pub fn body(&mut self) -> ParseResult<Body> {
        self.parse_rule(|parser| {
            let mut stmts = Vec::new();

            loop {
                if (matches!(parser.peek_kind(), TokenKind::Please)
                    && matches!(parser.maybe_peek_nth_kind(1), Some(TokenKind::End)))
                    || matches!(parser.peek_kind(), TokenKind::Otherwise)
                    || matches!(parser.peek_kind(), TokenKind::Eof)
                {
                    break;
                }

                let next = parser.stmt()?;
                stmts.push(next);
            }

            let span = stmts
                .first()
                .map(Stmt::span)
                .and_then(|fst_span| stmts.last().map(|last| fst_span.extend(last.span())))
                .unwrap_or_else(Span::dummy);

            Ok(Body { span, stmts })
        })
    }

    pub fn typed_ident(&mut self) -> ParseResult<TypedIdent> {
        self.parse_rule(|parser| {
            let (ident, ident_span) = parser.ident()?;
            parser.expect_kind(TokenKind::As)?;
            let ty = parser.ty()?;

            Ok(TypedIdent {
                span: ident_span.extend(ty.span),
                name: ident,
                ty,
            })
        })
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_rule(|parser| {
            parser.expect_kind(TokenKind::Please)?;

            let stmt = match parser.peek_kind() {
                TokenKind::Initialize => Stmt::VarInit(parser.var_init()?),
                TokenKind::Set => Stmt::VarSet(parser.var_set()?),
                TokenKind::Check => Stmt::If(parser.if_stmt()?),
                TokenKind::Repeat => Stmt::While(parser.while_stmt()?),
                TokenKind::Create => Stmt::FnDecl(parser.fn_decl()?),
                TokenKind::Break => Stmt::Break(parser.break_stmt()?),
                TokenKind::Return => Stmt::Return(parser.return_stmt()?),
                TokenKind::Go => Stmt::Terminate(parser.terminate()?),
                _ => Stmt::Expr(parser.expr()?),
            };

            parser.expect_kind(TokenKind::Dot)?;

            Ok(stmt)
        })
    }

    pub fn var_init(&mut self) -> ParseResult<VarInit> {
        self.parse_rule(|parser| {
            let init_span = parser.expect_kind(TokenKind::Initialize)?;
            parser.expect_kind(TokenKind::Variable)?;
            let name = parser.typed_ident()?;
            parser.expect_kinds([
                TokenKind::With,
                TokenKind::The,
                TokenKind::Value,
                TokenKind::Of,
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
            let set_span = parser.expect_kind(TokenKind::Set)?;
            parser.expect_kinds([TokenKind::The, TokenKind::Variable])?;
            let (name, _) = parser.ident()?;

            parser.expect_kinds([
                TokenKind::To,
                TokenKind::The,
                TokenKind::Value,
                TokenKind::Of,
            ])?;

            let expr = parser.expr()?;

            Ok(VarSet {
                span: set_span.extend(expr.span()),
                name,
                expr,
            })
        })
    }

    pub fn if_stmt(&mut self) -> ParseResult<If> {
        self.parse_rule(|parser| {
            let if_part = parser.if_part()?;

            parser.expect_kinds([TokenKind::Please, TokenKind::End])?;
            let check2_span = parser.expect_kind(TokenKind::Check)?;

            Ok(If {
                span: if_part.span.extend(check2_span),
                if_part,
            })
        })
    }

    pub fn if_part(&mut self) -> ParseResult<IfPart> {
        self.parse_rule(|parser| {
            let check_span = parser.expect_kind(TokenKind::Check)?;
            parser.expect_kind(TokenKind::Whether)?;
            let cond = parser.expr()?;
            parser.expect_kinds([TokenKind::Comma, TokenKind::Then, TokenKind::Do])?;

            let body = parser.body()?;

            let else_part = if let TokenKind::Otherwise = parser.peek_kind() {
                Some(parser.else_stmt()?)
            } else {
                None
            };

            Ok(IfPart {
                span: check_span.extend(else_part.as_ref().map_or(body.span, |e| e.span)),
                cond: Box::new(cond),
                body,
                else_part,
            })
        })
    }

    pub fn else_stmt(&mut self) -> ParseResult<Else> {
        self.parse_rule(|parser| {
            let otherwise = parser.expect_kind(TokenKind::Otherwise)?;
            parser.expect_kind(TokenKind::Comma)?;

            let kind = if let TokenKind::Check = parser.peek_kind() {
                ElseKind::ElseIf(Box::new(parser.if_part()?))
            } else {
                ElseKind::Else(parser.body()?)
            };

            Ok(Else {
                span: otherwise.extend(kind.span()),
                kind,
            })
        })
    }

    pub fn while_stmt(&mut self) -> ParseResult<While> {
        self.parse_rule(|parser| {
            parser.in_while_depth += 1;

            let repeat_span = parser.expect_kind(TokenKind::Repeat)?;
            parser.expect_kind(TokenKind::While)?;

            let cond = parser.expr()?;

            parser.expect_kind(TokenKind::Do)?;
            let body = parser.body()?;
            parser.expect_kinds([TokenKind::Please, TokenKind::End])?;
            let while_span = parser.expect_kind(TokenKind::While)?;

            parser.in_while_depth -= 1;

            Ok(While {
                span: repeat_span.extend(while_span),
                cond,
                body,
            })
        })
    }

    pub fn break_stmt(&mut self) -> ParseResult<Break> {
        self.parse_rule(|parser| {
            let first_span = parser.expect_kind(TokenKind::Break)?;
            parser.expect_kinds([TokenKind::Out, TokenKind::Of, TokenKind::This])?;
            let last_span = parser.expect_kind(TokenKind::While)?;

            if parser.in_while_depth == 0 {
                return Err(ParseError {
                    span: first_span.extend(last_span),
                    message: "Cannot use break outside of while".to_string(),
                });
            }

            Ok(Break {
                span: first_span.extend(last_span),
            })
        })
    }

    pub fn fn_decl(&mut self) -> ParseResult<FnDecl> {
        self.parse_rule(|parser| {
            // todo is the while depth getting fucked up here?
            parser.in_fn_depth += 1;

            let create_span = parser.expect_kind(TokenKind::Create)?;
            parser.expect_kind(TokenKind::Function)?;

            let (fn_name, _) = parser.ident()?;

            parser.expect_kind(TokenKind::With)?;

            let params = parser.params()?;
            let fn_return = parser.fn_return()?;

            let body = parser.body()?;

            parser.expect_kinds([TokenKind::Please, TokenKind::End, TokenKind::Function])?;

            let (close_name, end_span) = parser.ident()?;

            if fn_name != close_name {
                return Err(ParseError {
                    span: end_span,
                    message: format!(
                        "End name '{}' does not match function name '{}'",
                        close_name, fn_name
                    ),
                });
            }

            parser.in_fn_depth -= 1;

            Ok(FnDecl {
                span: create_span.extend(end_span),
                name: fn_name,
                params,
                fn_return,
                body,
            })
        })
    }

    pub fn params(&mut self) -> ParseResult<FnParams> {
        self.parse_rule(|parser| {
            if let Some(no_token) = parser.try_consume_kind(TokenKind::No) {
                let param_span = parser.expect_kind(TokenKind::Parameters)?;
                Ok(FnParams {
                    span: no_token.span.extend(param_span),
                    params: vec![],
                })
            } else if let Some(the_token) = parser.try_consume_kind(TokenKind::The) {
                if parser.try_consume_kind(TokenKind::Parameter).is_some() {
                    let param = parser.typed_ident()?;

                    Ok(FnParams {
                        span: the_token.span.extend(param.span),
                        params: vec![param],
                    })
                } else if parser.try_consume_kind(TokenKind::Parameters).is_some() {
                    parser.multi_params(the_token.span)
                } else {
                    let next = parser.peek();
                    Err(ParseError {
                        span: next.span,
                        message: format!("Expected `parameter(s)`, found {}", next.kind),
                    })
                }
            } else {
                let next = parser.peek();
                Err(ParseError {
                    span: next.span,
                    message: format!("Expected `the` or `no`, found {}", next.kind),
                })
            }
        })
    }

    pub fn multi_params(&mut self, the_span: Span) -> ParseResult<FnParams> {
        self.parse_rule(|parser| {
            let first = parser.typed_ident()?;

            let mut params = vec![first];

            while parser.try_consume_kind(TokenKind::Comma).is_some() {
                let next = parser.typed_ident()?;
                params.push(next);
            }

            parser.expect_kind(TokenKind::And)?;
            let last = parser.typed_ident()?;
            let last_span = last.span;
            params.push(last);

            Ok(FnParams {
                span: the_span.extend(last_span),
                params,
            })
        })
    }

    pub fn fn_return(&mut self) -> ParseResult<FnReturn> {
        self.parse_rule(|parser| {
            let that_span = parser.expect_kind(TokenKind::That)?;
            parser.expect_kind(TokenKind::Returns)?;

            let ty = parser.ty()?;

            Ok(FnReturn {
                span: that_span.extend(ty.span),
                ty,
            })
        })
    }

    pub fn return_stmt(&mut self) -> ParseResult<Return> {
        self.parse_rule(|parser| {
            let ret_span = parser.expect_kind(TokenKind::Return)?;
            let expr = parser.expr()?;

            parser.expect_kinds([TokenKind::From, TokenKind::The])?;
            let fn_keyword_span = parser.expect_kind(TokenKind::Function)?;

            if parser.in_fn_depth == 0 {
                return Err(ParseError {
                    span: ret_span.extend(fn_keyword_span),
                    message: "Cannot return outside of function".to_string(),
                });
            }

            Ok(Return {
                span: ret_span.extend(fn_keyword_span),
                expr,
            })
        })
    }

    pub fn terminate(&mut self) -> ParseResult<Terminate> {
        self.parse_rule(|parser| {
            let go_span = parser.expect_kind(TokenKind::Go)?;
            parser.expect_kind(TokenKind::To)?;
            let sleep_span = parser.expect_kind(TokenKind::Sleep)?;

            Ok(Terminate {
                span: go_span.extend(sleep_span),
            })
        })
    }

    pub fn call(&mut self) -> ParseResult<Call> {
        self.parse_rule(|parser| {
            let call_span = parser.expect_kind(TokenKind::Call)?;
            let (fn_name, _) = parser.ident()?;

            parser.expect_kind(TokenKind::With)?;

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
            if let Some(token) = parser.try_consume_kind(TokenKind::No) {
                let arguments_span = parser.expect_kind(TokenKind::Arguments)?;
                Ok(CallArgs {
                    span: token.span.extend(arguments_span),
                    args: vec![],
                })
            } else if let Some(the_token) = parser.try_consume_kind(TokenKind::The) {
                if parser.try_consume_kind(TokenKind::Argument).is_some() {
                    let arg = parser.call_arg()?;

                    Ok(CallArgs {
                        span: the_token.span.extend(arg.span),
                        args: vec![arg],
                    })
                } else if parser.try_consume_kind(TokenKind::Arguments).is_some() {
                    parser.multi_args(the_token.span)
                } else {
                    let next = parser.peek();
                    Err(ParseError {
                        span: next.span,
                        message: format!(
                            "Expected `argument(s)` after `the` in function call, got {}",
                            next.kind
                        ),
                    })
                }
            } else {
                let next = parser.peek();
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

    pub fn multi_args(&mut self, the_span: Span) -> ParseResult<CallArgs> {
        self.parse_rule(|parser| {
            let arg = parser.call_arg()?;

            let mut call_args = vec![arg];

            while parser.try_consume_kind(TokenKind::Comma).is_some() {
                let arg = parser.call_arg()?;
                call_args.push(arg);
            }

            parser.expect_kind(TokenKind::And)?;

            let last_arg = parser.call_arg()?;
            let last_arg_span = last_arg.span;
            call_args.push(last_arg);

            Ok(CallArgs {
                span: the_span.extend(last_arg_span),
                args: call_args,
            })
        })
    }

    pub fn call_arg(&mut self) -> ParseResult<CallArg> {
        self.parse_rule(|parser| {
            let first_arg_val = parser.expr()?;
            parser.expect_kind(TokenKind::As)?;
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
            let token = parser.next();

            let ty_kind = match token.kind {
                TokenKind::Absent => TyKind::Absent,
                TokenKind::Null => TyKind::Null,
                TokenKind::NoValue => TyKind::NoValue,
                TokenKind::Undefined => TyKind::Undefined,
                TokenKind::Ident(value) => match &*value {
                    "integer" => TyKind::Integer,
                    "float" => TyKind::Float,
                    "boolean" => TyKind::Boolean,
                    "string" => TyKind::String,
                    _ => TyKind::Name(value),
                },
                _ => {
                    return Err(ParseError {
                        span: token.span,
                        message: format!("Expected type, found {}", &token.kind),
                    });
                }
            };

            Ok(Ty {
                span: token.span,
                kind: ty_kind,
            })
        })
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(Parser::comparison)
    }

    pub fn comparison(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let lhs = parser.term()?;

            let (rhs, kind) = match parser.peek_kind() {
                TokenKind::Does => {
                    parser.expect_kinds([
                        TokenKind::Does,
                        TokenKind::Not,
                        TokenKind::Have,
                        TokenKind::The,
                        TokenKind::Value,
                    ])?;
                    (parser.comparison()?, ComparisonKind::NotEq)
                }
                TokenKind::Has => {
                    parser.expect_kinds([TokenKind::Has, TokenKind::The, TokenKind::Value])?;
                    (parser.comparison()?, ComparisonKind::Eq)
                }
                TokenKind::Is => {
                    let is_span = parser.expect_kind(TokenKind::Is)?;

                    let comp_kind = if parser.try_consume_kind(TokenKind::Greater).is_some() {
                        if parser.try_consume_kind(TokenKind::Or).is_some() {
                            parser.expect_kind(TokenKind::Equal)?;
                            ComparisonKind::GreaterEq
                        } else {
                            ComparisonKind::Greater
                        }
                    } else if parser.try_consume_kind(TokenKind::Less).is_some() {
                        if parser.try_consume_kind(TokenKind::Or).is_some() {
                            parser.expect_kind(TokenKind::Equal)?;
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

                    parser.expect_kind(TokenKind::Than)?;

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

    pub fn term(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match parser.peek_kind() {
            TokenKind::Add => {
                let op_span = parser.expect_kind(TokenKind::Add)?;
                let lhs = parser.factor()?;
                parser.expect_kind(TokenKind::To)?;
                let rhs = parser.term()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Add,
                }))
            }
            TokenKind::Sub => {
                let op_span = parser.expect_kind(TokenKind::Sub)?;
                let lhs = parser.factor()?;
                parser.expect_kind(TokenKind::From)?;
                let rhs = parser.term()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Sub,
                }))
            }
            _ => parser.factor(),
        })
    }

    pub fn factor(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match parser.peek_kind() {
            TokenKind::Mul => {
                let op_span = parser.expect_kind(TokenKind::Mul)?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::With)?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Mul,
                }))
            }
            TokenKind::Div => {
                let op_span = parser.expect_kind(TokenKind::Div)?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::By)?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Div,
                }))
            }
            TokenKind::Take => {
                let op_span = parser.expect_kind(TokenKind::Take)?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::Mod)?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Mod,
                }))
            }
            _ => parser.call_expr(),
        })
    }

    pub fn call_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match *parser.peek_kind() {
            TokenKind::Call => Ok(Expr::Call(parser.call()?)),
            _ => parser.primary_expr(),
        })
    }

    pub fn primary_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let expr = match parser.peek_kind() {
                TokenKind::ParenOpen => {
                    parser.expect_kind(TokenKind::ParenOpen)?;
                    let expr = parser.expr()?;
                    parser.expect_kind(TokenKind::ParenClose)?;
                    expr
                }
                _ => Expr::Literal(parser.literal()?),
            };

            Ok(expr)
        })
    }

    pub fn literal(&mut self) -> ParseResult<Literal> {
        self.parse_rule(|parser| {
            let token = parser.next();

            let literal_kind = match token.kind {
                TokenKind::Absent => LiteralKind::Absent,
                TokenKind::Null => LiteralKind::Null,
                TokenKind::NoValue => LiteralKind::NoValue,
                TokenKind::Undefined => LiteralKind::Undefined,
                TokenKind::True => LiteralKind::True,
                TokenKind::False => LiteralKind::False,
                TokenKind::String(value) => LiteralKind::String(value),
                TokenKind::Int(value) => LiteralKind::Int(value),
                TokenKind::Float(value) => LiteralKind::Float(value),
                TokenKind::Ident(name) => LiteralKind::Ident(name),
                _ => {
                    return Err(ParseError {
                        span: token.span,
                        message: format!("Expected literal, found {}", &token.kind),
                    });
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
            let next = parser.next();

            if let TokenKind::Ident(name) = next.kind {
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
