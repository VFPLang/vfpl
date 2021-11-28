use super::{ParseError, ParseResult, Parser};
use crate::error::Span;
use crate::lexer::tokens::{Token, TokenType};
use crate::parse::ast::*;

type Todo = ();

impl Parser {
    const MAX_DEPTH: usize = 500;

    fn enter_parse_rule(&mut self) -> ParseResult<()> {
        self.depth += 1;
        if self.depth > Self::MAX_DEPTH {
            Err(ParseError {
                span: Span::dummy(),
                message: "Nesting too deep".to_string(),
            })
        } else {
            Ok(())
        }
    }

    fn leave_parse_rule(&mut self) {
        self.depth -= 1;
    }

    fn next(&mut self) -> ParseResult<Token> {
        self.tokens.next().ok_or_else(|| ParseError {
            span: Span::dummy(),
            message: "reached end of file".to_string(),
        })
    }

    fn peek_kind(&mut self) -> ParseResult<&TokenType> {
        self.tokens
            .peek()
            .map(|token| &token.kind)
            .ok_or_else(|| ParseError {
                span: Span::dummy(),
                message: "reached end of file".to_string(),
            })
    }

    fn try_consume_kind(&mut self, expected_kind: TokenType) -> ParseResult<Option<Token>> {
        if self.peek_kind()? == &expected_kind {
            Ok(Some(self.next()?))
        } else {
            Ok(None)
        }
    }

    fn expect_kind(&mut self, expected_kind: TokenType) -> ParseResult<()> {
        let next = self.next()?;
        if next.kind == expected_kind {
            Ok(())
        } else {
            Err(ParseError {
                span: next.span,
                message: format!("expected {}, found {}", expected_kind, next.kind),
            })
        }
    }

    fn expect_kinds<const N: usize>(&mut self, expected_kinds: [TokenType; N]) -> ParseResult<()> {
        for kind in expected_kinds {
            self.expect_kind(kind)?;
        }
        Ok(())
    }

    ////// rules

    pub fn program(&mut self) -> ParseResult<Program> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn body(&mut self) -> ParseResult<Body> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn typed_ident(&mut self) -> ParseResult<TypedIdent> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn var_init(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn var_set(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn add(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn subtract(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn multiply(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn divide(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn modulo(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn if_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn if_part(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn else_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn while_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn break_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn fn_decl(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn params(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn no_params(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn single_param(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn multi_param(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn fn_return(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn return_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn terminate(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn call_expr(&mut self) -> ParseResult<Expr> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn call_args(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn no_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn single_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn multi_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn ty(&mut self) -> ParseResult<Ty> {
        self.enter_parse_rule()?;
        let token = self.next()?;

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

        self.leave_parse_rule();

        Ok(Ty {
            span: token.span,
            kind: ty_kind,
        })
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn comparison(&mut self) -> ParseResult<Expr> {
        self.enter_parse_rule()?;

        let lhs = self.call_expr()?;

        let rhs_kind = match *self.peek_kind()? {
            TokenType::Does => {
                self.expect_kinds([
                    TokenType::Does,
                    TokenType::Not,
                    TokenType::Have,
                    TokenType::The,
                    TokenType::Value,
                ])?;
                Some((self.comparison()?, ComparisonKind::NotEq))
            }
            TokenType::Has => {
                self.expect_kinds([TokenType::Has, TokenType::The, TokenType::Value])?;
                Some((self.comparison()?, ComparisonKind::Eq))
            }
            TokenType::Is => {
                self.expect_kind(TokenType::Is)?;

                let comp_kind = if let Some(token) = self.try_consume_kind(TokenType::Greater)? {
                    if let Some(token) = self.try_consume_kind(TokenType::Or)? {
                        ComparisonKind::GreaterEq
                    } else {
                        ComparisonKind::Greater
                    }
                } else if let Some(token) = self.try_consume_kind(TokenType::Less)? {
                    if let Some(token) = self.try_consume_kind(TokenType::Or)? {
                        ComparisonKind::LessEq
                    } else {
                        ComparisonKind::Less
                    }
                } else {
                    return Err(ParseError {
                        span: todo!(),
                        message: todo!(),
                    });
                };

                Some((self.comparison()?, comp_kind))
            }
            _ => None,
        };

        self.leave_parse_rule();

        if let Some(rhs) = rhs_kind {
            todo!()
        } else {
            todo!()
        }

        todo!()
    }

    pub fn call(&mut self) -> ParseResult<Expr> {
        self.enter_parse_rule()?;

        let expr = match *self.peek_kind()? {
            TokenType::Call => self.call_expr(),
            _ => self.primary_expr(),
        };

        self.leave_parse_rule();

        expr
    }

    pub fn primary_expr(&mut self) -> ParseResult<Expr> {
        self.enter_parse_rule()?;

        let expr = match self.peek_kind()? {
            TokenType::ParenOpen => {
                let expr = self.expr()?;
                self.expect_kind(TokenType::ParenClose)?;
                expr
            }
            _ => Expr::Literal(self.literal()?),
        };

        self.leave_parse_rule();

        Ok(expr)
    }

    pub fn literal(&mut self) -> ParseResult<Literal> {
        self.enter_parse_rule()?;
        let token = self.next()?;

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

        self.leave_parse_rule();

        Ok(Literal {
            span: token.span,
            kind: literal_kind,
        })
    }
}
