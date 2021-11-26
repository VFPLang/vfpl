#![allow(dead_code)]
use crate::error::Span;
use crate::lexer::tokens::Token;
use crate::parse::ast::{Body, Program, Stmt, TypedIdent};
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::vec::IntoIter;

pub mod ast;

type ParseResult<T> = Result<T, ParseError>;

type Todo = ();

#[derive(Debug)]
struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    depth: usize,
}

pub fn parse(tokens: IntoIter<Token>) -> ParseResult<Program> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
        depth: 0,
    };
    parser.program()
}

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

    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens.peek().ok_or_else(|| ParseError {
            span: Span::dummy(),
            message: "reached end of file".to_string(),
        })
    }

    ////// rules

    fn program(&mut self) -> ParseResult<Program> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn body(&mut self) -> ParseResult<Body> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn typed_ident(&mut self) -> ParseResult<TypedIdent> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn stmt(&mut self) -> ParseResult<Stmt> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn var_init(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn var_set(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn add(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn subtract(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn multiply(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn divide(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn modulo(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn if_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn if_part(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn else_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn while_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn break_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn fn_decl(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn params(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn no_params(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn single_param(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn multi_param(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn fn_return(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn return_stmt(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn terminate(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn ty(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn nullable(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn literal(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn number(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn int(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn float(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn expr(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn comparison(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn primary_expr(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn call(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn call_args(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn no_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn single_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
    fn multi_arg(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }
}

#[derive(Debug)]
pub struct ParseError {
    span: Span,
    message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ParseError {}
