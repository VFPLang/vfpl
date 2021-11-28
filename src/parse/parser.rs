use super::{ParseError, ParseResult, Parser};
use crate::error::Span;
use crate::lexer::tokens::Token;
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

    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens.peek().ok_or_else(|| ParseError {
            span: Span::dummy(),
            message: "reached end of file".to_string(),
        })
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

    pub fn ty(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn nullable(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn literal(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn number(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn int(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn float(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn expr(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn comparison(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn primary_expr(&mut self) -> ParseResult<Todo> {
        self.enter_parse_rule()?;
        self.leave_parse_rule();
        todo!()
    }

    pub fn call(&mut self) -> ParseResult<Todo> {
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
}
