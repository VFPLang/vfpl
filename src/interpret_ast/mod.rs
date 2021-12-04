#![allow(dead_code)]

use crate::error::{CompilerError, Span};
use crate::parse::ast::{Body, Program, TyKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::rc::Rc;

mod exec;
mod native;
#[cfg(test)]
mod test;

type Ident = Rc<str>;

type IResult = Result<(), Interrupt>;

type ValueResult = Result<Value, Interrupt>;

type RcEnv = Rc<RefCell<Env>>;

#[derive(Debug)]
enum Interrupt {
    Break,
    Terminate,
    Return(Value),
    Error(InterpreterError),
}

#[derive(Debug)]
pub struct InterpreterError {
    span: Span,
    message: String,
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for InterpreterError {}

#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::enum_variant_names)]
enum Value {
    Absent,
    Null,
    NoValue,
    Undefined,
    Bool(bool),
    String(Rc<str>),
    Int(i64),
    Float(f64),
    Fn(Rc<RefCell<RuntimeFn>>),
}

#[derive(Debug, Clone)]
struct RuntimeFn {
    params: Vec<(Ident, TyKind)>,
    ret_ty: TyKind,
    body: FnImpl,
    captured_env: RcEnv,
}

#[derive(Clone)]
enum FnImpl {
    Native(fn(&mut Vm) -> IResult),
    Custom(Body),
}

impl FnImpl {
    fn span(&self) -> Span {
        match self {
            FnImpl::Native(_) => Span::dummy(),
            FnImpl::Custom(body) => body.span,
        }
    }
}

impl Debug for FnImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FnImpl::Native(_) => f.write_str("[native code]"),
            FnImpl::Custom(body) => body.fmt(f),
        }
    }
}

impl PartialEq for RuntimeFn {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
struct EvalCallArg {
    value: Value,
    span: Span,
    name: Ident,
}

#[derive(Debug, Default)]
struct Env {
    outer: Option<RcEnv>,
    vars: HashMap<Ident, Value>,
}

impl Env {
    fn replace(&mut self, ident: Ident, value: Value) -> Option<()> {
        match self.vars.get_mut(&ident) {
            Some(var) => *var = value,
            None => match &self.outer {
                Some(outer) => outer.borrow_mut().replace(ident, value)?,
                None => return None,
            },
        }
        Some(())
    }

    fn modify_var<F, E, R>(&mut self, ident: Ident, f: F, err: E) -> Result<R, Interrupt>
    where
        F: FnOnce(&mut Value) -> Result<R, Interrupt>,
        E: FnOnce() -> Interrupt,
    {
        match self.vars.get_mut(&ident) {
            Some(var) => f(var),
            None => match &self.outer {
                Some(outer) => outer.borrow_mut().modify_var(ident, f, err),
                None => Err(err()),
            },
        }
    }

    fn insert(&mut self, ident: Ident, value: Value) {
        self.vars.insert(ident, value);
    }
}

struct Vm {
    current_env: RcEnv,
    call_stack: Vec<RcEnv>,
    recur_depth: usize,
    stdout: Box<dyn Write>,
}

impl Debug for Vm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("current_env", &self.current_env)
            .field("call_stack", &self.call_stack)
            .field("recur_depth", &self.recur_depth)
            .finish()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self {
            current_env: Rc::default(),
            call_stack: Vec::default(),
            recur_depth: 0,
            stdout: Box::new(std::io::stdout()),
        }
    }
}

///
/// Runs the parsed program
pub fn run(program: Program) -> Result<(), InterpreterError> {
    let mut vm = Vm::default();

    match vm.start(&program) {
        Ok(()) => Err(InterpreterError {
            span: Span::dummy(),
            message: "Program did not terminate properly.".to_string(),
        }),
        Err(Interrupt::Error(err)) => Err(err),
        Err(Interrupt::Break) => unreachable!("break on top level, this should not parse"),
        Err(Interrupt::Return(_)) => unreachable!("return on top level, this should not parse"),
        Err(Interrupt::Terminate) => Ok(()),
    }
}

impl Value {
    fn display_type(&self) -> &'static str {
        match self {
            Value::Absent => "absent",
            Value::Null => "null",
            Value::NoValue => "no value",
            Value::Undefined => "undefined",
            Value::Bool(_) => "Boolean",
            Value::String(_) => "String",
            Value::Int(_) => "Integer",
            Value::Float(_) => "Float",
            Value::Fn { .. } => "Function",
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Absent => f.write_str("absent"),
            Value::Null => f.write_str("null"),
            Value::NoValue => f.write_str("no value"),
            Value::Undefined => f.write_str("undefined"),
            Value::Bool(value) => Display::fmt(value, f),
            Value::String(value) => Display::fmt(value, f),
            Value::Int(value) => Display::fmt(value, f),
            Value::Float(value) => Display::fmt(value, f),
            Value::Fn(value) => match &value.borrow().body {
                FnImpl::Native(_) => f.write_str("[native function]"),
                FnImpl::Custom(_) => f.write_str("[function]"),
            },
        }
    }
}

impl From<InterpreterError> for Interrupt {
    fn from(error: InterpreterError) -> Self {
        Interrupt::Error(error)
    }
}

impl CompilerError for InterpreterError {
    fn span(&self) -> Span {
        self.span
    }

    fn message(&self) -> String {
        self.message.clone()
    }

    fn note(&self) -> Option<String> {
        None
    }
}
