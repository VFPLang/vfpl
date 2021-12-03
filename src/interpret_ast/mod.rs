#![allow(dead_code)]

use crate::error::Span;
use crate::parse::ast::{Body, Program, TyKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

mod exec;

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
    msg: String,
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
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
    body: Body,
    captured_env: RcEnv,
}

impl PartialEq for RuntimeFn {
    fn eq(&self, _: &Self) -> bool {
        false
    }
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

#[derive(Debug, Default)]
struct Vm {
    current_env: RcEnv,
    call_stack: Vec<RcEnv>,
    recur_depth: usize,
}

pub fn run(program: Program) -> Result<(), InterpreterError> {
    let mut vm = Vm::default();

    match vm.start(program) {
        Ok(()) => Err(InterpreterError {
            span: Span::dummy(),
            msg: "Program did not terminate properly.".to_string(),
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

impl From<InterpreterError> for Interrupt {
    fn from(error: InterpreterError) -> Self {
        Interrupt::Error(error)
    }
}
