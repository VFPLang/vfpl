#![allow(dead_code)]

use crate::error::Span;
use crate::parse::ast::{Body, Program, TyKind};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

mod exec;

type Ident = Rc<str>;

type IResult = Result<(), InterpreterError>;

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

#[derive(Debug, Clone)]
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
    Fn {
        params: Vec<(Ident, TyKind)>,
        ret_ty: TyKind,
        body: Rc<Body>,
    },
}

#[derive(Debug, Default)]
struct Env {
    outer: Option<Box<Env>>,
    vars: HashMap<Ident, Value>,
}

impl Env {
    fn lookup(&mut self, ident: Ident) -> Option<&mut Value> {
        self.vars.get_mut(&ident).or_else(|| match &mut self.outer {
            Some(outer) => outer.lookup(ident),
            None => None,
        })
    }

    fn insert(&mut self, ident: Ident, value: Value) {
        self.vars.insert(ident, value);
    }
}

#[derive(Debug, Default)]
struct Vm {
    current_env: Env,
}

pub fn run(program: Program) -> Result<(), InterpreterError> {
    let mut vm = Vm::default();

    vm.start(program)
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
