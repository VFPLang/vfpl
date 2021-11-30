#![allow(dead_code)]

use crate::error::Span;
use crate::parse::ast::Program;
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

#[derive(Debug)]
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
}

#[derive(Debug, Default)]
struct Env {
    outer: Option<Box<Env>>,
    vars: HashMap<Ident, Rc<Value>>,
}

impl Env {
    fn lookup(&self, ident: Ident) -> Option<Rc<Value>> {
        self.vars
            .get(&ident)
            .cloned()
            .or_else(|| match &self.outer {
                Some(outer) => outer.lookup(ident),
                None => None,
            })
    }

    fn insert(&mut self, ident: Ident, value: Rc<Value>) {
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
