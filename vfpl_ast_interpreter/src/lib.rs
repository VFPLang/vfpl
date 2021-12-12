use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::rc::Rc;
use vfpl_ast::{Body, Program, TyKind};
use vfpl_error::{CompilerError, Span, VfplError};
use vfpl_global::{GlobalCtx, Spur};

mod exec;
mod native;
#[cfg(test)]
mod test;

/// Runs the parsed program
pub fn run(program: &Program, global_ctx: Rc<RefCell<GlobalCtx>>) -> Result<(), VfplError> {
    let mut vm = Vm::with_stdout(Rc::new(RefCell::new(std::io::stdout())), global_ctx);

    vm.run(program)
}

type Ident = Spur;

type IResult = Result<(), Interrupt>;

type ValueResult = Result<Value, Interrupt>;

type RcEnv = Rc<RefCell<Env>>;

// manual debug impl
pub struct Vm {
    current_env: RcEnv,
    call_stack: Vec<RcEnv>,
    recur_depth: usize,
    stdout: Rc<RefCell<dyn Write>>,
    global_ctx: Rc<RefCell<GlobalCtx>>,
}

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
    Struct(Ident, Rc<RefCell<HashMap<Ident, Value>>>),
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

#[derive(Debug)]
enum Interrupt {
    Break,
    Terminate,
    Return(Value),
    Error(InterpreterError),
}

#[derive(Debug, Default)]
struct Env {
    outer: Option<RcEnv>,
    vars: HashMap<Ident, Value>,
}

impl Env {
    fn get_value(&self, ident: &Ident) -> Option<Value> {
        self.vars.get(ident).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get_value(ident))
        })
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

impl Vm {
    pub fn with_stdout(stdout: Rc<RefCell<dyn Write>>, global_ctx: Rc<RefCell<GlobalCtx>>) -> Self {
        Self {
            current_env: Rc::new(RefCell::new(Default::default())),
            call_stack: vec![],
            recur_depth: 0,
            stdout,
            global_ctx,
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<(), VfplError> {
        match self.start(program) {
            Ok(()) => Err(InterpreterError::full(
                Span::dummy(),
                "Program did not terminate properly.".to_string(),
                "You did not tell me to go to sleep. I am still here.".to_string(),
                "add the `please go to sleep.` statement to the end of the program. I'm really tired.".to_string()
            ).into()),
            Err(Interrupt::Error(err)) => Err(err.into()),
            Err(Interrupt::Break) => unreachable!("break on top level, this should not parse"),
            Err(Interrupt::Return(_)) => unreachable!("return on top level, this should not parse"),
            Err(Interrupt::Terminate) => Ok(()),
        }
    }
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

impl Vm {
    fn display_value_type(&self, value: &Value) -> String {
        match value {
            Value::Absent => "absent".to_string(),
            Value::Null => "null".to_string(),
            Value::NoValue => "novalue".to_string(),
            Value::Undefined => "undefined".to_string(),
            Value::Bool(_) => "Boolean".to_string(),
            Value::String(_) => "String".to_string(),
            Value::Int(_) => "Integer".to_string(),
            Value::Float(_) => "Float".to_string(),
            Value::Fn { .. } => "Function".to_string(),
            Value::Struct(name, _) => self.global_ctx.borrow().resolve_string(name).to_string(),
        }
    }

    fn display_value(&self, value: &Value) -> String {
        match value {
            Value::Absent => "absent".to_string(),
            Value::Null => "null".to_string(),
            Value::NoValue => "novalue".to_string(),
            Value::Undefined => "undefined".to_string(),
            Value::Bool(value) => value.to_string(),
            Value::String(value) => value.to_string(),
            Value::Int(value) => value.to_string(),
            Value::Float(value) => value.to_string(),
            Value::Fn(value) => match &value.borrow().body {
                FnImpl::Native(_) => "[native function]".to_string(),
                FnImpl::Custom(_) => "[function]".to_string(),
            },
            Value::Struct(name, _) => {
                format!("[struct {}]", self.global_ctx.borrow().resolve_string(name))
            }
        }
    }

    fn display_type_kind(&self, kind: &TyKind) -> String {
        match kind {
            TyKind::Any => "<Any>".to_string(),
            TyKind::Name(name) => self.global_ctx.borrow().resolve_string(name).to_string(),
            TyKind::Integer => "Integer".to_string(),
            TyKind::Float => "Float".to_string(),
            TyKind::Boolean => "Boolean".to_string(),
            TyKind::String => "String".to_string(),
            TyKind::Absent => "absent".to_string(),
            TyKind::Null => "null".to_string(),
            TyKind::NoValue => "novalue".to_string(),
            TyKind::Undefined => "Undefined".to_string(),
        }
    }
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

#[derive(Debug)]
struct InterpreterError {
    span: Span,
    message: String,
    note: Option<String>,
    suggestion: Option<String>,
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for InterpreterError {}

impl InterpreterError {
    pub fn full(span: Span, message: String, note: String, suggestion: String) -> Self {
        Self {
            span,
            message,
            note: Some(note),
            suggestion: Some(suggestion),
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
        self.note.clone()
    }

    fn suggestion(&self) -> Option<String> {
        self.suggestion.clone()
    }
}

impl From<InterpreterError> for VfplError {
    fn from(error: InterpreterError) -> Self {
        Self {
            span: error.span(),
            message: error.message(),
            note: error.note(),
            suggestion: error.suggestion(),
        }
    }
}
