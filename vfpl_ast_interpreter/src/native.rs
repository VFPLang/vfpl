use crate::{Env, FnImpl, IResult, InterpreterError, Interrupt, RuntimeFn, Value, Vm};
use std::cell::RefCell;
use std::rc::Rc;
use vfpl_ast::TyKind;
use vfpl_error::Span;
use vfpl_global::SpurCtx;

impl Vm {
    fn ident(&mut self, str: &str) -> SpurCtx {
        SpurCtx::new(
            self.global_ctx.borrow_mut().intern_string(str),
            self.global_ctx.clone(),
        )
    }

    pub fn add_global_functions(&mut self) {
        let println = self.println();

        let println_ident = self.ident("println");
        let time_ident = self.ident("time");

        let mut env = (*self.current_env).borrow_mut();
        let vars = &mut env.vars;

        vars.insert(println_ident, println);
        vars.insert(time_ident, Self::time());
    }

    //////// Native functions

    ///
    /// print a value to stdout
    /// Takes a single arg of any type, called "value"
    fn println(&mut self) -> Value {
        Value::Fn(Rc::new(RefCell::new(RuntimeFn {
            params: vec![(self.ident("x"), TyKind::Any)],
            ret_ty: TyKind::Absent,
            body: FnImpl::Native(Self::println_impl),
            captured_env: Rc::new(RefCell::new(Env::default())),
        })))
    }

    fn println_impl(&mut self) -> IResult {
        let x_ident = self.ident("x");

        let env = (*self.current_env).borrow_mut();

        let value = env
            .get_value(&x_ident)
            .unwrap_or_else(|| unreachable!("did not find function parameter"));

        let mut stdout_lock = self.stdout.borrow_mut();

        writeln!(stdout_lock, "{}", value).map_err(|err| {
        Interrupt::Error(InterpreterError::full(
            Span::dummy(),
            format!("Failed to write to stdout: {}", err),
            "I have no idea what went wrong here, but something appears to be broken with your setup.".to_string(),
            "fixing it? I honestly don't know what you could do here, I'm really sorry for that.".to_string()
        ))
    })?;

        Err(Interrupt::Return(Value::Absent))
    }

    ///
    /// returns the current time as unix millis
    fn time() -> Value {
        Value::Fn(Rc::new(RefCell::new(RuntimeFn {
            params: vec![],
            ret_ty: TyKind::Integer,
            body: FnImpl::Native(Self::time_impl),
            captured_env: Rc::new(RefCell::new(Env::default())),
        })))
    }

    fn time_impl(_: &mut Vm) -> IResult {
        use std::time;

        let now = time::SystemTime::now();
        let duration = now.duration_since(time::UNIX_EPOCH).map_err(|_| {
            // note: i don't think this can even happen? but would be funny if it did
            Interrupt::Error(InterpreterError::full(
                Span::dummy(),
                "Time is behind unix epoch".to_string(),
                "You played with your computer time too much and should feel bad. I'm normally very polite, but this is too much. I try to be nice and all but you bring this to me.".to_string(),
                "fix your time.".to_string()
            ))
        })?;

        Err(Interrupt::Return(Value::Int(duration.as_millis() as i64)))
    }
}
