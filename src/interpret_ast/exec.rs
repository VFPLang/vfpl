use crate::error::Span;
use crate::interpret_ast::{
    Env, EvalCallArg, FnImpl, IResult, Ident, InterpreterError, Interrupt, RuntimeFn, Value,
    ValueResult, Vm,
};
use crate::parse::ast::{
    ArithmeticOp, Call, Comparison, ComparisonKind, Else, ElseKind, Expr, FnDecl, IfPart, Literal,
    LiteralKind, Program, Return, Stmt, TyKind, VarInit, VarSet, While,
};
use std::cell::RefCell;
use std::rc::Rc;

impl Vm {
    pub fn start(&mut self, ast: &Program) -> IResult {
        self.add_global_functions();

        for stmt in &ast.stmts {
            self.dispatch(stmt)?;
        }

        Ok(())
    }

    fn store(&mut self, ident: Ident, value: Value) {
        self.env().insert(ident, value);
    }

    fn env(&mut self) -> std::cell::RefMut<'_, Env> {
        RefCell::borrow_mut(&self.current_env)
    }

    fn enter_env(&mut self) {
        let new_env = Rc::new(RefCell::new(Env::default()));
        let outer_env = std::mem::take(&mut self.current_env);
        new_env.borrow_mut().outer = Some(outer_env);
        self.current_env = new_env;
    }

    fn leave_env(&mut self) {
        let old_env = std::mem::take(&mut self.current_env);
        let outer = match old_env.borrow().outer {
            Some(ref s) => Rc::clone(s),
            None => panic!("Cannot leave outer env"),
        };
        self.current_env = outer;
    }

    ////// dispatch

    fn eval(&mut self, expr: &Expr) -> ValueResult {
        match expr {
            Expr::Literal(lit) => Ok(self.eval_literal(lit)),
            Expr::Call(call) => self.eval_call(call),
            Expr::Comparison(comp) => self.eval_comparison(comp),
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Value {
        match &lit.kind {
            LiteralKind::Absent => Value::Absent,
            LiteralKind::Null => Value::Null,
            LiteralKind::NoValue => Value::NoValue,
            LiteralKind::Undefined => Value::Undefined,
            LiteralKind::String(string) => Value::String(string.clone().into()),
            LiteralKind::Int(int) => Value::Int(*int),
            LiteralKind::Float(float) => Value::Float(*float),
            LiteralKind::True => Value::Bool(true),
            LiteralKind::False => Value::Bool(false),
            LiteralKind::Ident(_) => todo!(),
        }
    }

    fn eval_call(&mut self, call: &Call) -> ValueResult {
        let fn_name = call.fn_name.clone().into();

        // get the function value out
        let function = self.env().modify_var(
            fn_name,
            |function| {
                if let Value::Fn(function) = function {
                    let function = Rc::clone(function);
                    Ok(function)
                } else {
                    return Err(InterpreterError {
                        span: call.span,
                        message: format!("Variable is not a function: {}", call.fn_name),
                    }
                    .into());
                }
            },
            || {
                InterpreterError {
                    span: call.span,
                    message: format!("Variable not found: {}", call.fn_name),
                }
                .into()
            },
        )?;

        // this will be incredibly ugly
        let function = function.borrow_mut();

        let params = &function.params;
        let args = &call.args.args;

        if params.len() != args.len() {
            return Err(InterpreterError {
                span: call.args.span,
                message: format!(
                    "Called function with {} instead of {} arguments",
                    args.len(),
                    params.len()
                ),
            }
            .into());
        }

        let arg_values = args
            .iter()
            .map(|arg| {
                Ok(EvalCallArg {
                    value: self.eval(&arg.expr)?,
                    span: arg.span,
                    name: arg.name.clone().into(),
                })
            })
            .collect::<Result<Vec<_>, Interrupt>>()?;

        // do param type and name checking

        params
            .iter()
            .zip(arg_values.iter())
            .try_for_each(|((param_name, param_ty), arg)| {
                self.type_check(&arg.value, param_ty, arg.span)?;
                if *param_name != arg.name {
                    return Err(Interrupt::Error(InterpreterError {
                        span: arg.span,
                        message: format!(
                            "Mismatched names: expected {}, got {}",
                            param_name, arg.name
                        ),
                    }));
                }
                Ok(())
            })?;

        self.call_stack.push(Rc::clone(&self.current_env));

        {
            // only borrow the env mutably for this short time
            // insert the arguments into the new environment to make them available as variables
            let mut new_env = function.captured_env.borrow_mut();
            for arg_value in arg_values {
                new_env.vars.insert(arg_value.name, arg_value.value);
            }
        }

        self.current_env = Rc::clone(&function.captured_env);

        let result: IResult = match &function.body {
            FnImpl::Native(f) => f(self),
            FnImpl::Custom(body) => self.dispatch_stmts_in_env(&body.stmts),
        };

        // leave call
        self.current_env = self
            .call_stack
            .pop()
            .expect("Call stack empty after fn call");

        match result {
            Err(Interrupt::Return(ret)) => Ok(ret),
            Err(err) => Err(err),
            Ok(_) => Err(InterpreterError {
                span: function.body.span(),
                message: "Function did not return any value".to_string(),
            }
            .into()),
        }
    }

    fn eval_comparison(&mut self, comp: &Comparison) -> ValueResult {
        let lhs = self.eval(&comp.lhs)?;
        let rhs = self.eval(&comp.rhs)?;

        let bool = match (&comp.kind, lhs, rhs) {
            (ComparisonKind::Eq, lhs, rhs) => lhs == rhs,
            (ComparisonKind::NotEq, lhs, rhs) => lhs != rhs,
            (ComparisonKind::Greater, Value::Int(lhs), Value::Int(rhs)) => lhs > rhs,
            (ComparisonKind::Greater, Value::Float(lhs), Value::Float(rhs)) => lhs > rhs,
            (ComparisonKind::GreaterEq, Value::Int(lhs), Value::Int(rhs)) => lhs >= rhs,
            (ComparisonKind::GreaterEq, Value::Float(lhs), Value::Float(rhs)) => lhs >= rhs,
            (ComparisonKind::Less, Value::Int(lhs), Value::Int(rhs)) => lhs < rhs,
            (ComparisonKind::Less, Value::Float(lhs), Value::Float(rhs)) => lhs < rhs,
            (ComparisonKind::LessEq, Value::Int(lhs), Value::Int(rhs)) => lhs <= rhs,
            (ComparisonKind::LessEq, Value::Float(lhs), Value::Float(rhs)) => lhs <= rhs,
            (comp_kind, lhs, rhs) => {
                return Err(InterpreterError {
                    span: comp.span,
                    message: format!(
                        "Cannot compare {} and {} using `{}`",
                        lhs.display_type(),
                        rhs.display_type(),
                        comp_kind
                    ),
                }
                .into())
            }
        };

        Ok(Value::Bool(bool))
    }

    fn dispatch_stmts_in_env(&mut self, stmts: &[Stmt]) -> IResult {
        self.enter_env();
        for stmt in stmts {
            self.dispatch(stmt)?;
        }
        self.leave_env();

        Ok(())
    }

    fn dispatch(&mut self, stmt: &Stmt) -> IResult {
        match stmt {
            Stmt::VarInit(inner) => self.dispatch_var_init(inner),
            Stmt::VarSet(inner) => self.dispatch_var_set(inner),
            Stmt::Add(inner) => self.dispatch_add(inner),
            Stmt::Sub(inner) => self.dispatch_sub(inner),
            Stmt::Mul(inner) => self.dispatch_mul(inner),
            Stmt::Div(inner) => self.dispatch_div(inner),
            Stmt::Mod(inner) => self.dispatch_mod(inner),
            Stmt::If(inner) => self.dispatch_if(&inner.if_part),
            Stmt::While(inner) => self.dispatch_while(inner),
            Stmt::FnDecl(inner) => self.dispatch_fn_decl(inner),
            Stmt::Break(_) => self.dispatch_break(),
            Stmt::Return(inner) => self.dispatch_return(inner),
            Stmt::Terminate(_) => self.dispatch_terminate(),
            Stmt::Expr(inner) => self.dispatch_expr(inner),
        }
    }

    fn dispatch_var_init(&mut self, init: &VarInit) -> IResult {
        let name = init.name.name.clone().into();
        let value = self.eval(&init.init)?;
        let ty = &init.name.ty;

        self.type_check(&value, &ty.kind, ty.span)?;

        self.store(name, value);

        Ok(())
    }

    fn dispatch_var_set(&mut self, set: &VarSet) -> IResult {
        let name: Rc<_> = set.name.clone().into();
        let value = self.eval(&set.expr)?;

        self.env().modify_var(
            Rc::clone(&name),
            |var| {
                *var = value;

                Ok(())
            },
            || {
                InterpreterError {
                    span: set.span,
                    message: format!("Variable not found: {}", set.name),
                }
                .into()
            },
        )
    }

    fn dispatch_add(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr)?;

        self.env().modify_var(
            Rc::clone(&var_name),
            |var_value| {
                let new = match (&var_value, value) {
                    (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 + var2),
                    (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 + var2),
                    (&&mut Value::Float(var1), Value::Int(var2)) => {
                        Value::Float(var1 + var2 as f64)
                    }
                    (&&mut Value::String(ref str), Value::String(new)) => {
                        let mut new_string = String::with_capacity(str.len() + new.len());
                        new_string.push_str(str);
                        new_string.push_str(&new);

                        Value::String(new_string.into())
                    }
                    (var, new) => {
                        return Err(InterpreterError {
                            span: op.span,
                            message: format!(
                                "Invalid arguments to addition. Cannot add {} to {}",
                                var.display_type(),
                                new.display_type()
                            ),
                        }
                        .into())
                    }
                };

                *var_value = new;

                Ok(())
            },
            || {
                InterpreterError {
                    span: op.span,
                    message: format!("Variable not found: {}", &op.var),
                }
                .into()
            },
        )
    }

    fn dispatch_sub(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr)?;

        self.env().modify_var(
            Rc::clone(&var_name),
            |var_value| {
                let new = match (&var_value, value) {
                    (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 - var2),
                    (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 - var2),
                    (&&mut Value::Float(var1), Value::Int(var2)) => {
                        Value::Float(var1 - var2 as f64)
                    }
                    (var, new) => {
                        return Err(InterpreterError {
                            span: op.span,
                            message: format!(
                                "Invalid arguments to subtraction. Cannot add {} to {}",
                                var.display_type(),
                                new.display_type()
                            ),
                        }
                        .into())
                    }
                };

                *var_value = new;

                Ok(())
            },
            || {
                InterpreterError {
                    span: op.span,
                    message: format!("Variable not found: {}", &op.var),
                }
                .into()
            },
        )
    }

    fn dispatch_mul(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr)?;

        self.env().modify_var(
            Rc::clone(&var_name),
            |var_value| {
                let new = match (&var_value, value) {
                    (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 * var2),
                    (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 * var2),
                    (&&mut Value::Float(var1), Value::Int(var2)) => {
                        Value::Float(var1 * var2 as f64)
                    }
                    (var, new) => {
                        return Err(InterpreterError {
                            span: op.span,
                            message: format!(
                                "Invalid arguments to multiplication. Cannot add {} to {}",
                                var.display_type(),
                                new.display_type()
                            ),
                        }
                        .into())
                    }
                };

                *var_value = new;

                Ok(())
            },
            || {
                InterpreterError {
                    span: op.span,
                    message: format!("Variable not found: {}", &op.var),
                }
                .into()
            },
        )
    }

    fn dispatch_div(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr)?;

        self.env().modify_var(
            Rc::clone(&var_name),
            |var_value| {
                let new = match (&var_value, value) {
                    (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 / var2),
                    (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 / var2),
                    (&&mut Value::Float(var1), Value::Int(var2)) => {
                        Value::Float(var1 / var2 as f64)
                    }
                    (var, new) => {
                        return Err(InterpreterError {
                            span: op.span,
                            message: format!(
                                "Invalid arguments to division. Cannot add {} to {}",
                                var.display_type(),
                                new.display_type()
                            ),
                        }
                        .into())
                    }
                };

                *var_value = new;

                Ok(())
            },
            || {
                InterpreterError {
                    span: op.span,
                    message: format!("Variable not found: {}", &op.var),
                }
                .into()
            },
        )
    }

    fn dispatch_mod(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr)?;

        self.env().modify_var(
            Rc::clone(&var_name),
            |var_value| {
                let new = match (&var_value, value) {
                    (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 % var2),
                    (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 % var2),
                    (&&mut Value::Float(var1), Value::Int(var2)) => {
                        Value::Float(var1 % var2 as f64)
                    }
                    (var, new) => {
                        return Err(InterpreterError {
                            span: op.span,
                            message: format!(
                                "Invalid arguments to subtraction. Cannot add {} to {}",
                                var.display_type(),
                                new.display_type()
                            ),
                        }
                        .into())
                    }
                };

                *var_value = new;

                Ok(())
            },
            || {
                InterpreterError {
                    span: op.span,
                    message: format!("Variable not found: {}", &op.var),
                }
                .into()
            },
        )
    }

    fn dispatch_if(&mut self, if_part: &IfPart) -> IResult {
        let cond = self.eval(&if_part.cond)?;

        self.type_check(&cond, &TyKind::Boolean, if_part.cond.span())?;

        if let Value::Bool(true) = cond {
            self.dispatch_stmts_in_env(&if_part.body.stmts)?;
        } else {
            match &if_part.else_part {
                Some(Else {
                    kind: ElseKind::Else(body),
                    ..
                }) => self.dispatch_stmts_in_env(&body.stmts)?,
                Some(Else {
                    kind: ElseKind::ElseIf(else_if),
                    ..
                }) => self.dispatch_if(else_if)?,
                None => {}
            }
        }

        Ok(())
    }

    fn dispatch_while(&mut self, while_stmt: &While) -> IResult {
        while let Value::Bool(true) = {
            let cond = self.eval(&while_stmt.cond)?;
            self.type_check(&cond, &TyKind::Boolean, while_stmt.cond.span())?;
            cond
        } {
            self.dispatch_stmts_in_env(&while_stmt.body.stmts)?;
        }

        Ok(())
    }

    fn dispatch_fn_decl(&mut self, decl: &FnDecl) -> IResult {
        let name: Rc<_> = decl.name.clone().into();
        let params = decl
            .params
            .params
            .iter()
            .map(|typed_ident| (typed_ident.clone().name.into(), typed_ident.ty.kind.clone()))
            .collect::<Vec<_>>();

        let fn_value = Value::Fn(Rc::new(RefCell::new(RuntimeFn {
            params,
            ret_ty: TyKind::Integer,
            body: FnImpl::Custom(decl.body.clone()),
            captured_env: Rc::clone(&self.current_env),
        })));

        self.store(name, fn_value);

        Ok(())
    }

    fn dispatch_break(&self) -> IResult {
        Err(Interrupt::Break)
    }

    fn dispatch_return(&mut self, ret: &Return) -> IResult {
        let value = self.eval(&ret.expr)?;
        Err(Interrupt::Return(value))
    }

    fn dispatch_terminate(&self) -> IResult {
        Err(Interrupt::Terminate)
    }

    fn dispatch_expr(&mut self, expr: &Expr) -> IResult {
        self.eval(expr)?;
        Ok(())
    }

    fn type_check(&self, value: &Value, ty_kind: &TyKind, span: Span) -> IResult {
        match (value, ty_kind) {
            (Value::Absent, TyKind::Absent) => Ok(()),
            (Value::Null, TyKind::Null) => Ok(()),
            (Value::NoValue, TyKind::NoValue) => Ok(()),
            (Value::Undefined, TyKind::Undefined) => Ok(()),
            (Value::Bool(_), TyKind::Boolean) => Ok(()),
            (Value::String(_), TyKind::String) => Ok(()),
            (Value::Int(_), TyKind::Integer) => Ok(()),
            (Value::Float(_), TyKind::Float) => Ok(()),
            _ => Err(InterpreterError {
                span,
                message: format!(
                    "Type mismatch! {} is not assignable to {:?}",
                    value.display_type(),
                    ty_kind
                ),
            }
            .into()),
        }
    }
}
