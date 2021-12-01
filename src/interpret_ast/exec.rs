use crate::error::Span;
use crate::interpret_ast::{
    Env, IResult, Ident, InterpreterError, Interrupt, RuntimeFn, Value, ValueResult, Vm,
};
use crate::parse::ast::{
    ArithmeticOp, Call, Comparison, ComparisonKind, Else, ElseKind, Expr, FnDecl, IfPart, Literal,
    LiteralKind, Program, Return, Stmt, TyKind, VarInit, VarSet, While,
};
use std::cell::RefCell;
use std::rc::Rc;

impl Vm {
    pub fn start(&mut self, ast: Program) -> IResult {
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

    fn eval_literal(&mut self, lit: &Literal) -> Value {
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
        }
    }

    fn eval_call(&mut self, _call: &Call) -> ValueResult {
        todo!()
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
                    msg: format!(
                        "Cannot compare {} and {} using `{}`",
                        lhs.display_type(),
                        rhs.display_type(),
                        comp_kind
                    ),
                })
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
                    msg: format!("Variable not found: {}", set.name),
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
                            msg: format!(
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
                    msg: format!("Variable not found: {}", &op.var),
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
                            msg: format!(
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
                    msg: format!("Variable not found: {}", &op.var),
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
                            msg: format!(
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
                    msg: format!("Variable not found: {}", &op.var),
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
                            msg: format!(
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
                    msg: format!("Variable not found: {}", &op.var),
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
                            msg: format!(
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
                    msg: format!("Variable not found: {}", &op.var),
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

        let fn_value = Value::Fn(Box::new(RuntimeFn {
            params,
            ret_ty: TyKind::Integer,
            body: decl.body.clone(),
            captured_env: Rc::clone(&self.current_env),
        }));

        self.store(name, fn_value);

        Ok(())
    }

    fn dispatch_break(&mut self) -> IResult {
        Err(Interrupt::Break)
    }

    fn dispatch_return(&mut self, ret: &Return) -> IResult {
        let value = self.eval(&ret.expr)?;
        Err(Interrupt::Return(value))
    }

    fn dispatch_terminate(&mut self) -> IResult {
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
                msg: format!(
                    "Type mismatch! {} is not assignable to {:?}",
                    value.display_type(),
                    ty_kind
                ),
            }
            .into()),
        }
    }
}
