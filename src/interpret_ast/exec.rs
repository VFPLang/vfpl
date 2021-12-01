use crate::error::Span;
use crate::interpret_ast::{IResult, Ident, InterpreterError, Value, Vm};
use crate::parse::ast::{
    ArithmeticOp, Break, Else, ElseKind, Expr, FnDecl, IfPart, Program, Return, Stmt, Terminate,
    TyKind, VarInit, VarSet, While,
};
use std::rc::Rc;

impl Vm {
    pub fn start(&mut self, ast: Program) -> IResult {
        self.dispatch_stmts(&ast.stmts)
    }

    fn store(&mut self, ident: Ident, value: Value) {
        self.current_env.insert(ident, value);
    }

    fn load(&mut self, ident: Ident) -> Option<&mut Value> {
        self.current_env.lookup(ident)
    }

    fn eval(&mut self, _expr: &Expr) -> Value {
        Value::Absent
    }

    fn dispatch_stmts(&mut self, stmts: &[Stmt]) -> IResult {
        for stmt in stmts {
            self.dispatch(stmt)?;
        }

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
            Stmt::Break(inner) => self.dispatch_break(inner),
            Stmt::Return(inner) => self.dispatch_return(inner),
            Stmt::Terminate(inner) => self.dispatch_terminate(inner),
            Stmt::Expr(inner) => self.dispatch_expr(inner),
        }
    }

    fn dispatch_var_init(&mut self, init: &VarInit) -> IResult {
        let name = init.name.name.clone().into();
        let value = self.eval(&init.init);
        let ty = &init.name.ty;

        self.type_check(&value, &ty.kind, ty.span)?;

        self.store(name, value);

        Ok(())
    }

    fn dispatch_var_set(&mut self, set: &VarSet) -> IResult {
        let name: Rc<_> = set.name.clone().into();
        let value = self.eval(&set.expr);

        if self.load(Rc::clone(&name)).is_some() {
            self.store(name, value);
            Ok(())
        } else {
            Err(InterpreterError {
                span: set.span,
                msg: format!("Variable not found: {}", set.name),
            })
        }
    }

    fn dispatch_add(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr);

        let var_value = self.load(var_name).ok_or_else(|| InterpreterError {
            span: op.span,
            msg: format!("Variable not found: {}", &op.var),
        })?;

        // that double borrow is awful
        let new = match (&var_value, value) {
            (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 + var2),
            (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 + var2),
            (&&mut Value::Float(var1), Value::Int(var2)) => Value::Float(var1 + var2 as f64),
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
                })
            }
        };

        *var_value = new;

        Ok(())
    }

    fn dispatch_sub(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr);

        let var_value = self.load(var_name).ok_or_else(|| InterpreterError {
            span: op.span,
            msg: format!("Variable not found: {}", &op.var),
        })?;

        // that double borrow is awful
        let new = match (&var_value, value) {
            (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 - var2),
            (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 - var2),
            (&&mut Value::Float(var1), Value::Int(var2)) => Value::Float(var1 - var2 as f64),
            (var, new) => {
                return Err(InterpreterError {
                    span: op.span,
                    msg: format!(
                        "Invalid arguments to subtraction. Cannot add {} to {}",
                        var.display_type(),
                        new.display_type()
                    ),
                })
            }
        };

        *var_value = new;

        Ok(())
    }

    fn dispatch_mul(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr);

        let var_value = self.load(var_name).ok_or_else(|| InterpreterError {
            span: op.span,
            msg: format!("Variable not found: {}", &op.var),
        })?;

        // that double borrow is awful
        let new = match (&var_value, value) {
            (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 * var2),
            (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 * var2),
            (&&mut Value::Float(var1), Value::Int(var2)) => Value::Float(var1 * var2 as f64),
            (var, new) => {
                return Err(InterpreterError {
                    span: op.span,
                    msg: format!(
                        "Invalid arguments to multiplication. Cannot add {} to {}",
                        var.display_type(),
                        new.display_type()
                    ),
                })
            }
        };

        *var_value = new;

        Ok(())
    }

    fn dispatch_div(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr);

        let var_value = self.load(var_name).ok_or_else(|| InterpreterError {
            span: op.span,
            msg: format!("Variable not found: {}", &op.var),
        })?;

        // that double borrow is awful
        let new = match (&var_value, value) {
            (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 / var2),
            (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 / var2),
            (&&mut Value::Float(var1), Value::Int(var2)) => Value::Float(var1 / var2 as f64),
            (var, new) => {
                return Err(InterpreterError {
                    span: op.span,
                    msg: format!(
                        "Invalid arguments to division. Cannot add {} to {}",
                        var.display_type(),
                        new.display_type()
                    ),
                })
            }
        };

        *var_value = new;

        Ok(())
    }

    fn dispatch_mod(&mut self, op: &ArithmeticOp) -> IResult {
        let var_name: Rc<_> = op.var.clone().into();
        let value = self.eval(&op.expr);

        let var_value = self.load(var_name).ok_or_else(|| InterpreterError {
            span: op.span,
            msg: format!("Variable not found: {}", &op.var),
        })?;

        // that double borrow is awful
        let new = match (&var_value, value) {
            (&&mut Value::Int(var1), Value::Int(var2)) => Value::Int(var1 % var2),
            (&&mut Value::Float(var1), Value::Float(var2)) => Value::Float(var1 % var2),
            (&&mut Value::Float(var1), Value::Int(var2)) => Value::Float(var1 % var2 as f64),
            (var, new) => {
                return Err(InterpreterError {
                    span: op.span,
                    msg: format!(
                        "Invalid arguments to modulo. Cannot add {} to {}",
                        var.display_type(),
                        new.display_type()
                    ),
                })
            }
        };

        *var_value = new;

        Ok(())
    }

    fn dispatch_if(&mut self, if_part: &IfPart) -> IResult {
        let cond = self.eval(&if_part.cond);

        self.type_check(&cond, &TyKind::Boolean, if_part.cond.span())?;

        if let Value::Bool(true) = cond {
            self.dispatch_stmts(&if_part.body.stmts)
        } else {
            match &if_part.else_part {
                Some(Else {
                    kind: ElseKind::Else(body),
                    ..
                }) => self.dispatch_stmts(&body.stmts),
                Some(Else {
                    kind: ElseKind::ElseIf(else_if),
                    ..
                }) => self.dispatch_if(else_if),
                None => Ok(()),
            }
        }
    }

    fn dispatch_while(&mut self, while_stmt: &While) -> IResult {
        while let Value::Bool(true) = {
            let cond = self.eval(&while_stmt.cond);
            self.type_check(&cond, &TyKind::Boolean, while_stmt.cond.span())?;
            cond
        } {
            self.dispatch_stmts(&while_stmt.body.stmts)?;
        }

        Ok(())
    }

    fn dispatch_fn_decl(&mut self, _inner: &FnDecl) -> IResult {
        todo!()
    }

    fn dispatch_break(&mut self, _inner: &Break) -> IResult {
        todo!()
    }

    fn dispatch_return(&mut self, _inner: &Return) -> IResult {
        todo!()
    }

    fn dispatch_terminate(&mut self, _inner: &Terminate) -> IResult {
        todo!()
    }

    fn dispatch_expr(&mut self, _inner: &Expr) -> IResult {
        todo!()
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
            }),
        }
    }
}
