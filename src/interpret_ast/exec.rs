use crate::interpret_ast::{IResult, Ident, InterpreterError, Value, Vm};
use crate::parse::ast::{
    ArithmeticOp, Break, Expr, FnDecl, If, Program, Return, Stmt, Terminate, Ty, VarInit, VarSet,
    While,
};
use std::rc::Rc;

impl Vm {
    pub fn start(&mut self, ast: Program) -> IResult {
        for stmt in &ast.stmts {
            self.dispatch(stmt)?;
        }

        Ok(())
    }

    fn store(&mut self, ident: Ident, value: Rc<Value>) {
        self.current_env.insert(ident, value);
    }

    fn load(&self, ident: Ident) -> Option<Rc<Value>> {
        self.current_env.lookup(ident)
    }

    fn eval(&mut self, _expr: &Expr) -> Value {
        Value::Absent
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
            Stmt::If(inner) => self.dispatch_if(inner),
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
        let value = Rc::new(self.eval(&init.init));
        let ty = &init.name.ty;

        self.type_check(&value, ty)?;

        self.store(name, value);

        Ok(())
    }

    fn dispatch_var_set(&mut self, set: &VarSet) -> IResult {
        let name: Rc<_> = set.name.clone().into();
        let value = Rc::new(self.eval(&set.expr));

        if self.load(Rc::clone(&name)).is_some() {
            self.store(name, value);
            Ok(())
        } else {
            Err(InterpreterError {
                span: set.span,
                msg: "Variable not found".to_string(),
            })
        }
    }

    fn dispatch_add(&mut self, _inner: &ArithmeticOp) -> IResult {
        todo!()
    }
    fn dispatch_sub(&mut self, _inner: &ArithmeticOp) -> IResult {
        todo!()
    }
    fn dispatch_mul(&mut self, _inner: &ArithmeticOp) -> IResult {
        todo!()
    }
    fn dispatch_div(&mut self, _inner: &ArithmeticOp) -> IResult {
        todo!()
    }
    fn dispatch_mod(&mut self, _inner: &ArithmeticOp) -> IResult {
        todo!()
    }
    fn dispatch_if(&mut self, _inner: &If) -> IResult {
        todo!()
    }
    fn dispatch_while(&mut self, _inner: &While) -> IResult {
        todo!()
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

    fn type_check(&self, _value: &Value, _ty: &Ty) -> IResult {
        Ok(())
    }
}
