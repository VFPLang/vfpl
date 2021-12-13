use super::{ParseError, ParseResult, Parser};
use vfpl_ast::{
    ArithmeticOp, ArithmeticOpKind, Body, Break, Call, CallArgs, Comparison, ComparisonKind, Else,
    ElseKind, Expr, FnDecl, FnParams, FnReturn, If, IfPart, Literal, LiteralKind, Program, Return,
    Stmt, Struct, StructField, StructLiteral, Terminate, Ty, TyKind, TypedIdent, ValueIdent,
    VarInit, VarSet, While,
};
use vfpl_error::Span;
use vfpl_global::SpurCtx;
use vfpl_lexer::tokens::{CondKeyword as Ck, TokenKind};

impl Parser {
    pub fn program(&mut self) -> ParseResult<Program> {
        self.parse_rule(Parser::body)
    }

    pub fn body(&mut self) -> ParseResult<Body> {
        self.parse_rule(|parser| {
            let mut stmts = Vec::new();

            loop {
                if (matches!(parser.peek_kind(), TokenKind::Please)
                    && matches!(parser.maybe_peek_nth_kind(1), Some(TokenKind::End)))
                    || matches!(parser.peek_kind(), TokenKind::Otherwise)
                    || matches!(parser.peek_kind(), TokenKind::Eof)
                {
                    break;
                }

                let next = parser.stmt()?;
                stmts.push(next);
            }

            let span = stmts
                .first()
                .map(Stmt::span)
                .and_then(|fst_span| stmts.last().map(|last| fst_span.extend(last.span())))
                .unwrap_or_else(Span::dummy);

            Ok(Body { span, stmts })
        })
    }

    pub fn typed_ident(&mut self) -> ParseResult<TypedIdent> {
        self.parse_rule(|parser| {
            let (ident, ident_span) = parser.ident()?;
            parser.expect_kind(TokenKind::As)?;
            let ty = parser.ty()?;

            Ok(TypedIdent {
                span: ident_span.extend(ty.span),
                name: ident,
                ty,
            })
        })
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        self.parse_rule(|parser| {
            parser.expect_kind(TokenKind::Please)?;

            let stmt = match parser.peek_kind() {
                TokenKind::Initialize => Stmt::VarInit(parser.var_init()?),
                TokenKind::CondKw(Ck::Set) => Stmt::VarSet(parser.var_set()?),
                TokenKind::Check => Stmt::If(parser.if_stmt()?),
                TokenKind::Repeat => Stmt::While(parser.while_stmt()?),
                TokenKind::Create => Stmt::FnDecl(parser.fn_decl()?),
                TokenKind::Define => Stmt::Struct(parser.struct_decl()?),
                TokenKind::Break => Stmt::Break(parser.break_stmt()?),
                TokenKind::Return => Stmt::Return(parser.return_stmt()?),
                TokenKind::CondKw(Ck::Go) => Stmt::Terminate(parser.terminate()?),
                _ => Stmt::Expr(parser.expr()?),
            };

            parser.expect_kind(TokenKind::Dot)?;

            Ok(stmt)
        })
    }

    pub fn var_init(&mut self) -> ParseResult<VarInit> {
        self.parse_rule(|parser| {
            let init_span = parser.expect_kind(TokenKind::Initialize)?;
            parser.expect_kind(TokenKind::Variable)?;
            let name = parser.typed_ident()?;
            parser.expect_kinds([
                TokenKind::CondKw(Ck::With),
                TokenKind::CondKw(Ck::The),
                TokenKind::CondKw(Ck::Value),
                TokenKind::CondKw(Ck::Of),
            ])?;
            let init = parser.expr()?;

            Ok(VarInit {
                span: init_span.extend(init.span()),
                name,
                init,
            })
        })
    }

    pub fn var_set(&mut self) -> ParseResult<VarSet> {
        self.parse_rule(|parser| {
            let set_span = parser.expect_kind(TokenKind::CondKw(Ck::Set))?;
            parser.expect_kinds([TokenKind::CondKw(Ck::The), TokenKind::Variable])?;
            let (name, _) = parser.ident()?;

            parser.expect_kinds([
                TokenKind::CondKw(Ck::To),
                TokenKind::CondKw(Ck::The),
                TokenKind::CondKw(Ck::Value),
                TokenKind::CondKw(Ck::Of),
            ])?;

            let expr = parser.expr()?;

            Ok(VarSet {
                span: set_span.extend(expr.span()),
                name,
                expr,
            })
        })
    }

    pub fn if_stmt(&mut self) -> ParseResult<If> {
        self.parse_rule(|parser| {
            let if_part = parser.if_part()?;

            parser.expect_kinds([TokenKind::Please, TokenKind::End])?;
            let check2_span = parser.expect_kind(TokenKind::Check)?;

            Ok(If {
                span: if_part.span.extend(check2_span),
                if_part,
            })
        })
    }

    pub fn if_part(&mut self) -> ParseResult<IfPart> {
        self.parse_rule(|parser| {
            let check_span = parser.expect_kind(TokenKind::Check)?;
            parser.expect_kind(TokenKind::Whether)?;
            let cond = parser.expr()?;
            parser.expect_kinds([TokenKind::Comma, TokenKind::Then, TokenKind::Do])?;

            let body = parser.body()?;

            let else_part = if let TokenKind::Otherwise = parser.peek_kind() {
                Some(parser.else_stmt()?)
            } else {
                None
            };

            Ok(IfPart {
                span: check_span.extend(else_part.as_ref().map_or(body.span, |e| e.span)),
                cond: Box::new(cond),
                body,
                else_part,
            })
        })
    }

    pub fn else_stmt(&mut self) -> ParseResult<Else> {
        self.parse_rule(|parser| {
            let otherwise = parser.expect_kind(TokenKind::Otherwise)?;
            parser.expect_kind(TokenKind::Comma)?;

            let kind = if let TokenKind::Check = parser.peek_kind() {
                ElseKind::ElseIf(Box::new(parser.if_part()?))
            } else {
                ElseKind::Else(parser.body()?)
            };

            Ok(Else {
                span: otherwise.extend(kind.span()),
                kind,
            })
        })
    }

    pub fn while_stmt(&mut self) -> ParseResult<While> {
        self.parse_rule(|parser| {
            parser.in_while_depth += 1;

            let repeat_span = parser.expect_kind(TokenKind::Repeat)?;
            parser.expect_kind(TokenKind::While)?;

            let cond = parser.expr()?;

            parser.expect_kind(TokenKind::Do)?;
            let body = parser.body()?;
            parser.expect_kinds([TokenKind::Please, TokenKind::End])?;
            let while_span = parser.expect_kind(TokenKind::While)?;

            parser.in_while_depth -= 1;

            Ok(While {
                span: repeat_span.extend(while_span),
                cond,
                body,
            })
        })
    }

    pub fn break_stmt(&mut self) -> ParseResult<Break> {
        self.parse_rule(|parser| {
            let first_span = parser.expect_kind(TokenKind::Break)?;
            parser.expect_kinds([
                TokenKind::CondKw(Ck::Out),
                TokenKind::CondKw(Ck::Of),
                TokenKind::This,
            ])?;
            let last_span = parser.expect_kind(TokenKind::While)?;

            if parser.in_while_depth == 0 {
                return Err(ParseError::full(
                    first_span.extend(last_span),
                    "You can not break outside of a while statement.".to_string(),
                    "This is because I need something to break out of.".to_string(),
                    "remove this break. It is not needed.".to_string(),
                ));
            }

            Ok(Break {
                span: first_span.extend(last_span),
            })
        })
    }

    pub fn fn_decl(&mut self) -> ParseResult<FnDecl> {
        self.parse_rule(|parser| {
            // todo is the while depth getting fucked up here?
            parser.in_fn_depth += 1;

            let create_span = parser.expect_kind(TokenKind::Create)?;
            parser.expect_kind(TokenKind::Function)?;

            let (fn_name, _) = parser.ident()?;

            parser.expect_kind(TokenKind::CondKw(Ck::With))?;

            let params = parser.params()?;
            let fn_return = parser.fn_return()?;

            let body = parser.body()?;

            parser.expect_kinds([TokenKind::Please, TokenKind::End, TokenKind::Function])?;

            let (close_name, end_span) = parser.ident()?;

            if fn_name != close_name {
                return Err(ParseError::full(
                    end_span,
                    format!(
                        "End name '{}' does not match function name '{}'",
                        close_name, fn_name
                    ),
                    "To ensure that you didn't mistype your function name, the name needs to be reapeated twice.".to_string(),
                    format!("look whether you mistyped the name here or on the creation above, and use the correction version in both places.\
                    If you don't know which name is the correct one, I can help. I think you meant to call it `{}`, but I can't be sure.", vfpl_error::random_ident(&parser.rng()))
                ));
            }

            parser.in_fn_depth -= 1;

            Ok(FnDecl {
                span: create_span.extend(end_span),
                name: fn_name,
                params,
                fn_return,
                body,
            })
        })
    }

    pub fn params(&mut self) -> ParseResult<FnParams> {
        self.parse_rule(|parser| {
            let (params, span) = parser.list(
                TokenKind::CondKw(Ck::Parameter),
                TokenKind::CondKw(Ck::Parameters),
                Self::typed_ident,
                |typed_ident| typed_ident.span,
                |next| ParseError::full(
                    next.span,
                    format!("Expected `parameter(s)`, found {}", next.kind),
                    "When creating a function, you need to specify the parameters a function takes. This is done using the `parameter` or `parameters` keyword.".to_string(),
                    "add the `parameter` keyword before this here. If you want to take multiple parameters (which is cool too!), you need to use `parameters` instead.".to_string()
                ),
                |next| ParseError::full(
                    next.span,
                    format!("Expected `the` or `no`, found {}", &next.kind),
                    "If you don't want to take any parameters, then you need to say that you don't, and if you do you need to say that you do.".to_string(),
                    "add `no parameters` here, since I think that you don't want to take any paramters here.".to_string()
                )
            )?;


            Ok(FnParams {
                span,
                params,
            })
        })
    }

    pub fn fn_return(&mut self) -> ParseResult<FnReturn> {
        self.parse_rule(|parser| {
            let that_span = parser.expect_kind(TokenKind::CondKw(Ck::That))?;
            parser.expect_kind(TokenKind::CondKw(Ck::Returns))?;

            let ty = parser.ty()?;

            Ok(FnReturn {
                span: that_span.extend(ty.span),
                ty,
            })
        })
    }

    pub fn struct_decl(&mut self) -> ParseResult<Struct> {
        self.parse_rule(|parser| {
            let define_span = parser.expect_kind(TokenKind::Define)?;
            parser.expect_kind(TokenKind::Structure)?;
            let (name, _) = parser.ident()?;

            parser.expect_kind(TokenKind::CondKw(Ck::With))?;

            let fields = parser.struct_fields()?;

            parser.expect_kinds([TokenKind::Please, TokenKind::End])?;
            let define_end_span = parser.expect_kind(TokenKind::Define)?;

            Ok(Struct {
                span: define_span.extend(define_end_span),
                name,
                fields,
            })
        })
    }

    pub fn struct_fields(&mut self) -> ParseResult<Vec<StructField>> {
        self.parse_rule(|parser| {
            let mut fields = Vec::new();

            if let TokenKind::CondKw(Ck::The) = parser.peek_kind() {
                let first_field = parser.struct_field()?;
                fields.push(first_field);
            }

            if let TokenKind::Comma | TokenKind::And = parser.peek_kind() {
                // parse the rest of the fields

                while parser.try_consume_kind(TokenKind::Comma).is_some() {
                    let field = parser.struct_field()?;
                    fields.push(field);
                }

                parser.expect_kind(TokenKind::And)?;

                let last_field = parser.struct_field()?;
                fields.push(last_field);
            }

            Ok(fields)
        })
    }

    pub fn struct_field(&mut self) -> ParseResult<StructField> {
        self.parse_rule(|parser| {
            let the_span = parser.expect_kind(TokenKind::CondKw(Ck::The))?;
            parser.expect_kind(TokenKind::CondKw(Ck::Field))?;

            let ty_ident = parser.typed_ident()?;

            Ok(StructField {
                span: the_span.extend(ty_ident.span),
                ty_ident,
            })
        })
    }

    pub fn return_stmt(&mut self) -> ParseResult<Return> {
        self.parse_rule(|parser| {
            let ret_span = parser.expect_kind(TokenKind::Return)?;
            let expr = parser.expr()?;

            parser.expect_kinds([TokenKind::CondKw(Ck::From), TokenKind::CondKw(Ck::The)])?;
            let fn_keyword_span = parser.expect_kind(TokenKind::Function)?;

            if parser.in_fn_depth == 0 {
                return Err(ParseError::full(
                    ret_span.extend(fn_keyword_span),
                    "Cannot return outside of function".to_string(),
                    "Returning is a process that can only be done within functions. Returning from a program doesn't make sense, where would you want to return to?".to_string(),
                    "terminate the program using `please go to sleep.` instead. This allows me to get a little break.".to_string()
                ));
            }

            Ok(Return {
                span: ret_span.extend(fn_keyword_span),
                expr,
            })
        })
    }

    pub fn terminate(&mut self) -> ParseResult<Terminate> {
        self.parse_rule(|parser| {
            let go_span = parser.expect_kind(TokenKind::CondKw(Ck::Go))?;
            parser.expect_kind(TokenKind::CondKw(Ck::To))?;
            let sleep_span = parser.expect_kind(TokenKind::CondKw(Ck::Sleep))?;

            Ok(Terminate {
                span: go_span.extend(sleep_span),
            })
        })
    }

    pub fn call(&mut self) -> ParseResult<Call> {
        self.parse_rule(|parser| {
            let call_span = parser.expect_kind(TokenKind::Call)?;
            let (fn_name, _) = parser.ident()?;

            parser.expect_kind(TokenKind::CondKw(Ck::With))?;

            let args = parser.call_args()?;

            Ok(Call {
                span: call_span.extend(args.span),
                fn_name,
                args,
            })
        })
    }

    pub fn call_args(&mut self) -> ParseResult<CallArgs> {
        self.parse_rule(|parser| {

            let (args, span) = parser.list(
                TokenKind::CondKw(Ck::Argument),
                TokenKind::CondKw(Ck::Arguments),
                Self::value_ident,
                |value_ident| value_ident.span,
                |next|ParseError::full(
                    next.span,
                    format!(
                        "Expected `argument(s)` after `the` in function call, got {}",
                        next.kind
                    ),
                    "You want to call a function here. But to call a function, you need to specify the arguments you want to pass. I know a keyword just for that, called `argument`, or if you want multiple, `arguments`. It's pretty cool, check it out!".to_string(),
                    "add the cool keyword in there!".to_string()
                ),
                |next| ParseError::full(
                    next.span,
                    format!(
                        "Expected `no` or `the` after `with` in function call, got {}",
                       next.kind
                    ),
                    "You need to tell me the arguments you want to give to that function.".to_string(),
                    "either use `no arguments` if you don't want to give the poor function any, or `the argument`, `the arguments` if you are nice and want to the function to have some happy little arguments.".to_string()
                )
            )?;

            Ok(CallArgs{ span, args })
        })
    }

    pub fn value_ident(&mut self) -> ParseResult<ValueIdent> {
        self.parse_rule(|parser| {
            let first_arg_val = parser.expr()?;
            parser.expect_kind(TokenKind::As)?;
            let (first_name, first_name_span) = parser.ident()?;

            Ok(ValueIdent {
                span: first_arg_val.span().extend(first_name_span),
                expr: Box::new(first_arg_val),
                name: first_name,
            })
        })
    }

    pub fn ty(&mut self) -> ParseResult<Ty> {
        self.parse_rule(|parser| {
            let token = parser.next();

            let ty_kind = match token.kind {
                TokenKind::Absent => TyKind::Absent,
                TokenKind::Null => TyKind::Null,
                TokenKind::NoValue => TyKind::NoValue,
                TokenKind::Undefined => TyKind::Undefined,
                TokenKind::Ident(value) => {
                    let global_ctx = parser.global_ctx.borrow();
                    let str = global_ctx.resolve_string(&value.spur());
                    match str {
                        "integer" => TyKind::Integer,
                        "float" => TyKind::Float,
                        "boolean" => TyKind::Boolean,
                        "string" => TyKind::String,
                        _ => TyKind::Name(value),
                    }
                },
                _ => {
                    return Err(ParseError::full(
                        token.span,
                        format!("Expected type, found {}", token.kind),
                        "If you come from a dynamic language like python or Javascript, this might be new to you, but in VFPL you have to annotate your functions and variables with types, that tell me what types the values have. Using this, I can give you better errors earlier.".to_string(),
                        "add the type `String` here to try it out!".to_string()
                    ))
                }
            };

            Ok(Ty {
                span: token.span,
                kind: ty_kind,
            })
        })
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(Parser::comparison)
    }

    pub fn comparison(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let lhs = parser.term()?;

            let (rhs, kind) = match parser.peek_kind() {
                TokenKind::CondKw(Ck::Does) => {
                    parser.expect_kinds([
                        TokenKind::CondKw(Ck::Does),
                        TokenKind::Not,
                        TokenKind::CondKw(Ck::Have),
                        TokenKind::CondKw(Ck::The),
                        TokenKind::CondKw(Ck::Value),
                    ])?;
                    (parser.comparison()?, ComparisonKind::NotEq)
                }
                TokenKind::CondKw(Ck::Has) => {
                    parser.expect_kinds([TokenKind::CondKw(Ck::Has), TokenKind::CondKw(Ck::The), TokenKind::CondKw(Ck::Value)])?;
                    (parser.comparison()?, ComparisonKind::Eq)
                }
                TokenKind::CondKw(Ck::Is) => {
                    let is_span = parser.expect_kind(TokenKind::CondKw(Ck::Is))?;

                    let comp_kind = if parser.try_consume_kind(TokenKind::CondKw(Ck::Greater)).is_some() {
                        if parser.try_consume_kind(TokenKind::Or).is_some() {
                            parser.expect_kind(TokenKind::CondKw(Ck::Equal))?;
                            ComparisonKind::GreaterEq
                        } else {
                            ComparisonKind::Greater
                        }
                    } else if parser.try_consume_kind(TokenKind::CondKw(Ck::Less)).is_some() {
                        if parser.try_consume_kind(TokenKind::Or).is_some() {
                            parser.expect_kind(TokenKind::CondKw(Ck::Equal))?;
                            ComparisonKind::LessEq
                        } else {
                            ComparisonKind::Less
                        }
                    } else {
                        return Err(ParseError::full(
                            is_span,
                            "expected `greater` or `less` after `is`".to_string(),
                            "Looks like you are trying to make a comparison. After the is, we use `less` or `greater` to make clear which one is meant.".to_string(),
                            "use `greater than` after the `is`.".to_string()
                        ));
                    };

                    parser.expect_kind(TokenKind::CondKw(Ck::Than))?;

                    (parser.comparison()?, comp_kind)
                }
                _ => return Ok(lhs),
            };

            Ok(Expr::Comparison(Comparison {
                span: lhs.span().extend(rhs.span()),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind,
            }))
        })
    }

    pub fn term(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match parser.peek_kind() {
            TokenKind::CondKw(Ck::Add) => {
                let op_span = parser.expect_kind(TokenKind::CondKw(Ck::Add))?;
                let lhs = parser.factor()?;
                parser.expect_kind(TokenKind::CondKw(Ck::To))?;
                let rhs = parser.term()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Add,
                }))
            }
            TokenKind::CondKw(Ck::Sub) => {
                let op_span = parser.expect_kind(TokenKind::CondKw(Ck::Sub))?;
                let lhs = parser.factor()?;
                parser.expect_kind(TokenKind::CondKw(Ck::From))?;
                let rhs = parser.term()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Sub,
                }))
            }
            _ => parser.factor(),
        })
    }

    pub fn factor(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match parser.peek_kind() {
            TokenKind::CondKw(Ck::Mul) => {
                let op_span = parser.expect_kind(TokenKind::CondKw(Ck::Mul))?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::CondKw(Ck::With))?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Mul,
                }))
            }
            TokenKind::CondKw(Ck::Div) => {
                let op_span = parser.expect_kind(TokenKind::CondKw(Ck::Div))?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::CondKw(Ck::By))?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Div,
                }))
            }
            TokenKind::CondKw(Ck::Take) => {
                let op_span = parser.expect_kind(TokenKind::CondKw(Ck::Take))?;
                let lhs = parser.call_expr()?;
                parser.expect_kind(TokenKind::CondKw(Ck::Mod))?;
                let rhs = parser.factor()?;

                Ok(Expr::ArithmeticOp(ArithmeticOp {
                    span: op_span.extend(rhs.span()),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    kind: ArithmeticOpKind::Mod,
                }))
            }
            _ => parser.call_expr(),
        })
    }

    pub fn call_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| match *parser.peek_kind() {
            TokenKind::Call => Ok(Expr::Call(parser.call()?)),
            _ => parser.primary_expr(),
        })
    }

    pub fn primary_expr(&mut self) -> ParseResult<Expr> {
        self.parse_rule(|parser| {
            let expr = match parser.peek_kind() {
                TokenKind::ParenOpen => {
                    parser.expect_kind(TokenKind::ParenOpen)?;
                    let expr = parser.expr()?;
                    parser.expect_kind(TokenKind::ParenClose)?;
                    expr
                }
                _ => Expr::Literal(parser.literal()?),
            };

            Ok(expr)
        })
    }

    pub fn literal(&mut self) -> ParseResult<Literal> {
        self.parse_rule(|parser| {
            // ident literals are special because they might be several tokens long
            if let TokenKind::Ident(_) | TokenKind::CondKw(_) = parser.peek_kind(){
                return parser.ident_literal();
            }

            // a single token literal
            let token = parser.next();

            let literal_kind = match token.kind {
                TokenKind::Absent => LiteralKind::Absent,
                TokenKind::Null => LiteralKind::Null,
                TokenKind::NoValue => LiteralKind::NoValue,
                TokenKind::Undefined => LiteralKind::Undefined,
                TokenKind::True => LiteralKind::True,
                TokenKind::False => LiteralKind::False,
                TokenKind::String(value) => LiteralKind::String(value),
                TokenKind::Int(value) => LiteralKind::Int(value),
                TokenKind::Float(value) => LiteralKind::Float(value),
                _ => {
                    return Err(ParseError::full(
                        token.span,
                        format!("Expected literal, found {}", token.kind),
                        "A literal is either absent, null, novalue, undefined, True, False, a number, a string or an identifier. Yours is neither of them.".to_string(),
                        format!("use a number literal with the value {}.", vfpl_error::random_number(&parser.rng())),
                    ))
                }
            };

            Ok(Literal {
                span: token.span,
                kind: literal_kind,
            })
        })
    }

    pub fn ident_literal(&mut self) -> ParseResult<Literal> {
        self.parse_rule(|parser| {
            // before your read this, note:
            // the struct literal grammar is horribly ambiguous and a mistake
            // if you read this, there was probably a new edge case
            // good luck :)

            match (
                parser.maybe_peek_nth_kind(1).cloned(),
                parser.maybe_peek_nth_kind(2).cloned(),
                parser.maybe_peek_nth_kind(3).cloned(),
            ) {
                (
                    Some(TokenKind::CondKw(Ck::With)),
                    Some(TokenKind::CondKw(Ck::The)),
                    Some(TokenKind::CondKw(Ck::Field)) | Some(TokenKind::CondKw(Ck::Fields)),
                ) => parser.struct_literal(),
                (
                    Some(TokenKind::CondKw(Ck::With)),
                    Some(TokenKind::CondKw(Ck::No)),
                    Some(TokenKind::CondKw(Ck::Fields)),
                ) => parser.struct_literal(),
                _ => {
                    let (ident, ident_span) = parser.ident()?;

                    Ok(Literal {
                        span: ident_span,
                        kind: LiteralKind::Ident(ident),
                    })
                }
            }
        })
    }

    pub fn struct_literal(&mut self) -> ParseResult<Literal> {
        self.parse_rule(|parser| {
            let (name, name_span) = parser.ident()?;

            parser.expect_kind(TokenKind::CondKw(Ck::With))?;

            let (fields, _) = parser.list(
                TokenKind::CondKw(Ck::Field),
                TokenKind::CondKw(Ck::Fields),
                Self::value_ident,
                |value_ident| value_ident.span,
                |_next| todo!(),
                |_next| todo!(),
            )?;

            Ok(Literal {
                span: name_span,
                kind: LiteralKind::Struct(StructLiteral { name, fields }),
            })
        })
    }

    pub fn ident(&mut self) -> ParseResult<(SpurCtx, Span)> {
        self.parse_rule(|parser| {
            let next = parser.next();

            match next.kind {
                TokenKind::Ident(name) => Ok((name, next.span)),
                TokenKind::CondKw(kw) => {
                    let mut global_ctx = parser.global_ctx.borrow_mut();
                    Ok((SpurCtx::new(kw.intern(&mut global_ctx), parser.global_ctx.clone()), next.span))
                },
                _ => Err(ParseError::full(
                    next.span,
                    format!("Expected identifier, found {}", next.kind),
                    "It's not a valid identifier, identifiers consist of letters, _, $ and maybe some numbers in between. For more info, search for `unicode xid` on the internet.".to_string(),
                    format!("use the identifier `{}`.", vfpl_error::random_ident(&parser.rng())),
                ))
            }
        })
    }
}
