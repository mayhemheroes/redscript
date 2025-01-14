use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::hash::BuildHasher;
use std::ops::RangeInclusive;
use std::rc::Rc;
use std::{fmt, iter, mem, ptr};

use hashbrown::{HashMap, HashSet};
use identity_hash::{BuildIdentityHasher, IdentityHashable};
use indexmap::set::MutableValues;
use redscript_ast as ast;
use redscript_ast::{Span, Spanned};
use smallvec::SmallVec;
use thiserror::Error;

use crate::diagnostic::ErrorWithSpan;
use crate::symbols::{
    FieldId, FreeFunctionIndex, FunctionEntry, FunctionKey, FunctionKind, FunctionType, Symbols,
    TypeSchema,
};
use crate::types::{
    predef, CtxVar, RefType, Type, TypeApp, TypeId, TypeKind, Variance, MAX_FN_ARITY,
    MAX_STATIC_ARRAY_SIZE,
};
use crate::utils::{Lazy, ScopedMap};
use crate::{ir, FreeFunction, IndexMap, IndexSet, LowerReporter, MethodId};

pub type InferredType<'ctx> = Type<'ctx, Poly>;
pub type InferredTypeApp<'ctx> = TypeApp<'ctx, Poly>;
pub type InferredCtxVar<'ctx> = CtxVar<'ctx, Poly>;

pub type InferResult<'ctx, A> = Result<A, TypeError<'ctx>>;
pub type FreeFunctionIndexes = SmallVec<[FreeFunctionIndex; 1]>;

#[derive(Debug)]
pub struct Lower<'scope, 'ctx> {
    locals: Locals<'scope, 'ctx>,
    stmt_prefix: Vec<ir::Stmt<'ctx>>,
    return_type: PolyType<'ctx>,
    captures: IndexSet<Capture>,
    symbols: &'scope Symbols<'ctx>,
    reporter: &'scope mut LowerReporter<'ctx>,
}

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    #[inline]
    fn new(
        locals: Locals<'scope, 'ctx>,
        return_type: PolyType<'ctx>,
        symbols: &'scope Symbols<'ctx>,
        reporter: &'scope mut LowerReporter<'ctx>,
    ) -> Self {
        Self {
            locals,
            return_type,
            stmt_prefix: Vec::new(),
            captures: IndexSet::default(),
            symbols,
            reporter,
        }
    }

    #[inline]
    fn into_output(self) -> LowerOutput<'ctx> {
        LowerOutput::new(self.locals.locals, self.captures)
    }

    pub fn function(
        body: &ast::SourceFunctionBody<'ctx>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        env: Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> (ir::Block<'ctx>, LowerOutput<'ctx>, Vec<Error<'ctx>>) {
        let counter = Cell::new(0);
        let mut reporter = LowerReporter::default();
        let (block, output) = Lower::function_with(
            body,
            &counter,
            params,
            env,
            return_t,
            symbols,
            &mut reporter,
        );
        (block, output, reporter.into_reported())
    }

    fn function_with(
        body: &ast::SourceFunctionBody<'ctx>,
        local_counter: &Cell<u16>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        mut env: Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
        fn inner<'ctx>(
            symbols: &Symbols<'ctx>,
            reporter: &mut LowerReporter<'ctx>,
            locals: Locals<'_, 'ctx>,
            ret_t: PolyType<'ctx>,
            body: &ast::SourceFunctionBody<'ctx>,
            env: &mut Env<'_, 'ctx>,
        ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
            match body {
                ast::FunctionBody::Block(block) => {
                    let mut typer = Lower::new(locals, ret_t.clone(), symbols, reporter);
                    (typer.lower_block(&block.stmts, env), typer.into_output())
                }
                ast::FunctionBody::Inline(expr) => {
                    let result = Lower::expr(expr, locals, env, ret_t, symbols, reporter);
                    let (expr, output) = reporter.unwrap_err(result).unzip();
                    let body = ir::Block::new([ir::Stmt::Return(expr.map(Box::new))]);
                    (body, output.unwrap_or_default())
                }
            }
        }

        let mut locals = Locals::new(local_counter, env.locals.scope_iter().count());
        for (name, typ) in params {
            env.define_local(name, locals.add_param(typ, None).clone());
        }
        inner(symbols, reporter, locals, return_t, body, &mut env)
    }

    fn expr(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        locals: Locals<'_, 'ctx>,
        env: &Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, LowerOutput<'ctx>)> {
        let mut typer = Lower::new(locals, return_t.clone(), symbols, reporter);
        let (mut expr, typ) = typer.lower_expr_with(expr, Some(&return_t), env)?;
        typer.coerce(&mut expr, typ, return_t, env, *span)?;
        Ok((expr, typer.into_output()))
    }

    fn lower_block(
        &mut self,
        stmts: &[Spanned<ast::SourceStmt<'ctx>>],
        env: &mut Env<'_, 'ctx>,
    ) -> ir::Block<'ctx> {
        let mut accumulator = Vec::with_capacity(stmts.len());
        for (stmt, span) in stmts {
            let res = self.lower_stmt(stmt, env, *span);
            if let Some(stmt) = self.reporter.unwrap_err(res) {
                accumulator.append(&mut self.stmt_prefix);
                accumulator.push(stmt);
            }
        }
        ir::Block::new(accumulator)
    }

    #[inline]
    fn lower_scoped_block(
        &mut self,
        block: &[Spanned<ast::SourceStmt<'ctx>>],
        env: &Env<'_, 'ctx>,
    ) -> ir::Block<'ctx> {
        self.lower_block(block, &mut env.introduce_scope())
    }

    fn lower_cond_block(
        &mut self,
        block: &ast::SourceConditionalBlock<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, ir::CondBlock<'ctx>> {
        let cond @ (_, cond_span) = &block.cond;
        let (cond, typ) = self.lower_expr(cond, env)?;
        typ.constrain(&PolyType::nullary(predef::BOOL), self.symbols)
            .with_span(*cond_span)?;

        let ir = self.lower_scoped_block(&block.body.stmts, env);
        Ok(ir::CondBlock::new(cond, ir))
    }

    fn lower_case(
        &mut self,
        case: &ast::SourceCase<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Case<'ctx>, PolyType<'ctx>)> {
        let label @ (_, label_span) = &case.label;
        let (ir::Expr::Const(const_, _), typ) = self.lower_expr(label, env)? else {
            return Err(Error::InvalidCaseLabel(*label_span));
        };
        let ir = self.lower_scoped_block(&case.body, env);
        Ok((ir::Case::new(const_, ir), typ))
    }

    #[inline]
    fn lower_expr(
        &mut self,
        expr: &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        self.lower_expr_with(expr, None, env)
    }

    fn lower_expr_with(
        &mut self,
        (expr, span): &Spanned<ast::SourceExpr<'ctx>>,
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let result = match expr {
            &ast::Expr::Ident(name) => self.resolve_local(name, env, *span)?,
            ast::Expr::Constant(cn) => {
                let const_ = self.lower_constant(cn, hint, *span);
                let const_t = PolyType::nullary(const_.type_id());
                (ir::Expr::Const(const_, *span), const_t)
            }
            ast::Expr::ArrayLit(elems) => {
                let (elems, elem_typ) = elems.iter().try_fold(
                    (vec![], PolyType::fresh()),
                    |(mut acc, elem_typ), elem| {
                        let (elem, typ) = self.lower_expr(elem, env)?;
                        acc.push(elem);
                        Ok((acc, elem_typ.lub(&typ, self.symbols).with_span(*span)?))
                    },
                )?;

                let array_t = Type::app(predef::ARRAY, [elem_typ.clone()]).into_poly();
                let local = self.locals.add_var(array_t.clone(), None);

                self.stmt_prefix.push(ir::Stmt::InitArray {
                    local: local.id,
                    elements: elems.into(),
                    element_type: elem_typ.into(),
                    span: *span,
                });
                (ir::Expr::Local(local.id, *span), array_t)
            }
            ast::Expr::InterpolatedString(elems) => {
                let str = elems
                    .iter()
                    .try_fold(None, |acc, elem| {
                        let elem = match elem {
                            ast::StrPart::Expr(expr @ (_, span)) => {
                                let args = [self.lower_expr(expr, env)?];
                                let (call, _) =
                                    self.free_function_call("ToString", args, env, *span)?;
                                ir::Expr::Call {
                                    call: call.into(),
                                    span: *span,
                                }
                            }
                            ast::StrPart::Str(str) => {
                                ir::Expr::Const(ir::Const::Str(str.clone()), *span)
                            }
                        };
                        let res = match acc {
                            Some(lhs) => {
                                let args =
                                    [lhs, elem].map(|e| (e, PolyType::nullary(predef::STRING)));
                                let (call, _) =
                                    self.free_function_call("OperatorAdd", args, env, *span)?;
                                ir::Expr::Call {
                                    call: call.into(),
                                    span: *span,
                                }
                            }
                            None => elem,
                        };
                        Ok(Some(res))
                    })?
                    .unwrap_or_else(|| ir::Expr::Const(ir::Const::Str("".into()), *span));
                (str, PolyType::nullary(predef::STRING))
            }
            ast::Expr::Assign { lhs, rhs } => {
                let (lhs, lhs_t) = self.lower_expr(lhs, env)?;
                let (mut rhs, rhs_t) = self.lower_expr_with(rhs, Some(&lhs_t), env)?;
                self.coerce(&mut rhs, rhs_t, lhs_t, env, *span)?;

                let ir = ir::Expr::Assign {
                    place: lhs.into(),
                    expr: rhs.into(),
                    span: *span,
                };
                (ir, PolyType::nullary(predef::VOID))
            }
            ast::Expr::BinOp { lhs, op, rhs } => {
                let lhs = self.lower_expr(lhs, env)?;
                let rhs = self.lower_expr(rhs, env)?;
                let (call, typ) =
                    self.free_function_call(binop_name(op), [lhs, rhs], env, *span)?;
                let ir = ir::Expr::Call {
                    call: call.into(),
                    span: *span,
                };
                (ir, typ)
            }
            ast::Expr::UnOp { op, expr } => {
                let arg = self.lower_expr(expr, env)?;
                let (call, typ) = self.free_function_call(unop_name(op), [arg], env, *span)?;
                let ir = ir::Expr::Call {
                    call: call.into(),
                    span: *span,
                };
                (ir, typ)
            }
            ast::Expr::Call {
                expr,
                type_args,
                args,
            } => {
                let (call, typ) = self.lower_call(expr, type_args, args, hint, env, *span)?;
                let ir = ir::Expr::Call {
                    call: call.into(),
                    span: *span,
                };
                (ir, typ)
            }
            ast::Expr::Member { expr, member } => self.lower_member(expr, member, env)?,
            ast::Expr::Index { expr, index } => {
                let expr @ (_, expr_span) = &**expr;
                let elem_t = PolyType::fresh();
                let array_t = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();

                let (mut expr, expr_t) = self.lower_expr(expr, env)?;
                self.coerce(&mut expr, expr_t, array_t.clone(), env, *expr_span)?;

                let index @ (_, index_span) = &**index;
                let (index, index_t) = self.lower_expr(index, env)?;
                index_t
                    .constrain(&PolyType::nullary(predef::INT32), self.symbols)
                    .with_span(*index_span)?;

                (
                    ir::Expr::Index {
                        array_type: array_t.into(),
                        array: expr.into(),
                        index: index.into(),
                        span: *span,
                    },
                    elem_t,
                )
            }
            ast::Expr::DynCast { expr, typ } => {
                let expr @ (_, expr_span) = &**expr;
                let (expr, expr_t) = self.lower_expr(expr, env)?;
                let (typ, type_span) = &**typ;
                let Type::Data(typ) = env.types.resolve(typ, *type_span)? else {
                    return Err(Error::InvalidDynCastType(*type_span));
                };

                let target = InferredTypeApp::from_type(&typ);
                let expected = target.clone().into_type().into_poly();
                let inferred = if let Some((RefType::Weak, inner)) = expr_t.strip_ref(self.symbols)
                {
                    expected
                        .constrain(&inner, self.symbols)
                        .with_span(*expr_span)?;
                    Type::app(predef::WREF, [expected.clone()]).into_poly()
                } else {
                    expected
                        .constrain(&expr_t, self.symbols)
                        .with_span(*expr_span)?;
                    expected
                };

                let ir = ir::Expr::DynCast {
                    expr: expr.into(),
                    expr_type: expr_t.into(),
                    target_type: target.into(),
                    span: *span,
                };
                (ir, inferred)
            }
            ast::Expr::New { typ, args } => {
                let (typ, type_span) = &**typ;
                let Type::Data(typ) = env.types.resolve(typ, *type_span)? else {
                    return Err(Error::InvalidNewType(*type_span));
                };
                let def = &self.symbols[typ.id()];

                let type_args = if typ.args().is_empty() {
                    def.params()
                        .iter()
                        .map(|_| PolyType::fresh())
                        .collect::<Rc<_>>()
                } else if typ.args().len() != def.params().len() {
                    let expected = def.params().len();
                    return Err(Error::InvalidTypeArgCount(expected, *type_span));
                } else {
                    typ.args().iter().map(PolyType::from_type).collect()
                };
                let typ = TypeApp::new(typ.id(), type_args);
                let inferred = typ.clone().into_type().into_poly();

                let ir = match def.schema() {
                    TypeSchema::Aggregate(aggregate) if aggregate.flags().is_struct() => {
                        let field_count = aggregate.fields().len();
                        if args.len() != field_count {
                            return Err(Error::InvalidArgCount(field_count..=field_count, *span));
                        }
                        let type_env = typ.type_env(self.symbols);
                        let args = args
                            .iter()
                            .zip(aggregate.fields().iter())
                            .map(|(arg @ (_, arg_span), field)| {
                                let expected =
                                    PolyType::from_type_with_env(field.field().type_(), &type_env)
                                        .map_err(|var| Error::UnresolvedVar(var, *arg_span))?;
                                let (mut expr, typ) =
                                    self.lower_expr_with(arg, Some(&expected), env)?;
                                self.coerce(&mut expr, typ, expected, env, *arg_span)?;
                                Ok(expr)
                            })
                            .collect::<Result<_, _>>()?;
                        ir::Expr::NewStruct {
                            struct_type: typ.into(),
                            values: args,
                            span: *span,
                        }
                    }
                    TypeSchema::Aggregate(agg) if agg.flags().is_abstract() => {
                        return Err(Error::InstantiatingAbstract(typ.id(), *type_span))
                    }
                    TypeSchema::Aggregate(_) if args.is_empty() => ir::Expr::NewClass {
                        class_type: typ.into(),
                        span: *span,
                    },
                    TypeSchema::Aggregate(_) => {
                        return Err(Error::ClassConstructorHasArguments(*span))
                    }
                    _ => return Err(Error::InvalidNewType(*type_span)),
                };
                (ir, inferred)
            }
            ast::Expr::Conditional { cond, then, else_ } => {
                let cond @ (_, cond_span) = &**cond;
                let (cond, cond_t) = self.lower_expr(cond, env)?;
                cond_t
                    .constrain(&PolyType::nullary(predef::BOOL), self.symbols)
                    .with_span(*cond_span)?;

                let (then, then_t) = self.lower_expr(then, env)?;
                let (else_, else_t) = self.lower_expr(else_, env)?;

                let ir = ir::Expr::Conditional {
                    condition: cond.into(),
                    then: then.into(),
                    else_: else_.into(),
                    span: *span,
                };
                let inferred = then_t.lub(&else_t, self.symbols).with_span(*span)?;
                (ir, inferred)
            }
            ast::Expr::Lambda { params, body } => {
                self.lower_closure(params, body, hint, env, *span)?
            }
            ast::Expr::This => self.resolve_local("this", env, *span)?,
            ast::Expr::Super => self.resolve_local("this", env, *span)?,
            ast::Expr::Null => {
                let typ =
                    PolyType::with_bounds(Type::Nothing, Some(Type::nullary(predef::ISCRIPTABLE)));
                (ir::Expr::Null(*span), typ)
            }

            ast::Expr::Error => (ir::Expr::Null(*span), PolyType::fresh()),
        };
        Ok(result)
    }

    fn lower_stmt(
        &mut self,
        stmt: &ast::SourceStmt<'ctx>,
        env: &mut Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Stmt<'ctx>> {
        let res = match stmt {
            ast::Stmt::Let { name, typ, value } => {
                let &(name, name_span) = name;
                let typ = typ
                    .as_deref()
                    .map(|(typ, span)| self.resolve_type(typ, env, *span))
                    .transpose()?
                    .unwrap_or_else(PolyType::fresh);

                if let Some(value @ (_, value_span)) = value.as_deref() {
                    let (mut value, value_t) = self.lower_expr_with(value, Some(&typ), env)?;
                    self.coerce(&mut value, value_t, typ.clone(), env, *value_span)?;

                    let local = self.locals.add_var(typ, Some(name_span));
                    let local = env.define_local(name, local.clone());
                    let assign = ir::Expr::Assign {
                        place: ir::Expr::Local(local, name_span).into(),
                        expr: value.into(),
                        span,
                    };
                    ir::Stmt::Expr(assign.into())
                } else {
                    let local = self.locals.add_var(typ.clone(), Some(name_span));
                    let local = env.define_local(name, local.clone());
                    ir::Stmt::InitDefault {
                        local,
                        typ: typ.into(),
                    }
                }
            }
            ast::Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                let (expr, expr_t) = self.lower_expr(expr, env)?;
                let branches = cases
                    .iter()
                    .map(|case| {
                        let (_, span) = case.label;
                        let (case, case_t) = self.lower_case(case, env)?;
                        case_t.constrain(&expr_t, self.symbols).with_span(span)?;
                        Ok(case)
                    })
                    .collect::<Result<_, _>>()?;
                let default = default
                    .as_ref()
                    .map(|block| self.lower_scoped_block(block, env));
                ir::Stmt::Switch {
                    scrutinee: expr.into(),
                    scrutinee_type: expr_t.into(),
                    branches,
                    default,
                    span,
                }
            }
            ast::Stmt::If { blocks, else_ } => {
                let branches = blocks
                    .iter()
                    .map(|block| self.lower_cond_block(block, env))
                    .collect::<Result<_, _>>()?;
                let default = else_
                    .as_ref()
                    .map(|block| self.lower_scoped_block(&block.stmts, env));
                ir::Stmt::Branches {
                    branches,
                    default,
                    span,
                }
            }
            ast::Stmt::While(block) => {
                let block = self.lower_cond_block(block, env)?;
                ir::Stmt::While(block, span)
            }
            ast::Stmt::ForIn { name, iter, body } => {
                let block = self.lower_for_in(name, iter, body, env, span)?;
                ir::Stmt::Block(block, span)
            }
            ast::Stmt::Return(Some(val)) => {
                let val @ (_, val_span) = &**val;
                let rt = self.return_type.clone();
                let (mut val, val_t) = self.lower_expr_with(val, Some(&rt), env)?;
                self.coerce(&mut val, val_t, rt, env, *val_span)?;
                ir::Stmt::Return(Some(val.into()))
            }
            ast::Stmt::Return(None) => {
                self.return_type
                    .constrain(&PolyType::nullary(predef::VOID), self.symbols)
                    .with_span(span)?;
                ir::Stmt::Return(None)
            }
            ast::Stmt::Break => ir::Stmt::Break,
            ast::Stmt::Continue => ir::Stmt::Continue,
            ast::Stmt::Expr(expr) => {
                let (expr, _) = self.lower_expr(expr, env)?;
                ir::Stmt::Expr(expr.into())
            }
        };
        Ok(res)
    }

    fn lower_call(
        &mut self,
        expr @ (_, expr_span): &Spanned<ast::SourceExpr<'ctx>>,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        call_span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        'free_func: {
            let &(ast::Expr::Ident(name), _) = expr else {
                break 'free_func;
            };

            if name == "Cast" {
                return self.lower_static_cast(args, type_args, hint, env, *expr_span);
            };

            let mut candidates = env.query_free_functions(name, self.symbols).peekable();
            if candidates.peek().is_none() {
                break 'free_func;
            };

            let res = self
                .resolve_overload(name, args, type_args, candidates, None, env, *expr_span)?
                .into_call();
            return Ok(res);
        };

        let closure = 'instance: {
            let (ast::Expr::Member { expr, member }, _) = expr else {
                break 'instance None;
            };

            'static_method: {
                let (ast::Expr::Ident(ident), _) = **expr else {
                    break 'static_method;
                };
                let Some(&TypeRef::Name(typ)) = env.types.get(ident) else {
                    break 'static_method;
                };
                let TypeSchema::Aggregate(agg) = self.symbols[typ].schema() else {
                    break 'static_method;
                };
                let mut candidates = agg
                    .methods()
                    .by_name(member)
                    .filter(|entry| entry.func().flags().is_static())
                    .map(|entry| entry.map_key(|i| MethodId::new(typ, i)))
                    .peekable();
                if candidates.peek().is_none() {
                    break 'static_method;
                }

                let parent_args = self.symbols[typ]
                    .params()
                    .iter()
                    .map(|_| PolyType::fresh())
                    .collect::<Rc<_>>();
                let typ = TypeApp::new(typ, parent_args);

                let res = self
                    .resolve_overload(
                        member,
                        args,
                        type_args,
                        candidates,
                        Some(typ.clone()),
                        env,
                        *expr_span,
                    )?
                    .into_static_call(typ.id(), typ.args().iter().cloned());
                return Ok(res);
            }

            let (expr_ir, typ) = self.lower_expr(expr, env)?;
            let (ref_type, stripped) = typ.strip_ref(self.symbols).unzip();
            let upper_bound = stripped
                .unwrap_or(typ.clone())
                .force_upper_bound(self.symbols)
                .with_span(*expr_span)?
                .ok_or(Error::CannotLookupMember(*expr_span))?
                .into_owned();

            let (upper_bound, mode) = if matches!(**expr, (ast::Expr::Super, _)) {
                (
                    upper_bound
                        .instantiate_base(self.symbols)
                        .ok_or(Error::NonExistentSuperType(*expr_span))?,
                    ir::CallMode::ForceStatic,
                )
            } else {
                (upper_bound, ir::CallMode::Normal)
            };

            let mut candidates = self
                .symbols
                .query_methods(upper_bound.id(), member)
                .filter(|entry| !entry.func().flags().is_static())
                .peekable();
            if candidates.peek().is_none() {
                break 'instance Some(self.field(expr_ir, member, upper_bound, *expr_span)?);
            }

            let res = self
                .resolve_overload(
                    member,
                    args,
                    type_args,
                    candidates,
                    Some(upper_bound.clone()),
                    env,
                    *expr_span,
                )?
                .into_instance_call(expr_ir, upper_bound, ref_type, mode);
            return Ok(res);
        };

        let (closure, typ) = closure.map_or_else(|| self.lower_expr(expr, env), Ok)?;
        let id = TypeId::fn_with_arity(args.len()).ok_or(Error::UnsupportedArity(call_span))?;
        let mut checked_args = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        for arg in args {
            let (arg, typ) = self.lower_expr(arg, env)?;
            checked_args.push(arg);
            arg_types.push(typ);
        }

        let return_t = PolyType::fresh();
        let func_t_args = arg_types
            .into_iter()
            .chain([return_t.clone()])
            .collect::<Rc<_>>();
        let func_t = TypeApp::new(id, func_t_args);

        typ.constrain(&func_t.clone().into_type().into_poly(), self.symbols)
            .with_span(*expr_span)?;

        let call = ir::Call::Closure {
            closure: closure.into(),
            args: checked_args.into(),
            closure_type: func_t.into(),
        };
        Ok((call, return_t.clone()))
    }

    fn lower_static_cast(
        &mut self,
        args: &[Spanned<ast::SourceExpr<'ctx>>],
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        expr_span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        let target_t = match (type_args, hint) {
            ([(target, target_span)], _) => self.resolve_type(target, env, *target_span)?,
            ([], Some(hint)) => hint.clone(),
            ([], None) => return Err(Error::UnknownStaticCastType(expr_span)),
            _ => return Err(Error::InvalidTypeArgCount(1, expr_span)),
        };
        let candidates = env
            .query_free_functions("Cast", self.symbols)
            .filter(|entry| {
                target_t.is_subtype_compatible(entry.func().type_().return_type(), self.symbols)
            });
        let res = self
            .resolve_overload("Cast", args, &[], candidates, None, env, expr_span)?
            .into_call();
        Ok(res)
    }

    fn lower_closure(
        &mut self,
        params: &[Spanned<ast::SourceParam<'ctx>>],
        body: &ast::SourceFunctionBody<'ctx>,
        hint: Option<&PolyType<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let hint_bound = hint.and_then(|typ| typ.upper_bound(self.symbols));
        let arg_hints = match &hint_bound {
            Some(typ) if typ.id().is_fn() => typ.args(),
            _ => &[],
        };
        let arg_hints = arg_hints
            .iter()
            .cloned()
            .chain(iter::repeat_with(PolyType::fresh));
        let return_t = PolyType::fresh();

        let func_t_args = params
            .iter()
            .zip(arg_hints)
            .map(|((param, _), hint)| {
                param
                    .typ
                    .as_ref()
                    .map_or(Ok(hint), |(typ, span)| self.resolve_type(typ, env, *span))
            })
            .chain([Ok(return_t.clone())])
            .collect::<Result<Rc<_>, _>>()?;

        let types = params
            .iter()
            .zip(&*func_t_args)
            .map(|((param, _), typ)| (param.name, typ.clone()));
        let counter = self.locals.counter;
        let env = env.introduce_scope();
        let (block, output) = Lower::function_with(
            body,
            counter,
            types,
            env,
            return_t,
            self.symbols,
            self.reporter,
        );

        let (locals, captures) = output.into_inner();
        let captured = captures.iter().map(|cap| cap.captured).collect::<Box<_>>();
        self.captures.extend(captures);

        let id = TypeId::fn_with_arity(params.len()).ok_or(Error::UnsupportedArity(span))?;
        let typ = TypeApp::new(id, func_t_args);
        let closure = ir::Closure::new(typ.clone(), locals, captured, block);
        let ir = ir::Expr::NewClosure {
            closure: closure.into(),
            span,
        };
        Ok((ir, typ.into_type().into_poly()))
    }

    fn lower_member(
        &mut self,
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        member: &'ctx str,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        'enum_case: {
            let (ast::Expr::Ident(ident), span) = expr else {
                break 'enum_case;
            };
            let Some(&TypeRef::Name(type_id)) = env.types.get(ident) else {
                break 'enum_case;
            };
            let TypeSchema::Enum(enum_) = self.symbols[type_id].schema() else {
                break 'enum_case;
            };
            let (field_idx, _) = enum_
                .variant_by_name(member)
                .ok_or(Error::UnresolvedMember(type_id, member, *span))?;

            let ir = ir::Const::EnumVariant(FieldId::new(type_id, field_idx));
            let typ = PolyType::nullary(ir.type_id());
            return Ok((ir::Expr::Const(ir, *span), typ));
        };

        let (ir, typ) = self.lower_expr(expr, env)?;
        let upper_bound = typ
            .unwrap_ref_or_self(self.symbols)
            .force_upper_bound(self.symbols)
            .with_span(*span)?
            .ok_or(Error::CannotLookupMember(*span))?
            .into_owned();
        self.field(ir, member, upper_bound, *span)
    }

    fn lower_for_in(
        &mut self,
        (name, name_span): &Spanned<&'ctx str>,
        iter @ (_, iter_span): &Spanned<ast::SourceExpr<'ctx>>,
        body: &ast::SourceBlock<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Block<'ctx>> {
        let (array, array_t) = self.lower_expr(iter, env)?;
        let elem_t = PolyType::fresh();
        let expected_t = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();
        array_t
            .constrain(&expected_t, self.symbols)
            .with_span(*iter_span)?;

        let mut env = env.introduce_scope();
        let counter_t = PolyType::nullary(predef::INT32);
        let counter = self.locals.add_var(counter_t.clone(), None).id;
        let array_local = self.locals.add_var(array_t.clone(), None).id;
        let elem = env.define_local(name, self.locals.add_var(elem_t, Some(*name_span)).clone());

        let loop_body = {
            let (increment, _) = self.free_function_call(
                "OperatorAssignAdd",
                [
                    (ir::Expr::Local(counter, span), counter_t.clone()),
                    (ir::Expr::Const(ir::Const::I32(1), span), counter_t.clone()),
                ],
                &env,
                span,
            )?;
            let prologue = [
                ir::Stmt::Expr(
                    ir::Expr::Assign {
                        place: ir::Expr::Local(elem, *name_span).into(),
                        expr: ir::Expr::Index {
                            array_type: array_t.into(),
                            array: ir::Expr::Local(array_local, *iter_span).into(),
                            index: ir::Expr::Local(counter, span).into(),
                            span,
                        }
                        .into(),
                        span,
                    }
                    .into(),
                ),
                ir::Stmt::Expr(
                    ir::Expr::Call {
                        call: increment.into(),
                        span,
                    }
                    .into(),
                ),
            ];

            let mut stmts = Vec::with_capacity(prologue.len() + body.stmts.len());
            stmts.extend(prologue);
            for (stmt, span) in &body.stmts {
                stmts.push(self.lower_stmt(stmt, &mut env, *span)?);
            }

            stmts
        };

        let (array_size, array_size_t) = self.free_function_call(
            "ArraySize",
            [(ir::Expr::Local(array_local, *iter_span), expected_t.clone())],
            &env,
            span,
        )?;
        let (check, _) = self.free_function_call(
            "OperatorLess",
            [
                (ir::Expr::Local(counter, span), counter_t.clone()),
                (
                    ir::Expr::Call {
                        call: array_size.into(),
                        span,
                    },
                    array_size_t,
                ),
            ],
            &env,
            span,
        )?;

        Ok(ir::Block::new([
            ir::Stmt::Expr(
                ir::Expr::Assign {
                    place: ir::Expr::Local(array_local, *iter_span).into(),
                    expr: array.into(),
                    span,
                }
                .into(),
            ),
            ir::Stmt::Expr(
                ir::Expr::Assign {
                    place: ir::Expr::Local(counter, span).into(),
                    expr: ir::Expr::Const(ir::Const::I32(0), span).into(),
                    span,
                }
                .into(),
            ),
            ir::Stmt::While(
                ir::CondBlock::new(
                    ir::Expr::Call {
                        call: check.into(),
                        span,
                    },
                    ir::Block::new(loop_body),
                ),
                span,
            ),
        ]))
    }

    fn lower_constant(
        &mut self,
        cn: &ast::Constant<'ctx>,
        hint: Option<&PolyType<'ctx>>,
        span: Span,
    ) -> ir::Const<'ctx> {
        let Some(PolyType::Mono(Type::Data(app))) = hint else {
            return ir::Const::from(cn);
        };

        let res = match (cn, app.id()) {
            (ast::Constant::F32(f), id) if id == predef::DOUBLE => Some(ir::Const::F64(*f as f64)),

            (ast::Constant::I32(i), id) if id == predef::FLOAT => Some(ir::Const::F32(*i as f32)),
            (ast::Constant::I32(i), id) if id == predef::DOUBLE => Some(ir::Const::F64(*i as f64)),

            (ast::Constant::I32(i), id) if id == predef::INT8 => {
                i8::try_from(*i).map(ir::Const::I8).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::INT16 => {
                i16::try_from(*i).map(ir::Const::I16).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::INT64 => {
                Some(ir::Const::I64(i64::from(*i)))
            }

            (ast::Constant::I32(i), id) if id == predef::UINT8 => {
                u8::try_from(*i).map(ir::Const::U8).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT16 => {
                u16::try_from(*i).map(ir::Const::U16).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT32 => {
                u32::try_from(*i).map(ir::Const::U32).ok()
            }
            (ast::Constant::I32(i), id) if id == predef::UINT64 => {
                u64::try_from(*i).map(ir::Const::U64).ok()
            }

            (ast::Constant::U32(i), id) if id == predef::UINT64 => {
                Some(ir::Const::U64(u64::from(*i)))
            }

            (
                ast::Constant::String(_) | ast::Constant::Resource(_) | ast::Constant::TweakDbId(_),
                id,
            ) if id == predef::CNAME => {
                self.reporter
                    .report(Error::WrongStringLiteral(id, 'n', span));
                return ir::Const::from(cn);
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::TweakDbId(_),
                id,
            ) if id == predef::RESOURCE => {
                self.reporter
                    .report(Error::WrongStringLiteral(id, 'r', span));
                return ir::Const::from(cn);
            }
            (
                ast::Constant::String(_) | ast::Constant::CName(_) | ast::Constant::Resource(_),
                id,
            ) if id == predef::TWEAK_DB_ID => {
                self.reporter
                    .report(Error::WrongStringLiteral(id, 't', span));
                return ir::Const::from(cn);
            }

            _ => Some(ir::Const::from(cn)),
        };

        if let Some(const_) = res {
            const_
        } else {
            self.reporter
                .report(Error::LiteralOutOfRange(app.id(), span));
            ir::Const::from(cn)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn resolve_overload<'a, Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: impl IntoIterator<
            IntoIter = impl ExactSizeIterator<Item = &'a Spanned<ast::SourceExpr<'ctx>>> + Clone,
        >,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        candidates: impl IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
        receiver: Option<InferredTypeApp<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, FunctionResultWithArgs<'ctx, Key>>
    where
        'ctx: 'a,
        Key: FunctionKey<'ctx> + Copy,
        Name: Copy,
        Func: FunctionKind<'ctx> + Copy,
    {
        let args = args.into_iter();
        let argc = args.len();

        let mut arg_bounds: Option<RangeInclusive<usize>> = None;
        let mut filtered = candidates
            .into_iter()
            .filter(|f| {
                let range = f.func().type_().min_args()..=f.func().type_().max_args();
                arg_bounds = Some(arg_bounds.as_ref().map_or(range.clone(), |other| {
                    *other.start().min(range.start())..=*other.end().max(range.end())
                }));
                range.contains(&argc)
            })
            .peekable();

        let Some(primary) = filtered.next() else {
            match arg_bounds {
                Some(bounds) => return Err(Error::InvalidArgCount(bounds, span)),
                None => return Err(Error::UnresolvedFunction(name, span)),
            }
        };

        if filtered.peek().is_none() {
            let type_env = receiver
                .and_then(|typ| typ.base_type_env(primary.key().parent()?, self.symbols))
                .unwrap_or_default();
            let type_env = type_env.push_scope(
                self.function_env(primary.func().type_(), type_args, env, span)?
                    .pop_scope(),
            );
            let checked_args: Box<[_]> = args
                .zip(primary.func().type_().params())
                .map(|(arg @ (_, span), param)| {
                    let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                        .map_err(|var| Error::UnresolvedVar(var, *span))?;
                    let (mut expr, typ) = self.lower_expr_with(arg, Some(&expected), env)?;
                    self.coerce(&mut expr, typ.clone(), expected, env, *span)?;
                    Ok(expr)
                })
                .collect::<Result<_, _>>()?;

            let return_t =
                PolyType::from_type_with_env(primary.func().type_().return_type(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
            let type_args = type_env
                .pop_scope()
                .into_values()
                .map(ir::Type::from)
                .collect::<Box<[_]>>();
            let res = FunctionResolution::new(*primary.key(), type_args, return_t)
                .with_args(checked_args);
            Ok(res)
        } else {
            let mut checked_args = Vec::with_capacity(args.len());
            let mut arg_types = Vec::with_capacity(args.len());

            for arg in args {
                let (expr, typ) = self.lower_expr(arg, env)?;
                checked_args.push(expr);
                arg_types.push(typ);
            }

            let matches = iter::once(primary.clone()).chain(filtered);
            let fallback = Some(primary);
            let res = self
                .resolve_overload_typed(
                    name,
                    &mut checked_args,
                    arg_types,
                    matches,
                    fallback,
                    receiver,
                    env,
                    span,
                )?
                .with_args(checked_args);
            Ok(res)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn resolve_overload_typed<Key, Name, Func>(
        &mut self,
        name: &'ctx str,
        args: &mut [ir::Expr<'ctx>],
        arg_types: Vec<PolyType<'ctx>>,
        candidates: impl IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
        fallback: Option<FunctionEntry<Key, Name, Func>>,
        receiver: Option<InferredTypeApp<'ctx>>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, FunctionResolution<'ctx, Key>>
    where
        Key: FunctionKey<'ctx> + Copy,
        Func: FunctionKind<'ctx> + Copy,
    {
        fn matched_by_subtype<'a, 'ctx, It, Key, Name, Func>(
            it: It,
            arg_types: &'a [PolyType<'ctx>],
            symbols: &'a Symbols<'ctx>,
        ) -> impl Iterator<Item = FunctionEntry<Key, Name, Func>> + use<'a, 'ctx, It, Key, Name, Func>
        where
            It: IntoIterator<Item = FunctionEntry<Key, Name, Func>>,
            Func: FunctionKind<'ctx> + Copy,
        {
            it.into_iter().filter(|entry| {
                arg_types
                    .iter()
                    .zip(entry.func().type_().unwrapped_param_types())
                    .all(|(typ, param_t)| {
                        typ.unwrap_ref_or_self(symbols)
                            .is_subtype_compatible(param_t, symbols)
                    })
            })
        }

        let mut matches = matched_by_subtype(candidates, &arg_types, self.symbols).peekable();
        let selected = matches
            .next()
            .or(fallback)
            .ok_or(Error::UnresolvedFunction(name, span))?;

        let selected = if matches.peek().is_none() {
            selected
        } else {
            // retry with upper bounds lifted to resolve ambiguity
            for arg in &arg_types {
                arg.force_upper_bound(self.symbols).with_span(span)?;
            }
            let candidates = iter::once(selected).chain(matches);
            let mut matches = matched_by_subtype(candidates, &arg_types, self.symbols).peekable();

            let primary = matches
                .next()
                .ok_or(Error::UnresolvedFunction(name, span))?;
            if matches.peek().is_some() {
                let count = matches.count() + 1;
                return Err(Error::MultipleMatchingOverloads(name, count, span));
            }
            primary
        };

        let type_env = receiver
            .and_then(|typ| typ.base_type_env(selected.key().parent()?, self.symbols))
            .unwrap_or_default();
        let type_env = type_env.push_scope(
            self.function_env(selected.func().type_(), &[], env, span)?
                .pop_scope(),
        );
        for ((arg, typ), param) in args
            .iter_mut()
            .zip(&arg_types)
            .zip(selected.func().type_().params())
        {
            let span = arg.span();
            let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                .map_err(|var| Error::UnresolvedVar(var, span))?;
            self.coerce(&mut *arg, typ.clone(), expected, env, span)?;
        }

        let return_t =
            PolyType::from_type_with_env(selected.func().type_().return_type(), &type_env)
                .map_err(|var| Error::UnresolvedVar(var, span))?;
        let type_args = type_env
            .pop_scope()
            .into_values()
            .map(ir::Type::from)
            .collect::<Box<[_]>>();
        Ok(FunctionResolution::new(
            *selected.key(),
            type_args,
            return_t,
        ))
    }

    fn resolve_local(
        &mut self,
        name: &'ctx str,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let Some((loc, depth)) = env.locals.get_with_depth(name) else {
            return Err(Error::UnresolvedVar(name, span));
        };

        let ir = if depth > 0
            && env.locals.scope_iter().count() - usize::from(depth) < self.locals.depth
        {
            self.captures.insert(Capture::new(loc.id, depth));
            ir::Expr::Capture(loc.id, span)
        } else {
            ir::Expr::Local(loc.id, span)
        };
        Ok((ir, loc.typ.clone()))
    }

    #[inline]
    fn resolve_type(
        &self,
        typ: &ast::SourceType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, PolyType<'ctx>> {
        Ok(PolyType::from_type(&env.types.resolve(typ, span)?))
    }

    fn coerce(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        let name = match lhs.coerce(&rhs, self.symbols).with_span(span)? {
            Some(Coercion::From(RefType::Weak)) => ir::Intrinsic::WeakRefToRef,
            Some(Coercion::Into(RefType::Weak)) => ir::Intrinsic::RefToWeakRef,
            Some(Coercion::From(RefType::Script)) => ir::Intrinsic::Deref,
            Some(Coercion::Into(RefType::Script)) => ir::Intrinsic::AsRef,
            None => return Ok(()),
        };
        let arg = mem::replace(expr, ir::Expr::Null(span));
        let (res, _) = self.free_function_call(name.into(), [(arg, lhs)], env, span)?;
        *expr = ir::Expr::Call {
            call: res.into(),
            span,
        };
        Ok(())
    }

    fn free_function_call(
        &mut self,
        name: &'ctx str,
        args: impl IntoIterator<Item = (ir::Expr<'ctx>, PolyType<'ctx>)>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        let (mut args, arg_types): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let candidates = env.query_free_functions(name, self.symbols);
        let res = self
            .resolve_overload_typed(
                name, &mut args, arg_types, candidates, None, None, env, span,
            )?
            .with_args(args)
            .into_call();
        Ok(res)
    }

    fn field(
        &self,
        ir: ir::Expr<'ctx>,
        member: &'ctx str,
        upper_t: InferredTypeApp<'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let (target_id, (field_idx, field)) = self
            .symbols
            .base_iter(upper_t.id())
            .find_map(|(id, typ)| {
                Some((id, typ.schema().as_aggregate()?.fields().by_name(member)?))
            })
            .ok_or_else(|| Error::UnresolvedMember(upper_t.id(), member, span))?;
        let this_t = upper_t
            .instantiate_as(target_id, self.symbols)
            .expect("should instantiate subtype");
        let env = this_t.type_env(self.symbols);
        let typ = PolyType::from_type_with_env(field.type_(), &env)
            .map_err(|var| Error::UnresolvedVar(var, span))?;

        let field = FieldId::new(target_id, field_idx);
        let ir = ir::Expr::Field {
            receiver_type: this_t.into(),
            receiver: ir.into(),
            field,
            span,
        };
        Ok((ir, typ))
    }

    fn function_env(
        &self,
        func_t: &FunctionType<'ctx>,
        type_args: &[Spanned<ast::SourceType<'ctx>>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ScopedMap<'_, &'ctx str, PolyType<'ctx>>> {
        let map: ScopedMap<'_, _, _> = func_t
            .type_params()
            .iter()
            .map(|var| (var.name(), PolyType::fresh()))
            .collect();

        for ((_, var), param) in map.top().iter().zip(func_t.type_params()) {
            if let Some(upper) = param.upper() {
                let constraint = PolyType::from_type_with_env(upper, &map)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
                var.constrain(&constraint, self.symbols).with_span(span)?;
            }
            if let Some(lower) = param.lower() {
                PolyType::from_type_with_env(lower, &map)
                    .map_err(|var| Error::UnresolvedVar(var, span))?
                    .constrain(var, self.symbols)
                    .with_span(span)?;
            }
        }

        if type_args.is_empty() {
            return Ok(map);
        }

        if type_args.len() != func_t.type_params().len() {
            let expected = func_t.type_params().len();
            return Err(Error::InvalidTypeArgCount(expected, span));
        }

        for ((_, var), (arg, span)) in map.top().iter().zip(type_args) {
            var.constrain(&self.resolve_type(arg, env, *span)?, self.symbols)
                .with_span(*span)?;
        }

        Ok(map)
    }
}

#[derive(Debug, Default)]
pub struct LowerOutput<'ctx> {
    locals: Vec<ir::LocalInfo<'ctx>>,
    captures: IndexSet<Capture>,
}

impl<'ctx> LowerOutput<'ctx> {
    pub fn new(locals: Vec<ir::LocalInfo<'ctx>>, mut captures: IndexSet<Capture>) -> Self {
        // we can safely discard captures that are no longer in scope
        captures.retain2(|cap| cap.pop_scope().map(|next| *cap = next).is_some());
        Self { locals, captures }
    }

    #[inline]
    pub fn locals(&self) -> &[ir::LocalInfo<'ctx>] {
        &self.locals
    }

    pub fn into_inner(self) -> (Vec<ir::LocalInfo<'ctx>>, IndexSet<Capture>) {
        (self.locals, self.captures)
    }
}

#[derive(Debug)]
pub struct Locals<'scope, 'ctx> {
    locals: Vec<ir::LocalInfo<'ctx>>,
    counter: &'scope Cell<u16>,
    depth: usize,
}

impl<'scope, 'ctx> Locals<'scope, 'ctx> {
    #[inline]
    pub fn new(counter: &'scope Cell<u16>, depth: usize) -> Self {
        Self {
            locals: vec![],
            counter,
            depth,
        }
    }

    #[inline]
    pub fn add_var(&mut self, typ: PolyType<'ctx>, span: Option<Span>) -> &ir::LocalInfo<'ctx> {
        self.add(ir::Local::Var(self.counter.get()), typ, span)
    }

    #[inline]
    pub fn add_param(&mut self, typ: PolyType<'ctx>, span: Option<Span>) -> &ir::LocalInfo<'ctx> {
        self.add(ir::Local::Param(self.counter.get()), typ, span)
    }

    #[inline]
    fn add(
        &mut self,
        local: ir::Local,
        typ: PolyType<'ctx>,
        span: Option<Span>,
    ) -> &ir::LocalInfo<'ctx> {
        self.counter.set(self.counter.get() + 1);
        let len = self.locals.len();
        self.locals.push(ir::LocalInfo::new(local, typ, span));
        &self.locals[len]
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Capture {
    captured: ir::Local,
    depth: u16,
}

impl Capture {
    #[inline]
    fn new(captured: ir::Local, depth: u16) -> Self {
        Self { captured, depth }
    }

    #[inline]
    pub fn pop_scope(&self) -> Option<Self> {
        Some(Self::new(self.captured, self.depth.checked_sub(1)?))
    }
}

impl<'ctx> InferredTypeApp<'ctx> {
    fn from_type_with_env(
        typ: &TypeApp<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<Self, &'ctx str> {
        let args = typ
            .args()
            .iter()
            .map(|typ| PolyType::from_type_with_env(typ, env))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(TypeApp::new(typ.id(), args))
    }

    pub fn from_type(typ: &TypeApp<'ctx>) -> Self {
        TypeApp::new(
            typ.id(),
            typ.args()
                .iter()
                .map(PolyType::from_type)
                .collect::<Rc<_>>(),
        )
    }

    #[inline]
    pub fn coalesce(&self, symbols: &Symbols<'ctx>) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        Simplifier::coalesce_app(self, symbols)
    }

    pub fn instantiate_as(
        self,
        target: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> Option<InferredTypeApp<'ctx>> {
        let mut cur = self;
        while cur.id() != target {
            cur = cur.instantiate_base(symbols)?;
        }
        Some(cur)
    }

    pub fn instantiate_base(self, symbols: &Symbols<'ctx>) -> Option<InferredTypeApp<'ctx>> {
        let class = &symbols[self.id()];
        let args = self
            .args()
            .iter()
            .cloned()
            .chain(iter::repeat_with(PolyType::fresh));
        let env = class.vars().zip(args).collect();
        TypeApp::from_type_with_env(class.schema().base_type()?, &env).ok()
    }

    #[inline]
    fn base_type_env<'scope, 'sym>(
        self,
        target: TypeId<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Option<ScopedMap<'scope, &'ctx str, PolyType<'ctx>>> {
        Some(self.instantiate_as(target, symbols)?.type_env(symbols))
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let id = symbols
            .glb(self.id(), other.id())
            .ok_or_else(|| TypeError::Incompatible(self.clone(), other.clone()))?;
        if id == self.id() {
            Ok(self.clone())
        } else {
            Ok(other.clone())
        }
    }

    fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let id = symbols
            .lub(self.id(), other.id())
            .ok_or_else(|| TypeError::Incompatible(self.clone(), other.clone()))?;
        let lhs = self
            .clone()
            .instantiate_as(id, symbols)
            .expect("should always match the lub type");
        let rhs = other
            .clone()
            .instantiate_as(id, symbols)
            .expect("should always match the lub type");
        let args = symbols[id]
            .params()
            .iter()
            .zip(lhs.args().iter().zip(rhs.args()))
            .map(|(v, (l, r))| match v.variance() {
                Variance::Covariant => l.lub(r, symbols),
                Variance::Contravariant => l.glb(r, symbols),
                Variance::Invariant => l.lub(r, symbols)?.glb(r, symbols),
            })
            .collect::<Result<Rc<_>, _>>()?;
        Ok(TypeApp::new(id, args))
    }
}

impl fmt::Display for InferredTypeApp<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

#[derive(Debug, Clone, Error)]
pub enum TypeError<'ctx> {
    #[error("type mismatch: found {0} when expected {1}")]
    Mismatch(InferredType<'ctx>, InferredType<'ctx>),
    #[error("type error: {0} is not compatible with {1}")]
    Incompatible(InferredTypeApp<'ctx>, InferredTypeApp<'ctx>),
    #[error("type error: cannot unify {0} and {1}")]
    CannotUnify(InferredType<'ctx>, InferredType<'ctx>),
}

impl<'ctx> InferredType<'ctx> {
    #[inline]
    pub fn into_poly(self) -> PolyType<'ctx> {
        PolyType::Mono(self)
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Nothing, _) | (_, Self::Nothing) => Self::Nothing,
            (Self::Data(lhs), Self::Data(rhs)) => Self::Data(lhs.glb(rhs, symbols)?),
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => Self::Ctx(lhs.clone()),
            (Self::Ctx(ctx), t) | (t, Self::Ctx(ctx)) => match ctx.upper() {
                Some(upper) => upper.glb(t, symbols)?,
                None => return Err(TypeError::CannotUnify(self.clone(), other.clone())),
            },
        };
        Ok(typ)
    }

    fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Nothing, other) => other.clone(),
            (_, Self::Nothing) => self.clone(),
            (Self::Data(lhs), Self::Data(rhs)) => Self::Data(lhs.lub(rhs, symbols)?),
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => Self::Ctx(lhs.clone()),
            (Self::Ctx(ctx), t) | (t, Self::Ctx(ctx)) => {
                ctx.lower().unwrap_or(&Type::Nothing).lub(t, symbols)?
            }
        };
        Ok(typ)
    }

    fn constrain(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Nothing, _) => {}
            (Self::Data(lhs), Self::Data(rhs)) => {
                let l0 = lhs
                    .clone()
                    .instantiate_as(rhs.id(), symbols)
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), other.clone()))?;
                let r0 = rhs;
                for (v, (l, r)) in symbols[r0.id()]
                    .params()
                    .iter()
                    .zip(l0.args().iter().zip(r0.args()))
                {
                    match v.variance() {
                        Variance::Covariant => l.constrain(r, symbols)?,
                        Variance::Contravariant => r.constrain(l, symbols)?,
                        Variance::Invariant => l.constrain_invariant(r, symbols)?,
                    }
                }
            }
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => {}
            _ => return Err(TypeError::Mismatch(self.clone(), other.clone())),
        };
        Ok(())
    }

    fn is_subtype_compatible(&self, other: &Type<'ctx>, symbols: &Symbols<'ctx>) -> bool {
        match (self, other) {
            (Self::Nothing, _) | (_, Type::Ctx(_)) => true,
            (Self::Data(lhs), Type::Data(rhs)) => symbols.is_subtype(lhs.id(), rhs.id()),
            (Self::Ctx(lhs), Type::Data(rhs)) => lhs
                .upper()
                .and_then(|typ| Some(typ.upper_bound()?.id()))
                .is_some_and(|id| symbols.is_subtype(id, rhs.id())),
            _ => false,
        }
    }
}

impl<'ctx> From<&Type<'ctx>> for InferredType<'ctx> {
    fn from(typ: &Type<'ctx>) -> Self {
        match typ {
            Type::Nothing => Self::Nothing,
            Type::Data(typ) => Self::Data(InferredTypeApp::from_type(typ)),
            Type::Ctx(var) => Self::Ctx(InferredCtxVar::from_var(var).into()),
        }
    }
}

impl<'ctx> InferredCtxVar<'ctx> {
    pub fn from_var(var: &CtxVar<'ctx>) -> Self {
        let lower = var.lower().map(Type::from);
        let upper = var.upper().map(Type::from);
        Self::new(var.name(), var.variance(), lower, upper)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyType<'ctx> {
    Mono(InferredType<'ctx>),
    Var(Var<'ctx>),
}

impl<'ctx> PolyType<'ctx> {
    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Type::nullary(id).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the same environment.
    #[inline]
    pub fn from_type(typ: &Type<'ctx>) -> Self {
        Type::from(typ).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the provided environment.
    pub fn from_type_with_env(
        typ: &Type<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<PolyType<'ctx>, &'ctx str> {
        let result = match typ {
            Type::Nothing => Type::Nothing,
            Type::Data(typ) => Type::Data(InferredTypeApp::from_type_with_env(typ, env)?),
            Type::Ctx(var) => return env.get(var.name()).cloned().ok_or(var.name()),
        };
        Ok(result.into_poly())
    }

    #[inline]
    pub fn coalesce(&self, symbols: &Symbols<'ctx>) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        Simplifier::coalesce(self, symbols)
    }

    #[inline]
    fn fresh() -> Self {
        Self::Var(Var::fresh())
    }

    #[inline]
    fn with_bounds(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        Self::Var(Var::new(lower, upper))
    }

    fn force_upper_bound(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, Option<Cow<'_, InferredTypeApp<'ctx>>>> {
        match self {
            Self::Mono(typ) => Ok(typ.upper_bound().map(Cow::Borrowed)),
            Self::Var(var) => {
                let lower = var.lower();
                var.add_upper(&lower, symbols)?;
                Ok(self.upper_bound(symbols))
            }
        }
    }

    fn upper_bound(&self, symbols: &Symbols<'ctx>) -> Option<Cow<'_, InferredTypeApp<'ctx>>> {
        match self {
            Self::Mono(typ) => typ.upper_bound().map(Cow::Borrowed),
            Self::Var(var) if var.lower().is_primitive(symbols) => {
                var.lower().upper_bound().map(|typ| Cow::Owned(typ.clone()))
            }
            Self::Var(var) => var
                .upper()
                .as_ref()
                .and_then(Type::upper_bound)
                .map(|typ| Cow::Owned(typ.clone())),
        }
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => Self::Mono(l.glb(r, symbols)?),
            (Self::Mono(l), Self::Var(r)) => {
                r.add_upper(l, symbols)?;
                Self::Var(r.clone())
            }
            (Self::Var(l), Self::Mono(r)) => {
                l.add_upper(r, symbols)?;
                Self::Var(l.clone())
            }
            (Self::Var(l), Self::Var(r)) => {
                l.unify(r, symbols)?;
                Self::Var(r.clone())
            }
        };
        Ok(typ)
    }

    fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => Self::Mono(l.lub(r, symbols)?),
            (Self::Mono(l), Self::Var(r)) => {
                r.add_lower(l, symbols)?;
                Self::Var(r.clone())
            }
            (Self::Var(l), Self::Mono(r)) => {
                l.add_lower(r, symbols)?;
                Self::Var(l.clone())
            }
            (Self::Var(l), Self::Var(r)) => {
                l.unify(r, symbols)?;
                Self::Var(r.clone())
            }
        };
        Ok(typ)
    }

    fn constrain(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => l.constrain(r, symbols),
            (Self::Mono(l), Self::Var(r)) => r.add_lower(l, symbols),
            (Self::Var(l), Self::Mono(r)) => l.add_upper(r, symbols),
            (Self::Var(l), Self::Var(r)) => l.unify(r, symbols),
        }
    }

    #[inline]
    fn constrain_invariant(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        self.constrain(other, symbols)?;
        other.constrain(self, symbols)
    }

    fn coerce(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Option<Coercion>> {
        match (self.strip_ref(symbols), other.strip_ref(symbols)) {
            (Some((from, pointee)), None) => {
                pointee.constrain(other, symbols)?;
                Ok(Some(Coercion::From(from)))
            }
            (None, Some((to, pointee))) => {
                self.constrain(&pointee, symbols)?;
                Ok(Some(Coercion::Into(to)))
            }
            _ => {
                self.constrain(other, symbols)?;
                Ok(None)
            }
        }
    }

    #[inline]
    pub fn unwrap_ref(&self, symbols: &Symbols<'ctx>) -> Option<PolyType<'ctx>> {
        Some(self.strip_ref(symbols)?.1)
    }

    #[inline]
    pub fn unwrap_ref_or_self(&self, symbols: &Symbols<'ctx>) -> PolyType<'ctx> {
        self.unwrap_ref(symbols).unwrap_or_else(|| self.clone())
    }

    #[inline]
    fn strip_ref(&self, symbols: &Symbols<'ctx>) -> Option<(RefType, PolyType<'ctx>)> {
        self.upper_bound(symbols)
            .as_deref()
            .and_then(TypeApp::strip_ref)
            .map(|(rt, typ)| (rt, typ.clone()))
    }

    fn is_subtype_compatible(&self, other: &Type<'ctx>, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Mono(typ) => typ.is_subtype_compatible(other, symbols),
            Self::Var(var) => {
                var.upper()
                    .is_some_and(|typ| typ.is_subtype_compatible(other, symbols))
                    || var.upper().is_none()
            }
        }
    }
}

impl<'ctx> From<(TypeId<'ctx>, Rc<[PolyType<'ctx>]>)> for PolyType<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[PolyType<'ctx>]>)) -> Self {
        Type::app(id, args).into_poly()
    }
}

impl fmt::Display for PolyType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mono(typ) => typ.fmt(f),
            Self::Var(_) => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var<'ctx>(Rc<RefCell<VarState<'ctx>>>);

impl<'ctx> Var<'ctx> {
    #[inline]
    fn new(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        Self(Rc::new(RefCell::new(VarState::new(lower, upper))))
    }

    #[inline]
    fn fresh() -> Self {
        Self::new(Type::Nothing, None)
    }

    #[inline]
    fn key(&self) -> VarKey<'ctx> {
        VarKey(self.0.as_ptr())
    }

    fn repr(&self) -> Var<'ctx> {
        match &mut self.0.borrow_mut().repr {
            Some(var) => {
                let rep = var.repr();
                if rep != *self {
                    *var = rep.clone();
                }
                rep
            }
            None => self.clone(),
        }
    }

    #[inline]
    fn lower(&self) -> InferredType<'ctx> {
        self.repr().0.borrow().lower.clone()
    }

    #[inline]
    fn upper(&self) -> Option<InferredType<'ctx>> {
        self.repr().0.borrow().upper.clone()
    }

    fn add_upper(&self, ub: &InferredType<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        // TODO: occurs check
        let repr = self.repr();
        let mut repr = repr.0.borrow_mut();

        let new = repr
            .upper
            .as_ref()
            .map_or_else(|| Ok(ub.clone()), |u| u.glb(ub, symbols))?;
        repr.upper = Some(new);
        repr.lower.constrain(ub, symbols)
    }

    fn add_lower(&self, lb: &InferredType<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        // TODO: occurs check
        let repr = self.repr();
        let mut repr = repr.0.borrow_mut();
        repr.lower = repr.lower.lub(lb, symbols)?;
        if let Some(ub) = &repr.upper {
            lb.constrain(ub, symbols)?;
        }
        Ok(())
    }

    fn unify(&self, other: &Var<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        let repr0 = self.repr();
        let repr1 = other.repr();
        if repr0 == repr1 {
            return Ok(());
        }
        // TODO: occurs check
        repr1.add_lower(&repr0.lower(), symbols)?;
        if let Some(ub) = &repr0.upper() {
            repr1.add_upper(ub, symbols)?;
        }
        repr0.0.borrow_mut().repr = Some(repr1.clone());
        Ok(())
    }
}

impl PartialEq for Var<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl Eq for Var<'_> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct VarKey<'ctx>(*mut VarState<'ctx>);

impl IdentityHashable for VarKey<'_> {}

#[derive(Debug, Clone)]
struct VarState<'ctx> {
    lower: InferredType<'ctx>,
    upper: Option<InferredType<'ctx>>,
    repr: Option<Var<'ctx>>,
}

impl<'ctx> VarState<'ctx> {
    #[inline]
    fn new(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        Self {
            lower,
            upper,
            repr: None,
        }
    }
}

#[derive(Debug)]
pub struct TypeEnv<'scope, 'ctx>(ScopedMap<'scope, &'ctx str, TypeRef<'scope, 'ctx>>);

impl<'scope, 'ctx> TypeEnv<'scope, 'ctx> {
    pub fn with_default_types() -> Self {
        let mut map = IndexMap::default();
        map.insert("array", TypeRef::Name(predef::ARRAY));
        map.insert("ref", TypeRef::Name(predef::REF));
        map.insert("wref", TypeRef::Name(predef::WREF));
        map.insert("script_ref", TypeRef::Name(predef::SCRIPT_REF));

        Self(ScopedMap::from(map))
    }

    #[inline]
    pub fn introduce_scope(&'scope self) -> Self {
        Self(self.0.introduce_scope())
    }

    #[inline]
    pub fn pop_scope(self) -> IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.0.pop_scope()
    }

    #[inline]
    pub fn push_scope(&'scope self, scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>) -> Self {
        Self(self.0.push_scope(scope))
    }

    #[inline]
    pub fn top_level(&self) -> &IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.0.top()
    }

    #[inline]
    pub fn get(&self, name: &'ctx str) -> Option<&TypeRef<'scope, 'ctx>> {
        self.0.get(name)
    }

    #[inline]
    pub fn add(&mut self, name: &'ctx str, typ: TypeRef<'scope, 'ctx>) {
        self.0.insert(name, typ);
    }

    pub fn resolve(
        &self,
        typ: &ast::SourceType<'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, Type<'ctx>> {
        match typ {
            ast::Type::Named { name, args } => match (self.0.get(name), &args[..]) {
                (Some(&TypeRef::Name(id)), [(arg, span)]) if id == predef::REF => {
                    Ok(self.resolve(arg, *span)?)
                }
                (Some(&TypeRef::Name(id)), _) => {
                    let args = args
                        .iter()
                        .map(|(typ, span)| self.resolve(typ, *span))
                        .collect::<Result<Rc<_>, _>>()?;
                    Ok(Type::app(id, args))
                }
                (Some(TypeRef::Var(var)), _) => Ok(Type::Ctx(var.clone())),
                (Some(TypeRef::LazyVar(stub)), _) => Ok(Type::Ctx(
                    stub.get(self).map_err(|_| Error::CyclicType(span))??,
                )),
                (None, _) => Err(Error::UnresolvedType(name, span)),
            },
            ast::Type::Array(elem) => {
                let (elem, span) = &**elem;
                let elem = self.resolve(elem, *span)?;
                Ok(Type::app(predef::ARRAY, [elem]))
            }
            ast::Type::StaticArray(elem, size) => {
                let (elem, elem_span) = &**elem;
                let elem = self.resolve(elem, *elem_span)?;
                let id = TypeId::array_with_size(*size)
                    .ok_or(Error::UnsupportedStaticArraySize(span))?;
                Ok(Type::app(id, [elem]))
            }
            ast::Type::Fn {
                params,
                return_type: return_typ,
            } => {
                let args = params
                    .iter()
                    .chain([&**return_typ])
                    .map(|(typ, span)| self.resolve(typ, *span))
                    .collect::<Result<Rc<_>, _>>()?;

                let id =
                    TypeId::fn_with_arity(params.len()).ok_or(Error::UnsupportedArity(span))?;
                Ok(Type::app(id, args))
            }
        }
    }

    pub fn resolve_param(
        &self,
        param: &ast::SourceTypeParam<'ctx>,
    ) -> LowerResult<'ctx, CtxVar<'ctx>> {
        let (name, _) = param.name;
        let variance = param.variance.into();
        let upper = param
            .upper_bound
            .as_deref()
            .map(|(typ, span)| self.resolve(typ, *span))
            .transpose()?;
        Ok(CtxVar::new(name, variance, None, upper))
    }
}

impl Default for TypeEnv<'_, '_> {
    fn default() -> Self {
        Self::with_default_types()
    }
}

#[derive(Debug)]
pub struct Env<'scope, 'ctx> {
    types: &'scope TypeEnv<'scope, 'ctx>,
    funcs: &'scope ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
    locals: ScopedMap<'scope, &'ctx str, ir::LocalInfo<'ctx>>,
}

impl<'scope, 'ctx> Env<'scope, 'ctx> {
    pub fn new(
        types: &'scope TypeEnv<'scope, 'ctx>,
        globals: &'scope ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
    ) -> Self {
        Self {
            types,
            funcs: globals,
            locals: ScopedMap::default(),
        }
    }

    #[inline]
    pub fn define_local(&mut self, name: &'ctx str, info: ir::LocalInfo<'ctx>) -> ir::Local {
        let id = info.id;
        self.locals.insert(name, info);
        id
    }

    #[inline]
    fn introduce_scope(&'scope self) -> Self {
        Self {
            types: self.types,
            funcs: self.funcs,
            locals: self.locals.introduce_scope(),
        }
    }

    fn query_free_functions<'a>(
        &self,
        name: &'a str,
        symbols: &'scope Symbols<'ctx>,
    ) -> impl Iterator<Item = FunctionEntry<FreeFunctionIndex, &'a str, &'scope FreeFunction<'ctx>>>
    {
        self.funcs
            .scope_iter()
            .filter_map(|map| map.get(name))
            .flatten()
            .map(move |&idx| FunctionEntry::new(idx, name, &symbols[idx]))
    }
}

#[derive(Debug)]
pub enum TypeRef<'scope, 'ctx> {
    Name(TypeId<'ctx>),
    Var(Rc<CtxVar<'ctx>>),
    #[allow(clippy::type_complexity)]
    LazyVar(
        Box<
            Lazy<
                LowerResult<'ctx, Rc<CtxVar<'ctx>>>,
                Box<dyn Fn(&TypeEnv<'_, 'ctx>) -> LowerResult<'ctx, Rc<CtxVar<'ctx>>> + 'scope>,
            >,
        >,
    ),
}

impl<'ctx> TypeRef<'_, 'ctx> {
    pub fn force(self) -> Option<TypeRef<'static, 'ctx>> {
        match self {
            Self::Name(id) => Some(TypeRef::Name(id)),
            Self::Var(typ) => Some(TypeRef::Var(typ)),
            Self::LazyVar(lazy) => Some(TypeRef::Var(lazy.try_get()?.ok()?)),
        }
    }
}

#[derive(Debug)]
struct Simplifier<'sym, 'ctx> {
    cache: HashMap<VarKey<'ctx>, Type<'ctx>, BuildIdentityHasher<usize>>,
    covariant: HashSet<VarKey<'ctx>, BuildIdentityHasher<usize>>,
    contravariant: HashSet<VarKey<'ctx>, BuildIdentityHasher<usize>>,
    symbols: &'sym Symbols<'ctx>,
}

impl<'sym, 'ctx> Simplifier<'sym, 'ctx> {
    fn coalesce(
        typ: &PolyType<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let mut this = Self {
            cache: HashMap::default(),
            covariant: HashSet::default(),
            contravariant: HashSet::default(),
            symbols,
        };
        this.analyze_poly(typ, Variance::Covariant);
        this.simplify_poly(typ, Variance::Covariant)
    }

    fn coalesce_app(
        typ: &InferredTypeApp<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        let mut this = Self {
            cache: HashMap::default(),
            covariant: HashSet::default(),
            contravariant: HashSet::default(),
            symbols,
        };
        this.analyze_app(typ, Variance::Covariant);
        this.simplify_app(typ, Variance::Covariant)
    }

    fn simplify_poly(
        &mut self,
        typ: &PolyType<'ctx>,
        variance: Variance,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let res = match typ {
            PolyType::Var(var) => {
                let key = var.key();
                let hash = self.cache.hasher().hash_one(key);
                if let Some((_, e)) = self.cache.raw_entry().from_key_hashed_nocheck(hash, &key) {
                    return Ok(e.clone());
                }

                let typ = match (var.lower(), var.upper()) {
                    (lower, Some(upper)) if lower == upper => self.simplify(&lower, variance)?,
                    (bound, None) | (Type::Nothing, Some(bound))
                        if bound.is_primitive(self.symbols) =>
                    {
                        self.simplify(&bound, variance)?
                    }
                    (lower, _)
                        if variance == Variance::Covariant
                            && !self.contravariant.contains(&key) =>
                    {
                        self.simplify(&lower, variance)?
                    }
                    (_, Some(upper))
                        if variance == Variance::Contravariant
                            && !self.covariant.contains(&key) =>
                    {
                        self.simplify(&upper, variance)?
                    }
                    (lower, Some(upper))
                        if upper
                            .constrain(&lower, self.symbols)
                            .and_then(|_| lower.constrain(&upper, self.symbols))
                            .is_ok() =>
                    {
                        self.simplify(&lower, variance)?
                    }
                    (Type::Nothing, None) => Type::Nothing,
                    (lower, upper) => return Err(CoalesceError::CannotCoalesce(lower, upper)),
                };

                self.cache
                    .raw_entry_mut()
                    .from_key_hashed_nocheck(hash, &key)
                    .insert(key, typ.clone())
                    .get()
                    .clone()
            }
            PolyType::Mono(c) => self.simplify(c, variance)?,
        };
        Ok(res)
    }

    fn simplify_app(
        &mut self,
        typ: &InferredTypeApp<'ctx>,
        var: Variance,
    ) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        let vars = self.symbols[typ.id()].params();
        let args = typ
            .args()
            .iter()
            .zip(vars)
            .map(|(arg, v)| self.simplify_poly(arg, var.combined(v.variance())))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(TypeApp::new(typ.id(), args))
    }

    fn simplify_ctx_var(
        &mut self,
        var: &InferredCtxVar<'ctx>,
        variance: Variance,
    ) -> Result<CtxVar<'ctx>, CoalesceError<'ctx>> {
        let lower = var
            .lower()
            .map(|typ| self.simplify(typ, variance.combined(var.variance())))
            .transpose()?;
        let upper = var
            .upper()
            .map(|typ| self.simplify(typ, variance.combined(var.variance())))
            .transpose()?;
        Ok(CtxVar::new(var.name(), var.variance(), lower, upper))
    }

    fn simplify(
        &mut self,
        typ: &InferredType<'ctx>,
        var: Variance,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let res = match typ {
            Type::Data(typ) => Type::Data(self.simplify_app(typ, var)?),
            Type::Ctx(var) => Type::Ctx(self.simplify_ctx_var(var, var.variance())?.into()),
            Type::Nothing => Type::Nothing,
        };
        Ok(res)
    }

    fn analyze_poly(&mut self, typ: &PolyType<'ctx>, variance: Variance) {
        match typ {
            PolyType::Var(var) => {
                if variance != Variance::Covariant {
                    self.contravariant.insert(var.key());
                    if let Some(upper) = &var.upper() {
                        self.analyze(upper, variance);
                    }
                }
                if variance != Variance::Contravariant {
                    self.covariant.insert(var.key());
                    self.analyze(&var.lower(), variance);
                }
            }
            PolyType::Mono(typ) => self.analyze(typ, variance),
        }
    }

    fn analyze_app(&mut self, typ: &InferredTypeApp<'ctx>, variance: Variance) {
        for (arg, v) in typ.args().iter().zip(self.symbols[typ.id()].params()) {
            self.analyze_poly(arg, variance.combined(v.variance()));
        }
    }

    #[inline]
    fn analyze(&mut self, typ: &InferredType<'ctx>, variance: Variance) {
        if let Type::Data(typ) = typ {
            self.analyze_app(typ, variance);
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Coercion {
    From(RefType),
    Into(RefType),
}

#[derive(Debug)]
struct FunctionResolution<'ctx, K> {
    function: K,
    type_args: Box<[ir::Type<'ctx>]>,
    return_type: PolyType<'ctx>,
}

impl<'ctx, K> FunctionResolution<'ctx, K> {
    #[inline]
    fn new(function: K, type_args: Box<[ir::Type<'ctx>]>, return_type: PolyType<'ctx>) -> Self {
        Self {
            function,
            type_args,
            return_type,
        }
    }

    #[inline]
    fn with_args(self, args: impl Into<Box<[ir::Expr<'ctx>]>>) -> FunctionResultWithArgs<'ctx, K> {
        FunctionResultWithArgs {
            resulution: self,
            args: args.into(),
        }
    }
}

#[derive(Debug)]
struct FunctionResultWithArgs<'ctx, K> {
    resulution: FunctionResolution<'ctx, K>,
    args: Box<[ir::Expr<'ctx>]>,
}

impl<'ctx> FunctionResultWithArgs<'ctx, FreeFunctionIndex> {
    #[inline]
    fn into_call(self) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::FreeFunction {
            function: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
        };
        (call, self.resulution.return_type)
    }
}

impl<'ctx> FunctionResultWithArgs<'ctx, MethodId<'ctx>> {
    #[inline]
    fn into_instance_call(
        self,
        receiver: ir::Expr<'ctx>,
        receiver_type: impl Into<ir::TypeApp<'ctx>>,
        receiver_ref: Option<RefType>,
        mode: ir::CallMode,
    ) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::Instance {
            receiver: Box::new(receiver),
            receiver_type: receiver_type.into(),
            receiver_ref,
            method: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
            mode,
        };
        (call, self.resulution.return_type)
    }

    #[inline]
    fn into_static_call(
        self,
        parent_id: TypeId<'ctx>,
        parent_type_args: impl IntoIterator<Item = impl Into<ir::Type<'ctx>>>,
    ) -> (ir::Call<'ctx>, PolyType<'ctx>) {
        let call = ir::Call::Static {
            parent_id,
            parent_type_args: parent_type_args.into_iter().map(Into::into).collect(),
            method: self.resulution.function,
            type_args: self.resulution.type_args,
            args: self.args,
        };
        (call, self.resulution.return_type)
    }
}

#[derive(Debug)]
pub struct Poly;

impl TypeKind for Poly {
    type Type<'ctx> = PolyType<'ctx>;
}

#[derive(Debug)]
pub enum CoalesceError<'ctx> {
    CannotCoalesce(InferredType<'ctx>, Option<InferredType<'ctx>>),
}

impl fmt::Display for CoalesceError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CannotCoalesce(lower, upper) => {
                write!(f, "type is too broad ({lower} <: T <: ")?;
                if let Some(upper) = upper {
                    write!(f, "{upper}")?;
                } else {
                    write!(f, "Any")?;
                }
                write!(f, " for some type T), consider adding type annotations")
            }
        }
    }
}

impl std::error::Error for CoalesceError<'_> {}

pub type LowerResult<'id, A, E = Error<'id>> = Result<A, E>;

#[derive(Debug, Clone, Error)]
pub enum Error<'ctx> {
    #[error("{0}")]
    Type(Box<TypeError<'ctx>>, Span),
    #[error("'{0}' is not defined")]
    UnresolvedVar(&'ctx str, Span),
    #[error("'{0}' is not a known type")]
    UnresolvedType(&'ctx str, Span),
    #[error("'{0}' has no member named '{1}'")]
    UnresolvedMember(TypeId<'ctx>, &'ctx str, Span),
    #[error("{1} matching overloads found for method '{0}'")]
    MultipleMatchingOverloads(&'ctx str, usize, Span),
    #[error("there's no matching '{0}' function")]
    UnresolvedFunction(&'ctx str, Span),
    #[error("invalid number of arguments, expected {}", DisplayRangeInclusive(.0))]
    InvalidArgCount(RangeInclusive<usize>, Span),
    #[error(
        "insufficient type information available for member lookup, consider adding \
         type annotations"
    )]
    CannotLookupMember(Span),
    #[error("this type cannot be constructed with the 'new' operator")]
    InvalidNewType(Span),
    #[error("this type cannot be casted with the 'as' operator")]
    InvalidDynCastType(Span),
    #[error("the target type of this cast is not known, consider specifying it")]
    UnknownStaticCastType(Span),
    #[error("invalid number of type arguments, expected {0}")]
    InvalidTypeArgCount(usize, Span),
    #[error("class constructors do not accept arguments")]
    ClassConstructorHasArguments(Span),
    #[error(
        "unsupported arity, functions can only have up to {} parameters",
        MAX_FN_ARITY
    )]
    UnsupportedArity(Span),
    #[error(
        "unsupported static array size, static arrays can only have up to {} elements",
        MAX_STATIC_ARRAY_SIZE
    )]
    UnsupportedStaticArraySize(Span),
    #[error("this expression cannot be used as a case label")]
    InvalidCaseLabel(Span),
    #[error("invalid cyclic type reference")]
    CyclicType(Span),
    #[error("this literal is out of range for {0}{hint}", hint = NumberTypeRangeHint(*.0))]
    LiteralOutOfRange(TypeId<'ctx>, Span),
    #[error(
        r#"expected a {0} here, you should prefix your literal with '{1}', e.g. {1}"lorem ipsum""#
    )]
    WrongStringLiteral(TypeId<'ctx>, char, Span),
    #[error("'{0}' is an abstract class and cannot be instantiated")]
    InstantiatingAbstract(TypeId<'ctx>, Span),
    #[error("this type has no super type to refer to")]
    NonExistentSuperType(Span),
}

impl Error<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Type(_, span)
            | Self::UnresolvedVar(_, span)
            | Self::UnresolvedType(_, span)
            | Self::UnresolvedMember(_, _, span)
            | Self::MultipleMatchingOverloads(_, _, span)
            | Self::UnresolvedFunction(_, span)
            | Self::InvalidArgCount(_, span)
            | Self::CannotLookupMember(span)
            | Self::InvalidNewType(span)
            | Self::InvalidDynCastType(span)
            | Self::UnknownStaticCastType(span)
            | Self::InvalidTypeArgCount(_, span)
            | Self::ClassConstructorHasArguments(span)
            | Self::UnsupportedArity(span)
            | Self::UnsupportedStaticArraySize(span)
            | Self::InvalidCaseLabel(span)
            | Self::CyclicType(span)
            | Self::LiteralOutOfRange(_, span)
            | Self::WrongStringLiteral(_, _, span)
            | Self::InstantiatingAbstract(_, span)
            | Self::NonExistentSuperType(span) => *span,
        }
    }
}

struct NumberTypeRangeHint<'ctx>(TypeId<'ctx>);

impl fmt::Display for NumberTypeRangeHint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_range<T: fmt::Display>(f: &mut fmt::Formatter<'_>, min: T, max: T) -> fmt::Result {
            write!(f, ", provide a value between {} and {}", min, max)
        }
        match self.0 {
            id if id == predef::UINT8 => write_range(f, u8::MIN, u8::MAX),
            id if id == predef::UINT16 => write_range(f, u16::MIN, u16::MAX),
            id if id == predef::UINT32 => write_range(f, u32::MIN, u32::MAX),
            id if id == predef::UINT64 => write_range(f, u64::MIN, u64::MAX),
            id if id == predef::INT8 => write_range(f, i8::MIN, i8::MAX),
            id if id == predef::INT16 => write_range(f, i16::MIN, i16::MAX),
            id if id == predef::INT32 => write_range(f, i32::MIN, i32::MAX),
            id if id == predef::INT64 => write_range(f, i64::MIN, i64::MAX),
            _ => Ok(()),
        }
    }
}

struct DisplayRangeInclusive<'a, T>(&'a RangeInclusive<T>);

impl<T: fmt::Display + PartialEq> fmt::Display for DisplayRangeInclusive<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.0.start();
        let end = self.0.end();
        if start == end {
            write!(f, "{}", start)
        } else {
            write!(f, "between {} and {}", start, end)
        }
    }
}

fn binop_name(op: &ast::BinOp) -> &'static str {
    match op {
        ast::BinOp::AssignAdd => "OperatorAssignAdd",
        ast::BinOp::AssignSub => "OperatorAssignSubtract",
        ast::BinOp::AssignMul => "OperatorAssignMultiply",
        ast::BinOp::AssignDiv => "OperatorAssignDivide",
        ast::BinOp::AssignBitOr => "OperatorAssignOr",
        ast::BinOp::AssignBitAnd => "OperatorAssignAnd",
        ast::BinOp::Or => "OperatorLogicOr",
        ast::BinOp::And => "OperatorLogicAnd",
        ast::BinOp::BitOr => "OperatorOr",
        ast::BinOp::BitXor => "OperatorXor",
        ast::BinOp::BitAnd => "OperatorAnd",
        ast::BinOp::Eq => "OperatorEqual",
        ast::BinOp::Ne => "OperatorNotEqual",
        ast::BinOp::Lt => "OperatorLess",
        ast::BinOp::Le => "OperatorLessEqual",
        ast::BinOp::Gt => "OperatorGreater",
        ast::BinOp::Ge => "OperatorGreaterEqual",
        ast::BinOp::Add => "OperatorAdd",
        ast::BinOp::Sub => "OperatorSubtract",
        ast::BinOp::Mul => "OperatorMultiply",
        ast::BinOp::Div => "OperatorDivide",
        ast::BinOp::Mod => "OperatorModulo",
    }
}

fn unop_name(op: &ast::UnOp) -> &'static str {
    match op {
        ast::UnOp::Not => "OperatorLogicNot",
        ast::UnOp::Neg => "OperatorNeg",
        ast::UnOp::BitNot => "OperatorBitNot",
    }
}
