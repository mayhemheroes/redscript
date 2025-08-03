use std::cell::Cell;
use std::ops::RangeInclusive;
use std::rc::Rc;
use std::{iter, mem, slice};

use env::{Capture, Locals};
pub use env::{Env, TypeEnv, TypeRef};
pub use error::{CoalesceError, Error, LowerResult, TypeError};
use indexmap::set::MutableValues;
use redscript_ast as ast;
use redscript_ast::{Span, Spanned};
use types::Coercion;
pub use types::{InferredTypeApp, Poly, PolyType};

use crate::diagnostic::ErrorWithSpan;
use crate::symbols::{
    FieldId, FreeFunctionIndex, FunctionEntry, FunctionKey, FunctionKind, FunctionType, Symbols,
    TypeSchema, Visibility,
};
use crate::types::{RefType, Type, TypeApp, TypeId, predef};
use crate::utils::ScopedMap;
use crate::{IndexSet, LowerReporter, MethodId, Param, ir};

mod env;
mod error;
mod simplify;
mod types;

#[derive(Debug)]
pub struct Lower<'scope, 'ctx> {
    locals: Locals<'scope, 'ctx>,
    return_type: PolyType<'ctx>,
    captures: IndexSet<Capture>,
    stmt_prefix: Vec<Vec<ir::Stmt<'ctx>>>,
    symbols: &'scope Symbols<'ctx>,
    reporter: &'scope mut LowerReporter<'ctx>,
    context: Option<TypeId<'ctx>>,
}

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    #[inline]
    fn new(
        locals: Locals<'scope, 'ctx>,
        return_type: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &'scope Symbols<'ctx>,
        reporter: &'scope mut LowerReporter<'ctx>,
    ) -> Self {
        Self {
            locals,
            return_type,
            captures: IndexSet::default(),
            stmt_prefix: Vec::new(),
            symbols,
            reporter,
            context,
        }
    }

    #[inline]
    fn into_output(self) -> LowerOutput<'ctx> {
        LowerOutput::new(self.locals.into_vec(), self.captures)
    }

    pub fn function(
        body: &ast::SourceFunctionBody<'ctx>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        env: Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
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
            context,
            symbols,
            &mut reporter,
        );
        (block, output, reporter.into_reported())
    }

    #[allow(clippy::too_many_arguments)]
    fn function_with(
        body: &ast::SourceFunctionBody<'ctx>,
        local_counter: &Cell<u16>,
        params: impl IntoIterator<Item = (&'ctx str, PolyType<'ctx>)>,
        mut env: Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
        fn inner<'ctx>(
            symbols: &Symbols<'ctx>,
            reporter: &mut LowerReporter<'ctx>,
            locals: Locals<'_, 'ctx>,
            ret_t: PolyType<'ctx>,
            context: Option<TypeId<'ctx>>,
            body: &ast::SourceFunctionBody<'ctx>,
            env: &mut Env<'_, 'ctx>,
        ) -> (ir::Block<'ctx>, LowerOutput<'ctx>) {
            match body {
                ast::FunctionBody::Block(block) => {
                    let mut lower = Lower::new(locals, ret_t.clone(), context, symbols, reporter);
                    (lower.lower_block(&block.stmts, env), lower.into_output())
                }
                ast::FunctionBody::Inline(expr) => {
                    let result = Lower::expr(expr, locals, env, ret_t, context, symbols, reporter);
                    let (body, output) = reporter.unwrap_err(result).unzip();
                    (body.unwrap_or_default(), output.unwrap_or_default())
                }
            }
        }

        let mut locals = Locals::new(local_counter, env.locals().scope_iter().count());
        for (name, typ) in params {
            env.define_local(name, locals.add_param(typ, None).clone());
        }
        inner(symbols, reporter, locals, return_t, context, body, &mut env)
    }

    pub fn constant(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        env: &Env<'_, 'ctx>,
        expected: PolyType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> (Option<ir::Const<'ctx>>, Vec<Error<'ctx>>) {
        let mut reporter = LowerReporter::default();
        let counter = Cell::new(0);
        let locals = Locals::new(&counter, 0);
        let mut lower = Lower::new(locals, expected.clone(), None, symbols, &mut reporter);
        let res = (|| {
            let (mut expr, typ) = lower.lower_expr_with(expr, Some(&expected), env)?;
            lower.coerce(&mut expr, typ, expected, env, *span)?;

            if let ir::Expr::Const(const_, _) = &expr {
                Ok(const_.clone())
            } else {
                Err(Error::UnexpectedNonConstant(*span))
            }
        })();
        let res = reporter.unwrap_err(res);
        (res, reporter.into_reported())
    }

    fn expr(
        expr @ (_, span): &Spanned<ast::SourceExpr<'ctx>>,
        locals: Locals<'_, 'ctx>,
        env: &Env<'_, 'ctx>,
        return_t: PolyType<'ctx>,
        context: Option<TypeId<'ctx>>,
        symbols: &Symbols<'ctx>,
        reporter: &mut LowerReporter<'ctx>,
    ) -> LowerResult<'ctx, (ir::Block<'ctx>, LowerOutput<'ctx>)> {
        let mut lower = Lower::new(locals, return_t.clone(), context, symbols, reporter);
        let (mut expr, typ) = lower.lower_expr_with(expr, Some(&return_t), env)?;
        lower.coerce(&mut expr, typ, return_t, env, *span)?;

        let mut stmts = lower.stmt_prefix.pop().unwrap_or_default();
        stmts.push(ir::Stmt::Return(Some(expr.into()), *span));
        Ok((ir::Block::new(stmts), lower.into_output()))
    }

    fn push_prefix(&mut self, stmt: impl Into<ir::Stmt<'ctx>>) {
        let mut top = self.stmt_prefix.pop().unwrap_or_default();
        top.push(stmt.into());
        self.stmt_prefix.push(top);
    }

    fn lower_block(
        &mut self,
        stmts: &[Spanned<ast::SourceStmt<'ctx>>],
        env: &mut Env<'_, 'ctx>,
    ) -> ir::Block<'ctx> {
        let mut accumulator = Vec::with_capacity(stmts.len());
        self.stmt_prefix.push(vec![]);

        for (stmt, span) in stmts {
            let res = self.lower_stmt(stmt, env, *span);
            if let Some(prefix) = self.stmt_prefix.last_mut() {
                accumulator.append(prefix);
            }
            if let Some(stmt) = self.reporter.unwrap_err(res) {
                accumulator.push(stmt);
            }
        }

        self.stmt_prefix.pop();
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

    fn lower_conditional_block(
        &mut self,
        block: &ast::SourceConditionalBlock<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, (ir::ConditionalBlock<'ctx>, Option<ir::Stmt<'ctx>>)> {
        match &block.condition {
            ast::LetCondition::Expr(expr) => {
                let (_, span) = expr;
                let (condition, expr_t) = self.lower_expr(expr, env)?;
                expr_t
                    .constrain(&PolyType::nullary(predef::BOOL), self.symbols)
                    .with_span(*span)?;
                let block = self.lower_scoped_block(&block.body.stmts, env);
                Ok((ir::ConditionalBlock::new(condition, block), None))
            }
            ast::LetCondition::LetPattern(pattern, val) => {
                let (_, span) = pattern;
                let (expr, expr_t) = self.lower_expr(val, env)?;

                let local = self.locals.add_var(expr_t.clone(), expr.span()).id;
                let mut scope = env.introduce_scope();
                let (prologue, condition) = self.lower_pattern_into_prologue_and_condition(
                    local, &expr_t, &mut scope, pattern,
                )?;
                let mut block = self.lower_block(&block.body.stmts, &mut scope);
                block.push_prologue(prologue);

                Ok((
                    ir::ConditionalBlock::new(condition, block),
                    Some(
                        ir::Expr::Assign {
                            place: ir::Expr::Local(local, *span).into(),
                            expr: expr.into(),
                            span: *span,
                        }
                        .into(),
                    ),
                ))
            }
        }
    }

    fn lower_projection(
        &mut self,
        projection: &Projection<'_, 'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Expr<'ctx>> {
        match projection {
            Projection::Index {
                array,
                array_type,
                index,
            } if *index >= 0 => Ok(ir::Expr::Index {
                array_type: array_type.clone().into(),
                array: self.lower_projection(array, env, span)?.into(),
                index: ir::Expr::Const(ir::Const::I32(*index), span).into(),
                span,
            }),
            Projection::Index {
                array,
                array_type,
                index,
            } => {
                let array_arg = self.lower_projection(array, env, span)?;
                let (length, length_t) = self.new_free_function_call(
                    ir::Intrinsic::ArraySize,
                    [(array_arg, array_type.clone())],
                    &[],
                    env,
                    span,
                )?;
                let index = ir::Expr::Const(ir::Const::I32(-index), span);
                let (index, _) = self.new_free_function_call(
                    ast::BinOp::Sub,
                    [
                        (ir::Expr::call(length, span), length_t),
                        (index, PolyType::nullary(predef::INT32)),
                    ],
                    &[],
                    env,
                    span,
                )?;
                Ok(ir::Expr::Index {
                    array_type: array_type.clone().into(),
                    array: self.lower_projection(array, env, span)?.into(),
                    index: ir::Expr::call(index, span).into(),
                    span,
                })
            }
            Projection::Field {
                receiver,
                receiver_type,
                receiver_ref,
                field,
            } => Ok(ir::Expr::Field {
                field: *field,
                receiver_type: receiver_type.clone().into(),
                receiver_ref: *receiver_ref,
                receiver: self.lower_projection(receiver, env, span)?.into(),
                span,
            }),
            Projection::Scrutinee(local) => Ok(ir::Expr::Local(*local, span)),
        }
    }

    fn lower_pattern(
        &mut self,
        pattern: &ast::SourcePattern<'ctx>,
        projection: &Projection<'_, 'ctx>,
        projection_t: &PolyType<'ctx>,
        env: &mut Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, Pattern<'ctx>> {
        match pattern {
            ast::Pattern::Name((name, name_span)) => {
                let local = self.locals.add_var(projection_t.clone(), *name_span);
                let local = env.define_local(name, local.clone());
                let projected = self.lower_projection(projection, env, *name_span)?;
                Ok(Pattern {
                    conditions: vec![],
                    prologue: vec![
                        ir::Expr::Assign {
                            place: ir::Expr::Local(local, *name_span).into(),
                            expr: projected.into(),
                            span: *name_span,
                        }
                        .into(),
                    ],
                })
            }
            ast::Pattern::As(pattern, (as_type, type_span)) => {
                let (pattern, pattern_span) = &**pattern;

                let (ref_type, unref_t) = projection_t.strip_ref(self.symbols).unzip();
                let unref_t = unref_t.unwrap_or(projection_t.clone());

                let as_type = self.resolve_type(as_type, env, *type_span)?;
                as_type
                    .constrain(&unref_t, self.symbols)
                    .with_span(*pattern_span)?;

                let Pattern {
                    mut conditions,
                    prologue,
                } = self.lower_pattern(pattern, projection, &as_type, env)?;

                let projected = self.lower_projection(projection, env, *type_span)?;
                let type_check =
                    self.new_runtime_type_check(projected, &as_type, ref_type, env, *pattern_span)?;
                conditions.push(ir::Expr::call(type_check, *type_span));

                Ok(Pattern {
                    conditions,
                    prologue,
                })
            }
            ast::Pattern::Aggregate((name, span), fields) => {
                let Some(TypeRef::Name(type_id)) = env.types().get(name) else {
                    return Err(Error::UnresolvedType(name, *span));
                };

                let aggregate_t_app = TypeApp::from_id(*type_id, self.symbols);
                let aggregate_t = aggregate_t_app.clone().into_type().into_poly();
                let (receiver_ref, unref_t) = projection_t.strip_ref(self.symbols).unzip();
                let unref_t = unref_t.unwrap_or(projection_t.clone());

                aggregate_t
                    .constrain(&unref_t, self.symbols)
                    .with_span(*span)?;

                let projected = self.lower_projection(projection, env, *span)?;
                let type_check =
                    self.new_runtime_type_check(projected, &aggregate_t, receiver_ref, env, *span)?;

                let nested = fields
                    .into_iter()
                    .filter_map(|((field, field_span), pattern)| {
                        let field_res =
                            self.resolve_field(field, aggregate_t_app.clone(), *field_span);
                        let (field, field_t, receiver_type) =
                            self.reporter.unwrap_err(field_res)?;

                        let project = Projection::Field {
                            receiver: projection,
                            receiver_type,
                            receiver_ref,
                            field,
                        };

                        let pattern = self.lower_pattern(pattern, &project, &field_t, env);
                        self.reporter.unwrap_err(pattern)
                    });

                let mut accumulator = Pattern::from_conditions([ir::Expr::call(type_check, *span)]);
                accumulator.extend(nested);
                Ok(accumulator)
            }
            ast::Pattern::Nullable(pattern) => {
                let (pattern, span) = &**pattern;
                let Pattern {
                    mut conditions,
                    prologue,
                } = self.lower_pattern(pattern, projection, projection_t, env)?;
                let projected = self.lower_projection(projection, env, *span)?;
                let (call, _) = self.new_free_function_call(
                    ir::Intrinsic::IsDefined,
                    [(projected, projection_t.clone())],
                    &[],
                    env,
                    *span,
                )?;
                conditions.push(ir::Expr::call(call, *span));

                Ok(Pattern {
                    conditions,
                    prologue,
                })
            }
            ast::Pattern::Array(array_spread, patterns) => {
                let (patterns, patterns_span) = patterns;

                let pattern_count = i32::try_from(patterns.len()).expect("too many patterns");
                let range = match array_spread {
                    ast::ArraySpread::Start => -(pattern_count)..0,
                    ast::ArraySpread::End | ast::ArraySpread::None => 0..pattern_count,
                };
                let operator = match array_spread {
                    ast::ArraySpread::Start | ast::ArraySpread::End => ast::BinOp::Ge,
                    ast::ArraySpread::None => ast::BinOp::Eq,
                };

                let elem_t = PolyType::fresh();
                let expected = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();
                projection_t
                    .constrain(&expected, self.symbols)
                    .with_span(*patterns_span)?;

                let projected = self.lower_projection(projection, env, *patterns_span)?;
                let (length, length_t) = self.new_free_function_call(
                    ir::Intrinsic::ArraySize,
                    [(projected, projection_t.clone())],
                    &[],
                    env,
                    *patterns_span,
                )?;
                let (size_check, _) = self.new_free_function_call(
                    operator.name(),
                    [
                        (ir::Expr::call(length, *patterns_span), length_t.clone()),
                        (
                            ir::Expr::Const(ir::Const::I32(pattern_count), *patterns_span),
                            PolyType::nullary(predef::INT32),
                        ),
                    ],
                    &[],
                    env,
                    *patterns_span,
                )?;

                let nested = patterns.iter().zip(range).filter_map(|(pattern, index)| {
                    let projection = Projection::Index {
                        array: projection,
                        array_type: projection_t.clone(),
                        index,
                    };
                    let result = self.lower_pattern(pattern, &projection, &elem_t, env);
                    self.reporter.unwrap_err(result)
                });
                let mut pattern =
                    Pattern::from_conditions([ir::Expr::call(size_check, *patterns_span)]);
                pattern.extend(nested);
                Ok(pattern)
            }
        }
    }

    fn lower_case(
        &mut self,
        case: &ast::SourceCase<'ctx>,
        scrutinee_t: &PolyType<'ctx>,
        env: &Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, ir::Case<'ctx>> {
        let ast::Condition::Expr(expr) = &case.condition else {
            unimplemented!("pattern matching in regular case statements");
        };
        let (expr, expr_t) = self.lower_expr(expr, env)?;
        expr_t
            .constrain(scrutinee_t, self.symbols)
            .with_span(expr.span())?;
        let block = self.lower_scoped_block(&case.body, env);
        Ok(ir::Case::new(expr, block))
    }

    fn lower_pattern_case(
        &mut self,
        case: &ast::SourceCase<'ctx>,
        scrutinee: ir::Local,
        scrutinee_t: &PolyType<'ctx>,
        env: &mut Env<'_, 'ctx>,
    ) -> LowerResult<'ctx, ir::Case<'ctx>> {
        let mut scope = env.introduce_scope();
        let (prologue, condition) = match &case.condition {
            ast::Condition::Expr(expr) => {
                let (_, span) = expr;
                let (expr, expr_t) = self.lower_expr_with(expr, Some(scrutinee_t), &scope)?;
                scrutinee_t
                    .constrain(&expr_t, self.symbols)
                    .with_span(*span)?;

                let local = ir::Expr::Local(scrutinee, *span);
                let args = [(expr, expr_t), (local, scrutinee_t.clone())];
                let (call, _) =
                    self.new_free_function_call(ir::Intrinsic::Equals, args, &[], &scope, *span)?;
                (vec![], ir::Expr::call(call, *span))
            }
            ast::Condition::Pattern(pattern) => self.lower_pattern_into_prologue_and_condition(
                scrutinee,
                scrutinee_t,
                &mut scope,
                pattern,
            )?,
        };

        let mut block = self.lower_block(&case.body, &mut scope);

        if matches!(case.condition, ast::Condition::Pattern(_))
            && !matches!(
                block.stmts.back(),
                Some(ir::Stmt::Break(_) | ir::Stmt::Return(_, _))
            )
        {
            self.reporter
                .report(Error::MissingBreakInCaseLet(case.condition.span()));
        }

        block.push_prologue(prologue);
        Ok(ir::Case::new(condition, block))
    }

    fn lower_pattern_into_prologue_and_condition(
        &mut self,
        scrutinee: ir::Local,
        scrutinee_t: &PolyType<'ctx>,
        scope: &mut Env<'_, 'ctx>,
        (pattern, span): &Spanned<ast::SourcePattern<'ctx>>,
    ) -> Result<(Vec<ir::Stmt<'ctx>>, ir::Expr<'ctx>), Error<'ctx>> {
        let projection = Projection::Scrutinee(scrutinee);
        let Pattern {
            conditions,
            prologue,
        } = self.lower_pattern(pattern, &projection, scrutinee_t, scope)?;
        let expr = conditions
            .into_iter()
            .try_fold(None, |acc, cond| {
                let Some(acc) = acc else {
                    return Ok(Some(cond));
                };
                let typ = PolyType::nullary(predef::BOOL);
                let args = [(acc, typ.clone()), (cond, typ)];
                let (call, _) =
                    self.new_free_function_call(ast::BinOp::And, args, &[], scope, *span)?;
                Ok(Some(ir::Expr::call(call, *span)))
            })?
            .unwrap_or(ir::Expr::Const(ir::Const::Bool(true), *span));
        Ok((prologue, expr))
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
                let elem_hint = hint.and_then(|typ| {
                    let typ = typ.upper_bound(self.symbols)?;
                    (typ.id() == predef::ARRAY)
                        .then_some(typ.args().first().cloned())
                        .flatten()
                });

                let (elems, elem_typ) = elems.iter().try_fold(
                    (vec![], PolyType::fresh()),
                    |(mut acc, elem_typ), elem| {
                        let (mut elem, typ) = self.lower_expr(elem, env)?;
                        let typ = if let Some(hint) = &elem_hint {
                            let span = elem.span();
                            self.coerce(&mut elem, typ.clone(), hint.clone(), env, span)?;
                            hint.clone()
                        } else {
                            typ
                        };
                        acc.push(elem);
                        Ok((acc, elem_typ.lub(&typ, self.symbols).with_span(*span)?))
                    },
                )?;

                let array_t = Type::app(predef::ARRAY, [elem_typ.clone()]).into_poly();
                let local = self.locals.add_var(array_t.clone(), *span).id;

                self.push_prefix(ir::Stmt::InitArray {
                    local,
                    elements: elems.into(),
                    element_type: elem_typ.into(),
                    span: *span,
                });

                (ir::Expr::Local(local, *span), array_t)
            }
            ast::Expr::InterpolatedString(elems) => {
                let str = elems
                    .iter()
                    .try_fold(None, |acc, elem| {
                        let elem = match elem {
                            ast::StrPart::Expr(expr @ (_, span)) => {
                                let args = [self.lower_expr(expr, env)?];
                                let (call, _) = self.new_free_function_call(
                                    ir::Intrinsic::ToString,
                                    args,
                                    &[],
                                    env,
                                    *span,
                                )?;
                                ir::Expr::call(call, *span)
                            }
                            ast::StrPart::Str(str) => {
                                ir::Expr::Const(ir::Const::Str(str.clone()), *span)
                            }
                        };
                        let res = match acc {
                            Some(lhs) => {
                                let args =
                                    [lhs, elem].map(|e| (e, PolyType::nullary(predef::STRING)));
                                let (call, _) = self.new_free_function_call(
                                    ast::BinOp::Add,
                                    args,
                                    &[],
                                    env,
                                    *span,
                                )?;
                                ir::Expr::call(call, *span)
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

                if lhs.is_prvalue(self.symbols) {
                    self.reporter.report(Error::InvalidPlaceExpr(*span));
                }

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
                    self.new_free_function_call(op.name(), [lhs, rhs], &[], env, *span)?;
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::UnOp { op, expr } => {
                if let (ast::UnOp::Neg, &(ast::Expr::Constant(ast::Constant::I32(i)), span)) =
                    (op, &**expr)
                {
                    let const_ = self.lower_constant(&ast::Constant::I32(-i), hint, span);
                    let typ = PolyType::nullary(const_.type_id());
                    return Ok((ir::Expr::Const(const_, span), typ));
                }

                let arg = self.lower_expr(expr, env)?;
                let (call, typ) = self.new_free_function_call(op.name(), [arg], &[], env, *span)?;
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::Call {
                expr,
                type_args,
                args,
            } => {
                let (call, typ) = self.lower_call(expr, type_args, args, hint, env, *span)?;
                (ir::Expr::call(call, *span), typ)
            }
            ast::Expr::Member { expr, member } => self.lower_member(expr, member, env, *span)?,
            ast::Expr::Index { expr, index } => {
                let expr @ (_, expr_span) = &**expr;
                let elem_t = PolyType::fresh();

                let (mut expr, expr_t) = self.lower_expr(expr, env)?;
                let id = if let Some(typ) = expr_t
                    .upper_bound(self.symbols)
                    .filter(|typ| typ.id().is_static_array())
                {
                    typ.id()
                } else {
                    predef::ARRAY
                };

                let array_t = Type::app(id, [elem_t.clone()]).into_poly();
                self.coerce(&mut expr, expr_t, array_t.clone(), env, *expr_span)?;

                let index @ (_, index_span) = &**index;
                let (index, index_t) = self.lower_expr(index, env)?;
                index_t
                    .constrain(&PolyType::nullary(predef::INT32), self.symbols)
                    .with_span(*index_span)?;

                if expr.is_prvalue(self.symbols) {
                    self.reporter.report(Error::InvalidTemporary(*expr_span));
                }

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
                let (expr, expr_t) = self.lower_expr(expr, env)?;
                let (typ, type_span) = &**typ;
                let Type::Data(typ) = env.types().resolve(typ, self.symbols, *type_span)? else {
                    return Err(Error::InvalidDynCastType(*type_span));
                };
                let target = InferredTypeApp::from_type(&typ);
                let expected = target.clone().into_type().into_poly();
                let inferred = if let Some((RefType::Weak, inner)) = expr_t.strip_ref(self.symbols)
                {
                    expected
                        .constrain(&inner, self.symbols)
                        .map(|_| Type::app(predef::WREF, [expected.clone()]).into_poly())
                        .map_err(|err| (err, inner))
                } else {
                    expected
                        .constrain(&expr_t, self.symbols)
                        .map(|_| expected.clone())
                        .map_err(|err| (err, expr_t.clone()))
                };

                match inferred {
                    Err((err, inner)) => {
                        if inner.constrain(&expected, self.symbols).is_ok() {
                            return Err(Error::RedundantDynCast(*span));
                        }
                        return Err(Error::ImpossibleDynCast(err.into(), *span));
                    }
                    Ok(inferred) => {
                        let ir = ir::Expr::DynCast {
                            expr: expr.into(),
                            expr_type: expr_t.into(),
                            target_type: target.into(),
                            span: *span,
                        };
                        (ir, inferred)
                    }
                }
            }
            ast::Expr::New { typ, args } => {
                let (typ, type_span) = &**typ;
                let Type::Data(typ) = env.types().resolve(typ, self.symbols, *type_span)? else {
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

                        if aggregate.flags().is_native() && !aggregate.flags().is_sealed() {
                            if !args.is_empty() {
                                self.reporter.report(
                                    Error::NonFullyDefinedNativeStructConstruction(typ.id(), *span),
                                );
                            }
                        } else if args.len() != field_count {
                            self.reporter
                                .report(Error::InvalidArgCount(field_count..=field_count, *span));
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
                            args,
                            span: *span,
                        }
                    }
                    TypeSchema::Aggregate(agg) if agg.flags().is_abstract() => {
                        return Err(Error::InstantiatingAbstract(typ.id(), *type_span));
                    }
                    TypeSchema::Aggregate(_) if args.is_empty() => ir::Expr::NewClass {
                        class_type: typ.into(),
                        span: *span,
                    },
                    TypeSchema::Aggregate(_) => {
                        return Err(Error::ClassConstructorHasArguments(*span));
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

                let (then, then_t) = self.lower_expr_with(then, hint, env)?;
                let (else_, else_t) = self.lower_expr_with(else_, hint.or(Some(&then_t)), env)?;

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
                let is_weak = hint
                    .and_then(|typ| typ.ref_type(self.symbols))
                    .is_some_and(|ref_t| ref_t == RefType::Weak);
                let typ = if is_weak {
                    Type::app(predef::WREF, [typ]).into_poly()
                } else {
                    typ
                };
                let ir = ir::Expr::Null {
                    is_weak,
                    span: *span,
                };
                (ir, typ)
            }

            ast::Expr::Error => (ir::Expr::null(*span), PolyType::fresh()),
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
                    .transpose()?;
                if let Some(value @ (_, value_span)) = value.as_deref() {
                    let (mut value, value_t) = self.lower_expr_with(value, typ.as_ref(), env)?;
                    let typ = if let Some(typ) = typ {
                        self.coerce(&mut value, value_t, typ.clone(), env, *value_span)?;
                        typ
                    } else {
                        value_t
                    };
                    let local = self.locals.add_var(typ, name_span);
                    let local = env.define_local(name, local.clone());

                    ir::Expr::Assign {
                        place: ir::Expr::Local(local, name_span).into(),
                        expr: value.into(),
                        span,
                    }
                    .into()
                } else {
                    let typ = typ.unwrap_or_else(PolyType::fresh);
                    let local = self.locals.add_var(typ.clone(), name_span);
                    let local = env.define_local(name, local.clone());
                    ir::Stmt::InitDefault {
                        local,
                        typ: typ.into(),
                        span,
                    }
                }
            }
            ast::Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                let (expr, expr_t) = self.lower_expr(expr, env)?;
                let default = default
                    .as_ref()
                    .map(|block| self.lower_scoped_block(block, env));

                let (scrutinee, scrutinee_type, branches) = if cases
                    .iter()
                    .all(|case| matches!(case.condition, ast::Condition::Expr(_)))
                {
                    let branches = cases
                        .iter()
                        .map(|case| self.lower_case(case, &expr_t, env))
                        .collect::<Result<_, _>>()?;
                    (expr.into(), expr_t.into(), branches)
                } else {
                    let span = expr.span();
                    let local = self.extract_local(expr, &expr_t, span);
                    let branches = cases
                        .iter()
                        .map(|case| self.lower_pattern_case(case, local, &expr_t, env))
                        .collect::<Result<_, _>>()?;
                    (
                        ir::Expr::Const(ir::Const::Bool(true), span).into(),
                        PolyType::nullary(predef::BOOL).into(),
                        branches,
                    )
                };

                ir::Stmt::Switch {
                    scrutinee,
                    scrutinee_type,
                    branches,
                    default,
                    span,
                }
            }
            ast::Stmt::If { blocks, else_ } => {
                let branches = blocks
                    .iter()
                    .map(|block| {
                        let (block, prefix) = self.lower_conditional_block(block, env)?;
                        if let Some(prefix) = prefix {
                            self.push_prefix(prefix);
                        }
                        Ok(block)
                    })
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
                let (block, prefix) = self.lower_conditional_block(block, env)?;
                let Some(prefix) = prefix else {
                    return Ok(ir::Stmt::While(block, span));
                };

                let outer = ir::ConditionalBlock::new(
                    ir::Expr::Const(ir::Const::Bool(true), span),
                    ir::Block::new([
                        prefix,
                        ir::Stmt::Branches {
                            branches: [block].into(),
                            default: Some(ir::Block::new([ir::Stmt::Break(span)])),
                            span,
                        },
                    ]),
                );
                ir::Stmt::While(outer, span)
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
                ir::Stmt::Return(Some(val.into()), span)
            }
            ast::Stmt::Return(None) => {
                self.return_type
                    .constrain(&PolyType::nullary(predef::VOID), self.symbols)
                    .with_span(span)?;
                ir::Stmt::Return(None, span)
            }
            ast::Stmt::Break => ir::Stmt::Break(span),
            ast::Stmt::Continue => ir::Stmt::Continue(span),
            ast::Stmt::Expr(expr) => {
                let (expr, _) = self.lower_expr(expr, env)?;
                expr.into()
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

            match (name, type_args, args) {
                ("Cast", [] | [_], [_]) => {
                    return self.lower_static_cast(args, type_args, hint, env, *expr_span);
                }
                // legacy syntax
                ("NameOf", [], [(ast::Expr::Ident(name), name_span)]) => {
                    self.reporter.report(Error::DeprecatedNameOf(call_span));

                    return self.lower_call(
                        expr,
                        &[(ast::Type::plain(name), *name_span)],
                        &[],
                        hint,
                        env,
                        call_span,
                    );
                }
                _ => {}
            }

            let mut candidates = env.query_free_functions(name, self.symbols).peekable();
            if candidates.peek().is_none() {
                break 'free_func;
            };

            let res = self
                .resolve_overload(
                    name, args, type_args, candidates, None, hint, env, *expr_span,
                )?
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
                let Some(&TypeRef::Name(typ)) = env.types().get(ident) else {
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
                        hint,
                        env,
                        *expr_span,
                    )?
                    .into_static_call(typ.id(), typ.args().iter().cloned());
                return Ok(res);
            }

            let (ir, typ) = self.lower_expr(expr, env)?;
            let (ref_type, unref_t) = typ.strip_ref(self.symbols).unzip();
            let upper_bound = unref_t
                .unwrap_or(typ.clone())
                .force_upper_bound(self.symbols)
                .with_span(*expr_span)?
                .ok_or(Error::InsufficientTypeInformation(*expr_span))?
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
                .query_methods_by_name(upper_bound.id(), member)
                .filter(|entry| !entry.func().flags().is_static())
                .peekable();
            if candidates.peek().is_none() {
                let (field_expr, field_t, _) =
                    self.new_field_access(ir, member, upper_bound, ref_type, *expr_span)?;
                break 'instance Some((field_expr, field_t));
            }

            let resolved = self.resolve_overload(
                member,
                args,
                type_args,
                candidates,
                Some(upper_bound.clone()),
                hint,
                env,
                *expr_span,
            )?;

            let id = resolved.resulution.function;
            self.check_visibility(
                id.parent(),
                self.symbols[id].flags().visibility(),
                call_span,
            );

            return Ok(resolved.into_instance_call(ir, upper_bound, ref_type, mode));
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
                target_t.is_subtype_compatible(
                    entry.func().type_().return_type(),
                    self.symbols,
                    false,
                )
            });
        let res = self
            .resolve_overload("Cast", args, &[], candidates, None, hint, env, expr_span)?
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
        let counter = self.locals.counter();
        let env = env.introduce_scope();
        let (block, output) = Lower::function_with(
            body,
            counter,
            types,
            env,
            return_t,
            self.context,
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
        receiver @ &(_, receiver_span): &Spanned<ast::SourceExpr<'ctx>>,
        member: &'ctx str,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        'enum_case: {
            let (ast::Expr::Ident(ident), _) = receiver else {
                break 'enum_case;
            };
            let Some(&TypeRef::Name(type_id)) = env.types().get(ident) else {
                break 'enum_case;
            };
            let TypeSchema::Enum(enum_) = self.symbols[type_id].schema() else {
                break 'enum_case;
            };
            let (field_idx, _) = enum_
                .variant_by_name(member)
                .ok_or(Error::UnresolvedMember(type_id, member, span))?;

            let ir = ir::Const::EnumVariant(FieldId::new(type_id, field_idx));
            let typ = PolyType::nullary(ir.type_id());
            return Ok((ir::Expr::Const(ir, span), typ));
        };

        let (ir, typ) = self.lower_expr(receiver, env)?;
        let (ref_type, unref_t) = typ.strip_ref(self.symbols).unzip();
        let upper_bound = unref_t
            .unwrap_or(typ)
            .force_upper_bound(self.symbols)
            .with_span(receiver_span)?
            .ok_or(Error::InsufficientTypeInformation(receiver_span))?
            .into_owned();
        let (expr, typ, _) = self.new_field_access(ir, member, upper_bound, ref_type, span)?;
        Ok((expr, typ))
    }

    fn lower_for_in(
        &mut self,
        &(name, name_span): &Spanned<&'ctx str>,
        iter @ &(_, iter_span): &Spanned<ast::SourceExpr<'ctx>>,
        body: &ast::SourceBlock<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ir::Block<'ctx>> {
        let init_span = Span::new(span.start, span.start, span.file);
        let init_end_span = Span::new(iter_span.end, iter_span.end, iter_span.file);

        let (array, array_t) = self.lower_expr(iter, env)?;
        let elem_t = PolyType::fresh();
        let expected_t = Type::app(predef::ARRAY, [elem_t.clone()]).into_poly();
        array_t
            .constrain(&expected_t, self.symbols)
            .with_span(init_span)?;

        let mut env = env.introduce_scope();
        let counter_t = PolyType::nullary(predef::INT32);
        let counter = self.locals.add_var(counter_t.clone(), init_span).id;
        let array_local = self.extract_local(array, &array_t, init_span);
        let elem = env.define_local(name, self.locals.add_var(elem_t, name_span).clone());

        let loop_body = {
            let (increment, _) = self.new_free_function_call(
                ast::BinOp::AssignAdd,
                [
                    (ir::Expr::Local(counter, init_end_span), counter_t.clone()),
                    (
                        ir::Expr::Const(ir::Const::I32(1), init_end_span),
                        counter_t.clone(),
                    ),
                ],
                &[],
                &env,
                init_end_span,
            )?;
            let prologue = [
                ir::Expr::Assign {
                    place: ir::Expr::Local(elem, name_span).into(),
                    expr: ir::Expr::Index {
                        array_type: array_t.into(),
                        array: ir::Expr::Local(array_local, iter_span).into(),
                        index: ir::Expr::Local(counter, init_end_span).into(),
                        span: iter_span,
                    }
                    .into(),
                    span: name_span.merge(&iter_span),
                }
                .into(),
                ir::Expr::call(increment, init_end_span).into(),
            ];

            let mut body = self.lower_block(&body.stmts, &mut env);
            body.push_prologue(prologue);
            body
        };

        let (array_size, array_size_t) = self.new_free_function_call(
            ir::Intrinsic::ArraySize,
            [(ir::Expr::Local(array_local, iter_span), expected_t.clone())],
            &[],
            &env,
            iter_span,
        )?;
        let (check, _) = self.new_free_function_call(
            ast::BinOp::Lt,
            [
                (ir::Expr::Local(counter, init_span), counter_t.clone()),
                (ir::Expr::call(array_size, init_span), array_size_t),
            ],
            &[],
            &env,
            init_span,
        )?;

        Ok(ir::Block::new([
            ir::Expr::Assign {
                place: ir::Expr::Local(counter, init_span).into(),
                expr: ir::Expr::Const(ir::Const::I32(0), init_span).into(),
                span: init_span,
            }
            .into(),
            ir::Stmt::While(
                ir::ConditionalBlock::new(ir::Expr::call(check, init_span), loop_body),
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
            ) if id == predef::RES_REF => {
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
        return_hint: Option<&PolyType<'ctx>>,
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

        let type_args = type_args
            .iter()
            .map(|(typ, span)| self.resolve_type(typ, env, *span))
            .collect::<Result<Vec<_>, _>>()?;

        if filtered.peek().is_none() {
            let type_env = receiver
                .and_then(|typ| typ.base_type_env(primary.key().parent()?, self.symbols))
                .unwrap_or_default();

            let type_env = type_env.push_scope(
                self.new_function_env(primary.func().type_(), &type_args, span)?
                    .pop_scope(),
            );

            let return_t =
                PolyType::from_type_with_env(primary.func().type_().return_type(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, span))?;
            if let Some(hint) = return_hint {
                return_t.constrain(hint, self.symbols).ok();
            }

            let mut checked_args = Vec::with_capacity(args.len());
            for arg in args.zip(primary.func().type_().params()) {
                let (arg @ (_, span), param) = arg;
                let expected = PolyType::from_type_with_env(param.type_(), &type_env)
                    .map_err(|var| Error::UnresolvedVar(var, *span))?;
                let res = self.lower_expr_with(arg, Some(&expected), env);
                let Some((mut expr, typ)) = self.reporter.unwrap_err(res) else {
                    continue;
                };
                let res = self.new_arg(&mut expr, typ.clone(), expected, param, env, *span);
                self.reporter.unwrap_err(res);
                checked_args.push(expr);
            }

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
                    &type_args,
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
        type_args: &[PolyType<'ctx>],
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
        fn match_by_subtype<'a, 'ctx, It, Key, Name, Func>(
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
                        typ.unwrap_ref_or_self(symbols).is_subtype_compatible(
                            param_t,
                            symbols,
                            entry.func().intrinsic().is_none(),
                        )
                    })
            })
        }

        let mut matches = match_by_subtype(candidates, &arg_types, self.symbols).peekable();
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
            let mut matches = match_by_subtype(candidates, &arg_types, self.symbols).peekable();

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
            self.new_function_env(selected.func().type_(), type_args, span)?
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
            self.new_arg(&mut *arg, typ.clone(), expected, param, env, span)?;
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

    fn resolve_field(
        &mut self,
        member: &'ctx str,
        receiver_t: InferredTypeApp<'ctx>,
        span: Span,
    ) -> Result<(FieldId<'ctx>, PolyType<'ctx>, InferredTypeApp<'ctx>), Error<'ctx>> {
        let (target_id, (field_idx, field)) = self
            .symbols
            .base_iter(receiver_t.id())
            .find_map(|(id, typ)| {
                Some((id, typ.schema().as_aggregate()?.fields().by_name(member)?))
            })
            .ok_or_else(|| Error::UnresolvedMember(receiver_t.id(), member, span))?;

        self.check_visibility(receiver_t.id(), field.flags().visibility(), span);

        let this_t = receiver_t
            .instantiate_as(target_id, self.symbols)
            .expect("should instantiate as base type");
        let env = this_t.type_env(self.symbols);
        let typ = PolyType::from_type_with_env(field.type_(), &env)
            .map_err(|var| Error::UnresolvedVar(var, span))?;
        Ok((FieldId::new(target_id, field_idx), typ, this_t))
    }

    fn resolve_local(
        &mut self,
        name: &'ctx str,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>)> {
        let Some((loc, depth)) = env.locals().get_with_depth(name) else {
            return Err(Error::UnresolvedVar(name, span));
        };

        let ir = if depth > 0
            && env.locals().scope_iter().count() - usize::from(depth) < self.locals.depth()
        {
            self.captures.insert(Capture::new(loc.id, depth));
            ir::Expr::Capture(loc.id, span)
        } else {
            ir::Expr::Local(loc.id, span)
        };
        Ok((ir, loc.typ.clone()))
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
            Some(Coercion::FromRef(RefType::Weak)) => ir::Intrinsic::WeakRefToRef,
            Some(Coercion::IntoRef(RefType::Weak)) => ir::Intrinsic::RefToWeakRef,
            Some(Coercion::FromRef(RefType::Script)) => ir::Intrinsic::Deref,
            Some(Coercion::IntoRef(RefType::Script)) => ir::Intrinsic::AsRef,
            Some(Coercion::IntoVariant) => ir::Intrinsic::ToVariant,
            Some(Coercion::FromRef(RefType::Strong) | Coercion::IntoRef(RefType::Strong))
            | None => return Ok(()),
        };
        let arg = mem::replace(expr, ir::Expr::null(span));
        let (call, _) = self.new_free_function_call(name, [(arg, lhs)], &[], env, span)?;
        *expr = ir::Expr::call(call, span);
        Ok(())
    }

    fn check_visibility(&mut self, parent: TypeId<'ctx>, visibility: Visibility, span: Span) {
        match visibility {
            Visibility::Private if self.context != Some(parent) => {
                self.reporter.report(Error::PrivateMemberAccess(span));
            }
            Visibility::Protected
                if self.context.is_none_or(|context| {
                    !self.symbols.base_iter(context).any(|(id, _)| id == parent)
                }) =>
            {
                self.reporter.report(Error::ProtectedMemberAccess(span));
            }
            _ => {}
        }
    }

    fn new_arg(
        &mut self,
        expr: &mut ir::Expr<'ctx>,
        lhs: PolyType<'ctx>,
        rhs: PolyType<'ctx>,
        param: &Param<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, ()> {
        if param.flags().is_out() {
            rhs.constrain(&lhs, self.symbols).with_span(span)?;
            if expr.is_prvalue(self.symbols) {
                self.reporter.report(Error::InvalidTemporary(span));
            }
        }
        self.coerce(expr, lhs, rhs, env, span)?;
        Ok(())
    }

    fn new_free_function_call<'name: 'ctx>(
        &mut self,
        name: impl Into<&'name str>,
        args: impl IntoIterator<Item = (ir::Expr<'ctx>, PolyType<'ctx>)>,
        type_args: &[PolyType<'ctx>],
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Call<'ctx>, PolyType<'ctx>)> {
        let name = name.into();
        let (mut args, arg_types): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let candidates = env.query_free_functions(name, self.symbols);
        let res = self
            .resolve_overload_typed(
                name, &mut args, arg_types, type_args, candidates, None, None, env, span,
            )?
            .with_args(args)
            .into_call();
        Ok(res)
    }

    fn new_field_access(
        &mut self,
        ir: ir::Expr<'ctx>,
        member: &'ctx str,
        upper_t: InferredTypeApp<'ctx>,
        ref_t: Option<RefType>,
        span: Span,
    ) -> LowerResult<'ctx, (ir::Expr<'ctx>, PolyType<'ctx>, FieldId<'ctx>)> {
        let (field, field_t, this_t) = self.resolve_field(member, upper_t, span)?;

        if self.symbols[field.parent()]
            .schema()
            .as_aggregate()
            .is_some_and(|agg| agg.flags().is_struct())
            && ir.is_prvalue(self.symbols)
        {
            self.reporter.report(Error::InvalidTemporary(span));
        }

        let ir = ir::Expr::Field {
            receiver: ir.into(),
            receiver_type: this_t.into(),
            receiver_ref: ref_t,
            field,
            span,
        };
        Ok((ir, field_t, field))
    }

    fn new_runtime_type_check(
        &mut self,
        receiver: ir::Expr<'ctx>,
        receiver_t: &PolyType<'ctx>,
        ref_type: Option<RefType>,
        env: &mut Env<'_, 'ctx>,
        span: Span,
    ) -> Result<ir::Call<'ctx>, Error<'ctx>> {
        let upper_bound = receiver_t
            .force_upper_bound(self.symbols)
            .with_span(span)?
            .ok_or(Error::InsufficientTypeInformation(span))?
            .into_owned();
        receiver_t
            .constrain(&PolyType::nullary(predef::ISCRIPTABLE), self.symbols)
            .with_span(span)?;

        let (name_of, name_t) = self.new_free_function_call(
            ir::Intrinsic::NameOf,
            [],
            slice::from_ref(receiver_t),
            env,
            span,
        )?;
        let candidates = self
            .symbols
            .query_methods_by_name(upper_bound.id(), "IsA")
            .filter(|entry| !entry.func().flags().is_static());
        let mut args = [ir::Expr::call(name_of, span)];
        let (call, _) = self
            .resolve_overload_typed(
                "IsA",
                &mut args,
                vec![name_t],
                &[],
                candidates,
                None,
                Some(upper_bound.clone()),
                env,
                span,
            )?
            .with_args(args)
            .into_instance_call(receiver, upper_bound, ref_type, ir::CallMode::Normal);
        Ok(call)
    }

    fn new_function_env(
        &self,
        func_t: &FunctionType<'ctx>,
        type_args: &[PolyType<'ctx>],
        span: Span,
    ) -> LowerResult<'ctx, ScopedMap<'scope, &'ctx str, PolyType<'ctx>>> {
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

        for ((_, var), arg) in map.top().iter().zip(type_args) {
            arg.constrain(var, self.symbols).with_span(span)?;
        }

        Ok(map)
    }

    fn extract_local(
        &mut self,
        expr: ir::Expr<'ctx>,
        expr_t: &PolyType<'ctx>,
        span: Span,
    ) -> ir::Local {
        if let ir::Expr::Local(local, _) = expr {
            local
        } else {
            let local = self.locals.add_var(expr_t.clone(), span).id;
            self.push_prefix(ir::Expr::Assign {
                place: ir::Expr::Local(local, span).into(),
                expr: expr.into(),
                span,
            });
            local
        }
    }

    fn resolve_type(
        &self,
        typ: &ast::SourceType<'ctx>,
        env: &Env<'_, 'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, PolyType<'ctx>> {
        let typ = PolyType::from_type(&env.types().resolve(typ, self.symbols, span)?);
        self.check_type(&typ, span)?;
        Ok(typ)
    }

    fn check_type(&self, typ: &PolyType<'ctx>, span: Span) -> LowerResult<'ctx, ()> {
        match typ {
            PolyType::Mono(Type::Data(app)) => self.check_type_app(app, span),
            _ => Ok(()),
        }
    }

    fn check_type_app(&self, typ: &InferredTypeApp<'ctx>, span: Span) -> LowerResult<'ctx, ()> {
        let sym = &self.symbols[typ.id()];
        if sym.params().len() != typ.args().len() {
            return Err(Error::InvalidTypeArgCount(sym.params().len(), span));
        }
        typ.args()
            .iter()
            .try_for_each(|arg| self.check_type(arg, span))
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

struct Pattern<'ctx> {
    conditions: Vec<ir::Expr<'ctx>>,
    prologue: Vec<ir::Stmt<'ctx>>,
}

impl<'ctx> Pattern<'ctx> {
    pub fn from_conditions(conditions: impl IntoIterator<Item = ir::Expr<'ctx>>) -> Self {
        Self {
            conditions: conditions.into_iter().collect(),
            prologue: Vec::new(),
        }
    }

    pub fn merge(&mut self, other: Pattern<'ctx>) {
        self.conditions.extend(other.conditions);
        self.prologue.extend(other.prologue);
    }
}

impl<'ctx> Extend<Pattern<'ctx>> for Pattern<'ctx> {
    fn extend<T: IntoIterator<Item = Pattern<'ctx>>>(&mut self, iter: T) {
        for pattern in iter {
            self.merge(pattern);
        }
    }
}

#[derive(Clone)]
enum Projection<'scope, 'ctx> {
    Field {
        receiver: &'scope Self,
        receiver_type: InferredTypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
    },
    Index {
        array: &'scope Self,
        array_type: PolyType<'ctx>,
        index: i32,
    },
    Scrutinee(ir::Local),
}
