use std::vec;

use redscript::ast::{BinOp, Constant, Expr, Ident, Intrinsic, Literal, Seq, Span, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Definition, Local, LocalFlags};
use redscript::Ref;

use crate::error::{Cause, Error, ResultSpan};
use crate::scope::{Reference, Scope, TypeId, Value};
use crate::symbol::{FunctionSignature, FunctionSignatureBuilder};
use crate::transform::ExprTransformer;
use crate::typechecker::{type_of, Callable, TypedAst, TypedExpr};

pub struct Desugar<'a> {
    pool: &'a mut ConstantPool,
    scope: &'a mut Scope,
    name_count: usize,
    prefix_exprs: Vec<TypedExpr>,
    locals: Vec<PoolIndex<Local>>,
}

impl<'a> Desugar<'a> {
    pub fn new(scope: &'a mut Scope, pool: &'a mut ConstantPool) -> Self {
        Desugar {
            pool,
            scope,
            prefix_exprs: vec![],
            locals: vec![],
            name_count: 0,
        }
    }

    pub fn locals(self) -> Vec<PoolIndex<Local>> {
        self.locals
    }

    fn add_prefix(&mut self, expr: TypedExpr) {
        self.prefix_exprs.push(expr);
    }

    fn get_function(&self, signature: FunctionSignature<'_>) -> Result<Callable, Cause> {
        let fun_idx = self
            .scope
            .resolve_function(Ident::from_ref(signature.name()))?
            .by_id(&signature, self.pool)
            .ok_or_else(|| Cause::FunctionNotFound(Ident::from_ref(signature)))?;

        Ok(Callable::Function(fun_idx))
    }

    fn fresh_local(&mut self, type_: &TypeId) -> Result<Reference, Cause> {
        let fun_idx = self.scope.function.unwrap();
        let name_idx = self.pool.names.add(Ref::from(format!("synthetic${}", self.name_count)));
        let type_idx = self.scope.get_type_index(type_, self.pool)?;
        let local = Local::new(type_idx, LocalFlags::new());
        let def = Definition::local(name_idx, fun_idx, local);
        let idx = self.pool.add_definition(def);
        self.locals.push(idx);
        self.name_count += 1;
        Ok(Reference::Value(Value::Local(idx)))
    }
}

impl ExprTransformer<TypedAst> for Desugar<'_> {
    fn on_array_lit(
        &mut self,
        exprs: Box<[TypedExpr]>,
        type_: Option<Box<TypeId>>,
        pos: Span,
    ) -> Result<TypedExpr, Error> {
        let type_ = TypeId::Array(type_.unwrap());
        let local = self.fresh_local(&type_).with_span(pos)?;
        let array_ref = Expr::Ident(local.clone(), pos);

        self.add_prefix(Expr::Call(
            Callable::Intrinsic(Intrinsic::ArrayResize, TypeId::Void),
            [].into(),
            [array_ref, Expr::Constant(Constant::U64(exprs.len() as u64), pos)].into(),
            pos,
        ));

        for (i, expr) in exprs.into_vec().into_iter().enumerate() {
            let expr = self.on_expr(expr)?;
            let array_ref = Expr::Ident(local.clone(), pos);
            let index = Expr::Constant(Constant::U64(i as u64), pos);
            self.add_prefix(Expr::Assign(
                Expr::ArrayElem(array_ref.into(), index.into(), pos).into(),
                expr.into(),
                pos,
            ));
        }

        Ok(Expr::Ident(local, pos))
    }

    fn on_interpolated_string(
        &mut self,
        prefix: Ref<str>,
        parts: Vec<(TypedExpr, Ref<str>)>,
        span: Span,
    ) -> Result<TypedExpr, Error> {
        let mut acc = Expr::Constant(Constant::String(Literal::String, prefix), span);
        let str_type = self.scope.resolve_type(&TypeName::STRING, self.pool).with_span(span)?;

        let add_str = FunctionSignatureBuilder::new(BinOp::Add.to_string())
            .parameter(&TypeName::basic("Script_RefString"), false)
            .parameter(&TypeName::basic("Script_RefString"), false)
            .return_type(&TypeName::STRING);
        let add_str = self.get_function(add_str).with_span(span)?;

        let as_ref = Callable::Intrinsic(Intrinsic::AsRef, TypeId::ScriptRef(Box::new(str_type.clone())));
        let as_ref = |exp: TypedExpr| Expr::Call(as_ref.clone(), [].into(), [exp].into(), span);

        for (part, str) in parts {
            let part = self.on_expr(part)?;

            let part = match type_of(&part, self.scope, self.pool)? {
                TypeId::Void => return Err(Cause::UnsupportedFeature("formatting void").with_span(span)),
                TypeId::ScriptRef(idx) if idx.pretty(self.pool)? == TypeName::STRING.name() => part,
                typ if typ.pretty(self.pool)? == TypeName::STRING.name() => as_ref(part),
                _ => {
                    let to_string = Callable::Intrinsic(Intrinsic::ToString, str_type.clone());
                    as_ref(Expr::Call(to_string, [].into(), [part].into(), span))
                }
            };

            let combined = if str.is_empty() {
                part
            } else {
                let str: TypedExpr = as_ref(Expr::Constant(Constant::String(Literal::String, str), span));
                as_ref(Expr::Call(add_str.clone(), [].into(), [part, str].into(), span))
            };

            acc = Expr::Call(add_str.clone(), [].into(), [as_ref(acc), combined].into(), span);
        }
        Ok(acc)
    }

    fn on_for_in(
        &mut self,
        name: PoolIndex<Local>,
        array: TypedExpr,
        seq: Seq<TypedAst>,
        span: Span,
    ) -> Result<TypedExpr, Error> {
        let mut seq = self.on_seq(seq)?;

        let array = self.on_expr(array)?;
        let arr_type = type_of(&array, self.scope, self.pool)?;
        let arr_local = self.fresh_local(&arr_type).with_span(span)?;

        let counter_type = self.scope.resolve_type(&TypeName::INT32, self.pool).with_span(span)?;
        let counter_local = self.fresh_local(&counter_type).with_span(span)?;

        self.add_prefix(Expr::Assign(
            Box::new(Expr::Ident(arr_local.clone(), span)),
            Box::new(array),
            span,
        ));
        self.add_prefix(Expr::Assign(
            Box::new(Expr::Ident(counter_local.clone(), span)),
            Box::new(Expr::Constant(Constant::I32(0), span)),
            span,
        ));

        let array_size = Callable::Intrinsic(Intrinsic::ArraySize, counter_type);
        let assign_add = FunctionSignatureBuilder::new(BinOp::AssignAdd.to_string())
            .parameter(&TypeName::INT32, true)
            .parameter(&TypeName::INT32, false)
            .return_type(&TypeName::INT32);
        let assign_add = self.get_function(assign_add).with_span(span)?;

        let less_than = FunctionSignatureBuilder::new(BinOp::Less.to_string())
            .parameter(&TypeName::INT32, false)
            .parameter(&TypeName::INT32, false)
            .return_type(&TypeName::BOOL);
        let less_than = self.get_function(less_than).with_span(span)?;

        let condition = Expr::Call(
            less_than,
            [].into(),
            [
                Expr::Ident(counter_local.clone(), span),
                Expr::Call(
                    array_size,
                    [].into(),
                    [Expr::Ident(arr_local.clone(), span)].into(),
                    span,
                ),
            ]
            .into(),
            span,
        );
        let assign_iter_value = Expr::Assign(
            Box::new(Expr::Ident(Reference::Value(Value::Local(name)), span)),
            Box::new(Expr::ArrayElem(
                Box::new(Expr::Ident(arr_local, span)),
                Box::new(Expr::Ident(counter_local.clone(), span)),
                span,
            )),
            span,
        );
        let increment_counter = Expr::Call(
            assign_add,
            [].into(),
            [Expr::Ident(counter_local, span), Expr::Constant(Constant::I32(1), span)].into(),
            span,
        );

        let mut body = vec![assign_iter_value];
        body.append(&mut seq.exprs);
        body.push(increment_counter);

        Ok(Expr::While(Box::new(condition), Seq::new(body), span))
    }

    fn on_seq(&mut self, seq: Seq<TypedAst>) -> Result<Seq<TypedAst>, Error> {
        let mut processed = Vec::with_capacity(seq.exprs.len());
        for expr in seq.exprs {
            let done = self.on_expr(expr)?;
            if !self.prefix_exprs.is_empty() {
                processed.append(&mut self.prefix_exprs);
            }
            processed.push(done);
        }
        Ok(Seq::new(processed))
    }
}
