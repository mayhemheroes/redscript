use std::mem;
use std::ops::Not;
use std::rc::Rc;

use identity_hash::BuildIdentityHasher;
use redscript_ast::{self as ast, FileId, Span, Spanned};
use smallvec::smallvec;

use super::resolution::{Scope, THIS_IDENT, WRAPPED_METHOD_IDENT};
use crate::diagnostic::pass::DiagnosticPass;
use crate::lower::{Env, Lower};
use crate::symbols::FreeFunctionIndexes;
use crate::{
    CompileErrorReporter, FieldId, FieldIndex, FreeFunction, FreeFunctionIndex, FunctionIndex,
    FunctionType, IndexMap, IndexSet, MethodId, PolyType, Symbols, Type, TypeId, TypeRef,
    TypeScope, ir,
};

#[derive(Debug)]
pub struct TypeInference<'scope, 'ctx> {
    modules: Vec<InferStageModule<'scope, 'ctx>>,
    symbols: Symbols<'ctx>,
}

impl<'scope, 'ctx> TypeInference<'scope, 'ctx> {
    pub fn new(modules: Vec<InferStageModule<'scope, 'ctx>>, symbols: Symbols<'ctx>) -> Self {
        Self { symbols, modules }
    }

    pub fn finish(
        mut self,
        scope: &'scope Scope<'_, 'ctx>,
        reporter: &mut CompileErrorReporter<'ctx>,
    ) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
        let mut compiled = LoweredCompilationUnit::default();

        for mod_ in mem::take(&mut self.modules) {
            let scope = scope.push(mod_.type_scope, mod_.func_scope);

            for class in mod_.classes {
                let types = scope.types.push_scope(class.scope);
                let class_t = &self.symbols[class.id];

                let class_type_args = class_t
                    .params()
                    .iter()
                    .map(|param| Type::Ctx(param.clone()))
                    .collect::<Rc<_>>();
                let this_t = PolyType::from_type(&Type::app(class.id, class_type_args));

                let mut methods = IndexMap::default();
                for item in class.methods {
                    let id = MethodId::new(class.id, item.id);
                    let method = &self.symbols[id];
                    let func = lower_function(
                        method.type_(),
                        &item.params_names,
                        &item.body,
                        Env::new(&types.push_scope(item.scope), &scope.funcs),
                        method.flags().is_static().not().then(|| this_t.clone()),
                        Some(class.id),
                        &self.symbols,
                        reporter,
                        item.span,
                    );
                    methods.insert(item.id, func);
                }

                let mut fields = IndexMap::default();
                for item in class.fields {
                    let Some(default) = item.default else {
                        continue;
                    };
                    let id = FieldId::new(class.id, item.id);
                    let env = Env::new(&types, &scope.funcs);
                    if let Some(expr) = lower_constant(id, &default, &env, &self.symbols, reporter)
                    {
                        fields.insert(item.id, expr);
                    }
                }

                compiled.classes.insert(
                    class.id,
                    LoweredClass {
                        methods,
                        fields,
                        span: class.span,
                    },
                );
            }

            for enum_id in mod_.enums {
                compiled.enums.insert(enum_id);
            }

            for func in mod_.functions {
                match func {
                    FuncItemKind::FreeFunction(func) => {
                        let value = lower_function(
                            self.symbols[func.id].type_(),
                            &func.params_names,
                            &func.body,
                            Env::new(&scope.types.push_scope(func.scope), &scope.funcs),
                            None,
                            None,
                            &self.symbols,
                            reporter,
                            func.span,
                        );
                        compiled.functions.insert(func.id, value);
                    }
                    FuncItemKind::ReplaceMethod(func) => {
                        let sym = &self.symbols[func.id];
                        let lowered = lower_function(
                            sym.type_(),
                            &func.params_names,
                            &func.body,
                            Env::new(&scope.types.push_scope(func.scope), &scope.funcs),
                            sym.flags()
                                .is_static()
                                .not()
                                .then(|| PolyType::nullary(func.id.parent())),
                            Some(func.id.parent()),
                            &self.symbols,
                            reporter,
                            func.span,
                        );
                        compiled.method_replacements.insert(func.id, lowered);
                    }
                    FuncItemKind::AddMethod(func) => {
                        let lowered = func.body.as_ref().map(|body| {
                            let sym = &self.symbols[func.id];
                            let this = sym
                                .flags()
                                .is_static()
                                .not()
                                .then(|| PolyType::nullary(func.id.parent()));
                            lower_function(
                                sym.type_(),
                                &func.params_names,
                                body,
                                Env::new(&scope.types.push_scope(func.scope), &scope.funcs),
                                this,
                                Some(func.id.parent()),
                                &self.symbols,
                                reporter,
                                func.span,
                            )
                        });
                        compiled.added_methods.insert(func.id, lowered);
                    }
                    FuncItemKind::WrapMethod(func) => {
                        let typ = self.symbols[func.id].type_();
                        let free_func =
                            FreeFunction::new_alias(typ.clone(), func.id, Some(func.name_span));
                        let free_func = self
                            .symbols
                            .add_free_function(WRAPPED_METHOD_IDENT, free_func);
                        let mut funcs = scope.funcs.introduce_scope();
                        funcs.insert(WRAPPED_METHOD_IDENT, smallvec![free_func]);

                        let sym = &self.symbols[func.id];
                        let lowered = lower_function(
                            sym.type_(),
                            &func.params_names,
                            &func.body,
                            Env::new(&scope.types.push_scope(func.scope), &funcs),
                            sym.flags()
                                .is_static()
                                .not()
                                .then(|| PolyType::nullary(func.id.parent())),
                            Some(func.id.parent()),
                            &self.symbols,
                            reporter,
                            func.span,
                        );
                        compiled
                            .method_wrappers
                            .entry(func.id)
                            .or_default()
                            .push(lowered);
                    }
                }
            }

            for item in mod_.fields {
                let expr = item
                    .default
                    .and_then(|default| {
                        let env = Env::new(&scope.types, &scope.funcs);
                        lower_constant(item.id, &default, &env, &self.symbols, reporter)
                    })
                    .map(Box::new);

                compiled.added_fields.insert(item.id, expr);
            }

            if let Some(span) = mod_.span {
                let scope = scope
                    .types
                    .pop_scope()
                    .into_iter()
                    .filter_map(|(k, v)| Some((k, v.force()?)))
                    .collect();
                compiled.scopes.insert(span.file, scope);
            }
        }

        (compiled, self.symbols)
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_function<'ctx>(
    func_type: &FunctionType<'ctx>,
    param_names: &[&'ctx str],
    body: &ast::SourceFunctionBody<'ctx>,
    mut env: Env<'_, 'ctx>,
    this: Option<PolyType<'ctx>>,
    context: Option<TypeId<'ctx>>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    span: Span,
) -> LoweredFunction<'ctx> {
    let this = this.map(|lt| ir::LocalInfo::new(ir::Local::This, lt, None));
    if let Some(this) = &this {
        env.define_local(THIS_IDENT, this.clone());
    }

    let params = func_type
        .params()
        .iter()
        .zip(param_names)
        .map(|(param, &name)| (name, PolyType::from_type(param.type_())));
    let return_t = PolyType::from_type(func_type.return_type());

    let (block, output, errors) = Lower::function(body, params, env, return_t, context, symbols);
    reporter.report_many(errors);

    let locals = this.iter().chain(output.locals()).cloned().collect();

    LoweredFunction {
        block,
        locals,
        span,
    }
}

fn lower_constant<'ctx>(
    id: FieldId<'ctx>,
    expr: &Spanned<ast::SourceExpr<'ctx>>,
    env: &Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Option<ir::Const<'ctx>> {
    let field = &symbols[id];
    let (expr, errors) = Lower::constant(expr, env, PolyType::from_type(field.type_()), symbols);
    reporter.report_many(errors);
    expr
}

#[derive(Debug)]
pub struct InferStageModule<'scope, 'ctx> {
    type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
    func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,

    classes: Vec<ClassItem<'scope, 'ctx>>,
    enums: Vec<TypeId<'ctx>>,
    functions: Vec<FuncItemKind<'scope, 'ctx>>,
    fields: Vec<FieldItem<'ctx, FieldId<'ctx>>>,
    span: Option<Span>,
}

impl<'scope, 'ctx> InferStageModule<'scope, 'ctx> {
    #[inline]
    pub fn new(
        type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
        func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,
        classes: Vec<ClassItem<'scope, 'ctx>>,
        enums: Vec<TypeId<'ctx>>,
        functions: Vec<FuncItemKind<'scope, 'ctx>>,
        fields: Vec<FieldItem<'ctx, FieldId<'ctx>>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            type_scope,
            func_scope,
            functions,
            classes,
            enums,
            fields,
            span,
        }
    }

    #[inline]
    pub fn classes(&self) -> &[ClassItem<'scope, 'ctx>] {
        &self.classes
    }

    #[inline]
    pub fn functions(&self) -> &[FuncItemKind<'scope, 'ctx>] {
        &self.functions
    }

    #[inline]
    pub fn fields(&self) -> &[FieldItem<'ctx, FieldId<'ctx>>] {
        &self.fields
    }
}

#[derive(Debug)]
pub struct FieldItem<'ctx, K> {
    id: K,
    default: Option<Box<Spanned<ast::SourceExpr<'ctx>>>>,
}

impl<'ctx, K> FieldItem<'ctx, K> {
    #[inline]
    pub fn new(id: K, default: Option<Box<Spanned<ast::SourceExpr<'ctx>>>>) -> Self {
        Self { id, default }
    }

    #[inline]
    pub fn id(&self) -> &K {
        &self.id
    }
}

#[derive(Debug)]
pub struct FuncItem<'scope, 'ctx, K, B = ast::SourceFunctionBody<'ctx>> {
    id: K,
    span: Span,
    name_span: Span,
    params_names: Box<[&'ctx str]>,
    body: B,
    scope: TypeScope<'scope, 'ctx>,
}

impl<'scope, 'ctx, K, B> FuncItem<'scope, 'ctx, K, B> {
    #[inline]
    pub fn new(
        id: K,
        span: Span,
        name_span: Span,
        params_names: impl Into<Box<[&'ctx str]>>,
        body: B,
        scope: TypeScope<'scope, 'ctx>,
    ) -> Self {
        Self {
            id,
            span,
            name_span,
            params_names: params_names.into(),
            body,
            scope,
        }
    }

    #[inline]
    pub fn id(&self) -> &K {
        &self.id
    }
}

#[derive(Debug)]
pub struct ClassItem<'scope, 'ctx> {
    id: TypeId<'ctx>,
    span: Span,
    name_span: Span,
    scope: TypeScope<'scope, 'ctx>,
    methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
    fields: Vec<FieldItem<'ctx, FieldIndex>>,
}

impl<'scope, 'ctx> ClassItem<'scope, 'ctx> {
    #[inline]
    pub fn new(
        id: TypeId<'ctx>,
        span: Span,
        name_span: Span,
        scope: TypeScope<'scope, 'ctx>,
        methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
        fields: Vec<FieldItem<'ctx, FieldIndex>>,
    ) -> Self {
        Self {
            id,
            span,
            name_span,
            scope,
            methods,
            fields,
        }
    }

    #[inline]
    pub fn id(&self) -> TypeId<'ctx> {
        self.id
    }

    #[inline]
    pub fn name_span(&self) -> Span {
        self.name_span
    }
}

#[derive(Debug)]
pub enum FuncItemKind<'scope, 'ctx> {
    FreeFunction(FuncItem<'scope, 'ctx, FreeFunctionIndex>),
    ReplaceMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>>),
    WrapMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>>),
    AddMethod(FuncItem<'scope, 'ctx, MethodId<'ctx>, Option<ast::SourceFunctionBody<'ctx>>>),
}

#[derive(Debug)]
pub struct LoweredFunction<'ctx> {
    pub block: ir::Block<'ctx>,
    pub locals: Box<[ir::LocalInfo<'ctx>]>,
    pub span: Span,
}

impl<'ctx> LoweredFunction<'ctx> {
    pub fn find_local(&self, loc: ir::Local) -> Option<&PolyType<'ctx>> {
        self.locals
            .binary_search_by_key(&loc, |l| l.id)
            .ok()
            .map(|idx| &self.locals[idx].typ)
    }
}

#[derive(Debug, Default)]
pub struct LoweredCompilationUnit<'ctx> {
    pub classes: IndexMap<TypeId<'ctx>, LoweredClass<'ctx>, BuildIdentityHasher<usize>>,
    pub enums: IndexSet<TypeId<'ctx>, BuildIdentityHasher<usize>>,
    pub functions: IndexMap<FreeFunctionIndex, LoweredFunction<'ctx>>,

    pub added_fields: IndexMap<FieldId<'ctx>, Option<Box<ir::Const<'ctx>>>>,
    pub added_methods: IndexMap<MethodId<'ctx>, Option<LoweredFunction<'ctx>>>,
    pub method_replacements: IndexMap<MethodId<'ctx>, LoweredFunction<'ctx>>,
    pub method_wrappers: IndexMap<MethodId<'ctx>, Vec<LoweredFunction<'ctx>>>,

    pub scopes: IndexMap<FileId, IndexMap<&'ctx str, TypeRef<'static, 'ctx>>>,
}

impl<'ctx> LoweredCompilationUnit<'ctx> {
    pub fn all_functions(&self) -> impl Iterator<Item = &LoweredFunction<'ctx>> {
        self.classes
            .values()
            .flat_map(|class| class.methods.values())
            .chain(self.functions.values())
            .chain(self.added_methods.values().flatten())
            .chain(self.method_replacements.values())
            .chain(self.method_wrappers.values().flatten())
    }

    pub fn run_diagnostics(
        &self,
        passes: &[&'static dyn DiagnosticPass],
        reporter: &mut CompileErrorReporter<'ctx>,
    ) {
        if passes.is_empty() {
            return;
        }

        for func in self.all_functions() {
            for pass in passes {
                pass.run(func, reporter);
            }
        }
    }
}

#[derive(Debug)]
pub struct LoweredClass<'ctx> {
    pub methods: IndexMap<FunctionIndex, LoweredFunction<'ctx>>,
    pub fields: IndexMap<FieldIndex, ir::Const<'ctx>>,
    pub span: Span,
}
