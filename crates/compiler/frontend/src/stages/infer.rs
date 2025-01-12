use std::mem;
use std::rc::Rc;

use identity_hash::BuildIdentityHasher;
use redscript_ast::{self as ast, Span};
use smallvec::smallvec;

use super::resolution::{Scope, THIS_IDENT, WRAPPED_METHOD_IDENT};
use crate::lower::{Env, FreeFunctionIndexes, Lower};
use crate::{
    ir, CompileErrorReporter, Diagnostic, FieldId, FreeFunction, FreeFunctionIndex, FunctionIndex,
    FunctionType, IndexMap, IndexSet, MethodId, PolyType, Symbols, Type, TypeId, TypeRef,
    TypeScope,
};

#[derive(Debug)]
pub struct TypeInference<'scope, 'ctx> {
    modules: Vec<InferStageModule<'scope, 'ctx>>,
    symbols: Symbols<'ctx>,
    reporter: CompileErrorReporter<'ctx>,
}

impl<'scope, 'ctx> TypeInference<'scope, 'ctx> {
    pub fn new(
        modules: Vec<InferStageModule<'scope, 'ctx>>,
        symbols: Symbols<'ctx>,
        reporter: CompileErrorReporter<'ctx>,
    ) -> Self {
        Self {
            symbols,
            reporter,
            modules,
        }
    }

    pub fn finish(
        mut self,
        scope: &'scope Scope<'_, 'ctx>,
    ) -> (
        LoweredCompilationUnit<'ctx>,
        Symbols<'ctx>,
        Vec<Diagnostic<'ctx>>,
    ) {
        let mut compiled = LoweredCompilationUnit::default();

        for mod_ in mem::take(&mut self.modules) {
            let scope = scope.push(mod_.type_scope, mod_.func_scope);

            for class in mod_.classes {
                let types = scope.types.push_scope(class.scope);
                let mut methods = IndexMap::default();
                let class_t = &self.symbols[class.id];

                let class_type_args = class_t
                    .params()
                    .iter()
                    .map(|param| Type::Ctx(param.clone()))
                    .collect::<Rc<_>>();
                let this_t = PolyType::from_type(&Type::app(class.id, class_type_args));
                let this = ir::LocalInfo::new(ir::Local::This, this_t, None);

                for item in class.methods {
                    let id = MethodId::new(class.id, item.id);
                    let method = &self.symbols[id];
                    let types = types.push_scope(item.scope);

                    let mut env = Env::new(&types, &scope.funcs);
                    if !method.flags().is_static() {
                        env.define_local(THIS_IDENT, this.clone());
                    }

                    let func = lower_func(
                        method.type_(),
                        &item.body,
                        env,
                        &self.symbols,
                        &mut self.reporter,
                    );
                    methods.insert(item.id, func);
                }

                compiled.classes.insert(class.id, LoweredClass { methods });
            }

            for enum_id in mod_.enums {
                compiled.enums.insert(enum_id);
            }

            for func in mod_.functions {
                match func {
                    FuncItemKind::FreeFunction(func) => {
                        let types = scope.types.push_scope(func.scope);
                        let env = Env::new(&types, &scope.funcs);
                        let func_t = &self.symbols[func.id].type_();
                        let value =
                            lower_func(func_t, &func.body, env, &self.symbols, &mut self.reporter);
                        compiled.functions.insert(func.id, value);
                    }
                    FuncItemKind::ReplaceMethod(func) => {
                        let types = scope.types.push_scope(func.scope);
                        let env = Env::new(&types, &scope.funcs);
                        let body = &func.body;
                        let lowered =
                            lower_method(func.id, body, env, &self.symbols, &mut self.reporter);
                        compiled.method_replacements.insert(func.id, lowered);
                    }
                    FuncItemKind::AddMethod(func) => {
                        let types = scope.types.push_scope(func.scope);
                        let env = Env::new(&types, &scope.funcs);
                        let lowered = func.body.as_ref().map(|body| {
                            lower_method(func.id, body, env, &self.symbols, &mut self.reporter)
                        });
                        compiled.added_methods.insert(func.id, lowered);
                    }
                    FuncItemKind::WrapMethod(func) => {
                        let typ = self.symbols[func.id].type_().clone();
                        let free_func = FreeFunction::new_alias(typ, func.id, Some(func.name_span));
                        let free_func = self
                            .symbols
                            .add_free_function(WRAPPED_METHOD_IDENT, free_func);

                        let types = scope.types.push_scope(func.scope);
                        let mut funcs = scope.funcs.introduce_scope();
                        funcs.insert(WRAPPED_METHOD_IDENT, smallvec![free_func]);

                        let env = Env::new(&types, &funcs);
                        let body = &func.body;
                        let lowered =
                            lower_method(func.id, body, env, &self.symbols, &mut self.reporter);
                        compiled
                            .method_wrappers
                            .entry(func.id)
                            .or_default()
                            .push(lowered);
                    }
                }
            }

            for field in mod_.fields {
                compiled.added_fields.insert(field);
            }
        }

        (compiled, self.symbols, self.reporter.into_reported())
    }
}

fn lower_method<'ctx>(
    id: MethodId<'ctx>,
    body: &ast::SourceFunctionBody<'ctx>,
    mut env: Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> LoweredFunction<'ctx> {
    let func_sym = &symbols[id];
    if !func_sym.flags().is_static() {
        let this_t = PolyType::nullary(id.parent());
        let this = ir::LocalInfo::new(ir::Local::This, this_t, None);
        env.define_local(THIS_IDENT, this.clone());
    }

    lower_func(func_sym.type_(), body, env, symbols, reporter)
}

fn lower_func<'ctx>(
    func_type: &FunctionType<'ctx>,
    body: &ast::SourceFunctionBody<'ctx>,
    env: Env<'_, 'ctx>,
    symbols: &Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> LoweredFunction<'ctx> {
    let params = func_type
        .params()
        .iter()
        .map(|param| (param.name(), PolyType::from_type(param.type_())));
    let return_t = PolyType::from_type(func_type.return_type());

    let (block, output, errors) = Lower::function(body, params, env, return_t, symbols);
    reporter.report_many(errors);

    let mut locals = vec![];
    for local in output.locals() {
        match local.typ.coalesce(symbols) {
            Ok(loc) => locals.push((local.id, loc)),
            Err(err) => {
                let span = local.span.expect("local span should be present");
                reporter.report(Diagnostic::CoalesceError(err.into(), span));
            }
        }
    }

    LoweredFunction { block, locals }
}

#[derive(Debug)]
pub struct InferStageModule<'scope, 'ctx> {
    type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
    func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,

    classes: Vec<ClassItem<'scope, 'ctx>>,
    enums: Vec<TypeId<'ctx>>,
    functions: Vec<FuncItemKind<'scope, 'ctx>>,
    fields: Vec<FieldId<'ctx>>,
}

impl<'scope, 'ctx> InferStageModule<'scope, 'ctx> {
    #[inline]
    pub fn new(
        type_scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
        func_scope: IndexMap<&'ctx str, FreeFunctionIndexes>,
        classes: Vec<ClassItem<'scope, 'ctx>>,
        enums: Vec<TypeId<'ctx>>,
        functions: Vec<FuncItemKind<'scope, 'ctx>>,
        fields: Vec<FieldId<'ctx>>,
    ) -> Self {
        Self {
            type_scope,
            func_scope,
            functions,
            classes,
            enums,
            fields,
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
    pub fn fields(&self) -> &[FieldId<'ctx>] {
        &self.fields
    }
}

#[derive(Debug)]
pub struct FuncItem<'scope, 'ctx, K, B = ast::SourceFunctionBody<'ctx>> {
    id: K,
    name_span: Span,
    body: B,
    scope: TypeScope<'scope, 'ctx>,
}

impl<'scope, 'ctx, K, B> FuncItem<'scope, 'ctx, K, B> {
    #[inline]
    pub fn new(id: K, name_span: Span, body: B, scope: TypeScope<'scope, 'ctx>) -> Self {
        Self {
            id,
            name_span,
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
    name_span: Span,
    scope: TypeScope<'scope, 'ctx>,
    methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
}

impl<'scope, 'ctx> ClassItem<'scope, 'ctx> {
    #[inline]
    pub fn new(
        id: TypeId<'ctx>,
        name_span: Span,
        scope: TypeScope<'scope, 'ctx>,
        methods: Vec<FuncItem<'scope, 'ctx, FunctionIndex>>,
    ) -> Self {
        Self {
            id,
            name_span,
            scope,
            methods,
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

#[derive(Debug, Default)]
pub struct LoweredFunction<'ctx> {
    pub block: ir::Block<'ctx>,
    pub locals: Vec<(ir::Local, Type<'ctx>)>,
}

#[derive(Debug, Default)]
pub struct LoweredCompilationUnit<'ctx> {
    pub classes: IndexMap<TypeId<'ctx>, LoweredClass<'ctx>, BuildIdentityHasher<usize>>,
    pub enums: IndexSet<TypeId<'ctx>, BuildIdentityHasher<usize>>,
    pub functions: IndexMap<FreeFunctionIndex, LoweredFunction<'ctx>>,

    pub added_fields: IndexSet<FieldId<'ctx>>,
    pub added_methods: IndexMap<MethodId<'ctx>, Option<LoweredFunction<'ctx>>>,
    pub method_replacements: IndexMap<MethodId<'ctx>, LoweredFunction<'ctx>>,
    pub method_wrappers: IndexMap<MethodId<'ctx>, Vec<LoweredFunction<'ctx>>>,
}

#[derive(Debug, Default)]
pub struct LoweredClass<'ctx> {
    pub methods: IndexMap<FunctionIndex, LoweredFunction<'ctx>>,
}
