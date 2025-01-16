use std::borrow::Cow;
use std::collections::BTreeSet;
use std::ops::{BitAndAssign, Not};
use std::rc::Rc;
use std::{fmt, mem};

use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;
use redscript_ast::{self as ast, Span, Spanned};

use super::infer::{ClassItem, FuncItem, FuncItemKind, InferStageModule};
use super::TypeInference;
use crate::diagnostic::MissingMethod;
use crate::lower::{FreeFunctionIndexes, InferredTypeApp, TypeEnv};
use crate::modules::{Export, ModuleMap};
use crate::utils::{Lazy, ScopedMap};
use crate::{
    ir, predef, Aggregate, AggregateFlags, CompileErrorReporter, CtxVar, Diagnostic, Enum, Field,
    FieldFlags, FieldId, FieldMap, FreeFunction, FreeFunctionFlags, FreeFunctionIndex,
    FunctionIndex, FunctionType, IndexMap, IndexSet, LowerError, Method, MethodFlags, MethodId,
    MethodMap, Param, ParamFlags, PolyType, QualifiedName, Reporter, Symbols, Type, TypeApp,
    TypeDef, TypeId, TypeInterner, TypeRef, TypeSchema, TypeScope, Variance,
};

pub(super) const WRAP_METHOD_ANNOTATION: &str = "wrapMethod";
pub(super) const REPLACE_METHOD_ANNOTATION: &str = "replaceMethod";
pub(super) const ADD_METHOD_ANNOTATION: &str = "addMethod";
pub(super) const ADD_FIELD_ANNOTATION: &str = "addField";
pub(super) const INTRINSIC_ANNOTATION: &str = "intrinsic";
pub(super) const NEVER_REF_ANNOTATION: &str = "neverRef";

pub(super) const THIS_IDENT: &str = "this";
pub(super) const WRAPPED_METHOD_IDENT: &str = "wrappedMethod";

#[derive(Debug)]
pub struct NameResolution<'ctx> {
    modules: Vec<ResolutionStageModule<'ctx>>,

    symbols: Symbols<'ctx>,
    module_map: ModuleMap<'ctx>,
    reporter: CompileErrorReporter<'ctx>,
}

impl<'ctx> NameResolution<'ctx> {
    pub fn new(symbols: Symbols<'ctx>) -> Self {
        Self {
            symbols,
            module_map: ModuleMap::default(),
            reporter: Reporter::default(),
            modules: vec![],
        }
    }

    pub fn add_module(&mut self, module: ast::SourceModule<'ctx>, interner: &'ctx TypeInterner) {
        let path_root = module
            .path
            .as_ref()
            .map(|p| &p.segments[..])
            .unwrap_or_default();

        let mut imports = vec![];
        let mut classes = vec![];
        let mut structs = vec![];
        let mut functions = vec![];
        let mut enums = vec![];
        let mut lets = vec![];

        for (
            ast::ItemDecl {
                annotations,
                visibility,
                qualifiers,
                item,
                doc,
            },
            item_span,
        ) in module.items.into_vec()
        {
            let meta = ParsedMeta {
                annotations,
                visibility,
                qualifiers,
                doc,
            };
            match item {
                ast::Item::Import(import) => {
                    imports.push((import, item_span));
                }
                ast::Item::Class(ref aggregate) | ast::Item::Struct(ref aggregate) => {
                    let (name, name_span) = aggregate.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let id = interner.intern(Cow::from(&path));
                    let res = self
                        .module_map
                        .add_type(path, id)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    self.reporter.unwrap_err(res);

                    match item {
                        ast::Item::Class(aggregate) => {
                            classes.push(ParsedAggregate {
                                id,
                                meta,
                                aggregate,
                            });
                        }
                        ast::Item::Struct(aggregate) => {
                            structs.push(ParsedAggregate {
                                id,
                                meta,
                                aggregate,
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                ast::Item::Function(function) => {
                    let (name, name_span) = function.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let index = self
                        .symbols
                        .add_free_function(path.clone(), FreeFunction::default());

                    let mut annotation: Option<FunctionAnnotation<'_>> = None;

                    for (ann, ann_span) in &meta.annotations {
                        match (ann.name, &ann.args[..]) {
                            (
                                INTRINSIC_ANNOTATION
                                | WRAP_METHOD_ANNOTATION
                                | REPLACE_METHOD_ANNOTATION
                                | ADD_METHOD_ANNOTATION,
                                _,
                            ) if annotation.is_some() => {
                                self.reporter
                                    .report(Diagnostic::IncompatibleAnnotations(*ann_span));
                            }
                            (INTRINSIC_ANNOTATION, &[(ast::Expr::Ident(name), span)]) => {
                                let intrinsic = ir::Intrinsic::try_from(name).map_err(|name| {
                                    Diagnostic::UnknownIntrinsic(name.into(), span)
                                });
                                annotation = self
                                    .reporter
                                    .unwrap_err(intrinsic)
                                    .map(FunctionAnnotation::Intrinsic);
                            }
                            (REPLACE_METHOD_ANNOTATION, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Replace((name, span)));
                            }
                            (ADD_METHOD_ANNOTATION, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Add((name, span)));
                            }
                            (WRAP_METHOD_ANNOTATION, &[(ast::Expr::Ident(name), span)]) => {
                                annotation = Some(FunctionAnnotation::Wrap((name, span)));
                            }
                            _ => {
                                self.reporter
                                    .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                            }
                        }
                    }

                    if matches!(annotation, None | Some(FunctionAnnotation::Intrinsic(_))) {
                        let res = self
                            .module_map
                            .add_function(path, index)
                            .map_err(|_| Diagnostic::NameRedefinition(name_span));
                        self.reporter.unwrap_err(res);
                    }

                    functions.push(ParsedFunction {
                        index,
                        meta,
                        function,
                        annotation,
                    });
                }
                ast::Item::Enum(enum_) => {
                    let (name, name_span) = enum_.name;
                    let path = QualifiedName::from_base_and_name(path_root, name);
                    let id = interner.intern(Cow::from(&path));
                    let res = self
                        .module_map
                        .add_type(path, id)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    self.reporter.unwrap_err(res);

                    enums.push(ParsedEnum { id, meta, enum_ });
                }
                ast::Item::Let(let_) => {
                    lets.push(ParsedLet { meta, let_ });
                }
            }
        }

        self.modules.push(ResolutionStageModule {
            path: module.path,
            imports,
            classes,
            structs,
            functions,
            enums,
            lets,
        });
    }

    pub fn populate_globals(&mut self, scope: &mut Scope<'_, 'ctx>) {
        let funcs = scope.funcs.top_mut();
        for (name, export) in self
            .module_map
            .exports(&[])
            .expect("root should always exist")
        {
            match export {
                Export::FreeFunction(vec) => {
                    funcs.entry(name).or_default().extend(vec.iter().copied());
                }
                &Export::Type(typ) => {
                    scope.types.add(name, TypeRef::Name(typ));
                }
            }
        }
    }

    pub fn progress<'scope>(
        mut self,
        scope: &'scope Scope<'_, 'ctx>,
    ) -> TypeInference<'scope, 'ctx> {
        let mut modules = vec![];

        for module in mem::take(&mut self.modules) {
            let mut type_scope = scope.types.introduce_scope();
            let mut func_scope = scope.funcs.introduce_scope();

            for (import, span) in &module.imports {
                self.module_map.visit_import(
                    import,
                    |name, typ| type_scope.add(name, TypeRef::Name(typ)),
                    |name, func| func_scope.top_mut().entry(name).or_default().push(func),
                    |name| {
                        self.reporter
                            .report(Diagnostic::ImportNotFound(name, *span));
                    },
                );
            }

            for ((name, _), id) in module
                .classes
                .iter()
                .chain(&module.structs)
                .map(|p| (p.aggregate.name, p.id))
                .chain(module.enums.iter().map(|p| (p.enum_.name, p.id)))
            {
                type_scope.add(name, TypeRef::Name(id));
            }

            let mut classes = vec![];
            for entry in module.classes {
                classes.push(self.process_aggregate(entry, &type_scope, false));
            }
            for entry in module.structs {
                classes.push(self.process_aggregate(entry, &type_scope, true));
            }

            let mut enums = vec![];
            for entry in module.enums {
                enums.push(entry.id);
                self.process_enum(entry, &mut type_scope);
            }

            let mut functions = vec![];
            for entry in module.functions {
                if module.path.is_some() {
                    let (name, _) = entry.function.name;
                    func_scope
                        .top_mut()
                        .entry(name)
                        .or_default()
                        .push(entry.index);
                }
                if let Some(item) = self.process_free_function(entry, &type_scope) {
                    functions.push(item);
                }
            }

            let mut fields = vec![];
            for let_ in module.lets {
                if let Some(field) = self.process_free_field(let_, &type_scope) {
                    fields.push(field);
                }
            }

            modules.push(InferStageModule::new(
                type_scope.pop_scope(),
                func_scope.pop_scope(),
                classes,
                enums,
                functions,
                fields,
            ));
        }

        modules
            .iter()
            .for_each(|module| self.validate_module(module));
        self.process_inheritance(modules.iter().flat_map(InferStageModule::classes));

        TypeInference::new(modules, self.symbols, self.reporter)
    }

    fn process_aggregate<'scope>(
        &mut self,
        entry: ParsedAggregate<'ctx>,
        types: &TypeEnv<'scope, 'ctx>,
        is_struct: bool,
    ) -> ClassItem<'scope, 'ctx> {
        let aggregate = entry.aggregate;
        let (_, name_span) = aggregate.name;

        let mut qs = entry.meta.qualifiers;
        let is_import_only = qs.take_flag(ast::ItemQualifiers::IMPORT_ONLY);
        let is_native = is_import_only || qs.take_flag(ast::ItemQualifiers::NATIVE);
        let mut class_flags = AggregateFlags::default()
            .with_is_import_only(is_import_only)
            .with_is_native(is_native)
            .with_is_final(qs.take_flag(ast::ItemQualifiers::FINAL))
            .with_is_abstract(qs.take_flag(ast::ItemQualifiers::ABSTRACT))
            .with_is_struct(is_struct);

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        for (ann, ann_span) in &entry.meta.annotations {
            match (ann.name, &ann.args[..]) {
                (NEVER_REF_ANNOTATION, &[]) => {
                    class_flags.set_is_never_ref(true);
                }
                _ => {
                    self.reporter
                        .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                }
            }
        }

        let (types, vars) = self.create_scope_env(types, &aggregate.type_params);

        let mut fields = FieldMap::default();
        let mut methods = MethodMap::default();

        let mut method_items = vec![];

        for (item, item_span) in aggregate.items {
            match item.item {
                ast::Item::Function(function) => {
                    let (name, name_span) = function.name;
                    let qs = item.qualifiers;
                    let flags = self.process_method_flags(qs, &function, class_flags, name_span);

                    let (func_t, type_scope) = self.create_function_env(&function, &types);
                    if function.body.is_none()
                        && !flags.is_native()
                        && (!class_flags.is_abstract()
                            || !func_t.type_params().is_empty()
                            || flags.is_final()
                            || flags.is_static())
                    {
                        self.reporter
                            .report(Diagnostic::MissingFunctionBody(name_span));
                    };

                    let method = Method::new(flags, func_t, None, item.doc, Some(name_span));
                    let id = methods.add(name, method);
                    let Some(body) = function.body else {
                        continue;
                    };
                    method_items.push(FuncItem::new(id, name_span, body, type_scope));
                }
                ast::Item::Let(field) => {
                    let (name, name_span) = field.name;
                    let (typ, span) = field.typ.as_ref();

                    let Some(typ) = self.reporter.unwrap_err(types.resolve(typ, *span)) else {
                        continue;
                    };
                    let flags =
                        self.process_field_flags(item.qualifiers, class_flags, &typ, name_span);

                    let field = Field::new(flags, typ, item.doc, Some(name_span));
                    let res = fields
                        .add(name, field)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    self.reporter.unwrap_err(res);
                }
                _ => self.reporter.report(Diagnostic::UnexpectedItem(item_span)),
            }
        }

        let base = aggregate
            .extends
            .as_deref()
            .and_then(|(name, span)| {
                Some((self.reporter.unwrap_err(types.resolve(name, *span))?, *span))
            })
            .and_then(|(typ, span)| {
                if let Type::Data(type_app) = typ {
                    Some(type_app)
                } else {
                    self.reporter.report(Diagnostic::InvalidBaseType(span));
                    None
                }
            })
            .or_else(|| {
                is_struct
                    .not()
                    .then(|| TypeApp::nullary(predef::ISCRIPTABLE))
            });

        let aggregate = Aggregate::new(class_flags, base, fields, methods, Some(name_span));
        let schema = TypeSchema::Aggregate(aggregate.into());
        let def = TypeDef::new(vars, schema, entry.meta.doc);
        self.symbols.add_type(entry.id, def);

        let type_scope = types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();
        ClassItem::new(entry.id, name_span, type_scope, method_items)
    }

    fn process_enum(&mut self, entry: ParsedEnum<'ctx>, types: &mut TypeEnv<'_, 'ctx>) {
        let (name, name_span) = entry.enum_.name;

        let qs = entry.meta.qualifiers;
        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        types.add(name, TypeRef::Name(entry.id));

        let mut by_name = IndexMap::default();
        let mut by_val = BTreeSet::<i64>::new();

        for (variant, span) in &entry.enum_.variants {
            let val = if let Some(val) = variant.value {
                i64::from(val)
            } else if let Some(last) = by_val.last() {
                let Some(next) = last.checked_add(1) else {
                    self.reporter.report(Diagnostic::ValueOverflow(*span));
                    continue;
                };
                next
            } else {
                0
            };

            if by_name.insert(variant.name, val).is_some() {
                self.reporter
                    .report(Diagnostic::DuplicateVariantName(*span));
            }
            if !by_val.insert(val) {
                self.reporter
                    .report(Diagnostic::DuplicateVariantValue(*span));
            }
        }

        let schema = TypeSchema::Enum(Enum::new(by_name).into());
        self.symbols
            .add_type(entry.id, TypeDef::new([], schema, entry.meta.doc));
    }

    fn process_free_function<'scope>(
        &mut self,
        entry: ParsedFunction<'ctx>,
        types: &TypeEnv<'scope, 'ctx>,
    ) -> Option<FuncItemKind<'scope, 'ctx>> {
        let func = entry.function;
        let (name, name_span) = func.name;
        let (func_t, type_scope) = self.create_function_env(&func, types);

        let mut intrinsic: Option<ir::Intrinsic> = None;
        let mut replaced: Option<MethodId<'ctx>> = None;
        let mut wrapped: Option<MethodId<'ctx>> = None;

        match &entry.annotation {
            &Some(FunctionAnnotation::Intrinsic(i)) => {
                intrinsic = Some(i);
            }
            Some(ann @ FunctionAnnotation::Replace(class)) => {
                if let Some(id) =
                    self.resolve_existing_annotated_method(func.name, *class, &func_t, ann, types)
                {
                    replaced = Some(id);
                }
            }
            &Some(FunctionAnnotation::Add(class)) => {
                if !func_t.type_params().is_empty() {
                    self.reporter
                        .report(Diagnostic::GenericMethodAnnotation(name_span));
                }

                if self
                    .resolve_annotated_method(name, class, &func_t, types)
                    .is_some()
                {
                    self.reporter
                        .report(Diagnostic::DuplicateMethodAnnotation(name_span));
                }

                let (id, agg) = self.resolve_annotated_type(class, types)?;
                let parent_flags = agg.flags();
                if agg.is_user_defined() {
                    self.reporter
                        .report(Diagnostic::UserSymbolAnnotation(name_span));
                }
                let qs = entry.meta.qualifiers;
                let flags = self.process_method_flags(qs, &func, parent_flags, name_span);

                if func.body.is_none() && !flags.is_native() {
                    self.reporter
                        .report(Diagnostic::MissingFunctionBody(name_span));
                    return None;
                };

                let member = Method::new(flags, func_t, None, entry.meta.doc, Some(name_span));
                let idx = self.symbols[id]
                    .schema_mut()
                    .as_aggregate_mut()
                    .unwrap()
                    .methods_mut()
                    .add(name, member);

                let item = FuncItem::new(MethodId::new(id, idx), name_span, func.body, type_scope);
                return Some(FuncItemKind::AddMethod(item));
            }
            Some(ann @ FunctionAnnotation::Wrap(class)) => {
                if let Some(id) =
                    self.resolve_existing_annotated_method(func.name, *class, &func_t, ann, types)
                {
                    wrapped = Some(id);
                }
            }
            _ => {}
        }

        let mut qs = entry.meta.qualifiers;
        let (free_func, body) = match (func.body, intrinsic, replaced, wrapped) {
            (None, Some(intrinsic), None, None) => {
                let intrinsic =
                    FreeFunction::new_intrinsic(func_t, intrinsic, entry.meta.doc, Some(name_span));
                (intrinsic, None)
            }
            (body, None, None, None)
                if (body.is_some() && !qs.contains(ast::ItemQualifiers::NATIVE))
                    || (body.is_none() && qs.contains(ast::ItemQualifiers::NATIVE)) =>
            {
                let flags = FreeFunctionFlags::default()
                    .with_is_exec(qs.take_flag(ast::ItemQualifiers::EXEC))
                    .with_is_native(qs.take_flag(ast::ItemQualifiers::NATIVE));
                let func = FreeFunction::new(flags, func_t, entry.meta.doc, Some(name_span));
                (func, body)
            }
            (Some(body), None, Some(replaced), None) => {
                let func = FuncItem::new(replaced, name_span, body, type_scope);
                return Some(FuncItemKind::ReplaceMethod(func));
            }
            (Some(body), None, None, Some(wrapped)) => {
                let func = FuncItem::new(wrapped, name_span, body, type_scope);
                return Some(FuncItemKind::WrapMethod(func));
            }
            (Some(_), _, _, _) => {
                self.reporter
                    .report(Diagnostic::UnexpectedFunctionBody(name_span));
                return None;
            }
            (None, _, _, _) => {
                self.reporter
                    .report(Diagnostic::MissingFunctionBody(name_span));
                return None;
            }
        };

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        self.symbols.set_free_function(entry.index, free_func);
        body.map(|body| {
            FuncItemKind::FreeFunction(FuncItem::new(entry.index, name_span, body, type_scope))
        })
    }

    fn process_free_field<'scope>(
        &mut self,
        entry: ParsedLet<'ctx>,
        types: &TypeEnv<'scope, 'ctx>,
    ) -> Option<FieldId<'ctx>> {
        let field = entry.let_;
        let mut doc = entry.meta.doc;
        let (name, name_span) = field.name;
        let mut id = None;

        for (ann, ann_span) in &entry.meta.annotations {
            match (ann.name, &ann.args[..]) {
                (ADD_FIELD_ANNOTATION, &[(ast::Expr::Ident(type_name), span)]) => {
                    let (parent_t, agg) = self.resolve_annotated_type((type_name, span), types)?;
                    let flags = agg.flags();

                    if agg.is_user_defined() {
                        self.reporter
                            .report(Diagnostic::UserSymbolAnnotation(name_span));
                    }

                    if flags.is_struct() {
                        self.reporter
                            .report(Diagnostic::StructFieldAddition(name_span));
                    }

                    let (typ, span) = field.typ.as_ref();
                    let typ = self.reporter.unwrap_err(types.resolve(typ, *span))?;
                    let flags =
                        self.process_field_flags(entry.meta.qualifiers, flags, &typ, name_span);

                    let field = Field::new(flags, typ, mem::take(&mut doc), Some(name_span));
                    let res = self.symbols[parent_t]
                        .schema_mut()
                        .as_aggregate_mut()
                        .unwrap()
                        .fields_mut()
                        .add(name, field)
                        .map_err(|_| Diagnostic::NameRedefinition(name_span));
                    let idx = self.reporter.unwrap_err(res)?;

                    id = Some(FieldId::new(parent_t, idx));
                }
                _ => {
                    self.reporter
                        .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
                }
            }
        }

        if id.is_none() {
            self.reporter.report(Diagnostic::UnexpectedItem(name_span));
        }

        id
    }

    fn process_method_flags(
        &mut self,
        mut qs: ast::ItemQualifiers,
        func: &ast::SourceFunction<'ctx>,
        parent_flags: AggregateFlags,
        span: Span,
    ) -> MethodFlags {
        let is_native = qs.take_flag(ast::ItemQualifiers::NATIVE);
        let is_final = qs.take_flag(ast::ItemQualifiers::FINAL);
        let is_static = qs.take_flag(ast::ItemQualifiers::STATIC);
        let flags = MethodFlags::default()
            .with_is_native(is_native)
            .with_is_final(is_final)
            .with_is_static(is_static)
            .with_is_callback(qs.take_flag(ast::ItemQualifiers::CALLBACK))
            .with_is_unimplemented(func.body.is_none() && !is_native && !is_final && !is_static);

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, span));
        }

        if parent_flags.is_struct() && !flags.is_static() {
            self.reporter
                .report(Diagnostic::NonStaticStructMethod(span));
        }

        if !parent_flags.is_native() && flags.is_native() {
            self.reporter
                .report(Diagnostic::NativeMemberOfScriptedType(span));
        }

        flags
    }

    fn process_field_flags(
        &mut self,
        mut qs: ast::ItemQualifiers,
        parent_flags: AggregateFlags,
        typ: &Type<'ctx>,
        span: Span,
    ) -> FieldFlags {
        fn can_be_persisted(app: &TypeApp<'_>) -> bool {
            match (app.id(), app.args()) {
                (_, args) if !args.is_empty() => args
                    .iter()
                    .all(|arg| matches!(arg, Type::Data(app) if can_be_persisted(app))),
                (id, _) => id != predef::STRING && id != predef::VARIANT && id != predef::RESOURCE,
            }
        }

        let flags = FieldFlags::default()
            .with_is_native(qs.take_flag(ast::ItemQualifiers::NATIVE))
            .with_is_const(qs.take_flag(ast::ItemQualifiers::CONST))
            .with_is_persistent(qs.take_flag(ast::ItemQualifiers::PERSISTENT));

        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, span));
        }

        if flags.is_persistent() && !matches!(typ, Type::Data(app) if can_be_persisted(app)) {
            self.reporter
                .report(Diagnostic::InvalidPersistentField(span));
        }

        if !parent_flags.is_native() && flags.is_native() {
            self.reporter
                .report(Diagnostic::NativeMemberOfScriptedType(span));
        }

        flags
    }

    fn validate_module(&mut self, module: &InferStageModule<'_, 'ctx>) {
        for func in module.functions() {
            let (func_t, span) = match func {
                FuncItemKind::FreeFunction(func) => {
                    let func = &self.symbols[*func.id()];
                    let span = func.span().expect("user function should have a span");
                    (func.type_(), span)
                }
                _ => {
                    continue;
                }
            };
            self.reporter.unwrap_err(self.check_func_type(func_t, span));
        }

        for &field in module.fields() {
            let field = &self.symbols[field];
            let span = field.span().expect("user field should have a span");
            self.reporter
                .unwrap_err(self.check_type(field.type_(), Variance::Covariant, span));
        }

        for class in module.classes() {
            if self
                .symbols
                .base_iter(class.id())
                .skip(1)
                .any(|(id, _)| id == class.id())
            {
                self.reporter
                    .report(Diagnostic::CircularInheritance(class.name_span()));
                // remove base class to prevent base type traversal from looping
                self.symbols[class.id()]
                    .schema_mut()
                    .as_aggregate_mut()
                    .expect("class should be an aggregate")
                    .set_base(None);
            }

            let agg = self.symbols[class.id()]
                .schema()
                .as_aggregate()
                .expect("class should be an aggregate");

            if let Some(base) = agg.base() {
                let span = class.name_span();
                self.reporter
                    .unwrap_err(self.check_type_app(base, Variance::Covariant, span));

                let incompatible = match self.symbols[base.id()].schema() {
                    TypeSchema::Enum(_) => Some("an enum"),
                    TypeSchema::Aggregate(base)
                        if base.flags().is_struct() && !agg.flags().is_struct() =>
                    {
                        Some("a struct")
                    }
                    TypeSchema::Aggregate(base)
                        if !base.flags().is_struct() && agg.flags().is_struct() =>
                    {
                        Some("a class")
                    }
                    TypeSchema::Aggregate(base) if base.flags().is_final() => {
                        // TODO: maybe warn
                        None
                    }
                    TypeSchema::Primitive => Some("a primitive"),
                    _ => None,
                };
                if let Some(incompatible) = incompatible {
                    self.reporter
                        .report(Diagnostic::IncompatibleBaseType(incompatible, span));
                }
            }

            for entry in agg.fields().iter() {
                let field = entry.field();
                let span = field.span().expect("user field should have a span");
                self.reporter
                    .unwrap_err(self.check_type(field.type_(), Variance::Covariant, span));
            }

            for method in agg.methods().iter() {
                let method = method.func();
                let span = method.span().expect("user method should have a span");
                self.reporter
                    .unwrap_err(self.check_func_type(method.type_(), span));
            }
        }
    }

    fn process_inheritance<'a>(
        &mut self,
        classes: impl IntoIterator<Item = &'a ClassItem<'a, 'ctx>>,
    ) where
        'ctx: 'a,
    {
        let mut classes = classes.into_iter().collect::<Vec<_>>();
        classes.sort_unstable_by_key(|class| self.symbols.base_iter(class.id()).count());

        let mut virtuals =
            HashMap::<TypeId<'ctx>, HashSet<MethodId<'ctx>>, BuildIdentityHasher<usize>>::default();

        for class in classes {
            let class_id = class.id();
            let class_sym = &self.symbols[class_id];
            let aggregate = class_sym
                .schema()
                .as_aggregate()
                .expect("class should be an aggregate");

            let Some(base) = class_sym.schema().base_type() else {
                continue;
            };

            let this_args = class_sym
                .params()
                .iter()
                .map(|var| PolyType::from_type(&Type::Ctx(var.clone())))
                .collect::<Rc<_>>();
            let this_t = InferredTypeApp::new(class_id, this_args);

            let mut implemented: IndexMap<MethodId<'ctx>, FunctionIndex> = IndexMap::default();
            let unimplemented = virtuals.entry(class_id).or_default();
            let mut duplicates = IndexSet::<&'ctx str>::default();

            for method in aggregate.methods().iter() {
                let method_t = method.func().type_();
                if method.func().flags().is_static() || !method_t.type_params().is_empty() {
                    continue;
                }

                if method.func().flags().is_unimplemented() && !aggregate.flags().is_final() {
                    unimplemented.insert(MethodId::new(class_id, *method.key()));
                }

                let base = self
                    .symbols
                    .query_methods(class_id, method.name())
                    .find(|entry| {
                        if entry.key() == &MethodId::new(class.id(), *method.key())
                            || !entry.func().type_().type_params().is_empty()
                            || entry.func().type_().params().len() != method_t.params().len()
                        {
                            return false;
                        }

                        let entry_this_t = this_t
                            .clone()
                            .instantiate_as(entry.key().parent(), &self.symbols)
                            .expect("should instantiate as parent type");
                        let env = entry_this_t.type_env(&self.symbols);

                        entry
                            .func()
                            .type_()
                            .unwrapped_param_types()
                            .map(|param| PolyType::from_type_with_env(param, &env).unwrap())
                            .eq(method_t.unwrapped_param_types().map(PolyType::from_type))
                    });

                match base {
                    Some(base) if base.key().parent() == class_id => {
                        duplicates.insert(method.name());
                    }
                    Some(base) => {
                        if base.func().flags().is_final() {
                            self.reporter.report(Diagnostic::FinalMethodOverride(
                                method.name(),
                                class.name_span(),
                            ));
                        };
                        implemented.insert(*base.key(), *method.key());
                    }
                    _ => {}
                }
            }

            let [to_impl, unimpl] = virtuals.get_many_mut([&base.id(), &class_id]);
            let to_impl = to_impl.map(|set| set.iter()).unwrap_or_default();
            let unimpl = unimpl.expect("unimplemented set should exist");
            let mut missing = vec![];

            for &id in to_impl.filter(|&id| !implemented.contains_key(id)) {
                if aggregate.flags().is_abstract() {
                    unimpl.insert(id);
                } else {
                    let (name, base_method) = self
                        .symbols
                        .get_method(id)
                        .expect("base method should exist");

                    let parent = this_t
                        .clone()
                        .instantiate_as(id.parent(), &self.symbols)
                        .expect("should instantiate as parent type");
                    let env = parent.type_env(&self.symbols);
                    let base_types = base_method
                        .type_()
                        .param_types()
                        .map(|param| PolyType::from_type_with_env(param, &env).unwrap())
                        .collect::<Box<_>>();
                    let params = base_method
                        .type_()
                        .params()
                        .iter()
                        .zip(&base_types)
                        .map(|(param, typ)| {
                            Param::new(param.name(), *param.flags(), typ.clone(), None)
                        })
                        .collect::<Box<_>>();
                    let return_t =
                        PolyType::from_type_with_env(base_method.type_().return_type(), &env)
                            .unwrap();
                    missing.push(MissingMethod::new(name, params, return_t));
                }
            }

            for dup in duplicates {
                self.reporter
                    .report(Diagnostic::DuplicateMethod(dup, class.name_span()));
            }

            if !missing.is_empty() {
                self.reporter.report(Diagnostic::MissingMethodImpls(
                    missing.into(),
                    class.name_span(),
                ));
            }

            for (overloaded_id, idx) in implemented {
                let (_, method) = self
                    .symbols
                    .get_method_mut(MethodId::new(class_id, idx))
                    .expect("method should exist");
                method.set_overloaded(overloaded_id);
            }
        }
    }

    fn create_function_env<'scope>(
        &mut self,
        func: &ast::SourceFunction<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> (FunctionType<'ctx>, TypeScope<'scope, 'ctx>) {
        for param in &func.type_params {
            if param.variance != ast::Variance::Invariant {
                let (_, span) = param.name;
                self.reporter.report(Diagnostic::NonDataVariance(span));
            }
        }

        let (types, vars) = self.create_scope_env(types, &func.type_params);

        let params = func
            .params
            .iter()
            .filter_map(|(param, span)| {
                let (typ, type_span) = param
                    .typ
                    .as_ref()
                    .expect("parameter type should always be present");
                let typ = self.reporter.unwrap_err(types.resolve(typ, *type_span))?;
                let qs = param.qualifiers;
                let flags = ParamFlags::default()
                    .with_is_optional(qs.contains(ast::ParamQualifiers::OPTIONAL))
                    .with_is_out(qs.contains(ast::ParamQualifiers::OUT))
                    .with_is_const(qs.contains(ast::ParamQualifiers::CONST));
                Some(Param::new(param.name, flags, typ, Some(*span)))
            })
            .collect::<Vec<_>>();

        let return_t = func
            .return_type
            .as_deref()
            .and_then(|(ty, span)| self.reporter.unwrap_err(types.resolve(ty, *span)))
            .unwrap_or_else(|| Type::nullary(predef::VOID));

        let func_t = FunctionType::new(vars, params, return_t);

        let type_scope = types
            .pop_scope()
            .into_iter()
            .filter_map(|(k, v)| Some((k, v.force()?)))
            .collect();

        (func_t, type_scope)
    }

    fn create_scope_env<'scope>(
        &mut self,
        types: &'scope TypeEnv<'scope, 'ctx>,
        params: &'scope [ast::SourceTypeParam<'ctx>],
    ) -> (TypeEnv<'scope, 'ctx>, Box<[Rc<CtxVar<'ctx>>]>) {
        let mut types = types.introduce_scope();

        for param in params {
            let (name, name_span) = param.name;
            let typ = TypeRef::LazyVar(Box::new(Lazy::new(Box::new(|env| {
                env.resolve_param(param).map(Rc::new)
            }))));
            if types.get(name).is_some() {
                self.reporter
                    .report(Diagnostic::NameRedefinition(name_span));
            }
            types.add(name, typ);
        }

        let vars = types
            .top_level()
            .iter()
            .zip(params)
            .filter_map(|((_, typ), param)| {
                let (_, span) = param.name;
                let TypeRef::LazyVar(lazy) = typ else {
                    unreachable!()
                };
                let res = lazy
                    .get(&types)
                    .map_err(|_| LowerError::CyclicType(span))
                    .and_then(|res| res);
                self.reporter.unwrap_err(res)
            })
            .collect();

        (types, vars)
    }

    fn resolve_annotated_type(
        &mut self,
        (cls_name, cls_span): ast::Spanned<&'ctx str>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> Option<(TypeId<'ctx>, &Aggregate<'ctx>)> {
        let target = types
            .get(cls_name)
            .ok_or(LowerError::UnresolvedType(cls_name, cls_span));
        let typ = self.reporter.unwrap_err(target)?;
        let &TypeRef::Name(id) = typ else {
            self.reporter
                .report(Diagnostic::InvalidAnnotationType(cls_name, cls_span));
            return None;
        };
        let TypeSchema::Aggregate(agg) = self.symbols[id].schema() else {
            self.reporter
                .report(Diagnostic::InvalidAnnotationType(cls_name, cls_span));
            return None;
        };
        Some((id, agg))
    }

    fn resolve_existing_annotated_method(
        &mut self,
        (func_name, func_span): ast::Spanned<&'ctx str>,
        (cls_name, cls_span): ast::Spanned<&'ctx str>,
        func_type: &FunctionType<'ctx>,
        annotation: &FunctionAnnotation<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> Option<MethodId<'ctx>> {
        let Some((id, method)) =
            self.resolve_annotated_method(func_name, (cls_name, cls_span), func_type, types)
        else {
            self.reporter.report(Diagnostic::AnnotatedMethodNotFound(
                annotation.clone(),
                func_span,
            ));
            return None;
        };
        if method.is_user_defined() {
            self.reporter
                .report(Diagnostic::UserSymbolAnnotation(cls_span));
            return None;
        }
        Some(id)
    }

    fn resolve_annotated_method(
        &mut self,
        func_name: &str,
        (cls_name, cls_span): ast::Spanned<&'ctx str>,
        func_type: &FunctionType<'ctx>,
        types: &TypeEnv<'_, 'ctx>,
    ) -> Option<(MethodId<'ctx>, &Method<'ctx>)> {
        let (id, agg) = self.resolve_annotated_type((cls_name, cls_span), types)?;
        let entry = agg.methods().by_name(func_name).find(|entry| {
            entry
                .func()
                .type_()
                .param_types()
                .eq(func_type.param_types())
                && entry.func().type_().return_type() == func_type.return_type()
        })?;
        Some((MethodId::new(id, *entry.key()), *entry.func()))
    }

    fn check_type(
        &self,
        type_app: &Type<'ctx>,
        variance: Variance,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        match type_app {
            Type::Data(type_app) => self.check_type_app(type_app, variance, span),
            Type::Ctx(var)
                if var.variance() != Variance::Invariant && var.variance() != variance =>
            {
                Err(Diagnostic::InvalidVariance(var.name(), variance, span))
            }
            _ => Ok(()),
        }
    }

    fn check_type_app(
        &self,
        type_app: &TypeApp<'ctx>,
        variance: Variance,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        let params = self.symbols[type_app.id()].params();
        if type_app.args().len() == params.len() {
            type_app
                .args()
                .iter()
                .zip(params)
                .try_for_each(|(arg, param)| {
                    self.check_type(arg, variance * param.variance(), span)
                })
        } else {
            Err(Diagnostic::InvalidTypeArgCount(
                type_app.id(),
                params.len(),
                span,
            ))
        }
    }

    fn check_func_type(
        &self,
        func_type: &FunctionType<'ctx>,
        span: Span,
    ) -> Result<(), Diagnostic<'ctx>> {
        for param in func_type.params() {
            let span = param
                .span()
                .expect("user function param should have a span");
            self.check_type(param.type_(), Variance::Contravariant, span)?;
        }
        self.check_type(func_type.return_type(), Variance::Covariant, span)
    }

    pub(crate) fn reporter_mut(&mut self) -> &mut CompileErrorReporter<'ctx> {
        &mut self.reporter
    }
}

#[derive(Debug)]
struct ResolutionStageModule<'ctx> {
    path: Option<ast::Path<'ctx>>,
    imports: Vec<ast::Spanned<ast::Import<'ctx>>>,
    classes: Vec<ParsedAggregate<'ctx>>,
    structs: Vec<ParsedAggregate<'ctx>>,
    functions: Vec<ParsedFunction<'ctx>>,
    enums: Vec<ParsedEnum<'ctx>>,
    lets: Vec<ParsedLet<'ctx>>,
}

#[derive(Debug)]
struct ParsedMeta<'ctx> {
    annotations: Box<[ast::Spanned<ast::SourceAnnotation<'ctx>>]>,
    // TODO: visibility
    #[allow(unused)]
    visibility: Option<ast::Visibility>,
    qualifiers: ast::ItemQualifiers,
    doc: Box<[&'ctx str]>,
}

#[derive(Debug)]
struct ParsedAggregate<'ctx> {
    id: TypeId<'ctx>,
    meta: ParsedMeta<'ctx>,
    aggregate: ast::SourceAggregate<'ctx>,
}

#[derive(Debug)]
struct ParsedFunction<'ctx> {
    index: FreeFunctionIndex,
    meta: ParsedMeta<'ctx>,
    function: ast::SourceFunction<'ctx>,
    annotation: Option<FunctionAnnotation<'ctx>>,
}

#[derive(Debug)]
struct ParsedLet<'ctx> {
    meta: ParsedMeta<'ctx>,
    let_: ast::SourceField<'ctx>,
}

#[derive(Debug)]
struct ParsedEnum<'ctx> {
    id: TypeId<'ctx>,
    meta: ParsedMeta<'ctx>,
    enum_: ast::SourceEnum<'ctx>,
}

#[derive(Debug, Clone)]
pub enum FunctionAnnotation<'ctx> {
    Intrinsic(ir::Intrinsic),
    Replace(Spanned<&'ctx str>),
    Add(Spanned<&'ctx str>),
    Wrap(Spanned<&'ctx str>),
}

impl fmt::Display for FunctionAnnotation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionAnnotation::Intrinsic(i) => {
                write!(f, "@{INTRINSIC_ANNOTATION}({})", <&str>::from(*i))
            }
            FunctionAnnotation::Replace((name, _)) => {
                write!(f, "@{REPLACE_METHOD_ANNOTATION}({name})")
            }
            FunctionAnnotation::Add((name, _)) => write!(f, "@{ADD_METHOD_ANNOTATION}({name})"),
            FunctionAnnotation::Wrap((name, _)) => write!(f, "@{WRAP_METHOD_ANNOTATION}({name})"),
        }
    }
}

#[derive(Debug)]
pub struct Scope<'scope, 'ctx> {
    pub(super) types: TypeEnv<'scope, 'ctx>,
    pub(super) funcs: ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
}

impl<'scope, 'ctx> Scope<'scope, 'ctx> {
    pub fn new(symbols: &Symbols<'ctx>) -> Self {
        let mut types = TypeEnv::with_default_types();
        for (id, _) in symbols.types() {
            types.add(id.as_str(), TypeRef::Name(id));
        }

        let mut funcs = IndexMap::<&str, FreeFunctionIndexes>::default();
        for func in symbols.free_functions() {
            let key = func
                .name()
                .as_single_component()
                .expect("functions should not be scoped in base cache");
            funcs.entry(key).or_default().push(*func.key());
        }

        Self {
            types,
            funcs: funcs.into(),
        }
    }

    pub fn push(
        &self,
        types: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>,
        funcs: IndexMap<&'ctx str, FreeFunctionIndexes>,
    ) -> Scope<'_, 'ctx> {
        let types = self.types.push_scope(types);
        let funcs = self.funcs.push_scope(funcs);
        Scope { types, funcs }
    }
}

trait BitStructOps {
    fn take_flag(&mut self, flag: Self) -> bool;
}

impl<A> BitStructOps for A
where
    A: Copy + PartialEq + Not<Output = A> + BitAndAssign,
{
    #[inline]
    fn take_flag(&mut self, flag: A) -> bool {
        let prev = *self;
        *self &= !flag;
        prev != *self
    }
}
