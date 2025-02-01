use core::fmt;
use std::borrow::Cow;
use std::rc::Rc;

use hashbrown::HashMap;
use identity_hash::BuildIdentityHasher;
use redscript_compiler_frontend::ast::SourceMap;
use redscript_compiler_frontend::utils::fmt::{sep_by, DisplayFn};
use redscript_compiler_frontend::{
    predef, Aggregate, AggregateFlags, Enum, Field, FieldFlags, FieldMap, FreeFunction,
    FreeFunctionFlags, FreeFunctionIndex, FunctionIndex, FunctionMap, FunctionType, Immutable,
    Method, MethodFlags, MethodId, Mono, MonoType, Param, ParamFlags, Symbols, Type, TypeApp,
    TypeDef, TypeId, TypeInterner, TypeKind, TypeSchema,
};
use redscript_io::{
    CNameIndex, ClassIndex as PoolClassIndex, EnumIndex as PoolEnumIndex, Function as PoolFunction,
    FunctionIndex as PoolFunctionIndex, IndexedDefinition, Parameter as PoolParameter,
    ScriptBundle, Type as PoolType, TypeIndex as PoolTypeIndex, TypeKind as PoolTypeKind,
};
use thiserror::Error;

use crate::monomorph::Monomorphizer;
use crate::IndexMap;

const NEVER_REF_WHITELIST: [&str; 1] = ["ReactionData"];

#[derive(Debug)]
pub struct CompilationInputs<'ctx> {
    symbols: Symbols<'ctx>,
    mapping: PoolMappings<'ctx>,
}

impl<'ctx> CompilationInputs<'ctx> {
    pub fn load<'a>(
        bundle: &'a ScriptBundle<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> Result<Self, Error> {
        let mut inputs = CompilationInputs::default();
        let mut virtual_map: HashMap<
            TypeId<'ctx>,
            HashMap<CNameIndex, MethodId<'ctx>, BuildIdentityHasher<u32>>,
            BuildIdentityHasher<usize>,
        > = HashMap::default();

        for def in bundle.indexed_definitions() {
            match def {
                IndexedDefinition::Type(index, typ) => {
                    if typ.kind() == &PoolTypeKind::Primitive {
                        let name = bundle.get_item(typ.name()).ok_or_else(|| {
                            Error::MissingPoolItem("type name", typ.name().into())
                        })?;
                        let id = interner.intern(name.as_ref());
                        let def = TypeDef::new([], TypeSchema::Primitive, []);
                        inputs.symbols.add_type(id, def);
                    }
                    let typ = load_type::<Mono>(typ, bundle, interner)?;
                    inputs.mapping.types.insert(typ, index);
                }
                IndexedDefinition::Class(index, cls) => {
                    let class_name = bundle
                        .get_item(cls.name())
                        .ok_or_else(|| Error::MissingPoolItem("class name", cls.name().into()))?;
                    let class_id = interner.intern(class_name.as_ref());

                    let base = cls
                        .base()
                        .map(|base| {
                            let class = bundle
                                .get_item(base)
                                .ok_or_else(|| Error::MissingPoolItem("base class", base.into()))?;
                            let base_name = bundle.get_item(class.name()).ok_or_else(|| {
                                Error::MissingPoolItem("base class name", class.name().into())
                            })?;
                            Ok(TypeApp::nullary(interner.intern(base_name.as_ref())))
                        })
                        .transpose()?;

                    let mut fields = FieldMap::default();
                    let mut methods = FunctionMap::default();

                    let mut mappings = ClassMappings::default();

                    for &field_idx in cls.fields() {
                        let field = bundle
                            .get_item(field_idx)
                            .ok_or_else(|| Error::MissingPoolItem("field", field_idx.into()))?;
                        let name = bundle.get_item(field.name()).ok_or_else(|| {
                            Error::MissingPoolItem("field name", field.name().into())
                        })?;
                        let typ = bundle.get_item(field.type_()).ok_or_else(|| {
                            Error::MissingPoolItem("field type", field.type_().into())
                        })?;
                        let typ = load_type::<Immutable>(typ, bundle, interner)?;
                        let flags = FieldFlags::new()
                            .with_is_native(field.flags().is_native())
                            .with_is_editable(field.flags().is_editable())
                            .with_is_inline(field.flags().is_inline())
                            .with_is_const(field.flags().is_const())
                            .with_is_persistent(field.flags().is_persistent());

                        fields
                            .add(assert_borrowed(name), Field::new(flags, typ, [], None))
                            .map_err(|_| Error::DuplicateField(field_idx.into()))?;
                    }

                    let mut virtuals = base
                        .as_ref()
                        .and_then(|base| virtual_map.get(&base.id()).cloned())
                        .unwrap_or_default();

                    for &method_idx in cls.methods() {
                        let method = bundle
                            .get_item(method_idx)
                            .ok_or_else(|| Error::MissingPoolItem("method", method_idx.into()))?;
                        let name = bundle.get_item(method.name()).ok_or_else(|| {
                            Error::MissingPoolItem("method name", method.name().into())
                        })?;
                        let name = demangled_name(assert_borrowed(name));

                        let flags = MethodFlags::new()
                            .with_is_static(method.flags().is_static())
                            .with_is_final(method.flags().is_final())
                            .with_is_native(method.flags().is_native())
                            .with_is_callback(method.flags().is_callback())
                            .with_is_unimplemented(
                                method.body().is_empty() && !method.flags().is_native(),
                            );
                        let typ = load_function_type(method, bundle, interner)?;
                        let base = virtuals.get(&method.name()).copied();

                        let idx = methods.add(name, Method::new(flags, typ, base, [], None));
                        mappings.methods.insert(idx, method_idx);
                        let id = MethodId::new(class_id, idx);
                        if !method.flags().is_final() && !method.flags().is_static() {
                            virtuals.insert(method.name(), id);
                        }
                    }

                    let flags = AggregateFlags::new()
                        .with_is_abstract(cls.flags().is_abstract())
                        .with_is_native(cls.flags().is_native())
                        .with_is_final(cls.flags().is_final())
                        .with_is_import_only(cls.flags().is_import_only())
                        .with_is_struct(cls.flags().is_struct())
                        .with_is_never_ref(NEVER_REF_WHITELIST.contains(&class_name.as_ref()));
                    let agg =
                        Aggregate::new(flags, base, fields, methods, HashMap::default(), None);
                    let def = TypeDef::new([], TypeSchema::Aggregate(agg.into()), []);
                    inputs.symbols.add_type(class_id, def);

                    let mappings = ClassMappings::new(index, mappings.methods);
                    inputs
                        .mapping
                        .classes
                        .insert(TypeApp::nullary(class_id), mappings);

                    virtual_map.insert(class_id, virtuals);
                }
                IndexedDefinition::Enum(idx, enm) => {
                    let name = bundle
                        .get_item(enm.name())
                        .ok_or_else(|| Error::MissingPoolItem("enum name", enm.name().into()))?;
                    let id = interner.intern(name.as_ref());

                    let mut enum_ = Enum::default();
                    for &value in enm.values() {
                        let val = bundle
                            .get_item(value)
                            .ok_or_else(|| Error::MissingPoolItem("enum value", value.into()))?;
                        let name = bundle.get_item(val.name()).ok_or_else(|| {
                            Error::MissingPoolItem("enum value name", val.name().into())
                        })?;
                        enum_.add_variant(assert_borrowed(name), val.value());
                    }
                    inputs
                        .symbols
                        .add_type(id, TypeDef::new([], TypeSchema::Enum(enum_.into()), []));
                    inputs.mapping.enums.insert(id, idx);
                }
                IndexedDefinition::Function(idx, func) => {
                    if func.class().is_some() {
                        continue;
                    }

                    let name = bundle.get_item(func.name()).ok_or_else(|| {
                        Error::MissingPoolItem("free function name", func.name().into())
                    })?;
                    let name = demangled_name(assert_borrowed(name));
                    let flags = FreeFunctionFlags::new()
                        .with_is_exec(func.flags().is_exec())
                        .with_is_native(func.flags().is_native());
                    let typ = load_function_type(func, bundle, interner)?;
                    let func = FreeFunction::new(flags, typ, [], None);
                    let id = inputs.symbols.add_free_function(name, func);
                    inputs.mapping.functions.insert(Signature::new(id, []), idx);
                }
                _ => {}
            }
        }

        Ok(inputs)
    }

    pub fn into_inner(self) -> (Symbols<'ctx>, PoolMappings<'ctx>) {
        (self.symbols, self.mapping)
    }
}

impl Default for CompilationInputs<'_> {
    fn default() -> Self {
        Self {
            symbols: Symbols::with_default_types(),
            mapping: PoolMappings::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct PoolMappings<'ctx> {
    classes: IndexMap<MonoType<'ctx>, ClassMappings>,
    functions: IndexMap<Signature<'ctx, FreeFunctionIndex>, PoolFunctionIndex>,
    enums: IndexMap<TypeId<'ctx>, PoolEnumIndex>,
    types: IndexMap<MonoType<'ctx>, PoolTypeIndex>,
}

impl<'ctx> PoolMappings<'ctx> {
    pub fn into_monomorphizer(self, sources: &'ctx SourceMap) -> Monomorphizer<'ctx> {
        Monomorphizer::new(
            sources,
            self.classes,
            self.enums,
            self.functions,
            self.types,
        )
    }
}

#[derive(Debug, Default)]
pub struct ClassMappings {
    index: PoolClassIndex,
    methods: IndexMap<FunctionIndex, PoolFunctionIndex>,
    aliases: IndexMap<FunctionIndex, PoolFunctionIndex>,
}

impl ClassMappings {
    #[inline]
    pub fn new(index: PoolClassIndex, methods: IndexMap<FunctionIndex, PoolFunctionIndex>) -> Self {
        Self {
            index,
            methods,
            aliases: IndexMap::default(),
        }
    }

    #[inline]
    pub fn index(&self) -> PoolClassIndex {
        self.index
    }

    #[inline]
    pub fn methods(&self) -> &IndexMap<FunctionIndex, PoolFunctionIndex> {
        &self.methods
    }

    #[inline]
    pub fn methods_mut(&mut self) -> &mut IndexMap<FunctionIndex, PoolFunctionIndex> {
        &mut self.methods
    }

    #[inline]
    pub fn aliases(&self) -> &IndexMap<FunctionIndex, PoolFunctionIndex> {
        &self.aliases
    }

    #[inline]
    pub fn set_alias(&mut self, index: FunctionIndex, alias: PoolFunctionIndex) {
        self.aliases.insert(index, alias);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature<'ctx, A> {
    id: A,
    types: Box<[MonoType<'ctx>]>,
}

impl<'ctx, A> Signature<'ctx, A> {
    #[inline]
    pub fn new(item: A, types: impl Into<Box<[MonoType<'ctx>]>>) -> Self {
        Self {
            id: item,
            types: types.into(),
        }
    }

    #[inline]
    pub fn id(&self) -> &A {
        &self.id
    }

    #[inline]
    pub fn types(&self) -> &[MonoType<'ctx>] {
        &self.types
    }

    pub fn display_args(&self) -> impl fmt::Display + use<'_, 'ctx, A> {
        DisplayFn::new(|f: &mut fmt::Formatter<'_>| {
            if self.types.is_empty() {
                return Ok(());
            }
            write!(f, "<{}>", sep_by(&self.types, ","))
        })
    }
}

fn load_type<'ctx, K: TypeKind>(
    typ: &PoolType,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
) -> Result<K::Type<'ctx>, Error> {
    let (id, arg) = match typ.kind() {
        PoolTypeKind::Class | PoolTypeKind::Primitive => {
            let name = bundle
                .get_item(typ.name())
                .ok_or_else(|| Error::MissingPoolItem("type name", typ.name().into()))?;
            return Ok(K::Type::from((interner.intern(name.as_ref()), Rc::new([]))));
        }
        &PoolTypeKind::Ref(inner) => {
            let inner = bundle
                .get_item(inner)
                .ok_or_else(|| Error::MissingPoolItem("referenced type", inner.into()))?;
            return load_type::<K>(inner, bundle, interner);
        }
        &PoolTypeKind::WeakRef(inner) => (predef::WREF, inner),
        &PoolTypeKind::Array(inner) => (predef::ARRAY, inner),
        &PoolTypeKind::ScriptRef(inner) => (predef::SCRIPT_REF, inner),
        &PoolTypeKind::StaticArray { element_type, size } => {
            let id = TypeId::array_with_size(size as usize)
                .ok_or(Error::UnsupportedStaticArray(size))?;
            (id, element_type)
        }
    };
    let arg = bundle
        .get_item(arg)
        .ok_or_else(|| Error::MissingPoolItem("type argument", arg.into()))?;
    let arg = load_type::<K>(arg, bundle, interner)?;
    Ok(K::Type::from((id, Rc::new([arg]))))
}

fn load_function_type<'ctx>(
    func: &PoolFunction<'ctx>,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
) -> Result<FunctionType<'ctx>, Error> {
    let typ = func
        .return_type()
        .map(|typ| {
            let typ = bundle
                .get_item(typ)
                .ok_or_else(|| Error::MissingPoolItem("function return type", typ.into()))?;
            load_type::<Immutable>(typ, bundle, interner)
        })
        .transpose()?;
    let params = func
        .parameters()
        .iter()
        .map(|&param| {
            let param = bundle
                .get_item(param)
                .ok_or_else(|| Error::MissingPoolItem("function parameter", param.into()))?;
            load_param(param, bundle, interner)
        })
        .collect::<Result<Box<_>, _>>()?;
    let return_t = typ.unwrap_or_else(|| Type::nullary(predef::VOID));
    Ok(FunctionType::new([], params, return_t))
}

fn load_param<'ctx>(
    param: &PoolParameter,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
) -> Result<Param<'ctx>, Error> {
    let name = bundle
        .get_item(param.name())
        .ok_or_else(|| Error::MissingPoolItem("parameter name", param.name().into()))?;
    let typ = bundle
        .get_item(param.type_())
        .ok_or_else(|| Error::MissingPoolItem("parameter type", param.type_().into()))?;
    let typ = load_type::<Immutable>(typ, bundle, interner)?;
    let flags = ParamFlags::new()
        .with_is_optional(param.flags().is_optional())
        .with_is_out(param.flags().is_out())
        .with_is_const(param.flags().is_const());
    Ok(Param::new(assert_borrowed(name), flags, typ, None))
}

#[track_caller]
fn assert_borrowed<'a>(cow: &Cow<'a, str>) -> &'a str {
    match cow {
        Cow::Borrowed(s) => s,
        Cow::Owned(_) => panic!("expected a borrowed str"),
    }
}

fn demangled_name(name: &str) -> &str {
    name.split_once(';').map_or(name, |(name, _)| name)
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("missing {0} in the pool at index {1}")]
    MissingPoolItem(&'static str, u32),
    #[error("unsupported static array with size {0}")]
    UnsupportedStaticArray(u32),
    #[error("duplicate field name at index {0}")]
    DuplicateField(u32),
}
