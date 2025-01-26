use std::borrow::Cow;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::{fmt, ops, ptr};

use derive_where::derive_where;
use identity_hash::IdentityHashable;
use redscript_ast as ast;

use crate::utils::fmt::{sep_by, DisplayFn};
use crate::utils::ScopedMap;
use crate::{FrozenIndexSet, Symbols, TypeSchema};

pub(super) const MAX_FN_ARITY: usize = 8;
pub(super) const MAX_STATIC_ARRAY_SIZE: usize = 16;

pub type MonoType<'ctx> = TypeApp<'ctx, Mono>;

#[derive_where(Debug, Clone)]
pub enum Type<'ctx, K: TypeKind = Immutable> {
    Nothing,
    Data(TypeApp<'ctx, K>),
    Ctx(Rc<CtxVar<'ctx, K>>),
}

impl<'ctx, K: TypeKind> Type<'ctx, K> {
    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Self::Data(TypeApp::nullary(id))
    }

    #[inline]
    pub fn app(id: TypeId<'ctx>, args: impl Into<Rc<[K::Type<'ctx>]>>) -> Self {
        Self::Data(TypeApp::new(id, args))
    }

    pub fn upper_bound(&self) -> Option<&TypeApp<'ctx, K>> {
        match self {
            Self::Ctx(v) => v.upper().and_then(Self::upper_bound),
            Self::Data(t) => Some(t),
            _ => None,
        }
    }

    #[inline]
    pub fn unwrap_ref(&self) -> Option<&K::Type<'ctx>> {
        Some(self.strip_ref()?.1)
    }

    pub fn strip_ref(&self) -> Option<(RefType, &K::Type<'ctx>)> {
        self.upper_bound().and_then(TypeApp::strip_ref)
    }

    pub fn ref_type(&self) -> Option<RefType> {
        self.upper_bound().and_then(TypeApp::ref_type)
    }

    pub fn is_primitive(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Data(typ) => matches!(
                symbols[typ.id()].schema(),
                TypeSchema::Primitive | TypeSchema::Enum(_)
            ),
            _ => false,
        }
    }

    #[inline]
    pub fn display_compact(&self) -> impl fmt::Display + use<'_, 'ctx, K> {
        self.display(true)
    }

    fn display(&self, compact: bool) -> impl fmt::Display + use<'_, 'ctx, K> {
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| match self {
            Self::Nothing => write!(f, "Nothing"),
            Self::Data(t) => t.display(compact).fmt(f),
            Self::Ctx(v) => write!(f, "{}", v.name),
        })
    }
}

impl<'ctx> Type<'ctx> {
    #[inline]
    pub fn unwrap_ref_or_self(&self) -> &Self {
        self.unwrap_ref().map_or(self, |t| t)
    }

    pub fn mono(&self, env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>) -> MonoType<'ctx> {
        match self {
            Self::Ctx(var) => env
                .get(var.name())
                .expect("unresolved type variable")
                .clone(),
            Self::Data(typ) => typ.mono(env),
            Self::Nothing => MonoType::nullary(predef::NOTHING),
        }
    }
}

impl<'ctx> From<(TypeId<'ctx>, Rc<[Type<'ctx>]>)> for Type<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[Type<'ctx>]>)) -> Self {
        Self::app(id, args)
    }
}

impl<K: TypeKind> PartialEq for Type<'_, K> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nothing, Self::Nothing) => true,
            (Self::Data(a), Self::Data(b)) => a == b,
            (Self::Ctx(a), Self::Ctx(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl<K: TypeKind> Eq for Type<'_, K> {}

impl<'ctx> From<(TypeId<'ctx>, Rc<[MonoType<'ctx>]>)> for MonoType<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[MonoType<'ctx>]>)) -> Self {
        Self::new(id, args)
    }
}

impl<K: TypeKind> fmt::Display for Type<'_, K> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

pub trait TypeKind {
    type Type<'ctx>: fmt::Debug
        + fmt::Display
        + Clone
        + PartialEq
        + Eq
        + From<(TypeId<'ctx>, Rc<[Self::Type<'ctx>]>)>;
}

#[derive(Debug)]
pub struct Immutable;

impl TypeKind for Immutable {
    type Type<'ctx> = Type<'ctx>;
}

#[derive(Debug)]
pub struct Mono;

impl TypeKind for Mono {
    type Type<'ctx> = MonoType<'ctx>;
}

#[derive_where(Debug, Clone, PartialEq, Eq)]
pub struct TypeApp<'ctx, K: TypeKind = Immutable> {
    id: TypeId<'ctx>,
    args: Rc<[K::Type<'ctx>]>,
}

impl<'ctx, K: TypeKind> TypeApp<'ctx, K> {
    #[inline]
    pub fn new(id: TypeId<'ctx>, args: impl Into<Rc<[K::Type<'ctx>]>>) -> Self {
        Self {
            id,
            args: args.into(),
        }
    }

    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Self::new(id, [])
    }

    #[inline]
    pub fn into_type(self) -> Type<'ctx, K> {
        Type::Data(self)
    }

    #[inline]
    pub fn id(&self) -> TypeId<'ctx> {
        self.id
    }

    #[inline]
    pub fn args(&self) -> &[K::Type<'ctx>] {
        &self.args
    }

    #[inline]
    pub fn strip_ref(&self) -> Option<(RefType, &K::Type<'ctx>)> {
        match (self.ref_type(), &*self.args) {
            (Some(rt), [arg]) => Some((rt, arg)),
            _ => None,
        }
    }

    #[inline]
    pub fn ref_type(&self) -> Option<RefType> {
        match self.id {
            id if id == predef::WREF => Some(RefType::Weak),
            id if id == predef::SCRIPT_REF => Some(RefType::Script),
            _ => None,
        }
    }

    pub fn type_env<'scope>(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> ScopedMap<'scope, &'ctx str, K::Type<'ctx>> {
        symbols[self.id()]
            .vars()
            .zip(self.args().iter().cloned())
            .collect()
    }

    pub(super) fn display(&self, compact: bool) -> impl fmt::Display + use<'_, 'ctx, K> {
        let sep = if compact { "," } else { ", " };
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            write!(f, "{}", self.id)?;
            if !self.args.is_empty() {
                write!(f, "<{}>", sep_by(&self.args[..], sep))?;
            }
            Ok(())
        })
    }
}

impl<'ctx> TypeApp<'ctx> {
    pub fn mono(&self, env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>) -> MonoType<'ctx> {
        let args = self.args().iter().map(|t| t.mono(env)).collect::<Rc<_>>();
        MonoType::new(self.id(), args)
    }
}

impl<'ctx> MonoType<'ctx> {
    pub fn instantiate_as(
        &self,
        target: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> Option<Cow<'_, Self>> {
        let mut cur = Cow::Borrowed(self);
        while cur.id() != target {
            cur = Cow::Owned(cur.instantiate_base(symbols)?);
        }
        Some(cur)
    }

    pub fn instantiate_base(&self, symbols: &Symbols<'ctx>) -> Option<Self> {
        let class = &symbols[self.id()];
        assert_eq!(
            class.vars().len(),
            self.args().len(),
            "type args should match the underlying type"
        );
        let base = class.schema().base_type()?;
        let env = class.vars().zip(self.args().iter().cloned()).collect();
        Some(base.mono(&env))
    }
}

impl fmt::Display for TypeApp<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

impl fmt::Display for MonoType<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(true).fmt(f)
    }
}

impl Hash for MonoType<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.args.hash(state);
    }
}

#[derive_where(Debug)]
pub struct CtxVar<'ctx, K: TypeKind = Immutable> {
    name: &'ctx str,
    variance: Variance,
    lower: Option<Type<'ctx, K>>,
    upper: Option<Type<'ctx, K>>,
}

impl<'ctx, K: TypeKind> CtxVar<'ctx, K> {
    #[inline]
    pub fn new(
        name: &'ctx str,
        variance: Variance,
        lower: Option<Type<'ctx, K>>,
        upper: Option<Type<'ctx, K>>,
    ) -> Self {
        Self {
            name,
            variance,
            lower,
            upper,
        }
    }

    #[inline]
    pub fn name(&self) -> &'ctx str {
        self.name
    }

    #[inline]
    pub fn variance(&self) -> Variance {
        self.variance
    }

    #[inline]
    pub fn lower(&self) -> Option<&Type<'ctx, K>> {
        self.lower.as_ref()
    }

    #[inline]
    pub fn upper(&self) -> Option<&Type<'ctx, K>> {
        self.upper.as_ref()
    }
}

impl<K: TypeKind> Hash for CtxVar<'_, K> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<K: TypeKind> fmt::Display for CtxVar<'_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    #[default]
    Covariant,
    Contravariant,
    Invariant,
}

impl Variance {
    #[inline]
    pub fn combined(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            _ => Variance::Invariant,
        }
    }
}

impl ops::Not for Variance {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Covariant => Self::Contravariant,
            Self::Contravariant => Self::Covariant,
            Self::Invariant => Self::Invariant,
        }
    }
}

impl ops::Mul<Variance> for Variance {
    type Output = Self;

    fn mul(self, rhs: Variance) -> Self::Output {
        match (self, rhs) {
            (Variance::Invariant, _) | (_, Variance::Invariant) => Variance::Invariant,
            (Variance::Covariant, Variance::Covariant)
            | (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
            _ => Variance::Contravariant,
        }
    }
}

impl From<ast::Variance> for Variance {
    fn from(v: ast::Variance) -> Self {
        match v {
            ast::Variance::Covariant => Variance::Covariant,
            ast::Variance::Contravariant => Variance::Contravariant,
            ast::Variance::Invariant => Variance::Invariant,
        }
    }
}

impl fmt::Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variance::Covariant => f.write_str("covariant"),
            Variance::Contravariant => f.write_str("contravariant"),
            Variance::Invariant => f.write_str("invariant"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefType {
    Weak,
    Script,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId<'id>(&'id str);

impl TypeId<'_> {
    pub fn is_fn(&self) -> bool {
        predef::FN_BY_ARITY.contains(self)
    }

    #[inline]
    pub fn fn_with_arity(arity: usize) -> Option<Self> {
        predef::FN_BY_ARITY.get(arity).copied()
    }

    #[inline]
    pub fn array_with_size(size: usize) -> Option<Self> {
        match size {
            0 => Some(predef::ARRAY0),
            1 => Some(predef::ARRAY1),
            2 => Some(predef::ARRAY2),
            3 => Some(predef::ARRAY3),
            4 => Some(predef::ARRAY4),
            5 => Some(predef::ARRAY5),
            6 => Some(predef::ARRAY6),
            7 => Some(predef::ARRAY7),
            8 => Some(predef::ARRAY8),
            9 => Some(predef::ARRAY9),
            10 => Some(predef::ARRAY10),
            11 => Some(predef::ARRAY11),
            12 => Some(predef::ARRAY12),
            13 => Some(predef::ARRAY13),
            14 => Some(predef::ARRAY14),
            15 => Some(predef::ARRAY15),
            16 => Some(predef::ARRAY16),
            // special cases needed to load the full game cache
            30 => Some(predef::ARRAY30),
            32 => Some(predef::ARRAY32),
            45 => Some(predef::ARRAY45),
            100 => Some(predef::ARRAY100),
            128 => Some(predef::ARRAY128),
            _ => None,
        }
    }

    pub fn static_array_size(&self) -> Option<u8> {
        match *self {
            id if id == predef::ARRAY0 => Some(0),
            id if id == predef::ARRAY1 => Some(1),
            id if id == predef::ARRAY2 => Some(2),
            id if id == predef::ARRAY3 => Some(3),
            id if id == predef::ARRAY4 => Some(4),
            id if id == predef::ARRAY5 => Some(5),
            id if id == predef::ARRAY6 => Some(6),
            id if id == predef::ARRAY7 => Some(7),
            id if id == predef::ARRAY8 => Some(8),
            id if id == predef::ARRAY9 => Some(9),
            id if id == predef::ARRAY10 => Some(10),
            id if id == predef::ARRAY11 => Some(11),
            id if id == predef::ARRAY12 => Some(12),
            id if id == predef::ARRAY13 => Some(13),
            id if id == predef::ARRAY14 => Some(14),
            id if id == predef::ARRAY15 => Some(15),
            id if id == predef::ARRAY16 => Some(16),
            id if id == predef::ARRAY30 => Some(30),
            id if id == predef::ARRAY32 => Some(32),
            id if id == predef::ARRAY45 => Some(45),
            id if id == predef::ARRAY100 => Some(100),
            id if id == predef::ARRAY128 => Some(128),
            _ => None,
        }
    }
}

impl<'id> TypeId<'id> {
    #[inline]
    pub fn as_str(&self) -> &'id str {
        self.0
    }
}

impl PartialEq for TypeId<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl Eq for TypeId<'_> {}

impl Hash for TypeId<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0.as_ptr(), state);
    }
}

impl IdentityHashable for TypeId<'_> {}

impl AsRef<str> for TypeId<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl fmt::Display for TypeId<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

pub struct TypeInterner {
    names: FrozenIndexSet<Cow<'static, str>>,
}

impl TypeInterner {
    pub fn intern(&self, name: impl AsRef<str> + Into<String>) -> TypeId<'_> {
        match self.names.get(name.as_ref()) {
            Some(str) => TypeId(str),
            None => TypeId(self.names.insert(Cow::Owned(name.into()))),
        }
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self {
            names: predef::ALL_TYPES
                .iter()
                .map(|id| Cow::Borrowed(id.0))
                .collect(),
        }
    }
}

pub mod predef {
    use super::*;

    macro_rules! types {
        ($($id:ident => $name:literal),*) => {
            $(pub static $id: TypeId<'static> = TypeId($name);)*

            pub(super) static ALL_TYPES: &[TypeId<'static>] = &[$($id),*];
        };
    }

    types! {
        STRING => "String",
        CNAME => "CName",
        RESOURCE => "ResRef",
        TWEAK_DB_ID => "TweakDBID",
        FLOAT => "Float",
        DOUBLE => "Double",
        INT8 => "Int8",
        INT16 => "Int16",
        INT32 => "Int32",
        INT64 => "Int64",
        UINT8 => "Uint8",
        UINT16 => "Uint16",
        UINT32 => "Uint32",
        UINT64 => "Uint64",
        BOOL => "Bool",
        VARIANT => "Variant",
        ISCRIPTABLE => "IScriptable",

        VOID => "Void",
        NOTHING => "Nothing",

        REF => "ref",
        WREF => "wref",
        SCRIPT_REF => "script_ref",
        ARRAY => "array",

        // aliases for static arrays with specific sizes
        ARRAY0 => "array0",
        ARRAY1 => "array1",
        ARRAY2 => "array2",
        ARRAY3 => "array3",
        ARRAY4 => "array4",
        ARRAY5 => "array5",
        ARRAY6 => "array6",
        ARRAY7 => "array7",
        ARRAY8 => "array8",
        ARRAY9 => "array9",
        ARRAY10 => "array10",
        ARRAY11 => "array11",
        ARRAY12 => "array12",
        ARRAY13 => "array13",
        ARRAY14 => "array14",
        ARRAY15 => "array15",
        ARRAY16 => "array16",
        // special cases needed to load the full game cache,
        // these won't be needed if something like const generics is added
        ARRAY30 => "array30",
        ARRAY32 => "array32",
        ARRAY45 => "array45",
        ARRAY100 => "array100",
        ARRAY128 => "array128",

        FUNCTION0 => "Function0",
        FUNCTION1 => "Function1",
        FUNCTION2 => "Function2",
        FUNCTION3 => "Function3",
        FUNCTION4 => "Function4",
        FUNCTION5 => "Function5",
        FUNCTION6 => "Function6",
        FUNCTION7 => "Function7",
        FUNCTION8 => "Function8"
    }

    pub(super) static FN_BY_ARITY: &[TypeId<'static>] = &[
        predef::FUNCTION0,
        predef::FUNCTION1,
        predef::FUNCTION2,
        predef::FUNCTION3,
        predef::FUNCTION4,
        predef::FUNCTION5,
        predef::FUNCTION6,
        predef::FUNCTION7,
        predef::FUNCTION8,
    ];

    pub(crate) static STATIC_ARRAY_TYPES: &[TypeId<'static>] = &[
        predef::ARRAY0,
        predef::ARRAY1,
        predef::ARRAY2,
        predef::ARRAY3,
        predef::ARRAY4,
        predef::ARRAY5,
        predef::ARRAY6,
        predef::ARRAY7,
        predef::ARRAY8,
        predef::ARRAY9,
        predef::ARRAY10,
        predef::ARRAY11,
        predef::ARRAY12,
        predef::ARRAY13,
        predef::ARRAY14,
        predef::ARRAY15,
        predef::ARRAY16,
        predef::ARRAY30,
        predef::ARRAY32,
        predef::ARRAY45,
        predef::ARRAY100,
        predef::ARRAY128,
    ];
}
