use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use std::{fmt, iter, ptr};

use identity_hash::IdentityHashable;

use crate::lower::error::{CoalesceError, InferResult, TypeError};
use crate::lower::simplify::Simplifier;
use crate::symbols::{AnyBaseType, AnySupertype, BaseMode};
use crate::types::{CtxVar, RefType, Type, TypeApp, TypeId, Variance, predef};
use crate::utils::ScopedMap;
use crate::{Symbols, TypeKind};

#[derive(Debug)]
pub struct Poly;

impl TypeKind for Poly {
    type Type<'ctx> = PolyType<'ctx>;
}

pub type InferredType<'ctx> = Type<'ctx, Poly>;
pub type InferredTypeApp<'ctx> = TypeApp<'ctx, Poly>;
pub type InferredCtxVar<'ctx> = CtxVar<'ctx, Poly>;

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

    pub fn from_id(id: TypeId<'ctx>, symbols: &Symbols<'ctx>) -> Self {
        let class = &symbols[id];
        let args = class
            .params()
            .iter()
            .map(|_| PolyType::fresh())
            .collect::<Rc<_>>();
        TypeApp::new(id, args)
    }

    pub fn instantiate_as<Mode: BaseMode>(
        self,
        target: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> Option<InferredTypeApp<'ctx>> {
        let mut cur = self;
        while cur.id() != target {
            cur = cur.instantiate_base::<Mode>(symbols)?;
        }
        Some(cur)
    }

    pub fn instantiate_base<Mode: BaseMode>(
        self,
        symbols: &Symbols<'ctx>,
    ) -> Option<InferredTypeApp<'ctx>> {
        let class = &symbols[self.id()];
        let args = self
            .args()
            .iter()
            .cloned()
            .chain(iter::repeat_with(PolyType::fresh));
        let env = class.vars().zip(args).collect();
        TypeApp::from_type_with_env(Mode::base(class.schema().as_aggregate()?)?, &env).ok()
    }

    pub fn base_type_env<'scope, 'sym, Mode: BaseMode>(
        self,
        target: TypeId<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Option<ScopedMap<'scope, &'ctx str, PolyType<'ctx>>> {
        Some(
            self.instantiate_as::<Mode>(target, symbols)?
                .type_env(symbols),
        )
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
            .instantiate_as::<AnyBaseType>(id, symbols)
            .expect("should always match the lub type");
        let rhs = other
            .clone()
            .instantiate_as::<AnyBaseType>(id, symbols)
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

    fn constrain<Mode: BaseMode>(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Nothing, _) => {}
            (Self::Data(lhs), Self::Data(rhs)) => {
                let l0 = lhs
                    .clone()
                    .instantiate_as::<Mode>(rhs.id(), symbols)
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), other.clone()))?;
                let r0 = rhs;
                for (v, (l, r)) in symbols[r0.id()]
                    .params()
                    .iter()
                    .zip(l0.args().iter().zip(r0.args()))
                {
                    let res = match v.variance() {
                        Variance::Covariant => l.constrain::<AnySupertype>(r, symbols),
                        Variance::Contravariant => r.constrain::<AnySupertype>(l, symbols),
                        Variance::Invariant => l.constrain_invariant(r, symbols),
                    };
                    res.map_err(|err| TypeError::Nested(err.into(), self.clone(), other.clone()))?;
                }
            }
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => {}
            _ => return Err(TypeError::Mismatch(self.clone(), other.clone())),
        };
        Ok(())
    }

    pub fn constrain_invariant(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.constrain::<AnyBaseType>(other, symbols)?;
        other.constrain::<AnyBaseType>(self, symbols)
    }

    fn is_subtype_compatible(
        &self,
        other: &Type<'ctx>,
        variance: Variance,
        symbols: &Symbols<'ctx>,
    ) -> bool {
        #[inline]
        fn check<'ctx>(
            lhs: TypeId<'ctx>,
            rhs: TypeId<'ctx>,
            variance: Variance,
            symbols: &Symbols<'ctx>,
        ) -> bool {
            match variance {
                Variance::Covariant => symbols.has_base_type(lhs, rhs),
                Variance::Contravariant => symbols.has_base_type(rhs, lhs),
                Variance::Invariant => lhs == rhs,
            }
        }

        match (self, other) {
            (Self::Nothing, _) | (_, Type::Ctx(_)) => true,
            (Self::Data(lhs), Type::Data(rhs)) if lhs.id() == rhs.id() => lhs
                .args()
                .iter()
                .zip(rhs.args())
                .zip(symbols[rhs.id()].params())
                .all(|((l, r), t)| l.is_subtype_compatible(r, variance * t.variance(), symbols)),
            (Self::Data(lhs), Type::Data(rhs)) => check(lhs.id(), rhs.id(), variance, symbols),
            (Self::Ctx(lhs), Type::Data(rhs)) => lhs
                .upper()
                .and_then(|typ| typ.upper_bound())
                .is_some_and(|lhs| check(lhs.id(), rhs.id(), variance, symbols)),
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
    pub fn fresh() -> Self {
        Self::Var(Var::fresh())
    }

    #[inline]
    pub fn with_bounds(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        Self::Var(Var::new(lower, upper))
    }

    pub fn force_upper_bound(
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

    pub fn upper_bound(&self, symbols: &Symbols<'ctx>) -> Option<Cow<'_, InferredTypeApp<'ctx>>> {
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

    pub fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
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

    #[inline]
    pub fn constrain_base(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        self.constrain::<AnyBaseType>(other, symbols)
    }

    pub fn constrain<Mode: BaseMode>(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => l.constrain::<Mode>(r, symbols),
            (Self::Mono(l), Self::Var(r)) => r.add_lower(l, symbols),
            (Self::Var(l), Self::Mono(r)) => l.add_upper(r, symbols),
            (Self::Var(l), Self::Var(r)) => l.unify(r, symbols),
        }
    }

    fn constrain_invariant(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => l.constrain_invariant(r, symbols),
            (Self::Mono(l), Self::Var(r)) => {
                r.add_lower(l, symbols)?;
                r.constrain_lower(l, symbols)
            }
            (Self::Var(l), Self::Mono(r)) => {
                l.add_upper(r, symbols)?;
                l.constrain_by_upper(r, symbols)
            }
            (Self::Var(l), Self::Var(r)) => l.unify(r, symbols),
        }
    }

    pub fn coerce(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, Option<Coercion>> {
        match (self.strip_ref(symbols), other.strip_ref(symbols)) {
            (Some((from, pointee)), None) => {
                pointee.constrain::<AnyBaseType>(other, symbols)?;
                Ok(Some(Coercion::FromRef(from)))
            }
            (None, Some((to, pointee))) => {
                self.constrain::<AnyBaseType>(&pointee, symbols)?;
                Ok(Some(Coercion::IntoRef(to)))
            }
            _ if matches!(other.upper_bound(symbols), Some(app) if app.id() == predef::VARIANT)
                && matches!(self.upper_bound(symbols), Some(app) if app.id() != predef::VARIANT) =>
            {
                Ok(Some(Coercion::IntoVariant))
            }
            _ => {
                self.constrain::<AnyBaseType>(other, symbols)?;
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
    pub fn strip_ref(&self, symbols: &Symbols<'ctx>) -> Option<(RefType, PolyType<'ctx>)> {
        self.upper_bound(symbols)
            .as_deref()
            .and_then(TypeApp::strip_ref)
            .map(|(rt, typ)| (rt, typ.clone()))
    }

    pub fn ref_type(&self, symbols: &Symbols<'ctx>) -> Option<RefType> {
        self.upper_bound(symbols)
            .as_deref()
            .and_then(TypeApp::ref_type)
    }

    pub fn is_subtype_compatible(
        &self,
        other: &Type<'ctx>,
        variance: Variance,
        symbols: &Symbols<'ctx>,
    ) -> bool {
        match self {
            Self::Mono(typ) => typ.is_subtype_compatible(other, variance, symbols),
            Self::Var(var) => {
                var.lower().is_subtype_compatible(other, variance, symbols)
                    && var
                        .upper()
                        .is_none_or(|u| u.is_subtype_compatible(other, !variance, symbols))
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
    pub fn key(&self) -> VarKey<'ctx> {
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
    pub fn lower(&self) -> InferredType<'ctx> {
        self.repr().0.borrow().lower.clone()
    }

    #[inline]
    pub fn upper(&self) -> Option<InferredType<'ctx>> {
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
        repr.lower.constrain::<AnySupertype>(ub, symbols)
    }

    fn constrain_lower(
        &self,
        ub: &InferredType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.repr()
            .0
            .borrow()
            .lower
            .constrain::<AnySupertype>(ub, symbols)
    }

    fn add_lower(&self, lb: &InferredType<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        // TODO: occurs check
        let repr = self.repr();
        let mut repr = repr.0.borrow_mut();
        repr.lower = repr.lower.lub(lb, symbols)?;
        if let Some(ub) = &repr.upper {
            lb.constrain::<AnySupertype>(ub, symbols)?;
        }
        Ok(())
    }

    fn constrain_by_upper(
        &self,
        lb: &InferredType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.repr()
            .0
            .borrow()
            .upper
            .iter()
            .try_for_each(|ub| lb.constrain::<AnySupertype>(ub, symbols))
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
pub struct VarKey<'ctx>(*mut VarState<'ctx>);

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

#[derive(Debug, Clone, Copy)]
pub enum Coercion {
    FromRef(RefType),
    IntoRef(RefType),
    IntoVariant,
}
