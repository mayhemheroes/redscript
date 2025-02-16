use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::VecDeque;

use redscript_ast::{self as ast, Span};

use crate::lower::{InferredTypeApp, PolyType};
use crate::symbols::{FieldId, FreeFunctionIndex, MethodId};
use crate::types::{self, predef, TypeId};
use crate::{CoalesceError, RefType, Symbols};

#[derive(Debug)]
pub enum Stmt<'ctx> {
    Expr(Box<Expr<'ctx>>),
    Block(Block<'ctx>, Span),
    While(CondBlock<'ctx>, Span),
    Branches {
        branches: Box<[CondBlock<'ctx>]>,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    Switch {
        scrutinee: Box<Expr<'ctx>>,
        scrutinee_type: Type<'ctx>,
        branches: Box<[Case<'ctx>]>,
        default: Option<Block<'ctx>>,
        span: Span,
    },
    InitArray {
        local: Local,
        elements: Box<[Expr<'ctx>]>,
        element_type: Type<'ctx>,
        span: Span,
    },
    InitDefault {
        local: Local,
        typ: Type<'ctx>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Return(Option<Box<Expr<'ctx>>>, Span),
}

impl Stmt<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::Block(_, span)
            | Self::While(_, span)
            | Self::Branches { span, .. }
            | Self::Switch { span, .. }
            | Self::InitArray { span, .. }
            | Self::InitDefault { span, .. }
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Return(_, span) => *span,
        }
    }
}

#[derive(Debug)]
pub struct CondBlock<'ctx> {
    pub condition: Box<Expr<'ctx>>,
    pub block: Block<'ctx>,
}

impl<'ctx> CondBlock<'ctx> {
    #[inline]
    pub fn new(condition: impl Into<Box<Expr<'ctx>>>, block: Block<'ctx>) -> Self {
        Self {
            condition: condition.into(),
            block,
        }
    }
}

#[derive(Debug, Default)]
pub struct Block<'ctx> {
    pub stmts: VecDeque<Stmt<'ctx>>,
}

impl<'ctx> Block<'ctx> {
    #[inline]
    pub fn new(stmts: impl Into<VecDeque<Stmt<'ctx>>>) -> Self {
        Self {
            stmts: stmts.into(),
        }
    }

    pub fn span(&self) -> Option<Span> {
        let fst = self.stmts.front()?;
        let lst = self.stmts.back()?;
        Some(fst.span().merge(&lst.span()))
    }
}

#[derive(Debug)]
pub struct Case<'ctx> {
    pub constant: Const<'ctx>,
    pub block: Block<'ctx>,
}

impl<'ctx> Case<'ctx> {
    #[inline]
    pub fn new(constant: Const<'ctx>, block: Block<'ctx>) -> Self {
        Self { constant, block }
    }
}

#[derive(Debug)]
pub enum Expr<'ctx> {
    NewClass {
        class_type: TypeApp<'ctx>,
        span: Span,
    },
    NewStruct {
        values: Box<[Self]>,
        struct_type: TypeApp<'ctx>,
        span: Span,
    },
    NewClosure {
        closure: Box<Closure<'ctx>>,
        span: Span,
    },
    Call {
        call: Box<Call<'ctx>>,
        span: Span,
    },
    Assign {
        place: Box<Self>,
        expr: Box<Self>,
        span: Span,
    },
    Field {
        receiver: Box<Self>,
        receiver_type: TypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        field: FieldId<'ctx>,
        span: Span,
    },
    Index {
        array: Box<Self>,
        array_type: Type<'ctx>,
        index: Box<Self>,
        span: Span,
    },
    Conditional {
        condition: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
        span: Span,
    },
    DynCast {
        expr: Box<Self>,
        expr_type: Type<'ctx>,
        target_type: TypeApp<'ctx>,
        span: Span,
    },
    Local(Local, Span),
    Capture(Local, Span),
    Const(Const<'ctx>, Span),
    Null(Span),
}

impl<'ctx> Expr<'ctx> {
    pub fn span(&self) -> Span {
        match self {
            Self::NewClass { span, .. }
            | Self::NewStruct { span, .. }
            | Self::NewClosure { span, .. }
            | Self::Call { span, .. }
            | Self::Assign { span, .. }
            | Self::Field { span, .. }
            | Self::Index { span, .. }
            | Self::Conditional { span, .. }
            | Self::DynCast { span, .. }
            | Self::Local(_, span)
            | Self::Capture(_, span)
            | Self::Const(_, span)
            | Self::Null(span) => *span,
        }
    }

    pub fn is_prvalue(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Local(_, _) | Self::Capture(_, _) | Self::Field { .. } | Self::Index { .. } => {
                false
            }
            Self::Call { call, .. } => !matches!(
                &**call,
                Call::FreeFunction { function, .. }
                    if symbols[*function].intrinsic().is_some_and(|i| i == Intrinsic::Deref)
            ),
            _ => true,
        }
    }

    pub fn is_prvalue_ref(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Call { call, .. } => matches!(
                &**call,
                Call::FreeFunction { function, args, .. }
                    if symbols[*function].intrinsic().is_some_and(|i| i == Intrinsic::AsRef)
                        && matches!(&args[..], [arg] if arg.is_prvalue(symbols))
            ),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Call<'ctx> {
    FreeFunction {
        function: FreeFunctionIndex,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
    },
    Static {
        parent_id: TypeId<'ctx>,
        parent_type_args: Box<[Type<'ctx>]>,
        method: MethodId<'ctx>,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
    },
    Instance {
        receiver: Box<Expr<'ctx>>,
        receiver_type: TypeApp<'ctx>,
        receiver_ref: Option<RefType>,
        method: MethodId<'ctx>,
        type_args: Box<[Type<'ctx>]>,
        args: Box<[Expr<'ctx>]>,
        mode: CallMode,
    },
    Closure {
        closure: Box<Expr<'ctx>>,
        closure_type: TypeApp<'ctx>,
        args: Box<[Expr<'ctx>]>,
    },
}

#[derive(Debug, Clone)]
pub enum Const<'ctx> {
    Str(Cow<'ctx, str>),
    CName(Cow<'ctx, str>),
    Resource(Cow<'ctx, str>),
    TweakDbId(Cow<'ctx, str>),
    EnumVariant(FieldId<'ctx>),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Bool(bool),
}

impl<'ctx> Const<'ctx> {
    pub fn type_id(&self) -> TypeId<'ctx> {
        match self {
            Self::Str(_) => predef::STRING,
            Self::CName(_) => predef::CNAME,
            Self::Resource(_) => predef::RESOURCE,
            Self::TweakDbId(_) => predef::TWEAK_DB_ID,
            Self::EnumVariant(variant) => variant.parent(),
            Self::F32(_) => predef::FLOAT,
            Self::F64(_) => predef::DOUBLE,
            Self::I8(_) => predef::INT8,
            Self::I16(_) => predef::INT16,
            Self::I32(_) => predef::INT32,
            Self::I64(_) => predef::INT64,
            Self::U8(_) => predef::UINT8,
            Self::U16(_) => predef::UINT16,
            Self::U32(_) => predef::UINT32,
            Self::U64(_) => predef::UINT64,
            Self::Bool(_) => predef::BOOL,
        }
    }
}

impl<'ctx> From<&ast::Constant<'ctx>> for Const<'ctx> {
    fn from(ast: &ast::Constant<'ctx>) -> Self {
        match ast {
            ast::Constant::String(s) => Self::Str(s.clone()),
            ast::Constant::CName(s) => Self::CName(s.clone()),
            ast::Constant::Resource(s) => Self::Resource(s.clone()),
            ast::Constant::TweakDbId(s) => Self::TweakDbId(s.clone()),
            &ast::Constant::F32(f) => Self::F32(f),
            &ast::Constant::F64(f) => Self::F64(f),
            &ast::Constant::I32(i) => Self::I32(i),
            &ast::Constant::I64(i) => Self::I64(i),
            &ast::Constant::U32(u) => Self::U32(u),
            &ast::Constant::U64(u) => Self::U64(u),
            &ast::Constant::Bool(b) => Self::Bool(b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intrinsic {
    Equals,
    NotEquals,
    ArrayClear,
    ArraySize,
    ArrayResize,
    ArrayFindFirst,
    ArrayFindLast,
    ArrayContains,
    ArrayCount,
    ArrayPush,
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArrayGrow,
    ArrayErase,
    ArrayLast,
    ArraySort,
    ArraySortByPredicate,
    ToString,
    EnumInt,
    IntEnum,
    ToVariant,
    FromVariant,
    VariantIsRef,
    VariantIsArray,
    VariantTypeName,
    AsRef,
    Deref,
    RefToWeakRef,
    WeakRefToRef,
    IsDefined,
    NameOf,
}

impl From<Intrinsic> for &'static str {
    fn from(value: Intrinsic) -> Self {
        match value {
            Intrinsic::Equals => "Equals",
            Intrinsic::NotEquals => "NotEquals",
            Intrinsic::ArrayClear => "ArrayClear",
            Intrinsic::ArraySize => "ArraySize",
            Intrinsic::ArrayResize => "ArrayResize",
            Intrinsic::ArrayFindFirst => "ArrayFindFirst",
            Intrinsic::ArrayFindLast => "ArrayFindLast",
            Intrinsic::ArrayContains => "ArrayContains",
            Intrinsic::ArrayCount => "ArrayCount",
            Intrinsic::ArrayPush => "ArrayPush",
            Intrinsic::ArrayPop => "ArrayPop",
            Intrinsic::ArrayInsert => "ArrayInsert",
            Intrinsic::ArrayRemove => "ArrayRemove",
            Intrinsic::ArrayGrow => "ArrayGrow",
            Intrinsic::ArrayErase => "ArrayErase",
            Intrinsic::ArrayLast => "ArrayLast",
            Intrinsic::ArraySort => "ArraySort",
            Intrinsic::ArraySortByPredicate => "ArraySortByPredicate",
            Intrinsic::ToString => "ToString",
            Intrinsic::EnumInt => "EnumInt",
            Intrinsic::IntEnum => "IntEnum",
            Intrinsic::ToVariant => "ToVariant",
            Intrinsic::FromVariant => "FromVariant",
            Intrinsic::VariantIsRef => "VariantIsRef",
            Intrinsic::VariantIsArray => "VariantIsArray",
            Intrinsic::VariantTypeName => "VariantTypeName",
            Intrinsic::AsRef => "AsRef",
            Intrinsic::Deref => "Deref",
            Intrinsic::RefToWeakRef => "RefToWeakRef",
            Intrinsic::WeakRefToRef => "WeakRefToRef",
            Intrinsic::IsDefined => "IsDefined",
            Intrinsic::NameOf => "NameOf",
        }
    }
}

impl<'a> TryFrom<&'a str> for Intrinsic {
    type Error = UnknownIntrinsic<'a>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let res = match value {
            "Equals" => Intrinsic::Equals,
            "NotEquals" => Intrinsic::NotEquals,
            "ArrayClear" => Intrinsic::ArrayClear,
            "ArraySize" => Intrinsic::ArraySize,
            "ArrayResize" => Intrinsic::ArrayResize,
            "ArrayFindFirst" => Intrinsic::ArrayFindFirst,
            "ArrayFindLast" => Intrinsic::ArrayFindLast,
            "ArrayContains" => Intrinsic::ArrayContains,
            "ArrayCount" => Intrinsic::ArrayCount,
            "ArrayPush" => Intrinsic::ArrayPush,
            "ArrayPop" => Intrinsic::ArrayPop,
            "ArrayInsert" => Intrinsic::ArrayInsert,
            "ArrayRemove" => Intrinsic::ArrayRemove,
            "ArrayGrow" => Intrinsic::ArrayGrow,
            "ArrayErase" => Intrinsic::ArrayErase,
            "ArrayLast" => Intrinsic::ArrayLast,
            "ArraySort" => Intrinsic::ArraySort,
            "ArraySortByPredicate" => Intrinsic::ArraySortByPredicate,
            "ToString" => Intrinsic::ToString,
            "EnumInt" => Intrinsic::EnumInt,
            "IntEnum" => Intrinsic::IntEnum,
            "ToVariant" => Intrinsic::ToVariant,
            "FromVariant" => Intrinsic::FromVariant,
            "VariantIsRef" => Intrinsic::VariantIsRef,
            "VariantIsArray" => Intrinsic::VariantIsArray,
            "VariantTypeName" => Intrinsic::VariantTypeName,
            "AsRef" => Intrinsic::AsRef,
            "Deref" => Intrinsic::Deref,
            "RefToWeakRef" => Intrinsic::RefToWeakRef,
            "WeakRefToRef" => Intrinsic::WeakRefToRef,
            "IsDefined" => Intrinsic::IsDefined,
            "NameOf" => Intrinsic::NameOf,
            _ => return Err(UnknownIntrinsic(value)),
        };
        Ok(res)
    }
}

#[derive(Debug)]
pub struct UnknownIntrinsic<'a>(pub &'a str);

impl<'a> From<UnknownIntrinsic<'a>> for &'a str {
    fn from(UnknownIntrinsic(str): UnknownIntrinsic<'a>) -> Self {
        str
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Local {
    Var(u16),
    Param(u16),
    This,
}

impl Local {
    pub fn to_index(&self) -> u16 {
        match self {
            Self::This => 0,
            Self::Var(i) | Local::Param(i) => *i + 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalInfo<'ctx> {
    pub id: Local,
    pub typ: PolyType<'ctx>,
    pub span: Option<Span>,
}

impl<'ctx> LocalInfo<'ctx> {
    #[inline]
    pub fn new(id: Local, typ: PolyType<'ctx>, span: Option<Span>) -> Self {
        Self { id, typ, span }
    }
}

#[derive(Debug)]
pub struct Closure<'ctx> {
    pub typ: TypeApp<'ctx>,
    pub locals: Box<[LocalInfo<'ctx>]>,
    pub captures: Box<[Local]>,
    pub block: Block<'ctx>,
}

impl<'ctx> Closure<'ctx> {
    #[inline]
    pub fn new(
        typ: impl Into<TypeApp<'ctx>>,
        locals: impl Into<Box<[LocalInfo<'ctx>]>>,
        captures: impl Into<Box<[Local]>>,
        block: Block<'ctx>,
    ) -> Self {
        Self {
            typ: typ.into(),
            locals: locals.into(),
            captures: captures.into(),
            block,
        }
    }
}

#[derive(Debug)]
pub struct Type<'ctx>(Box<RefCell<TypeState<'ctx>>>);

impl<'ctx> Type<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::Type<'ctx>, CoalesceError<'ctx>> {
        let Self(state) = self;
        let typ = match &*state.borrow() {
            TypeState::Uninit(poly) => poly.coalesce(symbols)?,
            TypeState::Init(t) => return Ok(t.clone()),
        };
        *state.borrow_mut() = TypeState::Init(typ.clone());
        Ok(typ)
    }
}

impl<'ctx> From<PolyType<'ctx>> for Type<'ctx> {
    #[inline]
    fn from(poly: PolyType<'ctx>) -> Self {
        Self(Box::new(RefCell::new(TypeState::Uninit(poly))))
    }
}

#[derive(Debug)]
enum TypeState<'ctx> {
    Uninit(PolyType<'ctx>),
    Init(types::Type<'ctx>),
}

#[derive(Debug)]
pub struct TypeApp<'ctx>(Box<RefCell<TypeAppState<'ctx>>>);

impl<'ctx> TypeApp<'ctx> {
    pub fn coalesced(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> Result<types::TypeApp<'ctx>, CoalesceError<'ctx>> {
        let Self(state) = self;
        let typ = match &*state.borrow() {
            TypeAppState::Uninit(inferred) => inferred.coalesce(symbols)?,
            TypeAppState::Init(t) => return Ok(t.clone()),
        };
        *state.borrow_mut() = TypeAppState::Init(typ.clone());
        Ok(typ)
    }
}

impl<'ctx> From<InferredTypeApp<'ctx>> for TypeApp<'ctx> {
    #[inline]
    fn from(inferred: InferredTypeApp<'ctx>) -> Self {
        Self(Box::new(RefCell::new(TypeAppState::Uninit(inferred))))
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum CallMode {
    #[default]
    Normal,
    ForceStatic,
}

#[derive(Debug)]
enum TypeAppState<'ctx> {
    Uninit(InferredTypeApp<'ctx>),
    Init(types::TypeApp<'ctx>),
}
