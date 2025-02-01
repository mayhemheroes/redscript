use std::fmt;

use redscript_ast::Span;
use thiserror::Error;
use {redscript_ast as ast, redscript_parser as parser};

use crate::lower::{LowerResult, Poly, TypeError};
use crate::stages::FunctionAnnotation;
use crate::utils::fmt::{lowercase, sep_by, DisplayFn};
use crate::{cte, predef, CoalesceError, LowerError, Param, PolyType, Type, TypeId, Variance};

#[derive(Debug, Error)]
pub enum Diagnostic<'ctx> {
    #[error("{0}")]
    SyntaxError(#[from] parser::Error),
    #[error("{0}")]
    TypeError(LowerError<'ctx>),
    #[error("{0}")]
    CoalesceError(Box<CoalesceError<'ctx>>, Span),
    #[error("duplicate variant name")]
    DuplicateVariantName(Span),
    #[error("duplicate variant value")]
    DuplicateVariantValue(Span),
    #[error("value overflow")]
    ValueOverflow(Span),
    #[error("this function must have a body")]
    MissingFunctionBody(Span),
    #[error("this function cannot have a body")]
    UnexpectedFunctionBody(Span),
    #[error("{} qualifiers have no effect on this item", sep_by(.0.iter_names().map(|(name,_)| lowercase(name)), ", "))]
    UnusedItemQualifiers(ast::ItemQualifiers, Span),
    #[error("'{0}' is not a known intrinsic")]
    UnknownIntrinsic(&'ctx str, Span),
    #[error("items of this type are not allowed here")]
    UnexpectedItem(Span),
    #[error("base type must be a valid known type")]
    InvalidBaseType(Span),
    #[error("'{0}' is not a valid annotation in this context")]
    UnknownAnnotation(&'ctx str, Span),
    #[error("'{0}' could not be found")]
    ImportNotFound(&'ctx str, Span),
    #[error("this name is already defined in the scope")]
    NameRedefinition(Span),
    #[error("'{0}' is not a valid target for this annotation")]
    InvalidAnnotationType(&'ctx str, Span),
    #[error("could not find a method with a matching signature for the {0} annotation")]
    AnnotatedMethodNotFound(FunctionAnnotation<'ctx>, Span),
    #[error("the annotations on this function are incompatible with each other")]
    IncompatibleAnnotations(Span),
    #[error("this class is missing some required method implementation(s):\n{}", sep_by(.0, "\n"))]
    MissingMethodImpls(Box<[MissingMethod<'ctx>]>, Span),
    #[error("this class contains a duplicated implementation of the '{0}' method")]
    DuplicateMethod(&'ctx str, Span),
    #[error("this class overrides a final method '{0}'")]
    FinalMethodOverride(&'ctx str, Span),
    #[error("this class circularly extends itself")]
    CircularInheritance(Span),
    #[error("type {} expects {} type arguments", .0, .1)]
    InvalidTypeArgCount(TypeId<'ctx>, usize, Span),
    #[error("type {0} does not satisfy expected bound {1}")]
    UnsastisfiedBound(Box<Type<'ctx>>, Box<Type<'ctx>>, Span),
    #[error("this annotation attempts to modify a user-defined symbol, which is not allowed")]
    UserSymbolAnnotation(Span),
    #[error("the type '{0}' appears in {1} position, which is incompatible with its declaration")]
    InvalidVariance(&'ctx str, Variance, Span),
    #[error("this type cannot inherit from {0}")]
    IncompatibleBaseType(&'static str, Span),
    #[error("this annotation duplicates an existing method with the same signature")]
    DuplicateMethodAnnotation(Span),
    #[error("annotated methods cannot be generic")]
    GenericMethodAnnotation(Span),
    #[error("non-data types cannot have variance")]
    NonDataVariance(Span),
    #[error("struct methods must be static")]
    NonStaticStructMethod(Span),
    #[error("scripted types are not allowed to have native members")]
    NativeMemberOfScriptedType(Span),
    #[error("Strings, Variants and Resources cannot be persisted")]
    InvalidPersistentField(Span),
    #[error("fields cannot be added to structs")]
    StructFieldAddition(Span),
    #[error("{0}")]
    EvalFailed(#[from] cte::Error),
    #[error("the name of an implementation must be a valid identifier")]
    InvalidImplName(Span),
    #[error("the type of an implementation must have no free type variables")]
    InvalidImplType(Span),
    #[error("this implementation is a duplicate of a previous one")]
    DuplicateImpl(Span),
}

impl<'ctx> Diagnostic<'ctx> {
    pub fn is_fatal(&self) -> bool {
        !matches!(
            self,
            Self::UnusedItemQualifiers(_, _)
                | Self::DuplicateVariantValue(_)
                | Self::FinalMethodOverride(_, _)
        )
    }

    pub fn span(&self) -> Span {
        match self {
            Self::SyntaxError(err) => err.span(),
            Self::TypeError(err) => err.span(),
            Self::EvalFailed(err) => err.span(),
            Self::CoalesceError(_, span)
            | Self::DuplicateVariantName(span)
            | Self::DuplicateVariantValue(span)
            | Self::ValueOverflow(span)
            | Self::MissingFunctionBody(span)
            | Self::UnexpectedFunctionBody(span)
            | Self::UnusedItemQualifiers(_, span)
            | Self::UnknownIntrinsic(_, span)
            | Self::UnexpectedItem(span)
            | Self::InvalidBaseType(span)
            | Self::UnknownAnnotation(_, span)
            | Self::ImportNotFound(_, span)
            | Self::NameRedefinition(span)
            | Self::InvalidAnnotationType(_, span)
            | Self::AnnotatedMethodNotFound(_, span)
            | Self::IncompatibleAnnotations(span)
            | Self::MissingMethodImpls(_, span)
            | Self::DuplicateMethod(_, span)
            | Self::FinalMethodOverride(_, span)
            | Self::CircularInheritance(span)
            | Self::InvalidTypeArgCount(_, _, span)
            | Self::UnsastisfiedBound(_, _, span)
            | Self::UserSymbolAnnotation(span)
            | Self::InvalidVariance(_, _, span)
            | Self::IncompatibleBaseType(_, span)
            | Self::DuplicateMethodAnnotation(span)
            | Self::GenericMethodAnnotation(span)
            | Self::NonDataVariance(span)
            | Self::NonStaticStructMethod(span)
            | Self::NativeMemberOfScriptedType(span)
            | Self::InvalidPersistentField(span)
            | Self::StructFieldAddition(span)
            | Self::InvalidImplName(span)
            | Self::InvalidImplType(span)
            | Self::DuplicateImpl(span) => *span,
        }
    }

    pub fn display<'a>(
        &'a self,
        sources: &'a ast::SourceMap,
    ) -> Result<impl fmt::Display + use<'a, 'ctx>, UnknownSource> {
        let span = self.span();
        let file = sources.get(span.file).ok_or(UnknownSource(span))?;
        let start = file.lookup(span.start);
        let end = file.lookup(span.end);
        let line = file.line_contents(start.line).ok_or(UnknownSource(span))?;

        Ok(DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            writeln!(
                f,
                "At {}:{}:{}",
                file.path().display(),
                start.line + 1,
                start.col + 1
            )?;

            writeln!(f, "{}", line.trim_end())?;

            let pad = start.col;
            let underline_len = if start.line == end.line {
                (end.col - start.col).max(1)
            } else {
                3
            };
            writeln!(f, "{:>pad$}{:^>underline_len$}", "", "")?;
            writeln!(f, "{self}")
        }))
    }
}

impl<'ctx> From<LowerError<'ctx>> for Diagnostic<'ctx> {
    #[inline]
    fn from(err: LowerError<'ctx>) -> Self {
        Self::TypeError(err)
    }
}

#[derive(Debug)]
pub struct Reporter<A> {
    reported: Vec<A>,
}

impl<A> Reporter<A> {
    pub fn unwrap_err<A1, E1>(&mut self, res: Result<A1, E1>) -> Option<A1>
    where
        E1: Into<A>,
    {
        match res {
            Ok(res) => Some(res),
            Err(err) => {
                self.report(err.into());
                None
            }
        }
    }

    #[inline]
    pub fn report(&mut self, error: impl Into<A>) {
        self.reported.push(error.into());
    }

    pub fn report_many(&mut self, errors: impl IntoIterator<Item = impl Into<A>>) {
        self.reported.extend(errors.into_iter().map(Into::into));
    }

    #[inline]
    pub fn reported(&self) -> &[A] {
        &self.reported
    }

    #[inline]
    pub fn into_reported(self) -> Vec<A> {
        self.reported
    }
}

impl<E> Default for Reporter<E> {
    fn default() -> Self {
        Self {
            reported: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct MissingMethod<'ctx> {
    name: &'ctx str,
    params: Box<[Param<'ctx, Poly>]>,
    return_type: PolyType<'ctx>,
}

impl<'ctx> MissingMethod<'ctx> {
    pub fn new(
        name: &'ctx str,
        params: Box<[Param<'ctx, Poly>]>,
        return_type: PolyType<'ctx>,
    ) -> Self {
        Self {
            name,
            params,
            return_type,
        }
    }
}

impl fmt::Display for MissingMethod<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  func {}({}) ", self.name, sep_by(&self.params, ", "),)?;
        if !matches!(&self.return_type, PolyType::Mono(Type::Data(app)) if app.id() == predef::VOID)
        {
            write!(f, "-> {} ", self.return_type)?;
        }
        write!(f, "{{}}")
    }
}

#[derive(Debug, Error)]
#[error("the source of a diagnostic could not be determined (span: {0})")]
pub struct UnknownSource(Span);

pub trait ErrorWithSpan {
    type Result;
    fn with_span(self, span: Span) -> Self::Result;
}

impl<'id, A> ErrorWithSpan for Result<A, TypeError<'id>> {
    type Result = LowerResult<'id, A>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Result {
        self.map_err(|e| LowerError::Type(Box::new(e), span))
    }
}

impl<'id, A> ErrorWithSpan for Result<A, CoalesceError<'id>> {
    type Result = Result<A, Diagnostic<'id>>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Result {
        self.map_err(|e| Diagnostic::CoalesceError(Box::new(e), span))
    }
}
