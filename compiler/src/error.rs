use std::{fmt, io};

use itertools::Itertools;
use peg::error::ExpectedSet;
use redscript::ast::{Ident, Intrinsic, Pos, Span};
use redscript::bundle::PoolError;
use thiserror::Error;

use crate::diagnostics::DisplayFn;
use crate::source_map::Files;

const MAX_RESOLUTION_ERRORS: usize = 6;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("syntax error, expected {0}")]
    SyntaxError(ExpectedSet, Span),
    #[error("compilation error: {0}")]
    CompileError(Cause, Span),
    #[error("constant pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("multiple errors")]
    MultipleErrors(Vec<(&'static str, Span)>),
    #[error("compile-time eval error: {0}")]
    CteError(&'static str, Span),
}

#[derive(Debug, Error)]
pub enum Cause {
    #[error("pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("can't coerce '{0}' into '{1}'")]
    TypeError(Ident, Ident),
    #[error("function '{0}' not found")]
    FunctionNotFound(Ident),
    #[error("member '{0}' not found on '{1}'")]
    MemberNotFound(Ident, Ident),
    #[error("method '{0}' not found on '{1}'")]
    MethodNotFound(Ident, Ident),
    #[error("class '{0}' not found")]
    ClassNotFound(Ident),
    #[error("cannot instantiate '{0}' because it's abstract")]
    InstantiatingAbstract(Ident),
    #[error("unresolved reference '{0}'")]
    UnresolvedReference(Ident),
    #[error("unresolved type '{0}'")]
    UnresolvedType(Ident),
    #[error("unresolved import '{0}'")]
    UnresolvedImport(Ident),
    #[error("module '{0}' has no members or does not exist")]
    UnresolvedModule(Ident),
    #[error("invalid arguments for annotation")]
    InvalidAnnotationArgs,
    #[error("type cannot be inferred here, try annotating the variable")]
    TypeAnnotationRequired,
    #[error("'{0}' has no members")]
    InvalidMemberAccess(Ident),
    #[error("{0} cannot be accessed as an array")]
    InvalidArrayAccess(Ident),
    #[error("{0} is not supported on '{1}'")]
    UnsupportedOperation(&'static str, Ident),
    #[error("expected {1} arguments for '{0}'")]
    InvalidArgCount(Ident, usize),
    #[error("void cannot be used as a value")]
    VoidCannotBeUsed,
    #[error("expected a value, found a {0}")]
    UnexpectedToken(&'static str),
    #[error("function should return '{0}'")]
    UnexpectedVoidReturn(Ident),
    #[error("function cannot return a value")]
    UnexpectedValueReturn,
    #[error("invalid use of '{0}', unexpected '{1}'")]
    InvalidIntrinsicUse(Intrinsic, Ident),
    #[error("this method is static, it cannot be used on an instance of an object")]
    InvalidStaticMethodCall,
    #[error("this method is not static, it must be used on an instance of an object")]
    InvalidNonStaticMethodCall,
    #[error("no 'this' available in a static context")]
    UnexpectedThis,
    #[error("{0} is not supported")]
    UnsupportedFeature(&'static str),
    #[error("symbol with this name is already defined")]
    SymbolRedefinition(Option<Pos>),
    #[error("field with this name is already defined")]
    FieldRedefinition,
    #[error("this function must have a body")]
    MissingBody,
    #[error("this function can't have have a body")]
    UnexpectedBody,
    #[error("native member is not allowed in a non-native context")]
    UnexpectedNative,
    #[error("cannot unify '{0}' and '{1}'")]
    UnificationFailed(Ident, Ident),
    #[error("'{0}' cannot be made persistent")]
    UnsupportedPersistent(Ident),
    #[error(r#"this value must be a constant (e.g. 1, "string")"#)]
    InvalidConstant,
    #[error(
        "arguments passed to '{0}' do not match any of the overloads:\n{overloads}{suffix}",
        overloads=.1.iter().take(MAX_RESOLUTION_ERRORS).format("\n"),
        suffix=if .1.len() > MAX_RESOLUTION_ERRORS {"\n...and more"} else {""}
    )]
    NoMatchingOverload(Ident, Box<[FunctionMatchError]>),
    #[error(
        "this signature does not match any existing method, make sure that the function qualifiers \
         and argument types are correct"
    )]
    NoMethodWithMatchingSignature,
    #[error("no method with this name exists on the target type")]
    NoMethodWithMatchingName,
    #[error(
        "a ref/wref type here contains a non-class type '{0}', refs/wrefs must always point to a \
         class"
    )]
    NonClassRef(Ident),
    #[error(
        "a type here refers to class '{0}' without indirection, class types must be used through \
         ref or wref"
    )]
    ClassWithNoIndirection(Ident),
    #[error("annotations on native functions are not allowed")]
    AnnotationOnNative,
    #[error("invalid signature used here")]
    InvalidSignature,
}

impl Cause {
    #[inline]
    pub fn with_span(self, span: Span) -> Error {
        Error::CompileError(self, span)
    }

    #[inline]
    pub fn code(&self) -> &'static str {
        match self {
            Self::PoolError(_) => "POOL_ERR",
            Self::TypeError(_, _)
            | Self::UnexpectedValueReturn
            | Self::UnexpectedVoidReturn(_)
            | Self::InvalidIntrinsicUse(_, _)
            | Self::UnificationFailed(_, _) => "TYPE_ERR",
            Self::FunctionNotFound(_) => "UNRESOLVED_FN",
            Self::MethodNotFound(_, _) | Self::NoMethodWithMatchingSignature | Self::NoMethodWithMatchingName => {
                "UNRESOLVED_METHOD"
            }
            Self::MemberNotFound(_, _) => "UNRESOLVED_MEMBER",
            Self::ClassNotFound(_) | Self::UnresolvedType(_) => "UNRESOLVED_TYPE",
            Self::UnresolvedReference(_) => "UNRESOLVED_REF",
            Self::UnresolvedImport(_) | Self::UnresolvedModule(_) => "UNRESOLVED_IMPORT",
            Self::InvalidArgCount(_, _) | Self::NoMatchingOverload(_, _) => "NO_MATCHING_OVERLOAD",
            Self::InstantiatingAbstract(_) => "NEW_ABSTRACT",
            Self::TypeAnnotationRequired => "TYPE_ANN_REQUIRED",
            Self::InvalidAnnotationArgs => "INVALID_ANN_USE",
            Self::InvalidMemberAccess(_) => "INVALID_MEMBER_ACCESS",
            Self::InvalidArrayAccess(_) => "INVALID_ARRAY_ACCESS",
            Self::VoidCannotBeUsed => "INVALID_VOID_USE",
            Self::InvalidStaticMethodCall => "INVALID_STATIC_USE",
            Self::InvalidNonStaticMethodCall => "INVALID_NONSTATIC_USE",
            Self::UnexpectedThis => "UNEXPECTED_THIS",
            Self::SymbolRedefinition(_) => "SYM_REDEFINITION",
            Self::FieldRedefinition => "FIELD_REDEFINITION",
            Self::MissingBody => "MISSING_BODY",
            Self::UnexpectedBody => "UNEXPECTED_BODY",
            Self::UnexpectedNative => "UNEXPECTED_NATIVE",
            Self::UnsupportedPersistent(_) => "INVALID_PERSISTENT",
            Self::InvalidConstant => "INVALID_CONSTANT",
            Self::NonClassRef(_) | Self::ClassWithNoIndirection(_) | Self::InvalidSignature => "INVALID_TYPE",
            Self::AnnotationOnNative
            | Self::UnsupportedFeature(_)
            | Self::UnsupportedOperation(_, _)
            | Self::UnexpectedToken(_) => "UNSUPPORTED",
        }
    }

    #[inline]
    pub fn display<'a>(&'a self, files: &'a Files) -> impl fmt::Display + 'a {
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| match self {
            &Cause::SymbolRedefinition(Some(pos)) => {
                let loc = files.lookup(Span::new(pos, pos)).expect("Unknown file");
                write!(f, "the name of this type conflicts with a type defined at '{loc}'",)
            }
            &Cause::SymbolRedefinition(None) => {
                write!(
                    f,
                    "the name of this type conflicts with a type defined in pre-compiled scripts"
                )
            }
            other => write!(f, "{}", other),
        })
    }
}

#[derive(Debug, Error)]
pub enum FunctionMatchError {
    #[error("{} argument: expected '{expected}', given '{given}'", NthArg(*index))]
    ParameterMismatch {
        given: Ident,
        expected: Ident,
        index: usize,
    },
    #[error("return type '{expected}' does not match '{given}'")]
    ReturnMismatch { given: Ident, expected: Ident },
    #[error("expected {min}-{max} arguments, given {given}")]
    ArgumentCountMismatch { given: usize, min: usize, max: usize },
}

struct NthArg(usize);

impl fmt::Display for NthArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "1st"),
            1 => write!(f, "2nd"),
            2 => write!(f, "3rd"),
            n => write!(f, "{}th", n + 1),
        }
    }
}

pub trait ResultSpan {
    type Output;

    fn with_span(self, span: Span) -> Self::Output;
}

impl<A> ResultSpan for std::result::Result<A, Cause> {
    type Output = std::result::Result<A, Error>;

    #[inline]
    fn with_span(self, span: Span) -> Self::Output {
        self.map_err(|err| err.with_span(span))
    }
}
