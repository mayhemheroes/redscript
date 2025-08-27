mod cte;
mod diagnostic;
pub mod ir;
mod lower;
mod modules;
mod stages;
mod symbols;
pub mod types;
pub mod utils;
mod visitor;

pub use cte::Evaluator;
pub use diagnostic::{Diagnostic, DiagnosticLevel, Reporter, UnknownSource, pass};
pub use lower::{CoalesceError, Error as LowerError, PolyType, TypeError, TypeRef};
pub use redscript_ast as ast;
use redscript_parser as parser;
pub use stages::{LoweredClass, LoweredCompilationUnit, LoweredFunction};
use stages::{NameResolution, Scope};
pub use symbols::{
    Aggregate, AggregateFlags, Enum, Field, FieldFlags, FieldId, FieldIndex, FieldMap,
    FreeFunction, FreeFunctionFlags, FreeFunctionIndex, FunctionIndex, FunctionKind, FunctionMap,
    FunctionType, Method, MethodFlags, MethodId, MethodMap, Param, ParamFlags, QualifiedName,
    Symbols, TypeDef, TypeSchema, Visibility,
};
pub use types::{
    CtxVar, Immutable, Mono, MonoType, RefType, Type, TypeApp, TypeId, TypeIndex, TypeInterner,
    TypeKind, Variance, predef,
};

type IndexMap<K, V, S = hashbrown::DefaultHashBuilder> = indexmap::IndexMap<K, V, S>;
type IndexSet<K, S = hashbrown::DefaultHashBuilder> = indexmap::IndexSet<K, S>;
type FrozenIndexSet<K, S = hashbrown::DefaultHashBuilder> = elsa::FrozenIndexSet<K, S>;

pub type TypeScope<'scope, 'ctx> = IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>;
pub type CompileErrorReporter<'ctx> = Reporter<Diagnostic<'ctx>>;
type LowerReporter<'id> = Reporter<LowerError<'id>>;

pub fn infer_from_sources<'ctx>(
    sources: &'ctx ast::SourceMap,
    symbols: Symbols<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    let mods = parse_files(sources, reporter);
    let evaluator = Evaluator::from_modules(&mods);
    let (unit, symbols) = process_sources(mods, symbols, evaluator, reporter, interner);
    (unit, symbols)
}

pub fn parse_files<'ctx>(
    sources: &'ctx ast::SourceMap,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Vec<ast::SourceModule<'ctx>> {
    let (module, errs) = parser::parse_modules(sources.files().map(|(id, f)| (id, f.source())));
    reporter.report_many(errs);
    module
}

pub fn parse_file<'ctx>(
    id: ast::FileId,
    file: &'ctx ast::File,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Option<ast::SourceModule<'ctx>> {
    let (module, errs) = parser::parse_module(file.source(), id);
    reporter.report_many(errs);
    module
}

pub fn process_sources<'ctx>(
    modules: impl IntoIterator<Item = ast::SourceModule<'ctx>>,
    symbols: Symbols<'ctx>,
    evaluator: Evaluator<'ctx>,
    reporter: &mut CompileErrorReporter<'ctx>,
    interner: &'ctx TypeInterner,
) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
    let mut scope = Scope::new(&symbols);
    let mut resolution = NameResolution::new(modules, evaluator, symbols, reporter, interner);

    resolution.populate_globals(&mut scope);
    resolution.progress(&scope).finish(&scope, reporter)
}
