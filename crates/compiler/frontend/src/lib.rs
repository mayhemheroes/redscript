mod cte;
mod diagnostic;
pub mod ir;
mod lower;
mod modules;
mod stages;
mod symbols;
pub mod types;
pub mod utils;

pub use diagnostic::{Diagnostic, Reporter, UnknownSource};
pub use lower::{CoalesceError, Error as LowerError, PolyType, TypeRef};
pub use redscript_ast as ast;
use redscript_parser as parser;
pub use stages::{LoweredClass, LoweredCompilationUnit, LoweredFunction};
use stages::{NameResolution, Scope};
pub use symbols::{
    Aggregate, AggregateFlags, Enum, Field, FieldFlags, FieldId, FieldIndex, FieldMap,
    FreeFunction, FreeFunctionFlags, FreeFunctionIndex, FunctionIndex, FunctionKind, FunctionMap,
    FunctionType, Method, MethodFlags, MethodId, MethodMap, Param, ParamFlags, QualifiedName,
    Symbols, TypeDef, TypeSchema,
};
pub use types::{
    predef, CtxVar, Immutable, Mono, MonoType, RefType, Type, TypeApp, TypeId, TypeIndex,
    TypeInterner, TypeKind, Variance,
};

type IndexMap<K, V, S = hashbrown::DefaultHashBuilder> = indexmap::IndexMap<K, V, S>;
type IndexSet<K, S = hashbrown::DefaultHashBuilder> = indexmap::IndexSet<K, S>;
type FrozenIndexSet<K, S = hashbrown::DefaultHashBuilder> = elsa::FrozenIndexSet<K, S>;

pub type TypeScope<'scope, 'ctx> = IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>;
pub type CompileErrorReporter<'ctx> = Reporter<Diagnostic<'ctx>>;
type LowerReporter<'id> = Reporter<LowerError<'id>>;

pub fn infer_from_sources<'ctx>(
    sources: &'ctx ast::SourceMap,
    interner: &'ctx TypeInterner,
    symbols: Symbols<'ctx>,
) -> (
    LoweredCompilationUnit<'ctx>,
    Symbols<'ctx>,
    Vec<Diagnostic<'ctx>>,
) {
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_sources(sources, &mut reporter);
    process_sources(modules, interner, symbols, reporter)
}

pub fn parse_sources<'ctx>(
    sources: &'ctx ast::SourceMap,
    reporter: &mut CompileErrorReporter<'ctx>,
) -> Vec<ast::SourceModule<'ctx>> {
    let mut modules = vec![];
    for (id, file) in sources.files() {
        let (module, errs) = parser::parse_module(file.source(), id);
        reporter.report_many(errs);

        if let Some(module) = module {
            modules.push(module);
        }
    }
    modules
}

pub fn process_sources<'ctx>(
    modules: Vec<ast::SourceModule<'ctx>>,
    interner: &'ctx TypeInterner,
    symbols: Symbols<'ctx>,
    reporter: CompileErrorReporter<'ctx>,
) -> (
    LoweredCompilationUnit<'ctx>,
    Symbols<'ctx>,
    Vec<Diagnostic<'ctx>>,
) {
    let mut scope = Scope::new(&symbols);
    let mut resolution = NameResolution::new(modules, symbols, reporter, interner);

    resolution.populate_globals(&mut scope);
    resolution.progress(&scope).finish(&scope)
}
