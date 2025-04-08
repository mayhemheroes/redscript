use std::env;

use redscript_ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostic, SourceMapExt, Symbols, TypeInterner,
};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn compilation_errors() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path
            .strip_prefix(&current)
            .unwrap()
            .to_string_lossy()
            .replace("\\", "/");
        let sources = SourceMap::from_files(&[relative]).unwrap();
        sources.populate_boot_lib();

        let interner = TypeInterner::default();
        let symbols = Symbols::with_default_types();
        let mut reporter = CompileErrorReporter::default();

        let (_, _) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
        insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
    });
}

struct DisplayDiagnostics<'ctx>(Vec<Diagnostic<'ctx>>, &'ctx SourceMap);

impl std::fmt::Display for DisplayDiagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .try_for_each(|diagnostic| writeln!(f, "{}", diagnostic.display(self.1).unwrap()))
    }
}
