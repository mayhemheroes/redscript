use std::env;

use redscript_ast::SourceMap;
use redscript_compiler_api::{Diagnostic, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn compilation_errors() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path.strip_prefix(&current).unwrap();
        let sources = SourceMap::from_files(&[relative]).unwrap();
        sources.populate_boot_lib();

        let interner = TypeInterner::default();
        let symbols = Symbols::with_default_types();

        let (_, _, diagnostics) = infer_from_sources(&sources, &interner, symbols);
        insta::assert_snapshot!(DisplayDiagnostics(diagnostics, &sources));
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
