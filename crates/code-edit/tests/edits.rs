use std::{env, fs};

use redscript_code_edit::{CodeEdit, TextEdit};
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    CompileErrorReporter, SourceMapExt, Symbols, TypeInterner, infer_from_sources,
};

#[test]
fn code_edits() {
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

        let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);

        let reported = reporter.into_reported();
        let edits = reported
            .iter()
            .filter_map(CodeEdit::from_diagnostic)
            .map(|e| TextEdit::from_code_edit(&e, &symbols, &sources))
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();

        let mut body = fs::read_to_string(path).unwrap();
        TextEdit::apply_many(edits, &mut body);
        insta::assert_snapshot!(body);
    });
}
