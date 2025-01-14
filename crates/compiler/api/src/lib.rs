use std::fmt;
use std::path::Path;

pub use redscript_ast::{SourceMap, Span};
use redscript_compiler_backend::{
    AssembleError, CompilationInputs, Monomorphizer, PoolError, PoolMappings,
};
use redscript_compiler_frontend::{infer_from_sources, UnknownSource};
pub use redscript_compiler_frontend::{Diagnostic, LoweredCompilationUnit, Symbols, TypeInterner};
use redscript_io::byte;
pub use redscript_io::{SaveError, ScriptBundle};
use thiserror::Error;

#[derive(Debug)]
pub struct Compilation<'ctx> {
    symbols: Symbols<'ctx>,
    mappings: PoolMappings<'ctx>,
    unit: LoweredCompilationUnit<'ctx>,
    bundle: ScriptBundle<'ctx>,
    diagnostics: Diagnostics<'ctx>,
}

impl<'ctx> Compilation<'ctx> {
    pub fn new(
        bundle: &'ctx [u8],
        sources: &'ctx SourceMap,
        interner: &'ctx TypeInterner,
    ) -> Result<Self, Error> {
        let bundle = ScriptBundle::from_bytes(bundle)?;
        let (symbols, mappings) = CompilationInputs::load(&bundle, interner)?.into_inner();
        let (unit, symbols, mut diagnostics) = infer_from_sources(sources, interner, symbols);

        diagnostics.sort_by_key(Diagnostic::is_fatal);

        Ok(Self {
            symbols,
            mappings,
            unit,
            bundle,
            diagnostics: Diagnostics(diagnostics),
        })
    }

    pub fn flush(
        mut self,
        path: impl AsRef<Path>,
    ) -> Result<(Symbols<'ctx>, Diagnostics<'ctx>), FlushError<'ctx>> {
        if self.diagnostics.has_fatal_errors() {
            return Err(FlushError::CompilationErrors(self.diagnostics));
        }

        Monomorphizer::from(self.mappings).monomorphize(
            &self.unit,
            &self.symbols,
            &mut self.bundle,
        )?;
        self.bundle.into_writeable().save(path)?;
        Ok((self.symbols, self.diagnostics))
    }

    pub fn symbols(&self) -> &Symbols<'ctx> {
        &self.symbols
    }

    pub fn unit(&self) -> &LoweredCompilationUnit<'ctx> {
        &self.unit
    }

    pub fn bundle(&self) -> &ScriptBundle<'ctx> {
        &self.bundle
    }

    pub fn diagnostics(&self) -> &Diagnostics<'ctx> {
        &self.diagnostics
    }
}

#[derive(Debug)]
pub struct Diagnostics<'ctx>(Vec<Diagnostic<'ctx>>);

impl<'ctx> Diagnostics<'ctx> {
    pub fn into_vec(self) -> Vec<Diagnostic<'ctx>> {
        self.0
    }

    pub fn as_slice(&self) -> &[Diagnostic<'ctx>] {
        &self.0
    }

    pub fn has_fatal_errors(&self) -> bool {
        self.0.iter().any(Diagnostic::is_fatal)
    }

    pub fn dump(&self, sources: &SourceMap) -> Result<(), UnknownSource> {
        let mut warnings = 0;
        let mut errors = 0;

        for diagnostic in self.as_slice() {
            if diagnostic.is_fatal() {
                log::error!("{}", diagnostic.display(sources)?);
                errors += 1;
            } else {
                log::warn!("{}", diagnostic.display(sources)?);
                warnings += 1;
            }
        }
        log::info!("{} warnings, {} errors", warnings, errors);
        Ok(())
    }
}

impl<'ctx> From<Vec<Diagnostic<'ctx>>> for Diagnostics<'ctx> {
    fn from(diagnostics: Vec<Diagnostic<'ctx>>) -> Self {
        Self(diagnostics)
    }
}

impl fmt::Display for Diagnostics<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_slice()
            .iter()
            .try_for_each(|d| writeln!(f, "{}", d))
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("pool error: {0}")]
    Pool(#[from] PoolError),
    #[error("serialization error: {0}")]
    Decoding(#[from] byte::Error),
}

#[derive(Debug, Error)]
pub enum FlushError<'ctx> {
    #[error("fatal diagnostis found")]
    CompilationErrors(Diagnostics<'ctx>),
    #[error("code generation error: {0}")]
    Assemble(AssembleError<'ctx>),
    #[error("write error: {0}")]
    Write(#[from] SaveError),
}

impl<'ctx> From<AssembleError<'ctx>> for FlushError<'ctx> {
    fn from(err: AssembleError<'ctx>) -> Self {
        Self::Assemble(err)
    }
}

pub trait SourceMapExt {
    fn populate_boot_lib(&mut self);
}

impl SourceMapExt for SourceMap {
    fn populate_boot_lib(&mut self) {
        self.push_front(
            "boot.reds",
            include_str!("../../../../assets/reds/boot.reds"),
        );
    }
}
