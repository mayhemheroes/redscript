use std::fmt;
use std::path::{Path, PathBuf};

use hashbrown::HashSet;
use redscript_compiler_api::{Diagnostics, SourceMap};
use thiserror::Error;

#[derive(Debug)]
pub struct ErrorReport<'ctx> {
    cause: anyhow::Error,
    root: &'ctx Path,
}

impl<'ctx> ErrorReport<'ctx> {
    pub fn new(cause: anyhow::Error, root: &'ctx Path) -> Self {
        Self { cause, root }
    }
}

impl fmt::Display for ErrorReport<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "REDscript compilation has failed.")?;
        writeln!(f)?;

        if let Some(source) = self.cause.downcast_ref::<CompilationFailed>() {
            writeln!(f, "Fatal errors were found in the files listed below:")?;
            for file in &source.sources {
                let path = file.strip_prefix(self.root).unwrap_or(file);
                writeln!(f, "{}", path.display())?;
            }
            writeln!(f)?;
            writeln!(
                f,
                "You should check if these mods are outdated and update them if possible. They may \
                 also be incompatible with the current version of the game, in which case you \
                 should remove them and try again."
            )?;
        } else {
            writeln!(f, "Reason: {}", self.cause)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
#[error("fatal errors found")]
pub struct CompilationFailed {
    sources: Vec<PathBuf>,
}

impl CompilationFailed {
    pub fn new(diagnostics: &Diagnostics<'_>, sources: &SourceMap) -> Self {
        let sources = diagnostics
            .into_iter()
            .filter(|d| d.is_fatal())
            .map(|d| d.span().file)
            .collect::<HashSet<_>>()
            .into_iter()
            .filter_map(|f| Some(sources.get(f)?.path().to_owned()))
            .collect();
        Self { sources }
    }
}
