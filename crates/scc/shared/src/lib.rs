use std::path::PathBuf;
use std::{env, fs, io};

use anyhow::Context;
use fd_lock::RwLock;
use output::extract_refs;
pub use output::{SccOutput, SourceRef, SourceRefType};
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{Compilation, FlushError, SaveError, SourceMapExt, TypeInterner};
use report::{CompilationFailure, ErrorReport};
pub use settings::SccSettings;
use settings::{BACKUP_FILE_EXT, TIMESTAMP_FILE_EXT};
use timestamp::CompileTimestamp;
use vmap::Map;

mod hints;
mod logger;
mod output;
mod report;
mod settings;
mod timestamp;

pub fn compile(settings: &SccSettings) -> anyhow::Result<SccOutput> {
    logger::setup(settings.root_dir());

    match compile_inner(settings) {
        Ok(output) => {
            log::info!("Compilation successful");
            Ok(output)
        }
        Err(err) => {
            log::error!("Compilation failed: {err}");
            if settings.should_show_error_report() {
                let report = ErrorReport::new(&err).to_string();
                msgbox::create("REDscript error", &report, msgbox::IconType::Error).ok();
            }
            Err(err)
        }
    }
}

fn compile_inner(settings: &SccSettings) -> anyhow::Result<SccOutput> {
    log::info!("Running REDscript {}", env!("CARGO_PKG_VERSION"));

    let cache_file = settings.cache_file_path();

    let ts_path = cache_file.with_extension(TIMESTAMP_FILE_EXT);
    let mut ts_lock = RwLock::new(
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&ts_path)
            .context("failed to open the timestamp file")?,
    );
    let mut ts_file = ts_lock
        .write()
        .context("failed to acquire a write lock on the timestamp file")?;

    let input_file = prepare_input_cache(settings, &mut ts_file)?;
    let output_file = settings.output_cache_file_path();

    let sources = SourceMap::from_paths_recursively(settings.script_paths())?;
    log::info!(
        "Compiling {} files:\n{}",
        sources.len(),
        sources.display_at(settings.root_dir())
    );
    sources.populate_boot_lib();

    let interner = TypeInterner::default();
    let (mmap, _f) = Map::with_options().open(&input_file)?;
    let refs = {
        match Compilation::new_with(&mmap, &sources, &interner, &[])?.flush(&output_file) {
            Err(FlushError::Write(SaveError::Mmap(err)))
                if err.kind() == io::ErrorKind::PermissionDenied =>
            {
                anyhow::bail!(
                    "could not write to '{}', make sure the file is not read-only",
                    output_file.display()
                );
            }
            Err(FlushError::CompilationErrors(diagnostics)) => {
                diagnostics.dump(&sources)?;

                return Err(CompilationFailure::new(diagnostics, &sources, settings)?.into());
            }
            Err(err) => anyhow::bail!("{err}"),
            Ok((syms, diagnostics)) => {
                diagnostics.dump(&sources)?;

                log::info!("Succesfully written '{}'", output_file.display());
                extract_refs(&syms, &interner)
            }
        }
    };

    CompileTimestamp::try_from(&output_file.metadata()?)?.write(&mut *ts_file)?;

    Ok(SccOutput::new(sources, interner, refs))
}

fn prepare_input_cache(settings: &SccSettings, ts_file: &mut fs::File) -> anyhow::Result<PathBuf> {
    let cache_file = settings.cache_file_path();
    if !cache_file.exists() {
        let cache_dir = cache_file
            .parent()
            .context("provided cache file path has no parent directory")?;
        fs::create_dir_all(cache_dir).context("failed to create the cache directory")?;

        let base = settings.unmodified_cache_file_path();
        fs::copy(&base, &cache_file).context("could not copy the base cache file")?;
    }

    let backup_file = cache_file.with_extension(BACKUP_FILE_EXT);
    if !backup_file.exists() {
        let base = settings.default_backup_cache_file_path();
        if base.exists() {
            log::info!("Re-initializing the backup file from {}", base.display());

            fs::copy(&base, &backup_file)
                .context("could not copy the base redscript backup file")?;
        }
    }

    let write_ts = cache_file
        .metadata()
        .and_then(|meta| CompileTimestamp::try_from(&meta))
        .context("failed to obtain a timestamp of the cache file")?;
    let saved_ts = CompileTimestamp::read(&mut *ts_file)
        .context("failed to read the existing timestamp file")?;

    if settings
        .does_have_separate_output_cache()
        .context("failed to check for output cache")?
    {
        match saved_ts {
            saved_ts if saved_ts != Some(write_ts) && backup_file.exists() => {
                log::info!("Removing a stale backup file at {}", backup_file.display());

                fs::remove_file(&backup_file).context("failed to remove a stale backup file")?;
            }
            _ if backup_file.exists() => {
                log::info!("Restoring the backup file to {}", cache_file.display());

                fs::rename(&backup_file, &cache_file)
                    .context("failed to restore the backup file")?;
            }
            _ => {}
        }
        Ok(cache_file.into_owned())
    } else {
        match saved_ts {
            None if backup_file.exists() => {
                log::info!("Previous cache backup file found");
            }
            saved_ts if saved_ts != Some(write_ts) => {
                log::info!(
                    "The final.redscripts file is not ours, copying it to {}",
                    backup_file.display()
                );

                fs::copy(&cache_file, &backup_file).context("failed to copy the cache file")?;
            }
            Some(_) if !backup_file.exists() => {
                anyhow::bail!(
                    "a compilation timestamp was found, but backup files are missing, your \
                     installation might be corrupted, you should remove the contents of 'r6/cache/'
                     directory and verify game files with Steam/GOG"
                );
            }
            _ => {}
        }
        Ok(backup_file)
    }
}
