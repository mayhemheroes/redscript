use std::path::{Path, PathBuf};
use std::{env, fs, io};

use anyhow::{anyhow, Context};
use arguments::Arguments;
use fd_lock::RwLock;
use mimalloc::MiMalloc;
use redscript_compiler_api::{
    Compilation, FlushError, SaveError, SourceMap, SourceMapExt, TypeInterner,
};
use settings::{SccSettings, BACKUP_FILE_EXT, TIMESTAMP_FILE_EXT};
use timestamp::CompileTimestamp;
use vmap::Map;

mod arguments;
mod logger;
mod settings;
mod timestamp;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> anyhow::Result<()> {
    match Arguments::from_env_args(std::env::args()) {
        Ok(args) => {
            let r6 = args
                .scripts_dir
                .parent()
                .context("provided scripts directory has no parent directory")?;
            logger::setup(r6);

            let settings = SccSettings::from_r6_dir_and_args(r6.to_path_buf(), args);
            if let Err(err) = compile(&settings) {
                log::error!("An unexpected error ocurred: {err}");
                return Err(err);
            }
            Ok(())
        }
        Err(err) => {
            let r6 = env::current_dir()?
                .parent()
                .and_then(Path::parent)
                .map(|path| path.join("r6"))
                .filter(|path| path.exists())
                .ok_or_else(|| anyhow!("could not run scc: {err}"))?;
            logger::setup(&r6);

            log::error!("{err}");

            Err(err.into())
        }
    }
}

fn compile(settings: &SccSettings) -> anyhow::Result<()> {
    log::info!("REDscript 1.0.0 preview");

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

    let cache_file = prepare_cache(settings, &mut ts_file)?;
    let output_file = settings.output_cache_file_path();

    let mut sources = SourceMap::from_paths_recursively(settings.script_paths())?;
    log::info!("Compiling {} files:\n{}", sources.len(), &sources);
    sources.populate_boot_lib();

    let interner = TypeInterner::default();
    {
        let (mmap, _f) = Map::with_options().open(&cache_file)?;

        match Compilation::new(&mmap, &sources, &interner)?.flush(&output_file) {
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
                log::error!("Compilation failed");

                return Ok(());
            }
            Err(err) => anyhow::bail!("{err}"),
            Ok((_, diagnostics)) => {
                diagnostics.dump(&sources)?;
                log::info!("Compilation successful");
            }
        };
    };

    CompileTimestamp::try_from(&output_file.metadata()?)?.write(&mut *ts_file)?;

    Ok(())
}

fn prepare_cache(settings: &SccSettings, ts_file: &mut fs::File) -> anyhow::Result<PathBuf> {
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

                fs::rename(&backup_file, cache_file)
                    .context("failed to restore the backup file")?;
            }
            _ => {}
        }
        Ok(settings.output_cache_file_path().into_owned())
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
