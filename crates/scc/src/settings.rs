use std::borrow::Cow;
use std::fs::File;
use std::io::BufRead;
use std::path::{Path, PathBuf};
use std::{io, iter};

use file_id::get_file_id;

use crate::arguments::Arguments;

const BUNDLE_FILE: &str = "final.redscripts";
const BACKUP_BUNDLE_FILE: &str = "final.redscripts.bk";

const R6_DIR: &str = "r6";
const CACHE_DIR: &str = "cache";
const SCRIPTS_DIR: &str = "scripts";

pub(crate) const CACHE_FILE_EXT: &str = "redscripts";
pub(crate) const BACKUP_FILE_EXT: &str = "redscripts.bk";
pub(crate) const TIMESTAMP_FILE_EXT: &str = "redscripts.ts";

#[derive(Debug)]
pub struct SccSettings {
    root_dir: PathBuf,
    custom_cache_file: Option<PathBuf>,
    output_cache_file: Option<PathBuf>,
    additional_script_paths: Vec<PathBuf>,
}

impl SccSettings {
    pub fn from_root_dir_and_args(
        root_dir: impl Into<PathBuf>,
        args: Arguments,
    ) -> io::Result<Self> {
        let additional_script_paths = args
            .script_paths_file
            .as_deref()
            .map(|path| {
                io::BufReader::new(File::open(path)?)
                    .lines()
                    .map(|line| Ok(PathBuf::from(line?)))
                    .collect::<io::Result<Vec<_>>>()
            })
            .transpose()?
            .unwrap_or_default();

        Ok(Self {
            root_dir: root_dir.into(),
            custom_cache_file: args.cache_file.map(Into::into),
            output_cache_file: None,
            additional_script_paths,
        })
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    fn default_cache_file_path(&self) -> PathBuf {
        self.root_dir.join(R6_DIR).join(CACHE_DIR).join(BUNDLE_FILE)
    }

    fn default_scripts_dir_path(&self) -> PathBuf {
        self.root_dir.join(R6_DIR).join(SCRIPTS_DIR)
    }

    pub fn script_paths(&self) -> impl Iterator<Item = Cow<'_, Path>> {
        iter::once(self.default_scripts_dir_path().into())
            .chain(self.additional_script_paths.iter().map(Into::into))
    }

    pub fn cache_file_path(&self) -> Cow<'_, Path> {
        self.custom_cache_file
            .as_ref()
            .map_or_else(|| self.default_cache_file_path().into(), Into::into)
    }

    pub fn output_cache_file_path(&self) -> Cow<'_, Path> {
        self.output_cache_file
            .as_ref()
            .map_or_else(|| self.cache_file_path(), Into::into)
    }

    pub fn default_backup_cache_file_path(&self) -> PathBuf {
        self.root_dir
            .join(R6_DIR)
            .join(CACHE_DIR)
            .join(BACKUP_BUNDLE_FILE)
    }

    pub fn unmodified_cache_file_path(&self) -> PathBuf {
        let path = self.default_backup_cache_file_path();
        if path.exists() {
            path
        } else {
            path.with_extension(CACHE_FILE_EXT)
        }
    }

    pub fn does_have_separate_output_cache(&self) -> io::Result<bool> {
        let Some(output) = self.output_cache_file.as_ref() else {
            return Ok(false);
        };
        match (get_file_id(output), get_file_id(self.cache_file_path())) {
            (Ok(id1), Ok(id2)) => Ok(id1 != id2),
            (Err(err), Ok(_)) if err.kind() == io::ErrorKind::NotFound => Ok(true),
            (Err(err), _) | (_, Err(err)) => Err(err),
        }
    }
}
