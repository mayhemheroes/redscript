use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;

const DOTFILE_NAME: &str = ".redscript";

#[derive(Debug, Deserialize)]
pub struct Dotfile {
    #[serde(default = "default_roots")]
    pub source_roots: Vec<PathBuf>,
    #[serde(default)]
    pub format: FormatConfig,
}

impl Dotfile {
    pub fn load_or_default(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        match fs::read_to_string(path.as_ref().join(DOTFILE_NAME)) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Self::default()),
            Err(err) => Err(err.into()),
            Ok(content) => Ok(toml::from_str(&content)?),
        }
    }
}

impl Default for Dotfile {
    fn default() -> Self {
        Self {
            source_roots: default_roots(),
            format: FormatConfig::default(),
        }
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct FormatConfig {
    pub indent: Option<u16>,
    pub max_width: Option<u16>,
    pub max_chain_fields: Option<u8>,
    pub max_chain_calls: Option<u8>,
    pub max_chain_operators: Option<u8>,
    pub max_chain_total: Option<u8>,
}

fn default_roots() -> Vec<PathBuf> {
    vec![PathBuf::from(".")]
}
