use std::path::{Path, PathBuf};
use std::{env, fs};

use hashbrown::HashMap;
use leon::{Template, vals};
use serde::Deserialize;
use toml::Value;

const DOTFILE_NAME: &str = ".redscript";

#[derive(Debug, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Dotfile {
    #[serde(default = "default_roots")]
    pub source_roots: Vec<String>,
    #[serde(default)]
    pub format: FormatConfig,
    #[serde(default)]
    pub tools: HashMap<String, Value>,
}

impl Dotfile {
    pub fn expanded_source_roots(&self) -> Vec<PathBuf> {
        self.source_roots
            .iter()
            .map(|template| {
                let normalized = template.replace('\\', "/");
                let Ok(template) = Template::parse(&normalized) else {
                    return template.into();
                };
                template
                    .render(&vals(|key| Some(env::var(key).unwrap_or_default().into())))
                    .expect("should render template")
                    .into()
            })
            .collect()
    }

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
            tools: HashMap::new(),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Deserialize)]
pub struct FormatConfig {
    pub indent: Option<u16>,
    pub max_width: Option<u16>,
    pub max_chain_fields: Option<u8>,
    pub max_chain_calls: Option<u8>,
    pub max_chain_operators: Option<u8>,
    pub max_chain_total: Option<u8>,
}

fn default_roots() -> Vec<String> {
    vec![String::from(".")]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_dotfile() {
        let content = r#"
            source_roots = ["src", "lib"]
            format.indent = 4
        "#;

        let dotfile: Dotfile = toml::from_str(content).unwrap();
        assert_eq!(
            dotfile,
            Dotfile {
                source_roots: vec![String::from("src"), String::from("lib")],
                format: FormatConfig {
                    indent: Some(4),
                    ..Default::default()
                },
                tools: HashMap::new(),
            }
        )
    }

    #[test]
    fn parse_dotfile_with_tools() {
        let content = r#"
            source_roots = ["src", "lib"]
            format.indent = 4

            [tools.whatever]
            field = "value"

            [tools.whatever.nested]
            another_field = "another_value"
        "#;

        let dotfile: Dotfile = toml::from_str(content).unwrap();
        assert_eq!(
            dotfile.source_roots,
            vec![String::from("src"), String::from("lib")]
        );
        assert_eq!(
            dotfile.format,
            FormatConfig {
                indent: Some(4),
                ..Default::default()
            }
        );
    }

    #[test]
    fn expanded_source_roots() {
        let dotfile = Dotfile {
            source_roots: vec![String::from("{CARGO_MANIFEST_DIR}/src")],
            format: FormatConfig::default(),
            tools: HashMap::new(),
        };

        let expanded = dotfile.expanded_source_roots();
        assert_eq!(
            expanded,
            vec![PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src")]
        );
    }

    #[test]
    fn expanded_source_roots_with_backslashes() {
        let dotfile = Dotfile {
            source_roots: vec![String::from("{CARGO_MANIFEST_DIR}\\src")],
            format: FormatConfig::default(),
            tools: HashMap::new(),
        };

        let expanded = dotfile.expanded_source_roots();
        assert_eq!(
            expanded,
            vec![PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src")]
        );
    }
}
