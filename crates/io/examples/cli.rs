use std::path::PathBuf;

use argh::FromArgs;
use redscript_io::ScriptBundle;
use vmap::Map;

#[derive(FromArgs)]
/// Example CLI tool options
struct Opts {
    /// input file path
    #[argh(option)]
    input: PathBuf,
    /// output file path
    #[argh(option)]
    output: PathBuf,
}

fn main() {
    let opts = argh::from_env::<Opts>();

    let (map, _file) = Map::with_options().open(opts.input).unwrap();
    ScriptBundle::from_bytes(&map)
        .unwrap()
        .into_writeable()
        .save(opts.output)
        .unwrap();
}
