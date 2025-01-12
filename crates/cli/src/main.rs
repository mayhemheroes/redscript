use std::path::PathBuf;

use argh::FromArgs;
use mimalloc::MiMalloc;
use redscript_compiler_api::{Compilation, FlushError, SourceMap, SourceMapExt, TypeInterner};
use vmap::Map;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// redscript command line interface
#[derive(Debug, FromArgs)]
struct Args {
    #[argh(subcommand)]
    command: Command,
}

#[derive(Debug, FromArgs)]
#[argh(subcommand)]
enum Command {
    // TODO: decompiler
    #[allow(unused)]
    Decompile(DecompileOpts),
    Compile(CompileOpts),
    Lint(LintOpts),
}

/// decompile a .redscripts file
#[allow(unused)]
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "decompile")]
struct DecompileOpts {
    /// path to an input .redscripts file
    #[argh(option, short = 'i')]
    input: PathBuf,
    /// path to an output file or directory
    #[argh(option, short = 'o')]
    output: PathBuf,
    /// output mode, use 'code' to print redscript code, 'ast' to print a direct representation
    /// of the AST, 'bytecode' to print individual bytecode instructions
    #[argh(option, short = 'm', default = "String::from(\"code\")")]
    mode: String,
    /// write individual files based on the stored file index instead of a single file
    #[argh(switch, short = 'f')]
    dump_files: bool,
    /// include implicit operations in the output (conversions etc.)
    #[argh(switch, short = 'v')]
    verbose: bool,
}

/// compile redscript source code into a .redscripts file
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "compile")]
struct CompileOpts {
    /// path to an input source file or directory
    #[argh(option, short = 's')]
    src: Vec<PathBuf>,
    /// path to a .redscripts file to use for incremental compilation
    #[argh(option, short = 'b')]
    bundle: PathBuf,
    /// path to an output .redscripts file
    #[argh(option, short = 'o')]
    output: PathBuf,
}

/// lint redscript source code
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "lint")]
struct LintOpts {
    /// path to an input source file or directory
    #[argh(option, short = 's')]
    src: Vec<PathBuf>,
    /// path to a .redscripts file to use for incremental compilation
    #[argh(option, short = 'b')]
    bundle: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let colors = fern::colors::ColoredLevelConfig::default();
    fern::Dispatch::new()
        .format(move |out, message, record| {
            let level = colors.color(record.level());
            out.finish(format_args!("[{}] {}", level, message));
        })
        .chain(std::io::stdout())
        .apply()?;

    let args: Args = argh::from_env();

    match args.command {
        Command::Decompile(_) => todo!(),
        Command::Compile(opts) => compile(opts),
        Command::Lint(opts) => lint(opts),
    }
}

fn compile(opts: CompileOpts) -> anyhow::Result<()> {
    let (map, _f) = Map::with_options().open(opts.bundle)?;
    let interner = TypeInterner::default();
    let mut sources = SourceMap::from_paths_recursively(&opts.src)?;
    sources.populate_boot_lib();

    match Compilation::new(&map, &sources, &interner)?.flush(opts.output) {
        Ok((_, diagnostics)) => {
            diagnostics.dump(&sources)?;
            log::info!("Compilation successful");
        }
        Err(FlushError::CompilationErrors(diagnostics)) => {
            diagnostics.dump(&sources)?;
            log::error!("Compilation failed");
        }
        Err(err) => anyhow::bail!("{err}"),
    };
    Ok(())
}

fn lint(opts: LintOpts) -> anyhow::Result<()> {
    let (map, _f) = Map::with_options().open(opts.bundle)?;
    let interner = TypeInterner::default();
    let mut sources = SourceMap::from_files(&opts.src)?;
    sources.populate_boot_lib();

    Compilation::new(&map, &sources, &interner)?
        .diagnostics()
        .dump(&sources)?;
    Ok(())
}
