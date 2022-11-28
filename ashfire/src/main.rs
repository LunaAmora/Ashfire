#[macro_use] extern crate log;

use std::{
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
};

use ashfire_lib::{
    compile, logger,
    target::{Target, TargetConfig},
};
use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use firelib::{cmd_wait, Result};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    #[command(flatten)]
    verbose: Verbosity<InfoLevel>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a `.fire` file to WebAssembly.
    Com {
        #[arg(value_name = "FILE")]
        path: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Run the `.wasm` file with a runtime.
        #[arg(short, long)]
        run: bool,
        /// Decompile the `.wasm` file back into the `.wat`.
        #[arg(short, long)]
        wat: bool,
        /// Sets the compilation target.
        #[arg(short, long, value_enum, default_value_t = Target::Wasi)]
        target: Target,
        /// Sets the Wasi runtime to be used by the `-r` option.
        #[arg(long, default_value_t = format!("wasmtime"))]
        runtime: String,
    },
}

fn main() {
    let args = Cli::parse();
    if let Some(level) = args.verbose.log_level() {
        logger::formatted_builder()
            .filter_level(level.to_level_filter())
            .init();
    }

    if let Err(err) = match args.command {
        Commands::Com { path, output, run, wat, target, runtime } => {
            compile_command(&path, output.as_deref(), run, wat, target, runtime)
        }
    } {
        error!("{:#}", err);
    }
}

pub fn compile_command(
    path: &Path, output: Option<&Path>, run: bool, wat: bool, target: Target, runtime: String,
) -> Result<()> {
    let out = output.unwrap_or(path).with_extension("wat");

    let run_config = TargetConfig::new(target, runtime, run);

    let writer = BufWriter::new(File::create(&out)?);
    compile(path, writer, &run_config)?;

    info!("Generated {:?}", out);
    let out_wasm = out.with_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if wat {
        cmd_wait!("wasm2wat", &out_wasm, "-o", &out);
    }

    run_config.run(out_wasm)
}
