#![feature(const_trait_impl)]
#[macro_use] extern crate num_derive;
#[macro_use] extern crate log;

mod compiler;
mod logger;

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use firelib::cmd_wait;

use crate::compiler::types::Program;

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
        /// Sets the runtime to be used by the `-r` option.
        #[arg(short = 't', long, default_value_t = format!("wasmtime"))]
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
        Commands::Com { path, output, run, wat, runtime } => {
            compile_command(path, output, run, wat, runtime)
        }
    } {
        error!("{:#}", err);
    }
}

fn compile_command(
    path: PathBuf, output: Option<PathBuf>, run: bool, wat: bool, runtime: String,
) -> Result<()> {
    let out = output.unwrap_or_else(|| path.clone()).with_extension("wat");

    Program::new()
        .compile_file(&path)?
        .type_check()?
        .generate_wasm(&out)?;

    let out_wasm = out.with_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if wat {
        cmd_wait!("wasm2wat", &out_wasm, "-o", &out);
    }
    if run {
        cmd_wait!(runtime, &out_wasm);
    }

    Ok(())
}
