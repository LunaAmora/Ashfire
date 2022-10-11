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

use crate::compiler::{generator::*, parser::*, typechecker::*, types::Program};

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
        /// Optimize the `.wasm` file to reduce it's size. (Needs Binaryen).
        #[arg(short = 'p', long)]
        opt: bool,
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
        Commands::Com { path, output, run, opt } => compile_command(path, output, run, opt),
    } {
        error!("{:#}", err);
    }
}

fn compile_command(path: PathBuf, output: Option<PathBuf>, run: bool, opt: bool) -> Result<()> {
    let mut program = Program::new();
    compile_file(&path, &mut program)?;
    type_check(&mut program)?;

    let mut out = output.unwrap_or(path);
    out.set_extension("wat");

    generate_wasm(&program, &out)?;

    let mut out_wasm = out.clone();
    out_wasm.set_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if opt {
        cmd_wait!("wasm-opt", "-O4", "--enable-multivalue", &out_wasm, "-o", &out_wasm);
    }
    if run {
        cmd_wait!("wasmtime", &out_wasm);
    }

    Ok(())
}
