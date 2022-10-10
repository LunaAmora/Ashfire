#![feature(const_trait_impl)]
#[macro_use] extern crate num_derive;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate num;

mod compiler;
mod logger;

use std::{path::PathBuf, process::Command};

use anyhow::Result;
use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};

use crate::compiler::{generator::*, parser::*, typechecker::*};

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
    let mut program = compile_file(&path)?;
    type_check(&mut program)?;

    let mut out = output.unwrap_or(path);
    out.set_extension("wat");

    generate_wasm(&program, out.clone())?;

    let mut out_wasm = out.clone();
    out_wasm.set_extension("wasm");

    info!("Running wat2wasm");
    Command::new("wat2wasm")
        .arg(out)
        .arg("-o")
        .arg(out_wasm.clone())
        .spawn()?
        .wait()?;

    if opt {
        info!("Running wasm-opt");
        Command::new("wasm-opt")
            .arg("-O4")
            .arg("--enable-multivalue")
            .arg(out_wasm.clone())
            .arg("-o")
            .arg(out_wasm.clone())
            .spawn()?
            .wait()?;
    }

    if run {
        info!("Running wasmtime");
        Command::new("wasmtime").arg(out_wasm).spawn()?.wait()?;
    }

    Ok(())
}
