#![feature(never_type)]
#![feature(const_trait_impl)]
#[macro_use] extern crate num_derive;
#[macro_use] extern crate log;
#[macro_use] extern crate firelib;
extern crate env_logger;
extern crate num;

mod compiler;
mod logger;

use crate::compiler::{parser::*, typechecker::*};
use anyhow::Result;
use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
    #[clap(flatten)]
    verbose: Verbosity<InfoLevel>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a `.fire` file to WebAssembly.
    Com {
        #[clap(parse(from_os_str))]
        path: PathBuf,
        #[clap(short, long)]
        output: Option<PathBuf>,
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
        Commands::Com { path, output } => compile_command(path, output),
    } {
        error!("{:#}", err);
    }
}

fn compile_command(path: PathBuf, _output: Option<PathBuf>) -> Result<()> {
    let program = compile_file(path)?;
    type_check(&program)
}
