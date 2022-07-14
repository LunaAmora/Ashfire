#[macro_use] extern crate num_derive;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate num;

mod compiler;
mod logger;
use crate::compiler::parser::*;

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

    match args.command {
        Commands::Com { path, output: _ } =>
            if let Err(err) = compile_file(path) {
                error!("{:#}", err);
            },
    };
}
