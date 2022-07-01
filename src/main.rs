#[macro_use] extern crate log;
extern crate env_logger;

mod logger;
mod compiler;
use crate::compiler::parser::*;

use std::path::PathBuf;
use clap_verbosity_flag::{Verbosity, InfoLevel};
use clap::{Subcommand, Parser};

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
        output: Option<PathBuf>
    }
}

fn main() {
    let args = Cli::parse();
    if let Some(level) = args.verbose.log_level() {
        logger::formatted_builder()
            .filter_level(level.to_level_filter())
            .init();
    }

    match args.command {
        Commands::Com { path, output: _ } => {
            if let Err(err) = compile_file(path) {
                error!("{:#}", err);
            }
        },
    };
}
