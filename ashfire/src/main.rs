#![feature(const_trait_impl)]
#![feature(try_blocks)]
#[macro_use] extern crate num_derive;
#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

mod compiler;
mod logger;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use firelib::anyhow::Result;

use crate::compiler::program::Program;

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
            compile_command(&path, output, run, wat, runtime)
        }
    } {
        error!("{:#}", err);
    }
}

pub struct RuntimeConfig {
    pub module: String,
    pub imports_mem: bool,
}

impl RuntimeConfig {
    pub fn new(module: String, import_mem: bool) -> Self {
        Self { module, imports_mem: import_mem }
    }
}

impl From<&str> for RuntimeConfig {
    fn from(value: &str) -> Self {
        match value {
            "wasmtime" => Self::new("wasi_unstable".to_owned(), false),
            "w4" => Self::new("env".to_owned(), true),
            _ => todo!(),
        }
    }
}

fn compile_command(
    path: &PathBuf, output: Option<PathBuf>, run: bool, wat: bool, runtime: String,
) -> Result<()> {
    let out = output.unwrap_or_else(|| path.clone()).with_extension("wat");

    Program::new()
        .compile_file(path)?
        .type_check()?
        .generate_wasm(&out, RuntimeConfig::from(runtime.as_str()))?;

    let out_wasm = out.with_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if wat {
        cmd_wait!("wasm2wat", &out_wasm, "-o", &out);
    }
    if run {
        if runtime == "w4" {
            cmd_wait!(runtime, "run", &out_wasm);
        } else {
            cmd_wait!(runtime, &out_wasm);
        }
    }

    Ok(())
}
