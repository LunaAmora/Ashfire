#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

use std::{
    fs::File,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
};

use ashfire::{compile, compile_buffer, target::Target as LibTarget};
use clap::{Parser, Subcommand, ValueEnum};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use firelib::Result;
mod logger;

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
        path: Option<PathBuf>,
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

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum Target {
    Wasi,
    Wasm4,
}

impl From<Target> for LibTarget {
    fn from(value: Target) -> Self {
        match value {
            Target::Wasi => Self::Wasi,
            Target::Wasm4 => Self::Wasm4,
        }
    }
}

fn main() {
    let args = Cli::parse();
    if let Some(level) = args.verbose.log_level() {
        logger::formatted_builder()
            .filter_level(level.to_level_filter())
            .init();
    }

    let result = match args.command {
        Commands::Com { path, output, run, wat, target, runtime } => match path {
            Some(path) => {
                compile_command(&path, output.as_deref(), run, wat, target.into(), runtime)
            }
            None => compile_pipe(target.into(), &runtime, run),
        },
    };

    if let Err(err) = result {
        error!("{err:#}");
    }
}

fn compile_command(
    path: &Path, output: Option<&Path>, run: bool, wat: bool, target: LibTarget, runtime: String,
) -> Result<()> {
    let out = output.unwrap_or(path).with_extension("wat");

    let writer = BufWriter::new(File::create(&out)?);
    compile(path, writer, target)?;

    info!("Generated {out:?}");
    let out_wasm = out.with_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if wat {
        cmd_wait!("wasm2wat", &out_wasm, "-o", &out);
    }

    if run {
        new_runner(target, runtime)(out_wasm)?;
    }

    Ok(())
}

fn compile_stdin(out: impl Write, target: LibTarget, std: bool) -> Result<()> {
    compile_buffer("stdin", io::stdin(), out, target, std)
}

fn compile_pipe(target: LibTarget, runtime_name: &str, run: bool) -> Result<()> {
    match (run, target) {
        (true, LibTarget::Wasi) => {
            let mut runtime = cmd_piped!(runtime_name, "-");
            compile_stdin(runtime.stdin().unwrap(), target, true)?;

            runtime.wait_with_result()
        }

        (true, LibTarget::Wasm4) => {
            let mut wat2wasm = cmd_piped!("wat2wasm", "-", "--output=-");
            let w4 = cmd_piped!(wat2wasm.stdout().unwrap() => "w4", "run", "-");

            compile_stdin(wat2wasm.stdin().unwrap(), target, true)?;

            wat2wasm.wait_with_result()?;
            w4.wait_with_result()
        }

        (false, _) => compile_stdin(io::stdout(), target, true),
    }
}

pub fn new_runner(
    target: LibTarget, wasi_runtime: String,
) -> Box<dyn FnOnce(PathBuf) -> Result<()>> {
    match target {
        LibTarget::Wasi => Box::new(move |out| {
            cmd_wait!(wasi_runtime, out);
            Ok(())
        }),

        LibTarget::Wasm4 => Box::new(|out| {
            cmd_wait!("w4", "run", out);
            Ok(())
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::Cli;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }
}
