#[macro_use] extern crate log;

use std::{
    env,
    fs::File,
    io::{self, BufWriter},
    path::{Path, PathBuf},
};

use ashfire_lib::{
    compile, compile_buffer, logger,
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

fn main() {
    let args = Cli::parse();
    if let Some(level) = args.verbose.log_level() {
        logger::formatted_builder()
            .filter_level(level.to_level_filter())
            .init();
    }

    let result = match args.command {
        Commands::Com { path, output, run, wat, target, runtime } => match path {
            Some(path) => compile_command(&path, output.as_deref(), run, wat, target, runtime),
            None => compile_pipe(target, runtime, run),
        },
    };

    if let Err(err) = result {
        error!("{:#}", err);
    }
}

fn compile_command(
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

fn compile_pipe(target: Target, runtime: String, run: bool) -> Result<()> {
    let config = TargetConfig::new(target, runtime, run);
    let path = lib_folder().unwrap();

    if run {
        let out = env::temp_dir().join("out.wat");
        compile_buffer(&path, "stdin", io::stdin(), File::create(&out)?, &config)?;

        let out_wasm = out.with_extension("wasm");
        cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
        config.run(out_wasm)
    } else {
        compile_buffer(&path, "stdin", io::stdin(), io::stdout(), &config)
    }
}

fn lib_folder() -> io::Result<PathBuf> {
    Ok(env::current_dir()?.join("lib/_"))
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
