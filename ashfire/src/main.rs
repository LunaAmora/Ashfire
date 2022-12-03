#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

use std::{
    env,
    fs::File,
    io::{self, BufWriter},
    path::{Path, PathBuf},
};

use ashfire_lib::{compile, compile_buffer, logger, target::Target};
use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use firelib::Result;

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
            None => compile_pipe(target, &runtime, run),
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

    let writer = BufWriter::new(File::create(&out)?);
    compile(path, writer, target)?;

    info!("Generated {:?}", out);
    let out_wasm = out.with_extension("wasm");

    cmd_wait!("wat2wasm", &out, "-o", &out_wasm);
    if wat {
        cmd_wait!("wasm2wat", &out_wasm, "-o", &out);
    }

    if run {
        target.runner(runtime).run(out_wasm)?;
    }

    Ok(())
}

fn compile_pipe(target: Target, runtime_name: &str, run: bool) -> Result<()> {
    let path = lib_folder().unwrap();

    match (run, target) {
        (true, Target::Wasi) => {
            let mut runtime = cmd_piped!(runtime_name, "/dev/stdin");
            compile_buffer(&path, "stdin", io::stdin(), runtime.stdin().unwrap(), target)?;

            info!("[CMD] {runtime_name} /dev/stdin");
            runtime.wait_with_result()?;
        }

        (true, Target::Wasm4) => {
            let mut w4 = cmd_piped!("w4", "run", "/dev/stdin");
            let mut wat2wasm = cmd_piped!("wat2wasm", "-", "--output=-" => w4.stdin().unwrap());

            compile_buffer(&path, "stdin", io::stdin(), wat2wasm.stdin().unwrap(), target)?;

            info!("[CMD] | wat2wasm - --output=- | w4 run /dev/stdin");
            wat2wasm.wait_with_result()?;
            w4.wait_with_result()?;
        }

        (false, _) => {
            compile_buffer(&path, "stdin", io::stdin(), io::stdout(), target)?;
        }
    }

    Ok(())
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
