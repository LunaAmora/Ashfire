#![feature(try_blocks)]
#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

mod compiler;
pub mod logger;
pub mod target;

use std::{
    io::{Read, Write},
    path::Path,
};

use firelib::Result;

use crate::{compiler::program::Program, target::TargetConfig};

pub fn compile(path: &Path, writer: impl Write, run_config: &TargetConfig) -> Result<()> {
    Program::new()
        .compile_file(path)?
        .type_check()?
        .generate_wasm(writer, run_config)
}

pub fn compile_buffer(
    path: &Path, source: &str, reader: impl Read + 'static, writer: impl Write,
    run_config: &TargetConfig,
) -> Result<()> {
    Program::new()
        .compile_buffer(path, source, reader)?
        .type_check()?
        .generate_wasm(writer, run_config)
}

#[cfg(test)]
mod tests {
    use std::{env, io, path::PathBuf};

    use firelib::Result;

    use crate::{
        compile_buffer,
        target::{Target, TargetConfig},
    };

    const WASI_RUNTIME: &str = "Wasmtime";

    fn lib_folder() -> io::Result<PathBuf> {
        Ok(env::current_dir()?.join("../firelang/lib"))
    }

    #[test]
    fn buffer_compilation() -> Result<()> {
        compile(
            r#"
            include lib/wasi
            include lib/std
            
            _start export::
                "Hello World!" println
            end
            "#,
        )
    }

    fn compile(code: &'static str) -> Result<()> {
        let target = TargetConfig::new(Target::Wasi, WASI_RUNTIME.to_owned(), false);
        compile_buffer(&lib_folder().unwrap(), "buffer", code.as_bytes(), io::sink(), &target)
    }
}
