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

use crate::{compiler::program::Program, target::Target};

pub fn compile(path: &Path, writer: impl Write, target: Target) -> Result<()> {
    Program::new()
        .compile_file(path)?
        .type_check()?
        .generate_wasm(writer, target)
}

pub fn compile_buffer(
    path: &Path, source: &str, reader: impl Read + 'static, writer: impl Write, target: Target,
) -> Result<()> {
    Program::new()
        .compile_buffer(path, source, reader)?
        .type_check()?
        .generate_wasm(writer, target)
}

#[cfg(test)]
mod tests {
    use std::{env, io, path::PathBuf};

    use firelib::Result;

    use crate::{compile_buffer, target::Target};

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
        compile_buffer(&lib_folder()?, "buffer", code.as_bytes(), io::sink(), Target::Wasi)
    }
}
