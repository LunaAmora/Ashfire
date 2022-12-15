#![feature(try_blocks)]
#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

pub mod compiler;
pub mod target;

use std::{
    io::{Read, Write},
    path::Path,
};

use compiler::parsing::parser::Parser;
use firelib::Result;

use crate::{compiler::program::Program, target::Target};

pub fn compile(path: &Path, writer: impl Write, target: Target) -> Result<()> {
    Program::new()
        .compile_file(path)?
        .type_check()?
        .generate_wasm(writer, target)
}

pub fn compile_buffer(
    source: &str, reader: impl Read, writer: impl Write, target: Target, std: bool,
) -> Result<()> {
    info!("Compiling buffer: {:?}", source);
    let mut prog = Program::new();

    if std {
        let mut parser = Parser::new();
        prog.include_libs(&mut parser, target)?;
        prog.include(&mut parser, reader, source, "")?;

        prog.compile_parser(parser)?;
    } else {
        prog.compile_buffer(source, reader)?;
    };

    prog.type_check()?.generate_wasm(writer, target)
}

impl Program {
    fn include_libs(&mut self, parser: &mut Parser, target: Target) -> firelib::Result<()> {
        self.include(parser, &mut LIB_CORE.as_bytes(), "core", "lib")?;

        match target {
            Target::Wasi => self.include(parser, &mut LIB_WASI.as_bytes(), "wasi", "lib")?,
            Target::Wasm4 => self.include(parser, &mut LIB_WASM4.as_bytes(), "wasm4", "lib")?,
        };

        self.include(parser, &mut LIB_STD.as_bytes(), "std", "lib")?;
        Ok(())
    }
}

pub static LIB_CORE: &str = include_str!("../../firelang/lib/core.fire");
pub static LIB_STD: &str = include_str!("../../firelang/lib/std.fire");
pub static LIB_WASI: &str = include_str!("../../firelang/lib/wasi.fire");
pub static LIB_WASM4: &str = include_str!("../../firelang/lib/wasm4.fire");

#[cfg(test)]
mod tests {
    use std::io;

    use firelib::Result;

    use crate::{compile_buffer, target::Target};

    #[test]
    fn buffer_compilation() -> Result<()> {
        compile(
            r#"            
            _start export::
                "Hello World!" println
            end
            "#,
        )
    }

    fn compile(code: &str) -> Result<()> {
        compile_buffer("buffer", &mut code.as_bytes(), io::sink(), Target::Wasi, true)
    }
}
