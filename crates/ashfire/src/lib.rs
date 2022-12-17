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
        prog.include(&mut parser, reader, source, LANG_FOLDER)?;

        prog.compile_parser(parser)?;
    } else {
        prog.compile_buffer(source, reader)?;
    };

    prog.type_check()?.generate_wasm(writer, target)
}

impl Program {
    fn include_libs(&mut self, parser: &mut Parser, target: Target) -> Result<()> {
        let folder = Path::new(LANG_FOLDER);

        match target {
            Target::Wasi => self.include_path(parser, &folder.join("lib/wasi.fire"))?,
            Target::Wasm4 => self.include_path(parser, &folder.join("lib/wasm4.fire"))?,
        };

        self.include_path(parser, &folder.join("lib/std.fire"))
    }
}

pub static LANG_FOLDER: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/firelang");

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
        compile_buffer("buffer", code.as_bytes(), io::sink(), Target::Wasi, true)
    }
}
