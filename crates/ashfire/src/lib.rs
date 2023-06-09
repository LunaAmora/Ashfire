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

use crate::{compiler::ctx::Ctx, target::Target};

pub fn compile(path: &Path, writer: impl Write, target: Target) -> Result<()> {
    Ctx::new()
        .compile_file(path)?
        .type_check()?
        .generate_wasm(writer, target)
}

pub fn compile_buffer(
    source: &str, reader: impl Read, writer: impl Write, target: Target, std: bool,
) -> Result<()> {
    info!("Compiling buffer: {:?}", source);
    let mut ctx = Ctx::new();

    if std {
        let mut parser = Parser::new();
        ctx.include_libs(&mut parser, target)?;
        ctx.include(&mut parser, reader, source, LANG_FOLDER)?;

        ctx.compile_parser(parser)?;
    } else {
        ctx.compile_buffer(source, reader)?;
    };

    ctx.type_check()?.generate_wasm(writer, target)
}

impl Ctx {
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
