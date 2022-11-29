#![feature(try_blocks)]
#[macro_use] extern crate log;
#[macro_use] extern crate firelib;

pub mod compiler;
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
