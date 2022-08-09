#![allow(unused_variables)]
pub mod wasm_types;

use wasm_types::*;

#[derive(Default)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub globals: Vec<Global>,
    pub data: Vec<Data>,
    pub funcs: Vec<Func>,
}

impl Module {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn new_mem(&mut self) -> usize {
        todo!();
    }

    pub fn add_import(
        &mut self, module: &str, name: &str, label: &str, ins: &[WasmType], outs: &[WasmType],
    ) {
        todo!();
    }

    pub fn add_export(&mut self, label: &str, export: Bind) {
        todo!();
    }

    pub fn add_global(&mut self, label: &str, typ: WasmType, value: i32) -> usize {
        todo!();
    }

    pub fn add_fn(
        &mut self, label: &str, ins: &[WasmType], outs: &[WasmType], inst: Vec<Instruction>,
    ) -> usize {
        todo!();
    }
}
