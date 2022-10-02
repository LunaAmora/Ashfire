#![allow(unused_variables)]
pub mod wasm_types;

use wasm_types::*;

#[derive(Default)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub mems: Vec<Memory>,
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
        self.mems.push(Memory {});
        self.mems.len() - 1
    }

    pub fn add_import(
        &mut self, module: &str, name: &str, label: &str, ins: &[WasmType], outs: &[WasmType],
    ) -> usize {
        let func = self.new_fn(label, ins, outs, vec![]);

        self.imports.push(Import {
            module: module.to_owned(),
            label: name.to_owned(),
            import: Bind::Func(Ident::Id(func)),
        });

        func
    }

    pub fn add_export(&mut self, label: &str, export: Bind) {
        self.exports
            .push(Export { label: label.to_owned(), export })
    }

    pub fn add_global(&mut self, label: &str, wasm_type: WasmType, value: i32) -> usize {
        self.globals
            .push(Global { value: WasmValue { wasm_type, value } });
        self.globals.len() - 1
    }

    pub fn add_fn(&mut self, func: Func) -> usize {
        self.funcs.push(func);
        self.funcs.len() - 1
    }

    pub fn new_fn(
        &mut self, label: &str, ins: &[WasmType], outs: &[WasmType], code: Vec<Instruction>,
    ) -> usize {
        let contract = Ident::Id(self.new_contract(ins, outs));
        self.add_fn(Func { contract, code })
    }

    pub fn new_contract(&mut self, ins: &[WasmType], outs: &[WasmType]) -> usize {
        self.types
            .push(FuncType { param: ins.to_vec(), result: outs.to_vec() });
        self.types.len() - 1
    }
}
