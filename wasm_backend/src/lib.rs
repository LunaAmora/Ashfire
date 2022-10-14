pub mod wasm_types;

use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Write},
};

use anyhow::{bail, Result};
use hexstring::LowerHexString;
use itertools::Itertools;
use wasm_types::*;

#[derive(Default)]
pub struct Module {
    types: Vec<FuncType>,
    mems: Vec<Memory>,
    imports: Vec<Import>,
    exports: Vec<Export>,
    globals: Vec<Global>,
    data: Vec<String>,
    funcs: Vec<Func>,
    func_map: HashMap<String, usize>,
    global_map: HashMap<String, usize>,
}

impl Module {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn new_mem(&mut self) -> usize {
        self.mems.push(Memory::default());
        self.mems.len() - 1
    }

    pub fn add_import(
        &mut self, module: &str, name: &str, label: &str, ins: &[WasmType], outs: &[WasmType],
    ) -> usize {
        let func = self.add_fn(label, ins, outs, vec![]);

        self.imports.push(Import {
            module: module.to_owned(),
            label: name.to_owned(),
            bind: Bind::Func(Ident::Id(func)),
        });

        func
    }

    pub fn add_export(&mut self, label: &str, export: Bind) {
        self.exports
            .push(Export { label: label.to_owned(), bind: export })
    }

    pub fn add_global(
        &mut self, label: &str, wasm_type: WasmType, value: i32, is_mut: bool,
    ) -> usize {
        self.globals
            .push(Global { value: WasmValue { wasm_type, value, is_mut } });
        let index = self.globals.len() - 1;
        self.global_map.insert(label.to_owned(), index);

        index
    }

    pub fn add_fn(
        &mut self, label: &str, ins: &[WasmType], outs: &[WasmType], code: Vec<Instruction>,
    ) -> usize {
        let contract = self.new_contract(ins, outs);
        self.funcs.push(Func { contract, code });
        let index = self.funcs.len() - 1;
        self.func_map.insert(label.to_owned(), index);

        index
    }

    pub fn new_contract(&mut self, ins: &[WasmType], outs: &[WasmType]) -> usize {
        self.types
            .push(FuncType { param: ins.to_vec(), result: outs.to_vec() });
        self.types.len() - 1
    }

    pub fn add_data(&mut self, data: &str) {
        self.data.push(data.to_owned());
    }

    pub fn add_data_value(&mut self, data: i32) {
        self.add_data(&inverse_hex_representation(data));
    }

    fn func_label_by_id(&self, id: &Ident) -> String {
        match id {
            Ident::Id(index) => self
                .func_map
                .iter()
                .find(|pair| pair.1 == index)
                .unwrap()
                .0
                .to_owned(),
            Ident::Label(label) => label.to_owned(),
        }
    }

    fn func_by_id(&self, _id: &Ident) -> &Func {
        match _id {
            Ident::Id(index) => self.funcs.get(*index),
            Ident::Label(label) => self.funcs.get(*self.func_map.get(label).unwrap()),
        }
        .unwrap()
    }

    fn write_imports(&mut self, writer: &mut BufWriter<File>) -> Result<&mut Self> {
        for import in &self.imports {
            match &import.bind {
                Bind::Global(_) => todo!(),
                Bind::Func(id) => {
                    let func = self.func_by_id(id);
                    let contr = self.types.get(func.contract).unwrap();
                    let label = self.func_label_by_id(id);

                    writer.write_all(
                        format!(
                            "(import \"{}\" \"{}\" (func ${} {}))\n",
                            import.module, import.label, label, contr
                        )
                        .as_bytes(),
                    )?;

                    self.func_map.remove(&label);
                }
                Bind::Mem(_) => todo!(),
            }
        }
        Ok(self)
    }

    fn write_memories(&self, writer: &mut BufWriter<File>) -> Result<&Self> {
        for mem in &self.mems {
            writer.write_all(format!("{mem}\n").as_bytes())?;
        }
        Ok(self)
    }

    fn write_globals(&self, writer: &mut BufWriter<File>) -> Result<&Self> {
        for (label, index) in self.global_map.iter().sorted_by_key(|entry| entry.1) {
            let global = &self.globals.get(*index).unwrap().value;

            let global_type =
                format!("{}{}", if global.is_mut { "mut " } else { "" }, global.wasm_type);
            writer.write_all(
                format!(
                    "(global ${label} ({global_type}) ({}.const {}))\n",
                    global.wasm_type, global.value
                )
                .as_bytes(),
            )?;
        }
        Ok(self)
    }

    fn write_funcs(&self, writer: &mut BufWriter<File>) -> Result<&Self> {
        for (label, index) in self.func_map.iter().sorted_by_key(|entry| entry.1) {
            let func = self.funcs.get(*index).unwrap();
            let contr = self.types.get(func.contract).unwrap();

            writer.write_all(
                format!("(func ${} {}\n  {}\n)\n", label, contr, func.code.iter().join(" "))
                    .as_bytes(),
            )?;
        }
        Ok(self)
    }

    fn write_data(&self, _writer: &mut BufWriter<File>) -> Result<&Self> {
        _writer.write_all(
            format!(
                "(data (i32.const 0)\n{}\n)\n",
                self.data.iter().map(|d| format!("  \"{d}\"")).join("\n")
            )
            .as_bytes(),
        )?;
        Ok(self)
    }

    fn write_exports(&self, writer: &mut BufWriter<File>) -> Result<()> {
        for export in &self.exports {
            match &export.bind {
                Bind::Global(_) => todo!(),
                Bind::Func(id) => writer.write_all(
                    format!(
                        "(export \"{}\" (func ${}))\n",
                        export.label,
                        self.func_label_by_id(id)
                    )
                    .as_bytes(),
                )?,
                Bind::Mem(id) => {
                    let mem = match id {
                        Ident::Id(mem_index) => mem_index,
                        Ident::Label(_) => unreachable!(),
                    };

                    writer.write_all(format!("(export \"memory\" (memory {mem}))\n").as_bytes())?;
                }
            }
        }
        Ok(())
    }

    pub fn write_text(mut self, mut writer: BufWriter<File>) -> Result<()> {
        self.write_imports(&mut writer)?
            .write_memories(&mut writer)?
            .write_globals(&mut writer)?
            .write_funcs(&mut writer)?
            .write_data(&mut writer)?
            .write_exports(&mut writer)
    }
}

fn inverse_hex_representation(data: i32) -> String {
    bytes_from_value(data)
        .iter()
        .rev()
        .flat_map(u8_to_hex_representation)
        .collect()
}

fn bytes_from_value(data: i32) -> Vec<u8> {
    LowerHexString::new(format!("{:08x}", data)).unwrap().into()
}

fn u8_to_hex_representation(b: &u8) -> [char; 3] {
    let upper = nibble_to_hexchar(&((b & 0xf0) >> 4)).unwrap();
    let lower = nibble_to_hexchar(&(b & 0x0f)).unwrap();
    ['\\', upper, lower]
}

fn nibble_to_hexchar(b: &u8) -> Result<char> {
    match b {
        0 => Ok('0'),
        1 => Ok('1'),
        2 => Ok('2'),
        3 => Ok('3'),
        4 => Ok('4'),
        5 => Ok('5'),
        6 => Ok('6'),
        7 => Ok('7'),
        8 => Ok('8'),
        9 => Ok('9'),
        10 => Ok('a'),
        11 => Ok('b'),
        12 => Ok('c'),
        13 => Ok('d'),
        14 => Ok('e'),
        15 => Ok('f'),
        _ => bail!("Invalid nibble: {}", b),
    }
}
