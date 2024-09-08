#![feature(slice_as_chunks)]
pub mod wasm_types;

use std::{collections::HashMap, fmt::Write, io};

use itertools::Itertools;
use wasm_types::*;

#[derive(Default)]
pub struct Module {
    types: Vec<FuncType>,
    mems: Vec<Memory>,
    imports: Vec<Import>,
    exports: Vec<Export>,
    globals: Vec<Global>,
    data: (Vec<String>, u16),
    funcs: Vec<Func>,
    func_map: HashMap<String, usize>,
    global_map: HashMap<String, usize>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_mem(&mut self) -> usize {
        self.mems.push(Memory::default());
        self.mems.len() - 1
    }

    pub fn add_mem_import(&mut self, module: &str, name: &str, bind: Bind) {
        self.imports.push(Import {
            module: module.to_owned(),
            label: name.to_owned(),
            bind,
        });
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
            .push(Export { label: label.to_owned(), bind: export });
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

    pub fn set_data_offset(&mut self, offset: u16) {
        self.data.1 = offset;
    }

    pub fn add_data(&mut self, data: String) {
        self.data.0.push(data);
    }

    pub fn add_data_value(&mut self, data: i32) {
        self.add_data(wasm_data_format(data));
    }

    fn func_label_by_id(&self, id: &Ident) -> String {
        match id {
            Ident::Id(index) => self
                .func_map
                .iter()
                .find(|pair| pair.1 == index)
                .expect("The given `Ident` is invalid")
                .0
                .clone(),
            Ident::Label(label) => label.clone(),
        }
    }

    fn func_by_id(&self, id: &Ident) -> &Func {
        match id {
            Ident::Id(index) => &self.funcs[*index],
            Ident::Label(label) => &self.funcs[self.func_map[label]],
        }
    }

    fn write_imports(&mut self, writer: &mut impl io::Write) -> io::Result<&mut Self> {
        for import in &self.imports {
            match &import.bind {
                Bind::Global(_) => todo!(),
                Bind::Func(id) => {
                    let func = self.func_by_id(id);
                    let contr = &self.types[func.contract];
                    let label = self.func_label_by_id(id);

                    writer.write_all(
                        format!(
                            "(import \"{}\" \"{}\" (func ${label}{contr}))\n",
                            import.module, import.label
                        )
                        .as_bytes(),
                    )?;

                    self.func_map.remove(&label);
                }
                Bind::Mem(id) => {
                    let mem = match id {
                        Ident::Id(index) => &self.mems[*index],
                        Ident::Label(_) => unreachable!(),
                    };

                    writer.write_all(
                        format!("(import \"{}\" \"{}\" {mem})\n", import.module, import.label)
                            .as_bytes(),
                    )?;
                }
            }
        }
        Ok(self)
    }

    fn write_globals(&self, writer: &mut impl io::Write) -> io::Result<&Self> {
        for (label, index) in self.global_map.iter().sorted_by_key(|entry| entry.1) {
            let global = &self.globals[*index].value;

            let global_type = if global.is_mut {
                format!("(mut {})", global.wasm_type)
            } else {
                global.wasm_type.to_string()
            };
            writer.write_all(
                format!(
                    "(global ${label} {global_type} ({}.const {}))\n",
                    global.wasm_type, global.value
                )
                .as_bytes(),
            )?;
        }
        Ok(self)
    }

    fn write_funcs(&self, writer: &mut impl io::Write) -> io::Result<&Self> {
        for (label, index) in self.func_map.iter().sorted_by_key(|entry| entry.1) {
            let func = &self.funcs[*index];
            let contr = &self.types[func.contract];

            writer.write_all(
                format!(
                    "(func ${}{}\n  {}\n)\n",
                    label,
                    contr,
                    func.code
                        .iter()
                        .map(|inst| self.format_instruction(inst))
                        .join(" ")
                )
                .as_bytes(),
            )?;
        }
        Ok(self)
    }

    fn format_instruction(&self, instruction: &Instruction) -> String {
        let Instruction::Block(block, ident) = instruction else {
            return format!("{instruction}");
        };

        let Some(Ident::Id(id)) = ident else {
            unreachable!();
        };

        format!("{block}{}", &self.types[*id])
    }

    fn write_data(&self, writer: &mut impl io::Write) -> io::Result<&Self> {
        writer.write_all(
            format!(
                "(data (i32.const {})\n{}\n)\n",
                self.data.1,
                self.data.0.iter().map(|d| format!("  \"{d}\"")).join("\n")
            )
            .as_bytes(),
        )?;
        Ok(self)
    }

    fn write_exports(&self, writer: &mut impl io::Write) -> io::Result<()> {
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

                    let memory = &self.mems[*mem];
                    writer.write_all(
                        format!("{memory}\n(export \"memory\" (memory {mem}))\n").as_bytes(),
                    )?;
                }
            }
        }
        Ok(())
    }

    pub fn write_text(mut self, mut writer: impl io::Write) -> io::Result<()> {
        writer.write_all(b"(module\n")?;

        self.write_imports(&mut writer)?
            .write_globals(&mut writer)?
            .write_funcs(&mut writer)?
            .write_data(&mut writer)?
            .write_exports(&mut writer)?;

        writer.write_all(b")")
    }
}

fn wasm_data_format(data: i32) -> String {
    let mut result = String::with_capacity(12);

    for &[a, b] in format!("{data:08x}").as_bytes().as_rchunks::<2>().1 {
        let byte = (hex_to_nibble(a) << 4) | hex_to_nibble(b);
        write!(&mut result, "\\{:02x}", byte).unwrap();
    }

    result
}

fn hex_to_nibble(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'A'..=b'F' => b - b'A' + 10,
        b'a'..=b'f' => b - b'a' + 10,
        _ => unreachable!(),
    }
}
