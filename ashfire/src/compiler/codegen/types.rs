use std::collections::HashMap;

use ashfire_types::{
    core::{Op, Operand, StrKey, WORD_SIZE},
    data::{StructDef, StructInfo},
    proc::Contract,
};
use firelib::anyhow::{Context, Result};
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use NumMethod::*;
use Scope::*;

use crate::compiler::program::{InternalString, Program, Visitor};

pub struct Generator {
    current_proc: Option<usize>,
    current_func: Option<FuncGen>,
    _block_map: HashMap<i32, i32>,
}

impl Visitor for Generator {
    fn set_index(&mut self, i: Option<usize>) {
        self.current_proc = i;
    }

    fn get_index(&self) -> Option<usize> {
        self.current_proc
    }
}

impl Generator {
    pub fn new() -> Self {
        Self {
            current_proc: None,
            current_func: None,
            _block_map: HashMap::new(),
        }
    }

    pub fn prep_proc(&mut self, program: &Program, op: &Op) -> Result<bool> {
        if self.current_func.is_some() {
            bail!("Cannot start an Wasm function block without closing the current one");
        }

        let proc = self.visit_proc(program, op.index());
        let Some(data) = proc.get_data() else {
            return Ok(true);
        };

        self.current_func = Some(FuncGen::new(proc.name, &proc.contract));
        let func = self.current_fn()?;

        let proc_size = data.total_size();

        if proc_size > 0 {
            func.extend(vec![Const(proc_size), Call("aloc_local".into())]);
        }

        let mut index = 0..;
        for var in data.local_vars.units() {
            let i = index.next().unwrap();

            if var.value() > 0 {
                func.store_local(data.var_mem_offset(i), var.value());
            }
        }

        for i in 0..proc.contract.ins().len() {
            func.push(Get(local, Id(i)));
        }

        Ok(false)
    }

    pub fn end_proc(&mut self, program: &Program, wasm: &mut Module) -> Result<bool> {
        let proc = self.current_proc(program).unwrap();
        let Some(data) = proc.get_data() else {
            return Ok(true);
        };

        let mut func = self
            .current_func
            .take()
            .with_context(|| "No Wasm function block is open")?;

        let mem_to_free = data.total_size();

        if mem_to_free > 0 {
            func.extend(vec![Const(mem_to_free), Call("free_local".into())]);
        }

        let label = &func.label.as_str(program);
        let id = wasm.add_fn(label, &func.contract.0, &func.contract.1, func.code);

        if proc.is_export() {
            wasm.add_export(label, Bind::Func(Id(id)));
        }

        Ok(false)
    }

    pub fn current_fn(&mut self) -> Result<&mut FuncGen> {
        self.current_func
            .as_mut()
            .with_context(|| "No Wasm function block is open")
    }
}

pub struct FuncGen {
    label: StrKey,
    contract: (Vec<WasmType>, Vec<WasmType>),
    code: Vec<Instruction>,
    pub bind_count: i32,
}

impl FuncGen {
    pub fn new(label: StrKey, contract: &Contract) -> Self {
        Self {
            label,
            contract: as_wasm(contract),
            code: Vec::new(),
            bind_count: 0,
        }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.code.push(instruction);
    }

    pub fn extend<I>(&mut self, instructions: I)
    where
        I: IntoIterator<Item = Instruction>,
    {
        self.code.extend(instructions);
    }

    pub fn store_local(&mut self, offset: i32, var_value: i32) {
        self.code.extend(vec![
            Const(offset),
            Call("push_local".into()),
            Const(var_value),
            I32(store),
        ]);
    }
}

pub fn as_wasm(contract: &Contract) -> (Vec<WasmType>, Vec<WasmType>) {
    let (ins, outs) = contract.size();
    (vec![WasmType::I32; ins], vec![WasmType::I32; outs])
}

pub fn unpack_struct(stk: &StructDef) -> Vec<Instruction> {
    let mut instructions = vec![];

    match stk.count() as i32 {
        1 => instructions.push(I32(load)),
        2 => instructions.extend(vec![
            Call("dup".into()),
            I32(load),
            Call("swap".into()),
            Const(WORD_SIZE),
            I32(add),
            I32(load),
        ]),
        n => {
            instructions.push(Call("bind_local".into()));

            for offset in 0..n {
                instructions.extend(vec![
                    Const(WORD_SIZE),
                    Call("push_local".into()),
                    I32(load),
                    Const(WORD_SIZE * offset),
                    I32(add),
                    I32(load),
                ]);
            }

            instructions.extend(vec![Const(WORD_SIZE), Call("free_local".into())]);
        }
    };
    instructions
}
