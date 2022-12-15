use std::collections::HashMap;

use ashfire_types::{
    core::{Name, WORD_SIZE},
    data::{Primitive, StructInfo, TypeDescr},
    proc::{Contract, Proc},
};
use firelib::{Context, Result};
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

    pub fn prep_proc<'p>(&mut self, program: &'p Program, ip: usize) -> Result<Option<&'p Proc>> {
        if self.current_func.is_some() {
            bail!("Cannot start an Wasm function block without closing the current one");
        }

        let proc = self.visit_proc(program, ip);
        let Some(data) = proc.get_data() else {
            return Ok(None);
        };

        let func = self
            .current_func
            .insert(FuncGen::new(proc.name, &proc.contract));

        let proc_size = data.total_size();

        if proc_size > 0 {
            func.extend(vec![Const(proc_size), Call("aloc_local".into())]);
        }

        let data_instructions = data
            .local_vars
            .units()
            .zip(0..)
            .filter_map(|(var, i)| store_if_non_zero(data.var_mem_offset(i), &var))
            .flatten();

        func.extend(data_instructions);

        let range = 0..proc.contract.ins().len();
        func.extend(range.map(|i| Get(local, Id(i))));

        Ok(Some(proc))
    }

    pub fn end_proc(&mut self, program: &Program, wasm: &mut Module) -> Result<()> {
        let proc = self.current_proc(program).unwrap();
        let Some(data) = proc.get_data() else {
            return Ok(());
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
        let (ins, outs) = &func.contract;
        let id = wasm.add_fn(label, ins, outs, func.code);

        if proc.is_export() {
            wasm.add_export(label, Bind::Func(Id(id)));
        }

        Ok(())
    }

    pub fn current_fn(&mut self) -> Result<&mut FuncGen> {
        self.current_func
            .as_mut()
            .with_context(|| "No Wasm function block is open")
    }
}

fn store_if_non_zero(offset: i32, var: &Primitive) -> Option<Vec<Instruction>> {
    (var.value() > 0).then(|| store_local(offset, var.value()))
}

fn store_local(offset: i32, var_value: i32) -> Vec<Instruction> {
    vec![
        Const(offset),
        Call("push_local".into()),
        Const(var_value),
        I32(store),
    ]
}

pub struct FuncGen {
    label: Name,
    contract: (Vec<WasmType>, Vec<WasmType>),
    code: Vec<Instruction>,
    pub bind_offset: i32,
}

impl FuncGen {
    pub fn new(label: Name, contract: &Contract) -> Self {
        Self {
            label,
            contract: as_wasm(contract),
            code: Vec::new(),
            bind_offset: 0,
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
}

pub fn as_wasm(contract: &Contract) -> (Vec<WasmType>, Vec<WasmType>) {
    let (ins, outs) = contract.size();
    (vec![WasmType::I32; ins], vec![WasmType::I32; outs])
}

pub fn unpack_struct(stk: &TypeDescr) -> Vec<Instruction> {
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
            instructions.extend(vec![
                Const(WORD_SIZE),
                Call("aloc_local".into()),
                Const(WORD_SIZE),
                Call("bind_local".into()),
            ]);

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
