#![allow(unreachable_code)]
#![allow(dead_code)]
use std::{collections::HashMap, fs::File, io::BufWriter, path::PathBuf};

use super::types::{Contract, Op, OpType, Program, ProgramVisitor};
use anyhow::{bail, Result};
use wasm_backend::{wasm_types::*, Module};

pub struct Generator {
    writer: BufWriter<File>,
    current_proc: Option<usize>,
    block_map: HashMap<i32, i32>,
    current_func: Option<FuncGen>,
}

impl ProgramVisitor for Generator {
    fn set_index(&mut self, i: Option<usize>) {
        self.current_proc = i;
    }

    fn get_index(&self) -> Option<usize> {
        self.current_proc
    }
}

use Ident::*;
use Instruction::*;
use Scope::*;

impl Generator {
    fn new(writer: BufWriter<File>) -> Self {
        Self {
            writer,
            current_proc: None,
            block_map: HashMap::new(),
            current_func: None,
        }
    }

    fn generate(&mut self, program: &Program) -> Result<()> {
        let i1 = &[WasmType::I32; 1];
        let i2 = &[WasmType::I32; 2];
        let i3 = &[WasmType::I32; 3];
        let i4 = &[WasmType::I32; 4];

        let mut wasm = Module::new();

        wasm.add_import("wasi_unstable", "fd_write", "fd_write", i4, i1);

        let mem = wasm.new_mem();
        wasm.add_export("memory", Bind::Mem(Id(mem)));

        wasm.new_fn("dup", i1, i2, vec![Get(Local, Id(0)), Get(Local, Id(0))]);
        wasm.new_fn("swap", i2, i2, vec![Get(Local, Id(1)), Get(Local, Id(0))]);
        wasm.new_fn("over", i2, i3, vec![Get(Local, Id(0)), Get(Local, Id(1)), Get(Local, Id(0))]);
        wasm.new_fn("rot", i3, i3, vec![Get(Local, Id(1)), Get(Local, Id(2)), Get(Local, Id(0))]);

        let stack_start = program.mem_size + program.final_data_size() + program.total_vars_size();
        let stk = wasm.add_global("LOCAL_STACK", WasmType::MutI32, stack_start);

        let aloc_local = wasm.new_fn("aloc_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::add),
            Set(Global, Id(stk)),
        ]);

        wasm.new_fn("free_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::sub),
            Set(Global, Id(stk)),
        ]);

        wasm.new_fn("bind_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::store),
            Const(4),
            Call(Id(aloc_local)),
        ]);

        wasm.new_fn("push_local", i1, i1, vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::sub),
        ]);

        for op in program.ops.iter().cloned() {
            self.generate_op(op, program, &mut wasm)?;
        }

        todo!("Generate data section");

        Ok(())
    }

    fn generate_op(&mut self, op: Op, program: &Program, _module: &mut Module) -> Result<()> {
        match op.typ {
            OpType::PushData(_) => todo!(),
            OpType::PushStr => todo!(),
            OpType::PushLocalMem => todo!(),
            OpType::PushGlobalMem => todo!(),
            OpType::PushLocal => todo!(),
            OpType::PushGlobal => todo!(),
            OpType::OffsetLoad => todo!(),
            OpType::Offset => todo!(),
            OpType::Intrinsic => todo!(),
            OpType::Dup => self.current_fn()?.push(Call("dup".into())),
            OpType::Drop => todo!(),
            OpType::Swap => todo!(),
            OpType::Over => todo!(),
            OpType::Rot => todo!(),
            OpType::Call => todo!(),
            OpType::Equal => todo!(),
            OpType::PrepProc => self.prep_proc(program, op)?,
            OpType::IfStart => todo!(),
            OpType::Else => todo!(),
            OpType::EndIf => todo!(),
            OpType::EndElse => todo!(),
            OpType::EndProc => todo!(),
            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),
            OpType::Unpack => self.unpack_struct(program, op.operand as usize)?,
            OpType::ExpectType => todo!(),
            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        };
        Ok(())
    }

    fn prep_proc(&mut self, program: &Program, op: Op) -> Result<()> {
        if self.current_func.is_some() {
            bail!("Cannot start an Wasm function block without closing the current one");
        }

        let proc = self.visit_proc(program, op.operand as usize);
        self.current_func = Some(FuncGen::new(&proc.name, &proc.contract));
        let func = self.current_fn()?;

        let vars_count = proc.local_vars.len() as i32;

        if proc.mem_size + vars_count > 0 {
            func.extend(vec![
                Const(proc.mem_size + (vars_count * 4)),
                Call("aloc_local".into()),
            ]);
        }

        for (var, index) in proc.local_vars.iter().zip(0..) {
            let var_value = var.word.value;
            if var_value != 0 {
                func.extend(vec![
                    Const(proc.mem_size + (index + 1) * 4),
                    Call("push_local".into()),
                    Const(var_value),
                    I32(NumMethod::store),
                ]);
            }
        }

        for i in 0..proc.contract.ins.len() {
            func.push(Get(Local, Id(i)));
        }

        Ok(())
    }

    fn current_fn(&mut self) -> Result<&mut FuncGen> {
        Ok(self
            .current_func
            .as_mut()
            .expect("No Wasm function block is open"))
    }

    fn unpack_struct(&mut self, program: &Program, index: usize) -> Result<()> {
        let stk = program.structs_types.get(index).unwrap();
        let count = stk.members.len() as i32;
        let func = self.current_fn()?;

        match count {
            1 => func.push(I32(NumMethod::load)),
            2 => func.extend(vec![
                Call("dup".into()),
                I32(NumMethod::load),
                Call("swap".into()),
                Const(4),
                I32(NumMethod::add),
                I32(NumMethod::load),
            ]),
            _ => {
                func.push(Call("bind_local".into()));

                for offset in 0..count {
                    func.extend(vec![
                        Const(4),
                        Call("push_local".into()),
                        I32(NumMethod::load),
                        Const(4 * offset),
                        I32(NumMethod::add),
                        I32(NumMethod::load),
                    ]);
                }

                func.extend(vec![Const(4), Call("free_local".into())]);
            }
        };
        Ok(())
    }
}

struct FuncGen {
    label: String,
    contract: (Vec<WasmType>, Vec<WasmType>),
    code: Vec<Instruction>,
}

impl FuncGen {
    fn new(label: &String, contract: &Contract) -> Self {
        Self {
            label: label.to_owned(),
            contract: contract.into(),
            code: Vec::new(),
        }
    }

    fn push(&mut self, instruction: Instruction) {
        self.code.push(instruction);
    }

    fn extend(&mut self, instructions: Vec<Instruction>) {
        self.code.extend(instructions);
    }
}

impl From<&Contract> for (Vec<WasmType>, Vec<WasmType>) {
    fn from(contract: &Contract) -> Self {
        let ins = contract.ins.iter().map(|_| WasmType::I32).collect();
        let outs = contract.outs.iter().map(|_| WasmType::I32).collect();
        (ins, outs)
    }
}

pub fn generate_wasm(program: &Program, path: PathBuf, output: Option<PathBuf>) -> Result<()> {
    let mut out = output.unwrap_or(path);
    out.set_extension("wat");
    info!("Generating {:?}", out);

    let file = File::create(out)?;
    Generator::new(BufWriter::new(file)).generate(program)?;

    Ok(())
}
