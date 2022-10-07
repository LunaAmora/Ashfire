use std::{collections::HashMap, fs::File, io::BufWriter, path::PathBuf};

use super::types::{Contract, IntrinsicType, Op, OpType, Program, ProgramVisitor};
use anyhow::{bail, Context, Result};
use itertools::Itertools;
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use Scope::*;

pub struct Generator {
    current_proc: Option<usize>,
    _block_map: HashMap<i32, i32>,
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

impl Generator {
    fn new() -> Self {
        Self {
            current_proc: None,
            _block_map: HashMap::new(),
            current_func: None,
        }
    }

    fn generate_module(&mut self, program: &Program) -> Result<Module> {
        let i1 = &[WasmType::I32; 1];
        let i2 = &[WasmType::I32; 2];
        let i3 = &[WasmType::I32; 3];
        let i4 = &[WasmType::I32; 4];

        let mut wasm = Module::new();

        wasm.add_import("wasi_unstable", "fd_write", "fd_write", i4, i1);

        let mem = wasm.new_mem();
        wasm.add_export("memory", Bind::Mem(Id(mem)));

        wasm.add_fn("dup", i1, i2, vec![Get(local, Id(0)), Get(local, Id(0))]);
        wasm.add_fn("swap", i2, i2, vec![Get(local, Id(1)), Get(local, Id(0))]);
        wasm.add_fn("over", i2, i3, vec![Get(local, Id(0)), Get(local, Id(1)), Get(local, Id(0))]);
        wasm.add_fn("rot", i3, i3, vec![Get(local, Id(1)), Get(local, Id(2)), Get(local, Id(0))]);

        let stack_start = program.mem_size + program.final_data_size() + program.total_vars_size();
        let stk = wasm.add_global("LOCAL_STACK", WasmType::I32, stack_start, true);

        let aloc_local = wasm.add_fn("aloc_local", i1, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(NumMethod::add),
            Set(global, Id(stk)),
        ]);

        wasm.add_fn("free_local", i1, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(NumMethod::sub),
            Set(global, Id(stk)),
        ]);

        wasm.add_fn("bind_local", i1, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(NumMethod::store),
            Const(4),
            Call(Id(aloc_local)),
        ]);

        wasm.add_fn("push_local", i1, i1, vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(NumMethod::sub),
        ]);

        for op in program.ops.iter().cloned() {
            self.generate_op(op, program, &mut wasm)?;
        }

        wasm.add_export("_start", Bind::Func("start".into()));

        for data in program
            .data
            .clone()
            .iter()
            .filter(|d| d.offset >= 0)
            .sorted_by_key(|d| d.offset)
        {
            wasm.add_data(data)
        }

        for _var in &program.global_vars {
            todo!("align vars padding");
        }

        Ok(wasm)
    }

    fn generate_op(&mut self, op: Op, program: &Program, module: &mut Module) -> Result<()> {
        match op.typ {
            OpType::PushData(_) => self.current_fn()?.push(Const(op.operand)),
            OpType::PushStr => {
                let data = program.data.get(op.operand as usize).unwrap();
                self.current_fn()?
                    .extend(vec![Const(data.size()), Const(data.offset)])
            }
            OpType::PushLocalMem => {
                let ptr = self.current_fn()?.bind_count * 4 + op.operand;

                self.current_fn()?
                    .extend(vec![Const(ptr), Call("push_local".into())]);
            }
            OpType::PushGlobalMem => todo!(),
            OpType::PushLocal => {
                let proc = self.current_proc(program).unwrap();
                let ptr = (self.current_fn()?.bind_count + 1 + op.operand) * 4 + proc.mem_size;

                self.current_fn()?
                    .extend(vec![Const(ptr), Call("push_local".into())]);
            }
            OpType::PushGlobal => todo!(),
            OpType::OffsetLoad => todo!(),
            OpType::Offset => self
                .current_fn()?
                .extend(vec![Const(op.operand), I32(NumMethod::add)]),
            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus => self.current_fn()?.push(I32(NumMethod::add)),
                IntrinsicType::Minus => self.current_fn()?.push(I32(NumMethod::sub)),
                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),
                IntrinsicType::Greater => todo!(),
                IntrinsicType::GreaterE => todo!(),
                IntrinsicType::Lesser => todo!(),
                IntrinsicType::LesserE => todo!(),
                IntrinsicType::And => self.current_fn()?.push(I32(NumMethod::and)),
                IntrinsicType::Or => self.current_fn()?.push(I32(NumMethod::or)),
                IntrinsicType::Xor => todo!(),
                IntrinsicType::Load8 => todo!(),
                IntrinsicType::Store8 => todo!(),
                IntrinsicType::Load16 => todo!(),
                IntrinsicType::Store16 => todo!(),
                IntrinsicType::Load32 => todo!(),
                IntrinsicType::Store32 => self
                    .current_fn()?
                    .extend(vec![Call("swap".into()), I32(NumMethod::store)]),
                IntrinsicType::FdWrite => self.push_call("fd_write")?,
                IntrinsicType::Cast(_) => {}
            },
            OpType::Dup => self.push_call("dup")?,
            OpType::Drop => self.current_fn()?.push(Drop),
            OpType::Swap => self.push_call("swap")?,
            OpType::Over => todo!(),
            OpType::Rot => todo!(),
            OpType::Call => {
                let id = program
                    .procs
                    .get(op.operand as usize)
                    .unwrap()
                    .name
                    .to_owned();
                self.current_fn()?.push(Call(Label(id)));
            }
            OpType::Equal => todo!(),
            OpType::PrepProc => self.prep_proc(program, op)?,
            OpType::IfStart => todo!(),
            OpType::Else => todo!(),
            OpType::EndIf => todo!(),
            OpType::EndElse => todo!(),
            OpType::EndProc => {
                let mut func = self
                    .current_func
                    .take()
                    .with_context(|| "No Wasm function block is open")?;

                let proc = self.current_proc(program).unwrap();
                let mem_to_free = proc.mem_size + (proc.local_vars.len() as i32 * 4);

                if mem_to_free > 0 {
                    func.extend(vec![Const(mem_to_free), Call("free_local".into())]);
                }

                module.add_fn(&func.label, &func.contract.0, &func.contract.1, func.code);
            }
            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),
            OpType::Unpack => self.unpack_struct(program, op.operand as usize)?,
            OpType::ExpectType => {}
            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        };
        Ok(())
    }

    fn push_call(&mut self, label: &str) -> Result<()> {
        self.current_fn()?.push(Call(label.into()));
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
            func.push(Get(local, Id(i)));
        }

        Ok(())
    }

    fn current_fn(&mut self) -> Result<&mut FuncGen> {
        self.current_func
            .as_mut()
            .with_context(|| "No Wasm function block is open")
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
    bind_count: i32,
}

impl FuncGen {
    fn new(label: &String, contract: &Contract) -> Self {
        Self {
            label: label.to_owned(),
            contract: contract.into(),
            code: Vec::new(),
            bind_count: 0,
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

pub fn generate_wasm(program: &Program, output: PathBuf) -> Result<()> {
    info!("Generating {:?}", output);

    let writer = BufWriter::new(File::create(output)?);
    let mut module = Generator::new().generate_module(program)?;
    module.write_text(writer)?;

    Ok(())
}
