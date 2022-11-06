use std::{collections::HashMap, fs::File, io::BufWriter, path::Path};

use firelib::anyhow::{Context, Result};
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use Scope::*;

use super::{
    program::*,
    types::{Contract, IntrinsicType, Op, OpType, Proc},
};

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

        let stk = wasm.add_global("LOCAL_STACK", WasmType::I32, program.stack_start(), true);

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

        for op in program.ops.iter() {
            match op.op_type {
                OpType::PrepProc => self.prep_proc(program, op)?,
                OpType::EndProc => self.end_proc(program, &mut wasm)?,
                _ => {
                    let proc = self.current_proc(program).unwrap();
                    let func = self.current_fn()?;
                    func.extend(program.generate_op(op, proc, func, &mut wasm)?)
                }
            }
        }

        wasm.add_export("_start", Bind::Func("start".into()));

        for data in program.get_sorted_data() {
            wasm.add_data(data.to_string())
        }

        if !program.global_vars.is_empty() {
            let padding = 4 - (program.data_size() % 4) as usize;
            if padding < 4 {
                let pad_string = (0..padding).map(|_| "\\00").collect::<String>();
                wasm.add_data(pad_string);
            }
        }

        for var in program.global_vars.iter().flat_map(|v| v.units()) {
            wasm.add_data_value(var.value());
        }

        Ok(wasm)
    }

    fn prep_proc(&mut self, program: &Program, op: &Op) -> Result<()> {
        if self.current_func.is_some() {
            anybail!("Cannot start an Wasm function block without closing the current one");
        }

        let proc = self.visit_proc(program, op.operand as usize);
        self.current_func = Some(FuncGen::new(&proc.name, &proc.contract));
        let func = self.current_fn()?;

        let proc_size = proc.total_size();

        if proc_size > 0 {
            func.extend(vec![Const(proc_size), Call("aloc_local".into())]);
        }

        let mut index = (0..).into_iter();
        for var in proc.local_vars.iter().flat_map(|v| v.units()) {
            let i = index.next().unwrap();

            if var.value() > 0 {
                func.store_local(proc.var_mem_offset(i), var.value());
            }
        }

        for i in 0..proc.contract.ins().len() {
            func.push(Get(local, Id(i)));
        }

        Ok(())
    }

    fn end_proc(&mut self, program: &Program, wasm: &mut Module) -> Result<()> {
        let mut func = self
            .current_func
            .take()
            .with_context(|| "No Wasm function block is open")?;

        let proc = self.current_proc(program).unwrap();
        let mem_to_free = proc.total_size();

        if mem_to_free > 0 {
            func.extend(vec![Const(mem_to_free), Call("free_local".into())]);
        }

        wasm.add_fn(&func.label, &func.contract.0, &func.contract.1, func.code);
        Ok(())
    }

    fn current_fn(&mut self) -> Result<&mut FuncGen> {
        self.current_func
            .as_mut()
            .with_context(|| "No Wasm function block is open")
    }
}

impl Program {
    fn generate_op(
        &self, op: &Op, proc: &Proc, func: &FuncGen, module: &mut Module,
    ) -> Result<Vec<Instruction>> {
        Ok(match op.op_type {
            OpType::PushData(_) => vec![Const(op.operand)],

            OpType::PushStr => {
                let data = self.get_string(op.operand);
                vec![Const(data.size()), Const(data.offset())]
            }

            OpType::PushLocalMem => {
                let ptr = func.bind_count * 4 + op.operand;
                vec![Const(ptr), Call("push_local".into())]
            }

            OpType::PushGlobalMem => {
                vec![Const(self.global_vars_start() + op.operand)]
            }

            OpType::PushLocal => {
                let ptr = proc.var_mem_offset(func.bind_count + op.operand);
                vec![Const(ptr), Call("push_local".into())]
            }

            OpType::PushGlobal => vec![Const(self.mem_start() + op.operand * 4)],

            OpType::OffsetLoad => todo!(),

            OpType::Offset => vec![Const(op.operand), I32(NumMethod::add)],

            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Div => todo!(),
                IntrinsicType::Times => todo!(),

                IntrinsicType::Plus => vec![I32(NumMethod::add)],
                IntrinsicType::Minus => vec![I32(NumMethod::sub)],
                IntrinsicType::Greater => vec![I32(NumMethod::gt_s)],
                IntrinsicType::GreaterE => vec![I32(NumMethod::ge_s)],
                IntrinsicType::Lesser => vec![I32(NumMethod::lt_s)],
                IntrinsicType::LesserE => vec![I32(NumMethod::le_s)],
                IntrinsicType::And => vec![I32(NumMethod::and)],
                IntrinsicType::Or => vec![I32(NumMethod::or)],
                IntrinsicType::Xor => vec![I32(NumMethod::xor)],
                IntrinsicType::Load8 => vec![I32(NumMethod::load8_s)],
                IntrinsicType::Load16 => vec![I32(NumMethod::load16_s)],
                IntrinsicType::Load32 => vec![I32(NumMethod::load)],

                IntrinsicType::Store8 => vec![Call("swap".into()), I32(NumMethod::store8)],
                IntrinsicType::Store16 => vec![Call("swap".into()), I32(NumMethod::store16)],
                IntrinsicType::Store32 => vec![Call("swap".into()), I32(NumMethod::store)],
                IntrinsicType::FdWrite => vec![Call("fd_write".into())],

                IntrinsicType::Cast(_) => vec![],
            },

            OpType::Drop => vec![Drop],
            OpType::Dup => vec![Call("dup".into())],
            OpType::Swap => vec![Call("swap".into())],
            OpType::Over => vec![Call("over".into())],
            OpType::Rot => vec![Call("rot".into())],

            OpType::Call => {
                let label = self.procs[op.operand as usize].get_label();
                vec![Call(label.into())]
            }

            OpType::Equal => vec![I32(NumMethod::eq)],

            OpType::IfStart => {
                let (ins, outs) = self.block_contracts[&(op.operand as usize)];
                let contract =
                    module.new_contract(&vec![WasmType::I32; ins], &vec![WasmType::I32; outs]);

                vec![Block(BlockType::If, Some(Id(contract)))]
            }

            OpType::Else => vec![Else],

            OpType::EndIf | OpType::EndElse => vec![End],

            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),

            OpType::Unpack => self.unpack_struct(op.operand as usize),

            OpType::ExpectType => vec![],

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),

            OpType::PrepProc | OpType::EndProc => unreachable!(),
        })
    }

    fn unpack_struct(&self, index: usize) -> Vec<Instruction> {
        let stk = &self.structs_types[index];
        let count = stk.members().len() as i32;
        let mut instructions = vec![];

        match count {
            1 => instructions.push(I32(NumMethod::load)),
            2 => instructions.extend(vec![
                Call("dup".into()),
                I32(NumMethod::load),
                Call("swap".into()),
                Const(4),
                I32(NumMethod::add),
                I32(NumMethod::load),
            ]),
            _ => {
                instructions.push(Call("bind_local".into()));

                for offset in 0..count {
                    instructions.extend(vec![
                        Const(4),
                        Call("push_local".into()),
                        I32(NumMethod::load),
                        Const(4 * offset),
                        I32(NumMethod::add),
                        I32(NumMethod::load),
                    ]);
                }

                instructions.extend(vec![Const(4), Call("free_local".into())]);
            }
        };
        instructions
    }

    pub fn generate_wasm(&self, output: &Path) -> Result<()> {
        info!("Generating {:?}", output);

        let writer = BufWriter::new(File::create(output)?);
        Generator::new().generate_module(self)?.write_text(writer)
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

    fn store_local(&mut self, offset: i32, var_value: i32) {
        self.extend(vec![
            Const(offset),
            Call("push_local".into()),
            Const(var_value),
            I32(NumMethod::store),
        ]);
    }
}

impl From<&Contract> for (Vec<WasmType>, Vec<WasmType>) {
    fn from(contract: &Contract) -> Self {
        let (ins, outs) = contract.size();
        (vec![WasmType::I32; ins], vec![WasmType::I32; outs])
    }
}
