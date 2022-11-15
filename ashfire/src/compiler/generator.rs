use std::{collections::HashMap, fs::File, io::BufWriter, path::Path};

use firelib::anyhow::{Context, Result};
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use NumMethod::*;
use Scope::*;

use super::{program::*, types::*};

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
            I32(add),
            Set(global, Id(stk)),
        ]);

        wasm.add_fn("free_local", i1, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(sub),
            Set(global, Id(stk)),
        ]);

        wasm.add_fn("bind_local", i1, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(0)),
            I32(store),
            Const(4),
            Call(Id(aloc_local)),
        ]);

        wasm.add_fn("push_local", i1, i1, vec![Get(global, Id(stk)), Get(local, Id(0)), I32(sub)]);

        let mut skip = false;
        for op in program.ops.iter() {
            skip = match op.op_type {
                OpType::PrepProc | OpType::PrepInline => self.prep_proc(program, op)?,
                OpType::EndProc => self.end_proc(program, &mut wasm)?,
                _ => {
                    if !skip {
                        let proc = self.current_proc(program).unwrap();
                        self.current_fn()?.append_op(program, op, proc, &mut wasm)?;
                    }
                    skip
                }
            }
        }

        wasm.add_export("_start", Bind::Func("start".into()));

        for data in program.get_data() {
            wasm.add_data(data.to_string())
        }

        if !program.global_vars.is_empty() {
            let padding = 4 - program.data_size() % 4;
            if padding < 4 {
                wasm.add_data((0..padding).map(|_| "\\00").collect());
            }
        }

        for var in program.global_vars.iter().flat_map(StructType::units) {
            wasm.add_data_value(var.value());
        }

        Ok(wasm)
    }

    fn prep_proc(&mut self, program: &Program, op: &Op) -> Result<bool> {
        if self.current_func.is_some() {
            anybail!("Cannot start an Wasm function block without closing the current one");
        }

        let proc = self.visit_proc(program, op.index());
        let Some(data) = proc.get_data() else {
            return Ok(true);
        };

        self.current_func = Some(FuncGen::new(&proc.name, &proc.contract));
        let func = self.current_fn()?;

        let proc_size = data.total_size();

        if proc_size > 0 {
            func.extend(vec![Const(proc_size), Call("aloc_local".into())]);
        }

        let mut index = (0..).into_iter();
        for var in data.local_vars.iter().flat_map(StructType::units) {
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

    fn end_proc(&mut self, program: &Program, wasm: &mut Module) -> Result<bool> {
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

        wasm.add_fn(&func.label, &func.contract.0, &func.contract.1, func.code);
        Ok(false)
    }

    fn current_fn(&mut self) -> Result<&mut FuncGen> {
        self.current_func
            .as_mut()
            .with_context(|| "No Wasm function block is open")
    }
}

struct FuncGen {
    label: String,
    contract: (Vec<WasmType>, Vec<WasmType>),
    code: Vec<Instruction>,
    bind_count: i32,
}

impl FuncGen {
    fn append_op(
        &mut self, prog: &Program, op: &Op, proc: &Proc, module: &mut Module,
    ) -> Result<()> {
        match op.op_type {
            OpType::PushData(_) => self.push(Const(op.operand)),

            OpType::PushStr => {
                let (size, offset) = prog.get_string(op).data();
                self.extend([Const(size), Const(offset)])
            }

            OpType::PushLocalMem => {
                let ptr = self.bind_count * 4 + op.operand;
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushGlobalMem => {
                let ptr = prog.global_vars_start() + op.operand;
                self.push(Const(ptr))
            }

            OpType::PushLocal => {
                let ptr = proc
                    .get_data()
                    .unwrap()
                    .var_mem_offset(self.bind_count + op.operand);
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushGlobal => {
                let ptr = prog.mem_start() + op.operand * 4;
                self.push(Const(ptr))
            }

            OpType::Offset | OpType::OffsetLoad => self.extend([Const(op.operand), I32(add)]),

            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Div => todo!(),
                IntrinsicType::Times => todo!(),

                IntrinsicType::Plus => self.push(I32(add)),
                IntrinsicType::Minus => self.push(I32(sub)),
                IntrinsicType::Greater => self.push(I32(gt_s)),
                IntrinsicType::GreaterE => self.push(I32(ge_s)),
                IntrinsicType::Lesser => self.push(I32(lt_s)),
                IntrinsicType::LesserE => self.push(I32(le_s)),
                IntrinsicType::And => self.push(I32(and)),
                IntrinsicType::Or => self.push(I32(or)),
                IntrinsicType::Xor => self.push(I32(xor)),
                IntrinsicType::Load8 => self.push(I32(load8_s)),
                IntrinsicType::Load16 => self.push(I32(load16_s)),
                IntrinsicType::Load32 => self.push(I32(load)),

                IntrinsicType::Store8 => self.extend([Call("swap".into()), I32(store8)]),
                IntrinsicType::Store16 => self.extend([Call("swap".into()), I32(store16)]),
                IntrinsicType::Store32 => self.extend([Call("swap".into()), I32(store)]),
                IntrinsicType::FdWrite => self.extend([Call("fd_write".into())]),

                IntrinsicType::Cast(_) => {}
            },

            OpType::Drop => self.push(Drop),
            OpType::Dup => self.push(Call("dup".into())),
            OpType::Swap => self.push(Call("swap".into())),
            OpType::Over => self.push(Call("over".into())),
            OpType::Rot => self.push(Call("rot".into())),

            OpType::Call => {
                let label = prog.get_proc(op).get_label();
                self.push(Call(label.into()));
            }

            OpType::Equal => self.push(I32(eq)),

            OpType::IfStart => {
                let (ins, outs) = prog.get_contract(op);
                let contract =
                    module.new_contract(&vec![WasmType::I32; ins], &vec![WasmType::I32; outs]);

                self.push(Block(BlockType::If, Some(Id(contract))))
            }

            OpType::Else => self.push(Else),

            OpType::EndIf | OpType::EndElse => self.push(End),

            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),

            OpType::Unpack => self.extend(prog.unpack_struct(op.index())),

            OpType::ExpectType => {}

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),

            OpType::CallInline => {
                let ProcType::Inline(start, end) = prog.get_proc(op).data else {
                    unreachable!();
                };

                for ip in start + 1..end {
                    let in_op = &prog.ops[ip];
                    self.append_op(prog, in_op, proc, module)?;
                }
            }

            OpType::PrepProc | OpType::PrepInline | OpType::EndProc | OpType::EndInline => {
                unreachable!()
            }
        }
        Ok(())
    }

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

    fn extend<I>(&mut self, instructions: I)
    where
        I: IntoIterator<Item = Instruction>,
    {
        self.code.extend(instructions);
    }

    fn store_local(&mut self, offset: i32, var_value: i32) {
        self.code.extend(vec![
            Const(offset),
            Call("push_local".into()),
            Const(var_value),
            I32(store),
        ]);
    }
}

impl From<&Contract> for (Vec<WasmType>, Vec<WasmType>) {
    fn from(contract: &Contract) -> Self {
        let (ins, outs) = contract.size();
        (vec![WasmType::I32; ins], vec![WasmType::I32; outs])
    }
}

impl Program {
    fn unpack_struct(&self, index: usize) -> Vec<Instruction> {
        let stk = &self.structs_types[index];
        let count = stk.units().len() as i32;
        let mut instructions = vec![];

        match count {
            1 => instructions.push(I32(load)),
            2 => instructions.extend(vec![
                Call("dup".into()),
                I32(load),
                Call("swap".into()),
                Const(4),
                I32(add),
                I32(load),
            ]),
            _ => {
                instructions.push(Call("bind_local".into()));

                for offset in 0..count {
                    instructions.extend(vec![
                        Const(4),
                        Call("push_local".into()),
                        I32(load),
                        Const(4 * offset),
                        I32(add),
                        I32(load),
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
