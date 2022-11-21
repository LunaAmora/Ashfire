use std::{fs::File, io::BufWriter, path::Path};

use firelib::anyhow::Result;
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use NumMethod::*;
use Scope::*;

use super::types::{FuncGen, Generator};
use crate::compiler::{
    program::*,
    types::{
        core::{Op, Operand, WORD_SIZE, WORD_USIZE},
        data::StructInfo,
        enums::{IntrinsicType, OpType},
        proc::{Mode, Proc},
    },
};

impl Generator {
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
            Const(WORD_SIZE),
            Call(Id(aloc_local)),
        ]);

        wasm.add_fn("push_local", i1, i1, vec![Get(global, Id(stk)), Get(local, Id(0)), I32(sub)]);

        let mut skip = false;
        for op in &program.ops {
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

        for data in program.get_all_data() {
            wasm.add_data(data.as_string(program));
        }

        if !program.global_vars.is_empty() {
            let padding = WORD_USIZE - program.data_size() % WORD_USIZE;
            if padding < WORD_USIZE {
                wasm.add_data((0..padding).map(|_| "\\00").collect());
            }

            for var in program.global_vars.units() {
                wasm.add_data_value(var.value());
            }
        }

        Ok(wasm)
    }
}

impl FuncGen {
    fn append_op(
        &mut self, prog: &Program, op: &Op, proc: &Proc, module: &mut Module,
    ) -> Result<()> {
        match op.op_type {
            OpType::PushData(_) => self.push(Const(op.operand)),

            OpType::PushStr => {
                let (size, offset) = prog.get_data(op).data();
                self.extend([Const(size), Const(offset)]);
            }

            OpType::PushLocalMem => {
                let ptr = self.bind_count * WORD_SIZE + op.operand;
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushGlobalMem => {
                let ptr = prog.global_vars_start() + op.operand;
                self.push(Const(ptr));
            }

            OpType::PushLocal => {
                let ptr = proc
                    .get_data()
                    .unwrap()
                    .var_mem_offset(self.bind_count + op.operand);
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushGlobal => {
                let ptr = prog.mem_start() + op.operand * WORD_SIZE;
                self.push(Const(ptr));
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
                let label = prog.get_proc(op).name.as_str(prog);
                self.push(Call(label.into()));
            }

            OpType::Equal => self.push(I32(eq)),

            OpType::IfStart => {
                let (ins, outs) = prog.get_contract(op);
                let contract =
                    module.new_contract(&vec![WasmType::I32; ins], &vec![WasmType::I32; outs]);

                self.push(Block(BlockType::If, Some(Id(contract))));
            }

            OpType::Else => self.push(Else),

            OpType::EndIf | OpType::EndElse => self.push(End),

            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),

            OpType::Unpack => self.extend(prog.structs_types[op.index()].unpack_struct()),

            OpType::ExpectType => {}

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),

            OpType::CallInline => {
                let Mode::Inline(start, end) = prog.get_proc(op).mode else {
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
}

impl Program {
    pub fn generate_wasm(&self, output: &Path) -> Result<()> {
        info!("Generating {:?}", output);

        let writer = BufWriter::new(File::create(output)?);
        Generator::new().generate_module(self)?.write_text(writer)
    }
}
