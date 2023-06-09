use std::io::Write;

use ashfire_types::{
    core::{DataKey, Typed, WORD_USIZE},
    data::{DataType, Primitive, StructInfo, TypeId},
    enums::{ControlOp, DataOp, IndexOp, IntrinsicType, MemOp, OpType, StackOp},
    proc::{Binds, ModeData, Proc},
};
use firelib::{Context, Result};
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use NumMethod::*;
use Scope::*;

use super::types::{unpack_type, FuncGen, Generator};
use crate::{compiler::ctx::*, target::Target};

impl Generator {
    fn generate_module(&mut self, ctx: &Ctx, target: Target) -> Result<Module> {
        let i1 = &[WasmType::I32; 1];
        let i2 = &[WasmType::I32; 2];
        let i3 = &[WasmType::I32; 3];

        let mut wasm = Module::new();

        let module = &target.module();
        for import in ctx.procs().iter().filter(|p| p.is_import()) {
            let (ins, outs) = import.contract.as_vec(WasmType::I32, WasmType::I32);
            let name = &import.name.as_str(ctx);
            wasm.add_import(module, name, name, &ins, &outs);
        }

        let mem = wasm.new_mem();

        if target.imports_mem() {
            wasm.add_mem_import(module, "memory", Bind::Mem(Id(mem)));
        } else {
            wasm.add_export("memory", Bind::Mem(Id(mem)));
        }

        wasm.add_fn("dup", i1, i2, vec![Get(local, Id(0)), Get(local, Id(0))]);
        wasm.add_fn("swap", i2, i2, vec![Get(local, Id(1)), Get(local, Id(0))]);
        wasm.add_fn("over", i2, i3, vec![Get(local, Id(0)), Get(local, Id(1)), Get(local, Id(0))]);
        wasm.add_fn("rot", i3, i3, vec![Get(local, Id(1)), Get(local, Id(2)), Get(local, Id(0))]);

        let stk = wasm.add_global("LOCAL_STACK", WasmType::I32, ctx.stack_start().into(), true);

        wasm.add_fn("aloc_local", i1, &[], vec![
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

        wasm.add_fn("bind_local", i2, &[], vec![
            Get(global, Id(stk)),
            Get(local, Id(1)),
            I32(sub),
            Get(local, Id(0)),
            I32(store),
        ]);

        wasm.add_fn("push_local", i1, i1, vec![Get(global, Id(stk)), Get(local, Id(0)), I32(sub)]);

        let mut proc_ctx = None;
        for (ip, &(op_type, _)) in ctx.ops().iter().enumerate() {
            match (op_type, proc_ctx) {
                (OpType::ControlOp(ControlOp::PrepProc | ControlOp::PrepInline, proc_ip), _) => {
                    proc_ctx = self.prep_proc(ctx, proc_ip)?;
                }

                (OpType::ControlOp(ControlOp::EndProc, _), _) => {
                    self.end_proc(ctx, &mut wasm)?;
                    proc_ctx = None;
                }

                (_, Some(proc)) => self.current_fn()?.append_op(ctx, ip, proc, &mut wasm),

                _ => (),
            }
        }

        wasm.set_data_offset(ctx.data_start());

        for data in ctx.get_all_data() {
            wasm.add_data(data.as_string(ctx));
        }

        if !ctx.global_vars().is_empty() {
            let padding = WORD_USIZE - ctx.data_size() % WORD_USIZE;
            if padding < WORD_USIZE {
                wasm.add_data((0..padding).map(|_| "\\00").collect());
            }

            for var in ctx.global_vars().units() {
                wasm.add_data_value(ctx.final_value(&var));
            }
        }

        Ok(wasm)
    }
}

impl FuncGen {
    fn append_op(&mut self, ctx: &Ctx, ip: usize, proc: &Proc, module: &mut Module) {
        let (op_type, _) = ctx.ops()[ip];
        match op_type {
            OpType::PushData(_, operand) => self.push(Const(operand)),

            OpType::DataOp(op, data_type) => match op {
                DataOp::PushStr => {
                    let (size, offset) = ctx.get_data(data_type).data();
                    let ptr = offset + ctx.data_start();
                    self.extend([Const(size.into()), Const(ptr.into())]);
                }
            },

            OpType::MemOp(op, offset) => match op {
                MemOp::PushLocalMem => {
                    let ptr = self.bind_offset + offset;
                    self.extend([Const(ptr.into()), Call("push_local".into())]);
                }

                MemOp::PushGlobalMem => self.push(Const(offset.into())),

                MemOp::PushLocal => {
                    let data = proc
                        .get_data()
                        .expect("PushLocal can not be used outside of a procedure");
                    let ptr = self.bind_offset + data.var_mem_offset(offset);
                    self.extend([Const(ptr.into()), Call("push_local".into())]);
                }

                MemOp::PushGlobal => {
                    let ptr = ctx.global_vars_start() + offset;
                    self.push(Const(ptr.into()));
                }

                MemOp::Offset => self.extend([Const(offset.into()), I32(add)]),

                MemOp::PushBind => {
                    self.extend([Const(offset.into()), Call("push_local".into())]);
                }

                MemOp::LoadBind => {
                    self.extend([Const(offset.into()), Call("push_local".into()), I32(load)]);
                }
            },

            OpType::UnpackType(None) => self.push(I32(load)),

            OpType::UnpackType(Some(data_type)) => {
                self.extend(unpack_type(&ctx.get_type_descr(data_type)));
            }

            OpType::IndexOp(op, index) => match op {
                IndexOp::Call => {
                    let label: &str = &ctx.get_proc(index).name.as_str(ctx);
                    self.push(Call(label.into()));
                }

                IndexOp::CallInline => {
                    let inlined_proc = ctx.get_proc(index);
                    let ModeData::Inlined(start, end) = inlined_proc.mode_data else {
                        unreachable!();
                    };

                    for inlined_ip in (start + 1)..end {
                        self.append_op(ctx, inlined_ip, inlined_proc, module);
                    }
                }

                IndexOp::PushBind | IndexOp::LoadBind => unreachable!(),
            },

            OpType::StackOp(op) => match op {
                StackOp::Drop => self.push(Drop),
                StackOp::Dup => self.push(Call("dup".into())),
                StackOp::Swap => self.push(Call("swap".into())),
                StackOp::Over => self.push(Call("over".into())),
                StackOp::Rot => self.push(Call("rot".into())),
                StackOp::Equal => self.push(I32(eq)),
            },

            OpType::Intrinsic(intrinsic) => match intrinsic {
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

                IntrinsicType::Cast(_) => {}
            },

            OpType::ControlOp(op, index) => match op {
                ControlOp::IfStart => {
                    let contract = register_contract(ctx, index, module);
                    self.push(Block(BlockType::If, Some(contract)));
                }

                ControlOp::Else => self.push(Else),

                ControlOp::EndIf | ControlOp::EndElse => self.push(End),

                ControlOp::While => {
                    let loop_label = Ident::Label(format!("while{index}"));
                    let contract = register_contract(ctx, ip, module);
                    self.push(Block(BlockType::Loop(Some(loop_label)), Some(contract)));
                }

                ControlOp::Do => {
                    let contract = register_contract(ctx, ip, module);
                    self.push(Block(BlockType::If, Some(contract)));
                }

                ControlOp::EndWhile => {
                    let loop_label = Ident::Label(format!("while{index}"));
                    self.extend([Br(loop_label), End, End]);
                }

                ControlOp::BindStack => {
                    let Binds(bindings) = &proc.binds[index];
                    let mut inst = vec![];
                    let mut bind_size = 0;

                    for (_, typ) in bindings {
                        if let &Some(id) = typ {
                            for size in ctx.get_type_descr(id).units().map(|unit| unit.size()) {
                                bind_size += size;
                                inst.extend([Const(bind_size.into()), Call("bind_local".into())]);
                            }
                        } else {
                            bind_size += WORD_USIZE;
                            inst.extend([Const(bind_size.into()), Call("bind_local".into())]);
                        }
                    }

                    self.extend(vec![Const(bind_size.into()), Call("aloc_local".into())]);
                    self.extend(inst);
                    self.bind_offset += bind_size;
                }

                ControlOp::PopBind => {
                    let Binds(bindings) = &proc.binds[index];

                    let size = bindings.iter().fold(0, |acc, (_, typ)| {
                        acc + typ.map_or(WORD_USIZE, |id| ctx.get_type_descr(id).size())
                    });

                    self.extend(vec![Const(size.into()), Call("free_local".into())]);
                }

                ControlOp::CaseStart => todo!(),
                ControlOp::CaseMatch => todo!(),
                ControlOp::CaseOption => todo!(),
                ControlOp::EndCase => todo!(),

                ControlOp::PrepProc |
                ControlOp::PrepInline |
                ControlOp::EndProc |
                ControlOp::EndInline => {
                    unreachable!()
                }
            },

            OpType::ExpectType(_) => {}
            OpType::Offset(_) => unreachable!(),
        }
    }
}

fn register_contract(ctx: &Ctx, index: usize, module: &mut Module) -> Ident {
    let (ins, outs) = ctx.get_contract(index);
    Ident::Id(module.new_contract(&vec![WasmType::I32; ins], &vec![WasmType::I32; outs]))
}

impl Ctx {
    pub fn final_value(&self, var: &Primitive) -> i32 {
        match var.get_type() {
            DataType(TypeId::STR) => {
                let index = DataKey(var.value().try_into().expect("ICE"));
                let offset = self.get_data(index).value() + self.data_start();
                offset.into()
            }
            _ => var.value(),
        }
    }

    pub fn generate_wasm(&self, writer: impl Write, target: Target) -> Result<()> {
        Generator::new()
            .generate_module(self, target)?
            .write_text(writer)
            .with_context(|| "Failed to save the file")
    }
}
