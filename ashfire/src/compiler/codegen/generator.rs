use std::io::Write;

use ashfire_types::{
    core::{Op, Operand, WORD_SIZE, WORD_USIZE},
    data::{Primitive, StructInfo, StructType, TypeDescr, TypeId},
    enums::{IntrinsicType, OpType},
    proc::{Binds, Mode, Proc},
};
use firelib::{Context, Result};
use wasm_backend::{wasm_types::*, Module};
use Ident::*;
use Instruction::*;
use NumMethod::*;
use Scope::*;

use super::types::{as_wasm, unpack_struct, FuncGen, Generator};
use crate::{compiler::program::*, target::Target};

impl Generator {
    fn generate_module(&mut self, program: &Program, target: Target) -> Result<Module> {
        let i1 = &[WasmType::I32; 1];
        let i2 = &[WasmType::I32; 2];
        let i3 = &[WasmType::I32; 3];

        let mut wasm = Module::new();

        let module = &target.module();
        for import in program.procs.iter().filter(|p| p.is_import()) {
            let (ins, outs) = as_wasm(&import.contract);
            let name = &import.name.as_str(program);
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

        let stk = wasm.add_global("LOCAL_STACK", WasmType::I32, program.stack_start(), true);

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

        let mut proc = None;
        for (ip, op @ Op(op_type, ..)) in program.ops.iter().enumerate() {
            match (op_type, proc) {
                (OpType::PrepProc | OpType::PrepInline, _) => proc = self.prep_proc(program, op)?,
                (OpType::EndProc, _) => self.end_proc(program, &mut wasm)?,
                (_, Some(proc)) => self
                    .current_fn()?
                    .append_op(program, op, ip, proc, &mut wasm)?,
                _ => (),
            }
        }

        wasm.set_data_offset(program.data_start() as usize);

        for data in program.get_all_data() {
            wasm.add_data(data.as_string(program));
        }

        if !program.global_vars.is_empty() {
            let padding = WORD_USIZE - program.data_size() % WORD_USIZE;
            if padding < WORD_USIZE {
                wasm.add_data((0..padding).map(|_| "\\00").collect());
            }

            for var in program.global_vars.units() {
                wasm.add_data_value(program.final_value(&var));
            }
        }

        Ok(wasm)
    }
}

impl FuncGen {
    fn append_op(
        &mut self, prog: &Program, op: &Op, ip: usize, proc: &Proc, module: &mut Module,
    ) -> Result<()> {
        let &Op(op_type, operand, ..) = op;

        match op_type {
            OpType::PushData(_) | OpType::PushGlobalMem => self.push(Const(operand)),

            OpType::PushStr => {
                let (size, offset) = prog.get_data(op).data();
                self.extend([Const(size), Const(offset + prog.data_start())]);
            }

            OpType::PushLocalMem => {
                let ptr = self.bind_offset + operand;
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushLocal => {
                let ptr = self.bind_offset + proc.get_data().unwrap().var_mem_offset(operand);
                self.extend([Const(ptr), Call("push_local".into())]);
            }

            OpType::PushGlobal => {
                let ptr = prog.global_vars_start() + operand * WORD_SIZE;
                self.push(Const(ptr));
            }

            OpType::Offset => self.extend([Const(operand), I32(add)]),

            OpType::Intrinsic => match IntrinsicType::from(operand.index()) {
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
                let contract = register_contract(prog, op.index(), module);
                self.push(Block(BlockType::If, Some(contract)));
            }

            OpType::Else => self.push(Else),

            OpType::EndIf | OpType::EndElse => self.push(End),

            OpType::BindStack => {
                let Binds(bindings) = &proc.binds[op.index()];
                let mut inst = vec![];
                let mut bind_size = 0;

                for (_, typ) in bindings {
                    if let &Some(id) = typ {
                        let type_def = prog.get_type_descr(TypeId(id));

                        let sizes: Vec<_> = match type_def {
                            TypeDescr::Structure(StructType(fields, _)) => {
                                fields.units().map(|unit| unit.size()).collect()
                            }
                            TypeDescr::Primitive(prim) => vec![prim.size()],
                            TypeDescr::Reference(ptr) => vec![ptr.size()],
                        };

                        for size in sizes {
                            bind_size += size as i32;
                            inst.extend([Const(bind_size), Call("bind_local".into())]);
                        }
                    } else {
                        bind_size += WORD_SIZE;
                        inst.extend([Const(bind_size), Call("bind_local".into())]);
                    }
                }

                self.extend(vec![Const(bind_size), Call("aloc_local".into())]);
                self.extend(inst);
                self.bind_offset += bind_size;
            }

            OpType::PushBind => {
                self.extend([Const(operand), Call("push_local".into())]);
            }

            OpType::LoadBind => {
                self.extend([Const(operand), Call("push_local".into()), I32(load)]);
            }

            OpType::PopBind => {
                let Binds(bindings) = &proc.binds[op.index()];

                let size = bindings.iter().fold(0, |acc, (_, typ)| {
                    acc + typ.map_or(WORD_USIZE, |id| prog.get_type_descr(TypeId(id)).size())
                }) as i32;

                self.extend(vec![Const(size), Call("free_local".into())]);
            }

            OpType::While => {
                let loop_label = Ident::Label(format!("while{}", op.operand()));
                let contract = register_contract(prog, ip, module);
                self.push(Block(BlockType::Loop(Some(loop_label)), Some(contract)));
            }

            OpType::Do => {
                let contract = register_contract(prog, ip, module);
                self.push(Block(BlockType::If, Some(contract)));
            }

            OpType::EndWhile => {
                let loop_label = Ident::Label(format!("while{}", op.operand()));
                self.extend([Br(loop_label), End, End]);
            }

            OpType::Unpack => self.extend(unpack_struct(prog.get_type_descr(TypeId(op.index())))),

            OpType::ExpectType => {}

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),

            OpType::CallInline => {
                let inlined_proc = prog.get_proc(op);
                let Mode::Inlined(start, end) = inlined_proc.mode else {
                    unreachable!();
                };

                for ip in start + 1..end {
                    let in_op = &prog.ops[ip];
                    self.append_op(prog, in_op, ip, inlined_proc, module)?;
                }
            }

            OpType::PrepProc | OpType::PrepInline | OpType::EndProc | OpType::EndInline => {
                unreachable!()
            }
        }
        Ok(())
    }
}

fn register_contract(prog: &Program, index: usize, module: &mut Module) -> Ident {
    let (ins, outs) = prog.get_contract(index);
    Ident::Id(module.new_contract(&vec![WasmType::I32; ins], &vec![WasmType::I32; outs]))
}

impl Program {
    pub fn final_value(&self, var: &Primitive) -> i32 {
        if matches!(var.type_id(), TypeId::STR) {
            let offset = self.get_data(var.value()).get_value();
            return offset + self.data_start();
        }

        var.value()
    }

    pub fn generate_wasm(&self, writer: impl Write, target: Target) -> Result<()> {
        Generator::new()
            .generate_module(self, target)?
            .write_text(writer)
            .with_context(|| "Failed to save the file")
    }
}
