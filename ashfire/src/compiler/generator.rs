#![allow(unreachable_code)]
#![allow(dead_code)]
use std::{collections::HashMap, fs::File, io::BufWriter, path::PathBuf};

use super::types::{Op, OpType, Program, ProgramVisitor};
use anyhow::Result;
use wasm_backend::{wasm_types::*, *};

pub struct Generator {
    writer: BufWriter<File>,
    current_proc: Option<usize>,
    block_map: HashMap<i32, i32>,
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
    fn new(writer: BufWriter<File>) -> Self {
        Self {
            writer,
            current_proc: None,
            block_map: HashMap::new(),
        }
    }

    fn generate(&mut self, program: &Program) -> Result<()> {
        use Ident::*;
        use Instruction::*;
        use Scope::*;

        let i1 = &[WasmType::I32; 1];
        let i2 = &[WasmType::I32; 2];
        let i3 = &[WasmType::I32; 3];
        let i4 = &[WasmType::I32; 4];

        let mut wasm = Module::new();

        wasm.add_import("wasi_unstable", "fd_write", "fd_write", i4, i1);

        let mem = wasm.new_mem();
        wasm.add_export("memory", Bind::Mem(Id(mem)));

        wasm.add_fn("dup", i1, i2, vec![Get(Local, Id(0)), Get(Local, Id(0))]);
        wasm.add_fn("swap", i2, i2, vec![Get(Local, Id(1)), Get(Local, Id(0))]);
        wasm.add_fn("over", i2, i3, vec![Get(Local, Id(0)), Get(Local, Id(1)), Get(Local, Id(0))]);
        wasm.add_fn("rot", i3, i3, vec![Get(Local, Id(1)), Get(Local, Id(2)), Get(Local, Id(0))]);

        let stack_start = program.mem_size + program.final_data_size() + program.total_vars_size();
        let stk = wasm.add_global("LOCAL_STACK", WasmType::MutI32, stack_start);

        let aloc_local = wasm.add_fn("aloc_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::add),
            Set(Global, Id(stk)),
        ]);

        wasm.add_fn("free_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::sub),
            Set(Global, Id(stk)),
        ]);

        wasm.add_fn("bind_local", i1, &[], vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::store),
            Const(4),
            Call(Id(aloc_local)),
        ]);

        wasm.add_fn("push_local", i1, i1, vec![
            Get(Global, Id(stk)),
            Get(Local, Id(0)),
            I32(NumMethod::sub),
        ]);

        for op in program.ops.iter().cloned() {
            if let Some(_wasm_op) = self.generate_op(op, program) {
                todo!();
            }
        }

        Ok(())
    }

    fn generate_op(&mut self, op: Op, _program: &Program) -> Option<Vec<Instruction>> {
        Some(match op.typ {
            OpType::PushData(_) => todo!(),
            OpType::PushStr => todo!(),
            OpType::PushLocalMem => todo!(),
            OpType::PushGlobalMem => todo!(),
            OpType::PushLocal => todo!(),
            OpType::PushGlobal => todo!(),
            OpType::OffsetLoad => todo!(),
            OpType::Offset => todo!(),
            OpType::Intrinsic => todo!(),
            OpType::Dup => todo!(),
            OpType::Drop => todo!(),
            OpType::Swap => todo!(),
            OpType::Over => todo!(),
            OpType::Rot => todo!(),
            OpType::Call => todo!(),
            OpType::Equal => todo!(),
            OpType::PrepProc => todo!(),
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
            OpType::Unpack => todo!(),
            OpType::ExpectType => todo!(),
            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        })
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
