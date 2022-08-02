#![allow(unreachable_code)]
#![allow(dead_code)]
use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

use super::types::{Op, OpType, Program, ProgramVisitor};
use anyhow::Result;

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
        let stack_start = program.final_data_size() + program.mem_size + program.total_vars_size();

        self.writer.write_all(format!(concat!(
            "(import \"wasi_unstable\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))\n\n",

            "(memory 1)\n",
            "(export \"memory\" (memory 0))\n\n",

            "(global $LOCAL_STACK (mut i32) (i32.const {}))\n\n",

            "(func $dup  (param i32 )        (result i32 i32)     local.get 0 local.get 0)\n",
            "(func $swap (param i32 i32)     (result i32 i32)     local.get 1 local.get 0)\n",
            "(func $over (param i32 i32)     (result i32 i32 i32) local.get 0 local.get 1 local.get 0)\n",
            "(func $rot  (param i32 i32 i32) (result i32 i32 i32) local.get 1 local.get 2 local.get 0)\n\n",

            "(func $aloc_local (param i32) global.get $LOCAL_STACK local.get 0 i32.add global.set $LOCAL_STACK)\n",
            "(func $free_local (param i32) global.get $LOCAL_STACK local.get 0 i32.sub global.set $LOCAL_STACK)\n",
            "(func $bind_local (param i32) global.get $LOCAL_STACK local.get 0 i32.store i32.const 4 call $aloc_local)\n",
            "(func $push_local (param i32) (result i32) global.get $LOCAL_STACK local.get 0 i32.sub)"),
            stack_start).as_bytes())?;

        for op in program.ops.iter().cloned() {
            if let Some(wasm_op) = self.generate_op(op, program) {
                self.writer.write_all(wasm_op.as_bytes())?;
            }
        }

        Ok(())
    }

    fn generate_op(&mut self, op: Op, _program: &Program) -> Option<String> {
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
