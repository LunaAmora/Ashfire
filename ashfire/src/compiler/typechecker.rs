#![allow(dead_code)]
use super::types::{Op, Program, TypeFrame};
use anyhow::Result;
use lib_types::EvalStack;
use std::ops::{Deref, DerefMut};

#[derive(Default)]
struct TypeStack(EvalStack<TypeFrame>);

impl Deref for TypeStack {
    type Target = EvalStack<TypeFrame>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TypeStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct TypeBlock {
    data_stack: TypeStack,
    start_op: usize,
}

#[derive(Default)]
struct TypeChecker {
    block_stack: Vec<TypeBlock>,
    bind_stack: Vec<TypeFrame>,
    data_stack: TypeStack,
}

impl TypeChecker {
    fn new() -> Self {
        Self { ..Default::default() }
    }

    fn type_check_op(&mut self, _op: &Op, _program: &Program) -> Result<()> {
        todo!()
    }
}

pub fn type_check(program: &Program) -> Result<()> {
    let mut checker = TypeChecker::new();

    for op in program.ops.iter() {
        checker.type_check_op(op, program)?
    }

    Ok(())
}
