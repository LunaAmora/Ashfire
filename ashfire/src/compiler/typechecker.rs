#![allow(dead_code)]
use super::types::{IntrinsicType, Loc, Op, OpType, Program, TokenType, TypeFrame, ValueType};
use anyhow::{anyhow, Result};
use itertools::Itertools;
use lib_types::{EvalStack, Stack};
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

    fn type_check_op(&mut self, op: &Op, _program: &Program) -> Result<()> {
        match op.typ {
            OpType::PushData(value) => match value {
                ValueType::Int => todo!(),
                ValueType::Bool => todo!(),
                ValueType::Ptr => todo!(),
                ValueType::Any => todo!(),
                ValueType::Type(_) => todo!(),
            },
            OpType::PushStr => todo!(),
            OpType::PushLocalMem => todo!(),
            OpType::PushGlobalMem => todo!(),
            OpType::PushLocal => todo!(),
            OpType::PushGlobal => todo!(),
            OpType::OffsetLoad => todo!(),
            OpType::Offset => todo!(),
            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus => todo!(),
                IntrinsicType::Minus => todo!(),
                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),
                IntrinsicType::Greater => todo!(),
                IntrinsicType::GreaterE => todo!(),
                IntrinsicType::Lesser => todo!(),
                IntrinsicType::LesserE => todo!(),
                IntrinsicType::And => todo!(),
                IntrinsicType::Or => todo!(),
                IntrinsicType::Xor => todo!(),
                IntrinsicType::Load8 => todo!(),
                IntrinsicType::Store8 => todo!(),
                IntrinsicType::Load16 => todo!(),
                IntrinsicType::Store16 => todo!(),
                IntrinsicType::Load32 => todo!(),
                IntrinsicType::Store32 => todo!(),
                IntrinsicType::FdWrite => todo!(),
                IntrinsicType::Cast(_) => todo!(),
            },
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
        };
    }
}

pub enum ArityType {
    Any,
    Same,
    Type(TokenType),
}

pub trait Expect<T>: Stack<T> {
    fn get_type(&self, t: &T) -> TokenType;
    fn get_loc(&self, t: &T) -> Loc;

    fn program_arity(
        &self, program: &Program, stack: &[T], contract: &[TokenType], loc: &Loc,
    ) -> Result<()>;

    fn program_exact(
        &self, program: &Program, stack: &[T], contract: &[TokenType], loc: &Loc,
    ) -> Result<()>;

    fn expect_exact_pop(
        &mut self, contract: &[TokenType], program: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.program_exact(program, self.slice(), contract, loc)?;
        Ok(self.pop_n(contract.len()))
    }

    fn expect_arity_pop(
        &mut self, contract: &[TokenType], program: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.program_arity(program, self.slice(), contract, loc)?;
        Ok(self.pop_n(contract.len()))
    }

    fn expect_pop(&mut self, arity: ArityType, loc: &Loc) -> Result<T> {
        self.expect_arity(1, arity, loc)?;
        self.pop().ok_or_else(|| anyhow!("unreachable"))
    }

    fn expect_arity(&self, n: usize, arity: ArityType, loc: &Loc) -> Result<()> {
        self.expect_stack_size(n, loc)?;

        let typ = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => self.get_type(self.get(0).expect("unreachable")),
            ArityType::Type(typ) => typ,
        };

        for i in 0..n {
            self.expect_type(self.get(i).expect("unreachable"), typ, loc)?;
        }

        Ok(())
    }

    fn expect_type(&self, frame: &T, expected: TokenType, loc: &Loc) -> Result<()> {
        let typ = self.get_type(frame);
        let frame_loc = self.get_loc(frame);

        if !equals_any!(typ, ValueType::Any, expected) {
            anyhow::bail!(
                concat!(
                    "{}Expected type `{:?}`, but found `{:?}`\n",
                    "{}[INFO] The type found was declared here"
                ),
                loc,
                expected,
                typ,
                frame_loc
            );
        }
        Ok(())
    }

    fn expect_stack_size(&self, n: usize, loc: &Loc) -> Result<()> {
        let len = self.len();
        anyhow::ensure!(
            len >= n,
            concat!(
                "Stack has less elements than expected\n",
                "{}[INFO] Expected `{}` elements, but found `{}`"
            ),
            loc,
            n,
            len
        );
        Ok(())
    }
}

impl Program {
    fn expect_arity(
        &self, _stack: &[TypeFrame], _contract: &[TokenType], _loc: &Loc,
    ) -> Result<()> {
        todo!("369")
    }

    fn expect_stack_exact(
        &self, stack: &[TypeFrame], contract: &[TokenType], loc: &Loc,
    ) -> Result<()> {
        anyhow::ensure!(
            stack.len() == contract.len() && self.expect_arity(stack, contract, loc).is_ok(),
            concat!(
                "Found stack at the end of the context does match the expected types:\n",
                "{}[INFO] Expected types: {}\n",
                "{}[INFO] Actual types:   {}"
            ),
            loc,
            format_stack(contract),
            loc,
            self.format_frames(stack, true),
        );
        Ok(())
    }

    fn format_frames(&self, stack: &[TypeFrame], verbose: bool) -> String {
        let types: Vec<TokenType> = stack.iter().map(|t| t.typ).collect();
        if verbose {
            let info = &stack
                .iter()
                .map(|t| {
                    format!("{}[INFO] Type `{}` was declared here", t.loc, self.type_name(t.typ))
                })
                .join("\n");
            format!("{}\n{info}", format_stack(&types))
        } else {
            format_stack(&types)
        }
    }
}

fn format_stack(stack: &[TokenType]) -> String {
    format!("[{}] ->", stack.iter().map(|t| format!("<{:?}>", t)).join(","))
}

impl Expect<TypeFrame> for EvalStack<TypeFrame> {
    fn get_type(&self, t: &TypeFrame) -> TokenType {
        t.typ
    }

    fn get_loc(&self, t: &TypeFrame) -> Loc {
        t.loc.clone()
    }

    fn program_arity(
        &self, program: &Program, stack: &[TypeFrame], contract: &[TokenType], loc: &Loc,
    ) -> Result<()> {
        program.expect_arity(stack, contract, loc)
    }

    fn program_exact(
        &self, program: &Program, stack: &[TypeFrame], contract: &[TokenType], loc: &Loc,
    ) -> Result<()> {
        program.expect_stack_exact(stack, contract, loc)
    }
}

pub fn type_check(program: &mut Program) -> Result<()> {
    let mut checker = TypeChecker::new();

    for op in program.ops.iter() {
        checker.type_check_op(op, program)?
    }

    Ok(())
}
