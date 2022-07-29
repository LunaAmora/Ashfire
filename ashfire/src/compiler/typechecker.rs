#![allow(dead_code)]
use super::types::*;
use anyhow::{anyhow, Result};
use itertools::Itertools;
use lib_types::{EvalStack, Stack};

struct TypeBlock {
    data_stack: EvalStack<TypeFrame>,
    start_op: usize,
}

#[derive(Default)]
struct TypeChecker {
    block_stack: Vec<TypeBlock>,
    bind_stack: Vec<TypeFrame>,
    data_stack: EvalStack<TypeFrame>,
    current_proc: Option<usize>,
}

impl ProgramVisitor for TypeChecker {
    fn set_index(&mut self, i: Option<usize>) {
        self.current_proc = i;
    }

    fn get_index(&self) -> Option<usize> {
        self.current_proc
    }
}

impl TypeChecker {
    fn new() -> Self {
        Self { ..Default::default() }
    }

    fn type_check(&mut self, program: &mut Program) -> Result<()> {
        for op in &program.ops {
            self.type_check_op(op, program)?;
        }
        Ok(())
    }

    fn type_check_op(&mut self, op: &Op, program: &Program) -> Result<()> {
        let loc = &op.loc;
        match op.typ {
            OpType::PushData(value) => match value {
                ValueType::Int | ValueType::Bool | ValueType::Ptr => self.push_value(value, loc),
                ValueType::Any | ValueType::Type(_) => unreachable!(),
            },
            OpType::PushStr => {
                self.push_value(ValueType::Int, loc);
                self.push_value(ValueType::Ptr, loc);
            }
            OpType::PushLocalMem |
            OpType::PushGlobalMem |
            OpType::PushLocal |
            OpType::PushGlobal => self.push_value(ValueType::Ptr, loc),
            OpType::OffsetLoad => todo!(),
            OpType::Offset => todo!(),
            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus | IntrinsicType::Minus => todo!(),
                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),
                IntrinsicType::Greater |
                IntrinsicType::GreaterE |
                IntrinsicType::Lesser |
                IntrinsicType::LesserE => todo!(),
                IntrinsicType::And | IntrinsicType::Or | IntrinsicType::Xor => todo!(),
                IntrinsicType::Load8 | IntrinsicType::Load16 | IntrinsicType::Load32 => todo!(),
                IntrinsicType::Store8 | IntrinsicType::Store16 | IntrinsicType::Store32 => todo!(),
                IntrinsicType::FdWrite => todo!(),
                IntrinsicType::Cast(n) => {
                    self.data_stack.expect_pop(loc)?;

                    let cast: TokenType = match n {
                        1.. => ValueType::from((n - 1) as usize).into(),
                        0 => todo!("invalid value"),
                        _ => todo!("typechecking casting to ptr type not implemented yet"),
                    };
                    self.push_frame(cast, loc);
                }
            },
            OpType::Dup => todo!(),
            OpType::Drop => todo!(),
            OpType::Swap => todo!(),
            OpType::Over => todo!(),
            OpType::Rot => todo!(),
            OpType::Call => todo!(),
            OpType::Equal => todo!(),
            OpType::PrepProc => {
                let ins = self
                    .visit_proc(program, op.operand as usize)
                    .contract
                    .ins
                    .clone();

                for typ in ins {
                    self.push_frame(typ, loc);
                }
            }
            OpType::IfStart => todo!(),
            OpType::Else => todo!(),
            OpType::EndIf => todo!(),
            OpType::EndElse => todo!(),
            OpType::EndProc => {
                let mut outs = self
                    .current_proc(program)
                    .expect("unreachable")
                    .contract
                    .outs
                    .clone();
                outs.reverse();

                self.data_stack.expect_exact_pop(&outs, program, loc)?;
                self.exit_proc();
                self.data_stack = Default::default();
            }
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
        Ok(())
    }

    fn push_value(&mut self, value: ValueType, loc: &Loc) {
        self.data_stack.push((value, loc).into())
    }

    fn push_frame(&mut self, typ: TokenType, loc: &Loc) {
        self.data_stack.push((typ, loc).into())
    }
}

pub enum ArityType {
    Any,
    Same,
    Type(TokenType),
}

pub trait Expect<T>: Stack<T> {
    fn program_arity(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()>;
    fn program_exact(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()>;
    fn get_type(&self, t: &T) -> TokenType;
    fn get_loc(&self, t: &T) -> Loc;

    fn expect_exact_pop(
        &mut self, contract: &[TokenType], program: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.program_exact(program, contract, loc)?;
        Ok(self.pop_n(contract.len()))
    }

    fn expect_contract_pop(
        &mut self, contr: &[TokenType], prog: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contr.len(), loc)?;
        self.program_arity(prog, contr, loc)?;
        Ok(self.pop_n(contr.len()))
    }

    fn expect_arity_pop(&mut self, n: usize, arity: ArityType, loc: &Loc) -> Result<Vec<T>> {
        self.expect_arity(n, arity, loc)?;
        Ok(self.pop_n(n))
    }

    fn expect_peek(&mut self, arity_t: TokenType, loc: &Loc) -> Result<&T> {
        self.expect_arity(1, ArityType::Type(arity_t), loc)?;
        self.peek().ok_or_else(|| anyhow!("unreachable"))
    }

    fn expect_pop(&mut self, loc: &Loc) -> Result<T> {
        self.expect_stack_size(1, loc)?;
        self.pop().ok_or_else(|| anyhow!("unreachable"))
    }

    fn expect_pop_type(&mut self, arity_t: TokenType, loc: &Loc) -> Result<T> {
        self.expect_arity(1, ArityType::Type(arity_t), loc)?;
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

impl Expect<TypeFrame> for EvalStack<TypeFrame> {
    fn get_type(&self, t: &TypeFrame) -> TokenType {
        t.typ
    }

    fn get_loc(&self, t: &TypeFrame) -> Loc {
        t.loc.clone()
    }

    fn program_arity(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        program.expect_arity(self, contract, loc)
    }

    fn program_exact(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        program.expect_stack_exact(self, contract, loc)
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
    format!("[{}] ->", stack.iter().map(|t| format!("<{:?}>", t)).join(", "))
}

pub fn type_check(program: &mut Program) -> Result<()> {
    let mut checker = TypeChecker::new();
    checker.type_check(program)?;

    Ok(())
}
