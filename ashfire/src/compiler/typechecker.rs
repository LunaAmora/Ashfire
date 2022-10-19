use anyhow::{bail, ensure, Context, Result};
use ashlib::{EvalStack, Stack};
use firelib::equals_any;
use itertools::Itertools;

use super::{program::*, types::*};

type DataStack = EvalStack<TypeFrame>;

#[derive(Clone)]
struct TypeBlock {
    data_stack: DataStack,
    start_op: usize,
}

impl TypeBlock {
    fn new(other: &DataStack, start_op: usize) -> Self {
        Self { data_stack: other.clone(), start_op }
    }
}

impl From<TypeBlock> for (DataStack, usize) {
    fn from(block: TypeBlock) -> Self {
        (block.data_stack, block.start_op)
    }
}

#[derive(Default)]
pub struct TypeChecker {
    block_stack: Vec<TypeBlock>,
    _bind_stack: Vec<TypeFrame>,
    data_stack: DataStack,
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
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn type_check(&mut self, program: &mut Program) -> Result<()> {
        for ip in 0..program.ops.len() {
            self.type_check_op(ip, program)?;
        }
        Ok(())
    }

    fn type_check_op(&mut self, ip: usize, program: &mut Program) -> Result<()> {
        let op = program.ops.get(ip).unwrap();
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

            OpType::OffsetLoad => {
                let op = &op.clone();
                let offset_type = self.expect_struct_pointer(program, ip, op, ".")?;
                self.push_frame(offset_type, &op.loc);
            }

            OpType::Offset => {
                let op = &op.clone();
                match self.expect_struct_pointer(program, ip, op, ".*")? {
                    TokenType::DataType(offset_type) => {
                        self.push_frame(TokenType::DataPtr(offset_type), &op.loc)
                    }
                    _ => unreachable!(),
                }
            }

            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus | IntrinsicType::Minus => {
                    let [top, _] =
                        self.data_stack
                            .expect_arity_pop(ArityType::Same, program, loc)?;
                    self.push_frame(top.typ, loc);
                }

                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),

                IntrinsicType::Greater |
                IntrinsicType::GreaterE |
                IntrinsicType::Lesser |
                IntrinsicType::LesserE => todo!(),

                IntrinsicType::And | IntrinsicType::Or | IntrinsicType::Xor => {
                    self.data_stack.expect_array_pop([INT, INT], program, loc)?;
                    self.push_frame(INT, loc);
                }

                IntrinsicType::Load8 | IntrinsicType::Load16 | IntrinsicType::Load32 => {
                    self.data_stack.expect_pop_type(PTR, program, loc)?;
                    self.push_frame(ANY, loc)
                }

                IntrinsicType::Store8 | IntrinsicType::Store16 | IntrinsicType::Store32 => {
                    self.data_stack.expect_array_pop([ANY, ANY], program, loc)?;
                }

                IntrinsicType::FdWrite => {
                    self.data_stack
                        .expect_array_pop([INT, PTR, INT, PTR], program, loc)?;
                    self.push_frame(PTR, loc);
                }

                IntrinsicType::Cast(n) => {
                    self.data_stack.expect_pop(loc)?;

                    let cast: TokenType = match n {
                        1.. => ValueType::from((n - 1) as usize).into(),
                        0 => unreachable!(),
                        n => TokenType::DataPtr(ValueType::from((-n - 1) as usize)),
                    };
                    self.push_frame(cast, loc);
                }
            },

            OpType::Drop => {
                self.data_stack.expect_pop(loc)?;
            }

            OpType::Dup => {
                let typ = self
                    .data_stack
                    .expect_peek(ArityType::Any, program, loc)?
                    .typ;
                self.push_frame(typ, loc);
            }

            OpType::Swap => {
                let [a, b] = self.data_stack.expect_pop_n(loc)?;
                self.data_stack.push_n([b, a]);
            }

            OpType::Over => {
                let [a, b] = self.data_stack.expect_pop_n(loc)?;
                self.data_stack.push_n([a.clone(), b, a]);
            }

            OpType::Rot => {
                let [a, b, c] = self.data_stack.expect_pop_n(loc)?;
                self.data_stack.push_n([b, c, a]);
            }

            OpType::Call => {
                let Proc { contract: Contract { ins, outs }, .. } =
                    program.procs.get(op.operand as usize).unwrap();
                {
                    self.data_stack.expect_contract_pop(ins, program, loc)?;
                    for typ in outs {
                        self.push_frame(*typ, loc);
                    }
                }
            }

            OpType::Equal => {
                self.data_stack
                    .expect_arity_pop::<2>(ArityType::Same, program, loc)?;
                self.push_frame(BOOL, loc);
            }

            OpType::PrepProc => {
                for typ in &self.visit_proc(program, op.operand as usize).contract.ins {
                    self.push_frame(*typ, loc);
                }
            }

            OpType::IfStart => {
                self.data_stack.expect_pop_type(BOOL, program, loc)?;
                self.block_stack.push(TypeBlock::new(&self.data_stack, ip));
                self.data_stack.reset_max_count();
                program.set_operand(ip, ip);
            }

            OpType::Else => {
                let (old_stack, start_op) = (self.block_stack.last().cloned().unwrap()).into();
                self.block_stack
                    .push(TypeBlock::new(&self.data_stack, start_op));
                self.data_stack = DataStack::new(&old_stack);
            }

            OpType::EndIf => {
                let (expected, start_op) = self.block_stack.pop().unwrap().into();

                self.expect_stack_arity(
                    program,
                    &expected,
                    loc,
                    concat!(
                        "Else-less if block is not allowed to alter ",
                        "the types of the arguments on the data stack."
                    ),
                )?;

                let ins = self.data_stack.min_count.abs();
                let out = self.data_stack.stack_count + ins;
                info!("{op} {ins} {out}");

                program
                    .block_contracts
                    .insert(start_op, (ins as usize, out as usize));

                self.data_stack.min_count += expected.stack_count;
                self.data_stack.stack_count = expected.stack_count;
            }

            OpType::EndElse => {
                let (expected, start_op) = self.block_stack.pop().unwrap().into();

                self.expect_stack_arity(
                    program,
                    &expected,
                    loc,
                    concat!(
                        "Both branches of the if-block must produce ",
                        "the same types of the arguments on the data stack"
                    ),
                )?;

                let ins = (self.data_stack.min_count).min(expected.min_count).abs();
                let out = (self.data_stack.stack_count).max(expected.stack_count) + ins;

                program
                    .block_contracts
                    .insert(start_op, (ins as usize, out as usize));

                let old_stack = self.block_stack.pop().unwrap().data_stack;

                self.data_stack.min_count = old_stack.stack_count + ins;
                self.data_stack.stack_count = old_stack.stack_count;
            }

            OpType::EndProc => {
                let mut outs = self.current_proc(program).unwrap().contract.outs.clone();
                outs.reverse();

                if outs.is_empty() {
                    self.data_stack.program_exact(program, &[], loc)?;
                } else {
                    self.data_stack.expect_exact_pop(&outs, program, loc)?;
                }

                self.data_stack = Default::default();
                self.exit_proc();
            }

            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),

            OpType::Unpack => match self.data_stack.expect_pop(loc)?.typ {
                TokenType::DataPtr(n) => {
                    let index = usize::from(n);
                    let stk = program.structs_types.get(index).unwrap();

                    for typ in stk.members.iter().map(|member| member.typ) {
                        self.push_frame(typ, loc);
                    }

                    program.set_operand(ip, index);
                }
                top => {
                    bail!("{loc}Cannot unpack element of type: `{}`", program.type_name(top));
                }
            },

            OpType::ExpectType => {
                let typ = match op.operand {
                    0 => unreachable!(),
                    n @ 1.. => TokenType::DataType(ValueType::from((n - 1) as usize)),
                    n => TokenType::DataPtr(ValueType::from((-n - 1) as usize)),
                };
                self.data_stack
                    .expect_peek(ArityType::Type(typ), program, loc)?;
            }

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

    fn expect_struct_pointer(
        &mut self, program: &mut Program, ip: usize, op: &Op, prefix: &str,
    ) -> Result<TokenType> {
        match self.data_stack.expect_pop(&op.loc)?.typ {
            TokenType::DataPtr(value) => {
                let word = program.get_word(op.operand).strip_prefix(prefix).unwrap();
                let stk = program.structs_types.get(usize::from(value)).unwrap();

                let index = stk
                    .members
                    .iter()
                    .position(|mem| mem.name == word)
                    .with_context(|| {
                        format!(
                            "{}The struct {} does not contain a member with name: `{word}`",
                            op.loc, stk.name
                        )
                    })?;

                let result = stk.members.get(index).unwrap().typ;

                program.set_operand(ip, index * 4);
                Ok(result)
            }
            typ => bail!("Cannot `.` access elements of type: `{}`", program.type_name(typ)),
        }
    }

    fn expect_stack_arity(
        &self, program: &Program, expected: &[TypeFrame], loc: &Loc, error_text: &str,
    ) -> Result<()> {
        let expected: Vec<TokenType> = expected.iter().map(|a| a.typ).collect();
        if let Err(err) = program.expect_exact(&self.data_stack, &expected, loc) {
            bail!("{}\n[ERROR] {}", error_text, err)
        } else {
            Ok(())
        }
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
    fn program_type(
        &self, program: &Program, frame: &T, expected: TokenType, loc: &Loc,
    ) -> Result<()>;
    fn get_type(&self, t: &T) -> TokenType;

    fn expect_exact_pop(
        &mut self, contract: &[TokenType], program: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.program_exact(program, contract, loc)?;
        self.pop_n(contract.len())
    }

    fn expect_contract_pop(
        &mut self, contr: &[TokenType], program: &Program, loc: &Loc,
    ) -> Result<Vec<T>> {
        self.expect_stack_size(contr.len(), loc)?;
        self.program_arity(program, contr, loc)?;
        self.pop_n(contr.len())
    }

    fn expect_array_pop<const N: usize>(
        &mut self, contr: [TokenType; N], program: &Program, loc: &Loc,
    ) -> Result<[T; N]> {
        self.expect_stack_size(contr.len(), loc)?;
        self.program_arity(program, &contr, loc)?;
        self.pop_array()
    }

    fn expect_arity_pop<const N: usize>(
        &mut self, arity: ArityType, program: &Program, loc: &Loc,
    ) -> Result<[T; N]> {
        self.expect_arity(N, arity, program, loc)?;
        self.pop_array()
    }

    fn expect_peek(&mut self, arity_t: ArityType, program: &Program, loc: &Loc) -> Result<&T> {
        self.expect_arity(1, arity_t, program, loc)?;
        Ok(self.peek().unwrap())
    }

    fn expect_pop(&mut self, loc: &Loc) -> Result<T> {
        self.expect_stack_size(1, loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_pop_n<const N: usize>(&mut self, loc: &Loc) -> Result<[T; N]> {
        self.expect_stack_size(N, loc)?;
        self.pop_array()
    }

    fn expect_pop_type(&mut self, arity_t: TokenType, program: &Program, loc: &Loc) -> Result<T> {
        self.expect_arity(1, ArityType::Type(arity_t), program, loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_arity(&self, n: usize, arity: ArityType, program: &Program, loc: &Loc) -> Result<()> {
        self.expect_stack_size(n, loc)?;

        let (typ, start) = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => (self.get_type(self.get(0).unwrap()), 1),
            ArityType::Type(typ) => (typ, 0),
        };

        for i in start..n {
            self.program_type(program, self.get(i).unwrap(), typ, loc)?;
        }

        Ok(())
    }

    fn expect_stack_size(&self, n: usize, loc: &Loc) -> Result<()> {
        let len = self.len();
        ensure!(
            len >= n,
            concat!(
                "Stack has less elements than expected\n",
                "[INFO] {}Expected `{}` elements, but found `{}`"
            ),
            loc,
            n,
            len
        );
        Ok(())
    }
}

impl Expect<TypeFrame> for DataStack {
    fn get_type(&self, t: &TypeFrame) -> TokenType {
        t.typ
    }

    fn program_arity(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        program.expect_arity(self, contract, loc)
    }

    fn program_exact(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        program.expect_exact(self, contract, loc)
    }

    fn program_type(
        &self, program: &Program, frame: &TypeFrame, expected: TokenType, loc: &Loc,
    ) -> Result<()> {
        program.expect_type(expected, frame, loc)
    }
}

impl Program {
    pub fn expect_exact(
        &self, stack: &[TypeFrame], contract: &[TokenType], loc: &Loc,
    ) -> Result<()> {
        ensure!(
            stack.len() == contract.len() && self.expect_arity(stack, contract, loc).is_ok(),
            self.format_stack_diff(stack, contract, true, loc)
        );
        Ok(())
    }

    pub fn expect_arity(
        &self, stack: &[TypeFrame], contract: &[TokenType], loc: &Loc,
    ) -> Result<()> {
        for (stk, contr) in stack.iter().rev().zip(contract.iter().rev()) {
            self.expect_type(*contr, stk, loc)?;
        }
        Ok(())
    }

    pub fn expect_type(&self, expected: TokenType, frame: &TypeFrame, loc: &Loc) -> Result<()> {
        ensure!(
            equals_any!(expected, ValueType::Any, TokenType::DataPtr(ValueType::Any), frame.typ),
            self.format_type_diff(expected, frame, loc)
        );
        Ok(())
    }

    fn format_type_diff(&self, expected: TokenType, frame: &TypeFrame, loc: &Loc) -> String {
        format!(
            "{}Expected type `{}`, but found `{}`\n{}",
            loc,
            self.type_name(expected),
            self.type_name(frame.typ),
            self.format_frame(frame)
        )
    }

    fn format_stack_diff(
        &self, stack: &[TypeFrame], contract: &[TokenType], verbose: bool, loc: &Loc,
    ) -> String {
        let fmt = format!(
            concat!(
                "Found stack at the end of the context does not match the expected types:\n",
                "[INFO] {}Expected types: {}\n",
                "[INFO] {}Actual types:   {}"
            ),
            loc,
            self.format_stack(contract, |tok| *tok),
            loc,
            self.format_stack(stack, |frame| frame.typ)
        );

        if verbose & !stack.is_empty() {
            format!("{fmt}\n{}", self.format_frames(stack))
        } else {
            fmt
        }
    }

    fn format_stack<T>(&self, stack: &[T], f: impl Fn(&T) -> TokenType) -> String {
        format!(
            "[{}] ->",
            stack
                .iter()
                .map(|t| format!("<{}>", self.type_name(f(t))))
                .join(", ")
        )
    }

    pub fn format_frames(&self, stack: &[TypeFrame]) -> String {
        stack.iter().map(|t| self.format_frame(t)).join("\n")
    }

    fn format_frame(&self, t: &TypeFrame) -> String {
        format!("[INFO] {}Type `{}` was declared here", t.loc, self.type_name(t.typ))
    }

    fn set_operand(&mut self, ip: usize, index: usize) {
        let op = self.ops.get_mut(ip).unwrap();
        op.operand = index as i32;
    }

    pub fn type_check(&mut self) -> Result<&mut Self> {
        info!("Typechecking program");
        TypeChecker::new().type_check(self)?;
        info!("Typechecking done");

        Ok(self)
    }
}
