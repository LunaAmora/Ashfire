use ashlib::{EvalStack, UncheckedStack};
use firelib::{anyhow::Result, lazy::LazyCtx, lexer::Loc};

use super::{expect::*, program::*, types::*};

type DataStack = EvalStack<TypeFrame>;

impl Expect<TypeFrame> for DataStack {}

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

    pub fn type_check(&mut self, program: &mut Program) -> LazyResult<()> {
        let mut ip = 0;
        while ip < program.ops.len() {
            self.type_check_op(ip, program)?;
            ip += 1;
        }
        Ok(())
    }

    fn type_check_op(&mut self, ip: usize, program: &mut Program) -> LazyResult<()> {
        let op = &program.ops[ip];
        let loc = op.loc;
        match op.op_type {
            OpType::PushData(value) => match value {
                Value::Int | Value::Bool | Value::Ptr => self.push_value(value, loc),
                Value::Any | Value::Type(_) => unreachable!(),
            },

            OpType::PushStr => self.extend_value([Value::Int, Value::Ptr], loc),

            OpType::PushLocalMem |
            OpType::PushGlobalMem |
            OpType::PushLocal |
            OpType::PushGlobal => self.push_value(Value::Ptr, loc),

            OpType::OffsetLoad => match self.expect_struct_pointer(program, ip)? {
                TokenType::Data(Data::Typ(typ)) => {
                    self.push_frame(Data::Ptr(typ).get_type(), loc);
                    program.ops.insert(ip + 1, Op::new(OpType::Unpack, 0, loc))
                }
                _ => todo!(),
            },

            OpType::Offset => match self.expect_struct_pointer(program, ip)? {
                TokenType::Data(Data::Typ(offset_type)) => {
                    self.push_frame(Data::Ptr(offset_type).get_type(), loc)
                }
                _ => todo!(),
            },

            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus | IntrinsicType::Minus => self.data_stack.pop_push_arity(
                    |[top, _]| (top.get_type(), loc).into(),
                    ArityType::Same,
                    loc,
                )?,

                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),

                IntrinsicType::Greater |
                IntrinsicType::GreaterE |
                IntrinsicType::Lesser |
                IntrinsicType::LesserE => todo!(),

                IntrinsicType::And | IntrinsicType::Or | IntrinsicType::Xor => {
                    self.pop_push([INT, INT], INT, loc)?;
                }

                IntrinsicType::Load8 | IntrinsicType::Load16 | IntrinsicType::Load32 => {
                    self.data_stack.expect_pop_type(PTR, loc)?;
                    self.push_frame(ANY, loc)
                }

                IntrinsicType::Store8 | IntrinsicType::Store16 | IntrinsicType::Store32 => {
                    self.data_stack.expect_array_pop([ANY, ANY], loc)?;
                }

                IntrinsicType::FdWrite => {
                    self.pop_push([INT, PTR, INT, PTR], PTR, loc)?;
                }

                IntrinsicType::Cast(n) => {
                    self.data_stack.expect_pop(loc)?;

                    let cast = Data::from(n).get_type();
                    self.push_frame(cast, loc);
                }
            },

            OpType::Drop => {
                self.data_stack.expect_pop(loc)?;
            }

            OpType::Dup => self.data_stack.pop_extend(|[a]| [a, a], loc)?,
            OpType::Swap => self.data_stack.pop_extend(|[a, b]| [b, a], loc)?,
            OpType::Over => self.data_stack.pop_extend(|[a, b]| [a, b, a], loc)?,
            OpType::Rot => self.data_stack.pop_extend(|[a, b, c]| [b, c, a], loc)?,

            OpType::Call | OpType::CallInline => {
                let contr = &program.get_proc(op).contract;
                self.data_stack.expect_contract_pop(contr.ins(), loc)?;
                for &typ in contr.outs() {
                    self.push_frame(typ, loc);
                }
            }

            OpType::Equal => {
                self.data_stack
                    .pop_push_arity(|[_, _]| (BOOL, loc).into(), ArityType::Same, loc)?
            }

            OpType::PrepProc | OpType::PrepInline => {
                for &typ in self.visit_proc(program, op.index()).contract.ins() {
                    self.push_frame(typ, loc);
                }
            }

            OpType::IfStart => {
                self.data_stack.expect_pop_type(BOOL, loc)?;
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
                    &expected,
                    loc,
                    format!(
                        "Else-less if block is not allowed to alter {}",
                        "the types of the arguments on the data stack."
                    ),
                )?;

                let ins = self.data_stack.min_count.abs();
                let out = self.data_stack.stack_count + ins;

                program
                    .block_contracts
                    .insert(start_op, (ins as usize, out as usize));

                self.data_stack.min_count += expected.stack_count;
                self.data_stack.stack_count = expected.stack_count;
            }

            OpType::EndElse => {
                let (expected, start_op) = self.block_stack.pop().unwrap().into();

                self.expect_stack_arity(
                    &expected,
                    loc,
                    format!(
                        "Both branches of the if-block must produce {}",
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

            OpType::EndProc | OpType::EndInline => {
                let proc = &mut self.current_proc_mut(program).unwrap();
                let outs = proc.contract.outs();

                if outs.is_empty() {
                    self.data_stack.expect_exact::<TokenType>(&[], loc)?;
                } else {
                    let mut outs = outs.to_vec();
                    outs.reverse();
                    self.data_stack.expect_exact_pop(&outs, loc)?;
                }

                self.data_stack = Default::default();

                match proc.data {
                    ProcType::Inline(start, _) => proc.data = ProcType::Inline(start, ip),
                    _ => {}
                }
                self.exit_proc();
            }

            OpType::BindStack => todo!(),
            OpType::PushBind => todo!(),
            OpType::PopBind => todo!(),
            OpType::While => todo!(),
            OpType::Do => todo!(),
            OpType::EndWhile => todo!(),

            OpType::Unpack => match self.data_stack.expect_pop(op.loc)?.get_type() {
                TokenType::Data(Data::Ptr(n)) => {
                    let index = usize::from(n);
                    let stk = &program.structs_types[index];

                    for typ in stk.members().iter().flat_map(StructType::units) {
                        self.push_frame(typ.get_type(), loc);
                    }

                    program.set_operand(ip, index);
                }
                top => {
                    lazybail!(
                        |f| "{}Cannot unpack element of type: `{}`",
                        f.format(Fmt::Loc(loc)),
                        f.format(Fmt::Typ(top))
                    );
                }
            },

            OpType::ExpectType => {
                let typ = Data::from(op.operand).get_type();
                self.data_stack.expect_peek(ArityType::Type(typ), loc)?;
            }

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        };
        Ok(())
    }

    fn pop_push<const N: usize>(
        &mut self, contr: [TokenType; N], token: TokenType, loc: Loc,
    ) -> LazyResult<()> {
        self.data_stack.pop_push(contr, (token, loc).into(), loc)
    }

    fn extend_value<const N: usize>(&mut self, value: [Value; N], loc: Loc) {
        self.data_stack.extend(value.map(|v| (v, loc).into()))
    }

    fn push_value(&mut self, value: Value, loc: Loc) {
        self.data_stack.push((value, loc).into())
    }

    fn push_frame(&mut self, typ: TokenType, loc: Loc) {
        self.data_stack.push((typ, loc).into())
    }

    fn expect_struct_pointer(&mut self, prog: &mut Program, ip: usize) -> LazyResult<TokenType> {
        let op = &prog.ops[ip];
        let loc = op.loc;
        match self.data_stack.expect_pop(loc)?.get_type() {
            TokenType::Data(Data::Ptr(value)) => {
                let stk = &prog.structs_types[usize::from(value)];
                let word = prog.get_word(op).to_string();

                let Some((offset, index)) = get_field_pos(stk.members(), &word) else {
                        let name = stk.name().to_owned();
                        lazybail!(|f|
                            "{}The struct {name} does not contain a member with name: `{word}`",
                            f.format(Fmt::Loc(loc))
                        )
                };

                let result = match &stk.members()[index] {
                    StructType::Root(stk) => stk.get_type(),
                    StructType::Unit(typ) => typ.get_type(),
                };

                prog.set_operand(ip, offset * 4);
                Ok(result)
            }
            typ => lazybail!(
                |f| "{}Cannot `.` access elements of type: `{}`",
                f.format(Fmt::Loc(loc)),
                f.format(Fmt::Typ(typ))
            ),
        }
    }

    fn expect_stack_arity(
        &self, expected: &[TypeFrame], loc: Loc, error_text: String,
    ) -> LazyResult<()> {
        if let Err(err) = self.data_stack.expect_exact(expected, loc) {
            lazybail!(|f| "{}\n[ERROR] {}", error_text, err.apply(f))
        } else {
            Ok(())
        }
    }
}

impl Program {
    fn set_operand(&mut self, ip: usize, index: usize) {
        let op = &mut self.ops[ip];
        op.set_operand(index as i32);
    }

    pub fn type_check(&mut self) -> Result<&mut Self> {
        info!("Typechecking program");

        TypeChecker::new()
            .type_check(self)
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Typechecking done");

        Ok(self)
    }
}
