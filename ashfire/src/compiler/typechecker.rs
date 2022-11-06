use ashlib::{EvalStack, Stack};
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
        for ip in 0..program.ops.len() {
            self.type_check_op(ip, program)?;
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

            OpType::PushStr => {
                self.push_value(Value::Int, loc);
                self.push_value(Value::Ptr, loc);
            }

            OpType::PushLocalMem |
            OpType::PushGlobalMem |
            OpType::PushLocal |
            OpType::PushGlobal => self.push_value(Value::Ptr, loc),

            OpType::OffsetLoad => {
                let op = &op.clone();
                let offset_type = self.expect_struct_pointer(program, ip, op, ".")?;
                self.push_frame(offset_type, loc);
            }

            OpType::Offset => {
                let op = &op.clone();
                match self.expect_struct_pointer(program, ip, op, ".*")? {
                    TokenType::DataType(offset_type) => {
                        self.push_frame(TokenType::DataPtr(offset_type), loc)
                    }
                    _ => unreachable!(),
                }
            }

            OpType::Intrinsic => match IntrinsicType::from(op.operand) {
                IntrinsicType::Plus | IntrinsicType::Minus => {
                    let [top, _] = self.data_stack.expect_arity_pop(ArityType::Same, loc)?;
                    self.push_frame(top.get_type(), loc);
                }

                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),

                IntrinsicType::Greater |
                IntrinsicType::GreaterE |
                IntrinsicType::Lesser |
                IntrinsicType::LesserE => todo!(),

                IntrinsicType::And | IntrinsicType::Or | IntrinsicType::Xor => {
                    self.data_stack.expect_array_pop([INT, INT], loc)?;
                    self.push_frame(INT, loc);
                }

                IntrinsicType::Load8 | IntrinsicType::Load16 | IntrinsicType::Load32 => {
                    self.data_stack.expect_pop_type(PTR, loc)?;
                    self.push_frame(ANY, loc)
                }

                IntrinsicType::Store8 | IntrinsicType::Store16 | IntrinsicType::Store32 => {
                    self.data_stack.expect_array_pop([ANY, ANY], loc)?;
                }

                IntrinsicType::FdWrite => {
                    self.data_stack
                        .expect_array_pop([INT, PTR, INT, PTR], loc)?;
                    self.push_frame(PTR, loc);
                }

                IntrinsicType::Cast(n) => {
                    self.data_stack.expect_pop(loc)?;

                    let cast: TokenType = match n {
                        1.. => Value::from((n - 1) as usize).get_type(),
                        0 => unreachable!(),
                        _ => TokenType::DataPtr(Value::from((-n - 1) as usize)),
                    };
                    self.push_frame(cast, loc);
                }
            },

            OpType::Drop => {
                self.data_stack.expect_pop(loc)?;
            }

            OpType::Dup => {
                let typ = self.data_stack.expect_peek(ArityType::Any, loc)?.get_type();
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
                let contr = &program.procs[op.operand as usize].contract;
                self.data_stack.expect_contract_pop(contr.ins(), loc)?;
                for &typ in contr.outs() {
                    self.push_frame(typ, loc);
                }
            }

            OpType::Equal => {
                self.data_stack
                    .expect_arity_pop::<2>(ArityType::Same, loc)?;
                self.push_frame(BOOL, loc);
            }

            OpType::PrepProc => {
                for &typ in self.visit_proc(program, op.operand as usize).contract.ins() {
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

            OpType::EndProc => {
                let outs = self.current_proc(program).unwrap().contract.outs();

                if outs.is_empty() {
                    self.data_stack.expect_exact::<TokenType>(&[], loc)?;
                } else {
                    let mut outs = outs.to_vec();
                    outs.reverse();
                    self.data_stack.expect_exact_pop(&outs, loc)?;
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

            OpType::Unpack => match self.data_stack.expect_pop(op.loc)?.get_type() {
                TokenType::DataPtr(n) => {
                    let index = usize::from(n);
                    let stk = &program.structs_types[index];

                    for typ in stk.members().iter().map(Typed::get_type) {
                        self.push_frame(typ, loc);
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
                let typ = match op.operand {
                    n @ 1.. => Value::from((n - 1) as usize).get_type(),
                    0 => unreachable!(),
                    n => TokenType::DataPtr(Value::from((-n - 1) as usize)),
                };
                self.data_stack.expect_peek(ArityType::Type(typ), loc)?;
            }

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        };
        Ok(())
    }

    fn push_value(&mut self, value: Value, loc: Loc) {
        self.data_stack.push((value, loc).into())
    }

    fn push_frame(&mut self, typ: TokenType, loc: Loc) {
        self.data_stack.push((typ, loc).into())
    }

    fn expect_struct_pointer(
        &mut self, prog: &mut Program, ip: usize, op: &Op, prefix: &str,
    ) -> LazyResult<TokenType> {
        match self.data_stack.expect_pop(op.loc)?.get_type() {
            TokenType::DataPtr(value) => {
                let word = prog
                    .get_word(op.operand)
                    .strip_prefix(prefix)
                    .unwrap()
                    .to_string();

                let stk = &prog.structs_types[usize::from(value)];

                let index = get_struct_member_index(stk, word, op.loc)?;
                let result = stk.members()[index].get_type();

                prog.set_operand(ip, index * 4);
                Ok(result)
            }
            typ => {
                lazybail!(|f| "Cannot `.` access elements of type: `{}`", f.format(Fmt::Typ(typ)))
            }
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

fn get_struct_member_index(stk: &StructDef, word: String, loc: Loc) -> LazyResult<usize> {
    let name = stk.name().to_owned();
    stk.members()
        .iter()
        .position(|mem| mem.name() == word)
        .with_ctx(move |f| {
            format!(
                "{}The struct {name} does not contain a member with name: `{word}`",
                f.format(Fmt::Loc(loc)),
            )
        })
}

impl Program {
    fn set_operand(&mut self, ip: usize, index: usize) {
        let op = &mut self.ops[ip];
        op.operand = index as i32;
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
