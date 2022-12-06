use ashfire_types::{
    core::*,
    data::*,
    enums::{IntrinsicType, OpType},
    proc::{Binding, Mode},
};
use ashlib::{EvalStack, UncheckedStack};
use firelib::{lazy::LazyCtx, lexer::Loc, Result};

use super::{expect::*, types::TypeFrame};
use crate::compiler::{parsing::types::StructUtils, program::*, utils::err_loc};

type DataStack = EvalStack<TypeFrame>;

impl Expect<TypeFrame> for DataStack {}

#[derive(Clone)]
struct TypeBlock(DataStack, usize);

#[derive(Default)]
pub struct TypeChecker {
    block_stack: Vec<TypeBlock>,
    bind_stack: Vec<(TypeFrame, usize)>,
    data_stack: DataStack,
    current_proc: Option<usize>,
}

impl Visitor for TypeChecker {
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
        let &Op(op_type, operand, loc) = &program.ops[ip];
        match op_type {
            OpType::PushData(value) => match value {
                Value::INT | Value::BOOL | Value::PTR => self.push_value(value, loc),
                Value(_) => unreachable!(),
            },

            OpType::PushStr => self.extend_value([Value::INT, Value::PTR], loc),

            OpType::PushLocalMem |
            OpType::PushGlobalMem |
            OpType::PushLocal |
            OpType::PushGlobal => self.push_value(Value::PTR, loc),

            OpType::Offset => match self.expect_struct_pointer(program, ip)? {
                TokenType::Data(ValueType::Typ(offset_type)) => {
                    self.push_frame(ValueType::Ptr(offset_type).get_type(), loc);
                }
                _ => todo!(),
            },

            OpType::Intrinsic => match IntrinsicType::from(operand) {
                IntrinsicType::Plus |
                IntrinsicType::Minus |
                IntrinsicType::And |
                IntrinsicType::Or |
                IntrinsicType::Xor => {
                    self.pop_push([INT, INT], INT, loc)?;
                }

                IntrinsicType::Times => todo!(),
                IntrinsicType::Div => todo!(),

                IntrinsicType::Greater |
                IntrinsicType::GreaterE |
                IntrinsicType::Lesser |
                IntrinsicType::LesserE => todo!(),

                IntrinsicType::Load8 | IntrinsicType::Load16 | IntrinsicType::Load32 => {
                    self.data_stack.expect_pop_type(PTR, loc)?;
                    self.push_frame(ANY, loc);
                }

                IntrinsicType::Store8 | IntrinsicType::Store16 | IntrinsicType::Store32 => {
                    self.data_stack.expect_array_pop([ANY, ANY], loc)?;
                }

                IntrinsicType::Cast(n) => {
                    self.data_stack.expect_pop(loc)?;

                    let cast = ValueType::from(n).get_type();
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
                let contr = &program.get_proc(operand).contract;
                self.data_stack.expect_contract_pop(contr.ins(), loc)?;
                for &typ in contr.outs() {
                    self.push_frame(typ, loc);
                }
            }

            OpType::Equal => {
                self.data_stack.pop_push_arity(
                    |[_, _]| TypeFrame(BOOL, loc),
                    ArityType::Same,
                    loc,
                )?;
            }

            OpType::PrepProc | OpType::PrepInline => {
                let proc = &self.visit_proc(program, operand.index());

                if matches!(proc.mode, Mode::Imported) {
                    return Ok(());
                }

                for &typ in proc.contract.ins() {
                    self.push_frame(typ, loc);
                }
            }

            OpType::IfStart => {
                self.data_stack.expect_pop_type(BOOL, loc)?;
                self.push_stack(ip);
                self.data_stack.reset_max_count();
            }

            OpType::Else => {
                let TypeBlock(old_stack, start_op) = self.block_stack.last().cloned().unwrap();
                self.push_stack(start_op);
                self.data_stack = DataStack::new(old_stack);
            }

            OpType::EndIf => {
                let TypeBlock(expected, start_op) = self.block_stack.pop().unwrap();

                self.expect_stack_arity(
                    &expected,
                    loc,
                    format!(
                        "Else-less if block is not allowed to alter {}",
                        "the types of the arguments on the data stack."
                    ),
                )?;

                let ins = self.data_stack.min().abs();
                let out = self.data_stack.count() + ins;

                program
                    .block_contracts
                    .insert(start_op, (ins as usize, out as usize));

                let old_count = expected.count();
                self.data_stack
                    .set_count(self.data_stack.min() + old_count, old_count);
            }

            OpType::EndElse => {
                let TypeBlock(expected, start_op) = self.block_stack.pop().unwrap();

                self.expect_stack_arity(
                    &expected,
                    loc,
                    format!(
                        "Both branches of the if-block must produce {}",
                        "the same types of the arguments on the data stack"
                    ),
                )?;

                let ins = (self.data_stack.min()).min(expected.min()).abs();
                let out = (self.data_stack.count()).max(expected.count()) + ins;

                program
                    .block_contracts
                    .insert(start_op, (ins as usize, out as usize));

                let TypeBlock(old_stack, _) = self.block_stack.pop().unwrap();

                let old_count = old_stack.count();
                self.data_stack.set_count(old_count - ins, old_count);
            }

            OpType::EndProc | OpType::EndInline => {
                let proc = self.current_proc_mut(program).unwrap();

                if matches!(proc.mode, Mode::Imported) {
                    return Ok(());
                }

                let outs = proc.contract.outs();

                if outs.is_empty() {
                    self.data_stack.expect_exact::<TokenType>(&[], loc)?;
                } else {
                    self.data_stack.expect_exact_pop(outs, loc)?;
                }

                self.data_stack = EvalStack::default();

                if let Mode::Inlined(start, _) = proc.mode {
                    proc.mode = Mode::Inlined(start, ip);
                }

                self.exit_proc();
            }

            OpType::BindStack => {
                let proc = self.current_proc(program).unwrap();
                let Binding(bind) = &proc.bindings[operand.index()];
                let mut binds = vec![];

                for (_, typ) in bind.iter() {
                    if let Some(id) = typ {
                        let value = ValueType::from(*id);

                        let type_def = program.get_value_def(value);
                        let contract: Vec<_> = type_def.units().map(Typed::get_type).collect();

                        self.data_stack.expect_contract_pop(&contract, loc)?;

                        binds.push((TypeFrame(value.get_type(), loc), type_def.size()));
                    } else {
                        let top = self.data_stack.expect_pop(loc)?;
                        binds.push((top, WORD_USIZE));
                    }
                }

                self.bind_stack.extend(binds.iter().rev());
            }

            OpType::LoadBind => {
                let (typ, offset) = self.get_bind_type_offset(operand.index());

                self.push_frame(typ, loc);
                program.set_operand(ip, offset);
            }

            OpType::PushBind => {
                let (typ, offset) = self.get_bind_type_offset(operand.index());

                if let TokenType::Data(ValueType::Typ(val)) = typ {
                    self.push_frame(ValueType::Ptr(val).get_type(), loc);
                } else {
                    todo!()
                }

                program.set_operand(ip, offset);
            }

            OpType::PopBind => {
                let proc = self.current_proc(program).unwrap();
                let Binding(binds) = &proc.bindings[operand.index()];
                self.bind_stack
                    .truncate(self.bind_stack.len() - binds.len());
            }

            OpType::While => {
                self.push_stack(ip);
                self.data_stack.reset_max_count();
            }

            OpType::Do => {
                self.data_stack.expect_pop_type(BOOL, loc)?;
                let TypeBlock(expected, _) = self.block_stack.last().cloned().unwrap();

                self.expect_stack_arity(
                    &expected,
                    loc,
                    format!(
                        "While block is not allowed to alter {}",
                        "the types of the arguments on the data stack"
                    ),
                )?;

                self.push_stack(ip);
                self.data_stack = DataStack::new(expected);
            }

            OpType::EndWhile => {
                let TypeBlock(expected, do_op) = self.block_stack.pop().unwrap();

                self.expect_stack_arity(
                    &expected,
                    loc,
                    format!(
                        "Do block is not allowed to alter {}",
                        "the types of the arguments on the data stack"
                    ),
                )?;

                let ins = (self.data_stack.min()).min(expected.min()).abs();
                let out = (self.data_stack.count()).max(expected.count()) + ins;

                let TypeBlock(old_stack, start_op) = self.block_stack.pop().unwrap();

                let ip_dif = ip - start_op;
                program.set_operand(start_op, ip_dif);
                program.set_operand(ip, ip_dif);

                let contr = (ins as usize, out as usize);
                program
                    .block_contracts
                    .extend([(do_op, contr), (start_op, contr)]);

                let old_count = old_stack.count();
                self.data_stack.set_count(old_count - ins, old_count);
            }

            OpType::Unpack => match self.data_stack.expect_pop(loc)?.get_type() {
                TokenType::Data(ValueType::Ptr(Value(index))) => {
                    let stk = &program.structs_types[index];

                    for typ in stk.units().map(Typed::get_type) {
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
                let typ = ValueType::from(operand).get_type();
                self.data_stack.expect_peek(ArityType::Type(typ), loc)?;
            }

            OpType::CaseStart => todo!(),
            OpType::CaseMatch => todo!(),
            OpType::CaseOption => todo!(),
            OpType::EndCase => todo!(),
        };
        Ok(())
    }

    fn push_stack(&mut self, ip: usize) {
        self.block_stack
            .push(TypeBlock(self.data_stack.clone(), ip));
    }

    fn pop_push<const N: usize>(
        &mut self, contr: [TokenType; N], token: TokenType, loc: Loc,
    ) -> LazyResult<()> {
        self.data_stack.pop_push(contr, TypeFrame(token, loc), loc)
    }

    fn extend_value<const N: usize>(&mut self, value: [Value; N], loc: Loc) {
        self.data_stack
            .extend(value.map(|v| TypeFrame(v.get_type(), loc)));
    }

    fn push_value(&mut self, value: Value, loc: Loc) {
        self.data_stack.push(TypeFrame(value.get_type(), loc));
    }

    fn push_frame(&mut self, typ: TokenType, loc: Loc) {
        self.data_stack.push(TypeFrame(typ, loc));
    }

    fn get_bind_type_offset(&mut self, operand: usize) -> (TokenType, usize) {
        let mut offset = 0;
        for ((_, size), _) in self.bind_stack.iter().rev().zip(0..=operand) {
            offset += size;
        }

        let (bind, _) = self.bind_stack[self.bind_stack.len() - 1 - operand];

        (bind.get_type(), offset)
    }

    fn expect_struct_pointer(&mut self, prog: &mut Program, ip: usize) -> LazyResult<TokenType> {
        let &Op(_, operand, loc) = &prog.ops[ip];
        match self.data_stack.expect_pop(loc)?.get_type() {
            TokenType::Data(ValueType::Ptr(Value(index))) => {
                let stk = &prog.structs_types[index];
                let word = &operand.str_key();

                let Some((offset, index)) = stk.members().get_offset(word) else {
                    let error = format!("The struct {} does not contain a member with name: `{}`",
                        stk.as_str(prog), word.as_str(prog));
                    return Err(err_loc(error, loc));
                };

                let result = stk.members()[index].get_type();
                prog.set_operand(ip, offset * WORD_USIZE);
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
