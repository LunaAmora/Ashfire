use ashfire_types::{
    core::*,
    data::*,
    enums::{ControlOp, IndexOp, IntrinsicType, OpType, StackOp},
    lasso::Key,
    proc::{Binds, Mode},
};
use ashlib::{EvalStack, UncheckedStack};
use firelib::{lazy::LazyErrCtx, lexer::Loc, Result};

use super::{expect::*, types::TypeFrame};
use crate::compiler::{parsing::types::StructUtils, program::*, utils::err_loc};

type DataStack = EvalStack<TypeFrame>;

impl Expect<'_, TypeFrame> for DataStack {}

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
        let &Op(op_type, loc) = &program.ops[ip];
        match op_type {
            OpType::PushData(value, _) => match value {
                TypeId::INT | TypeId::BOOL | TypeId::PTR => self.push_value(value, loc),
                TypeId(_) => unreachable!(),
            },

            OpType::IndexOp(op, index) => match op {
                IndexOp::PushStr => self.extend_value([TypeId::INT, TypeId::PTR], loc),

                IndexOp::PushLocalMem |
                IndexOp::PushGlobalMem |
                IndexOp::PushLocal |
                IndexOp::PushGlobal => {
                    self.push_value(TypeId::PTR, loc);
                }

                IndexOp::Offset => match self.expect_struct_pointer(program, ip, index, loc)? {
                    TokenType::Data(ValueType(offset_id)) => {
                        self.push_frame(program.get_type_ptr(offset_id).get_type(), loc);
                    }
                    _ => todo!(),
                },

                IndexOp::Unpack => match self.data_stack.expect_pop(loc)?.get_type() {
                    TokenType::Data(ValueType(id @ TypeId(index))) => {
                        match program.get_type_descr(id) {
                            TypeDescr::Reference(ptr) => match program.get_type_descr(ptr.ptr_id())
                            {
                                TypeDescr::Structure(StructType(fields, TypeId(ptr_id))) => {
                                    for typ in fields.units().map(|u| u.get_type()) {
                                        self.push_frame(typ, loc);
                                    }

                                    program.set_index(ip, *ptr_id);
                                }

                                TypeDescr::Primitive(_) => {
                                    self.push_frame(ptr.ptr_id().get_type(), loc);
                                    program.set_index(ip, index);
                                }

                                TypeDescr::Reference(_) => todo!(),
                            },
                            stk => {
                                for typ in stk.units().map(|u| u.get_type()) {
                                    self.push_frame(typ, loc);
                                }
                                program.set_index(ip, index);
                            }
                        }
                    }

                    top => {
                        lazybail!(
                            |f| "{}Cannot unpack element of type: `{}`",
                            f.format(Fmt::Loc(loc)),
                            f.format(Fmt::Typ(top))
                        );
                    }
                },

                IndexOp::Call | IndexOp::CallInline => {
                    let contr = &program.get_proc(index).contract;
                    self.data_stack.expect_contract_pop(contr.ins(), loc)?;
                    for &typ in contr.outs() {
                        self.push_frame(typ, loc);
                    }
                }

                IndexOp::LoadBind => {
                    let (typ, offset) = self.get_bind_type_offset(index);

                    self.push_frame(typ, loc);
                    program.set_index(ip, offset);
                }

                IndexOp::PushBind => {
                    let (typ, offset) = self.get_bind_type_offset(index);

                    if let TokenType::Data(ValueType(id)) = typ {
                        self.push_frame(program.get_type_ptr(id).get_type(), loc);
                    } else {
                        todo!()
                    }

                    program.set_index(ip, offset);
                }
            },

            OpType::StackOp(op) => match op {
                StackOp::Drop => {
                    self.data_stack.expect_pop(loc)?;
                }

                StackOp::Dup => self.data_stack.pop_extend(|[a]| [a, a], loc)?,
                StackOp::Swap => self.data_stack.pop_extend(|[a, b]| [b, a], loc)?,
                StackOp::Over => self.data_stack.pop_extend(|[a, b]| [a, b, a], loc)?,
                StackOp::Rot => self.data_stack.pop_extend(|[a, b, c]| [b, c, a], loc)?,

                StackOp::Equal => {
                    self.data_stack.pop_push_arity(
                        |[_, _]| TypeFrame(BOOL, loc),
                        ArityType::Same,
                        loc,
                    )?;
                }
            },

            OpType::Intrinsic(intrinsic) => match intrinsic {
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

                IntrinsicType::Cast(type_id) => {
                    self.data_stack.expect_pop(loc)?;
                    self.push_frame(type_id.get_type(), loc);
                }
            },

            OpType::ControlOp(op, index) => match op {
                ControlOp::PrepProc | ControlOp::PrepInline => {
                    let proc = &self.visit_proc(program, index);

                    if matches!(proc.mode, Mode::Imported) {
                        return Ok(());
                    }

                    for &typ in proc.contract.ins() {
                        self.push_frame(typ, loc);
                    }
                }

                ControlOp::EndProc | ControlOp::EndInline => {
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

                ControlOp::IfStart => {
                    self.data_stack.expect_pop_type(BOOL, loc)?;
                    self.push_stack(ip);
                    self.data_stack.reset_max_count();
                }

                ControlOp::Else => {
                    let TypeBlock(old_stack, start_op) = self.block_stack.last().cloned().unwrap();
                    self.push_stack(start_op);
                    self.data_stack = DataStack::new(old_stack);
                }

                ControlOp::EndIf => {
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

                ControlOp::EndElse => {
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

                ControlOp::While => {
                    self.push_stack(ip);
                    self.data_stack.reset_max_count();
                }

                ControlOp::Do => {
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

                ControlOp::EndWhile => {
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
                    program.set_index(start_op, ip_dif);
                    program.set_index(ip, ip_dif);

                    let contr = (ins as usize, out as usize);
                    program
                        .block_contracts
                        .extend([(do_op, contr), (start_op, contr)]);

                    let old_count = old_stack.count();
                    self.data_stack.set_count(old_count - ins, old_count);
                }

                ControlOp::BindStack => {
                    let proc = self.current_proc(program).unwrap();
                    let Binds(bindings) = &proc.binds[index];
                    let mut binds = vec![];

                    for (_, typ) in bindings.iter() {
                        if let &Some(id) = typ {
                            let type_def = program.get_type_descr(TypeId(id));

                            let contract: Vec<_> = match type_def {
                                TypeDescr::Structure(StructType(fields, _)) => {
                                    fields.units().map(|u| u.get_type()).collect()
                                }
                                TypeDescr::Primitive(prim) => vec![prim.get_type()],
                                TypeDescr::Reference(ptr) => vec![ptr.get_type()],
                            };

                            self.data_stack.expect_contract_pop(&contract, loc)?;

                            binds.push((TypeFrame(TypeId(id).get_type(), loc), type_def.size()));
                        } else {
                            let top = self.data_stack.expect_pop(loc)?;
                            binds.push((top, WORD_USIZE));
                        }
                    }

                    self.bind_stack.extend(binds.iter().rev());
                }

                ControlOp::PopBind => {
                    let proc = self.current_proc(program).unwrap();
                    let Binds(bindings) = &proc.binds[index];
                    self.bind_stack
                        .truncate(self.bind_stack.len() - bindings.len());
                }

                ControlOp::CaseStart => todo!(),
                ControlOp::CaseMatch => todo!(),
                ControlOp::CaseOption => todo!(),
                ControlOp::EndCase => todo!(),
            },

            OpType::ExpectType(type_id) => {
                let typ = type_id.get_type();
                self.data_stack.expect_peek(ArityType::Type(typ), loc)?;
            }
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

    fn extend_value<const N: usize>(&mut self, value: [TypeId; N], loc: Loc) {
        self.data_stack
            .extend(value.map(|v| TypeFrame(v.get_type(), loc)));
    }

    fn push_value(&mut self, value: TypeId, loc: Loc) {
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

    fn expect_struct_pointer(
        &mut self, prog: &mut Program, ip: usize, operand: usize, loc: Loc,
    ) -> LazyResult<TokenType> {
        let typ = self.data_stack.expect_pop(loc)?.get_type();

        if let TokenType::Data(ValueType(id)) = typ {
            if let TypeDescr::Reference(ptr) = &prog.get_type_descr(id) {
                match prog.get_type_descr(ptr.ptr_id()) {
                    TypeDescr::Structure(StructType(fields, _)) => {
                        let word = Name::try_from_usize(operand).unwrap();

                        let Some((offset, index)) = fields.get_offset(word) else {
                            let error = format!("The struct {} does not contain a member with name: `{}`",
                                fields.name().as_str(prog), word.as_str(prog));
                            return Err(err_loc(error, loc));
                        };

                        let result = fields[index].type_id().get_type();
                        prog.set_index(ip, offset * WORD_USIZE);

                        return Ok(result);
                    }

                    TypeDescr::Primitive(_) => todo!(),
                    TypeDescr::Reference(_) => todo!(),
                }
            }
        }

        lazybail!(
            |f| "{}Cannot `.` access elements of type: `{}`",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(typ))
        )
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
    fn set_index(&mut self, ip: usize, value: usize) {
        let op = &mut self.ops[ip];
        op.set_index(value);
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