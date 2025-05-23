use ashfire_types::{
    core::*,
    data::*,
    enums::{ControlOp, DataOp, IndexOp, IntrinsicType, MemOp, OpType, StackOp},
    proc::{Binds, ModeData},
};
use ashlib::{EvalStack, UncheckedStack};
use firelib::{
    Result,
    lazy::{LazyCtx, LazyFormatter},
    lexer::Loc,
    span::Spanned,
};

use super::expect::*;
use crate::compiler::{ctx::*, parsing::types::StructUtils};

type TypeFrame = Spanned<DataType>;
type DataStack = EvalStack<TypeFrame>;

impl Expect<'_, TypeFrame> for DataStack {}

#[derive(Clone)]
struct TypeBlock(DataStack, usize);

#[derive(Default)]
pub struct TypeChecker {
    block_stack: Vec<TypeBlock>,
    bind_stack: Vec<(TypeFrame, u16)>,
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
        Self::default()
    }

    pub fn type_check(&mut self, ctx: &mut Ctx) -> LazyResult<()> {
        let mut ip = 0;
        while ip < ctx.ops().len() {
            self.type_check_op(ip, ctx)?;
            ip += 1;
        }
        Ok(())
    }

    fn block_stack_pop(&mut self) -> TypeBlock {
        self.block_stack
            .pop()
            .expect("Could not pop from an empty `block_stack`")
    }

    fn block_stack_last(&self) -> &TypeBlock {
        self.block_stack
            .last()
            .expect("Could not get the last block from an empty `block_stack`")
    }

    fn type_check_op(&mut self, ip: usize, ctx: &mut Ctx) -> LazyResult<()> {
        let &(op_type, loc) = &ctx.ops()[ip];
        match op_type {
            OpType::PushData(data @ DataType(id), _) => match id {
                TypeId::INT | TypeId::BOOL | TypeId::PTR => self.push_frame(data, loc),
                _ => unreachable!(),
            },

            OpType::DataOp(op, _) => match op {
                DataOp::PushStr => self.extend_value([INT, PTR], loc),
            },

            OpType::Offset(word) => {
                let (data_type, offset) = self.expect_type_ref(ctx, word, loc)?;
                ctx.update_op(ip, |op| *op = OpType::MemOp(MemOp::Offset, offset));

                self.push_frame(ctx.get_type_ptr(data_type), loc);
            }

            OpType::MemOp(op, _) => match op {
                MemOp::PushLocalMem |
                MemOp::PushGlobalMem |
                MemOp::PushLocal |
                MemOp::PushGlobal => {
                    self.push_frame(PTR, loc);
                }

                MemOp::Offset | MemOp::PushBind | MemOp::LoadBind => unreachable!(),
            },

            OpType::UnpackType(Some(_)) => todo!(),

            OpType::UnpackType(None) => {
                let data_type = self.data_stack.expect_pop(loc)?.get_type();
                let mut update_op = None;

                match &*ctx.get_type_descr(data_type) {
                    TypeDescr::Reference(ptr) => match &*ctx.get_type_descr(ptr.ptr_type()) {
                        TypeDescr::Structure(StructType(fields, ptr_id)) => {
                            for primitive in fields.units() {
                                self.push_frame(primitive.get_type(), loc);
                            }
                            update_op = Some(*ptr_id);
                        }

                        TypeDescr::Primitive(_) => {
                            self.push_frame(ptr.ptr_type(), loc);
                        }

                        TypeDescr::Reference(_) => todo!(),
                    },

                    _ => todo!(),
                }

                if let Some(ptr_id) = update_op {
                    ctx.update_op(ip, |op| *op = OpType::UnpackType(Some(ptr_id)));
                }
            }

            OpType::IndexOp(index_op, index) => match index_op {
                IndexOp::Call | IndexOp::CallInline => {
                    let contr = &ctx.get_proc(index).contract;
                    self.data_stack.expect_contract_pop(contr.ins(), loc)?;
                    for &typ in contr.outs() {
                        self.push_frame(typ, loc);
                    }
                }

                IndexOp::LoadBind => {
                    let (data_type, offset) = self.get_bind_type_offset(index);
                    self.push_frame(data_type, loc);

                    ctx.update_op(ip, |op| *op = OpType::MemOp(MemOp::LoadBind, offset));
                }

                IndexOp::PushBind => {
                    let (data_type, offset) = self.get_bind_type_offset(index);
                    self.push_frame(ctx.get_type_ptr(data_type), loc);

                    ctx.update_op(ip, |op| *op = OpType::MemOp(MemOp::PushBind, offset));
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
                    self.data_stack
                        .pop_push_arity(|[_, _]| (BOOL, loc), ArityType::Same, loc)?;
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

                IntrinsicType::Cast(data_type) => {
                    self.data_stack.expect_pop(loc)?;
                    self.push_frame(data_type, loc);
                }
            },

            OpType::ControlOp(op, index) => match op {
                ControlOp::PrepProc | ControlOp::PrepInline => {
                    let proc = self.visit_proc(ctx, index);

                    if matches!(proc.mode_data, ModeData::Imported) {
                        return Ok(());
                    }

                    for &typ in proc.contract.ins() {
                        self.push_frame(typ, loc);
                    }
                }

                ControlOp::EndProc | ControlOp::EndInline => {
                    let proc = self
                        .current_proc_mut(ctx)
                        .expect("Expected to be used inside a procedure");

                    if matches!(proc.mode_data, ModeData::Imported) {
                        return Ok(());
                    }

                    let outs = proc.contract.outs();

                    if outs.is_empty() {
                        self.data_stack.expect_exact::<DataType>(&[], loc)?;
                    } else {
                        self.data_stack.expect_exact_pop(outs, loc)?;
                    }

                    self.data_stack = EvalStack::default();

                    if let ModeData::Inlined(start, _) = proc.mode_data {
                        proc.mode_data = ModeData::Inlined(start, ip);
                    }

                    self.exit_proc();
                }

                ControlOp::IfStart => {
                    self.data_stack.expect_pop_type(BOOL, loc)?;
                    self.push_stack(ip);
                    self.data_stack.reset_max_count();
                }

                ControlOp::Else => {
                    let TypeBlock(old_stack, start_op) = self.block_stack_last().clone();
                    self.push_stack(start_op);
                    self.data_stack = DataStack::new(old_stack);
                }

                ControlOp::EndIf => {
                    let TypeBlock(expected, start_op) = self.block_stack_pop();

                    self.expect_stack_arity(
                        &expected,
                        loc,
                        concat!(
                            "Else-less if block is not allowed to alter ",
                            "the types of the arguments on the data stack."
                        ),
                    )?;

                    let ins = self.data_stack.min().abs();
                    let out = self.data_stack.count() + ins;

                    ctx.block_contracts()
                        .insert(start_op, (ins.unsigned_abs(), out.unsigned_abs()));

                    let old_count = expected.count();
                    self.data_stack
                        .set_count(self.data_stack.min() + old_count, old_count);
                }

                ControlOp::EndElse => {
                    let TypeBlock(expected, start_op) = self.block_stack_pop();

                    self.expect_stack_arity(
                        &expected,
                        loc,
                        concat!(
                            "Both branches of the if-block must produce ",
                            "the same types of the arguments on the data stack"
                        ),
                    )?;

                    let ins = (self.data_stack.min()).min(expected.min()).abs();
                    let out = (self.data_stack.count()).max(expected.count()) + ins;

                    ctx.block_contracts()
                        .insert(start_op, (ins.unsigned_abs(), out.unsigned_abs()));

                    let TypeBlock(old_stack, _) = self.block_stack_pop();

                    let old_count = old_stack.count();
                    self.data_stack.set_count(old_count - ins, old_count);
                }

                ControlOp::While => {
                    self.push_stack(ip);
                    self.data_stack.reset_max_count();
                }

                ControlOp::Do => {
                    self.data_stack.expect_pop_type(BOOL, loc)?;
                    let TypeBlock(expected, _) = self.block_stack_last().clone();

                    self.expect_stack_arity(
                        &expected,
                        loc,
                        concat!(
                            "While block is not allowed to alter ",
                            "the types of the arguments on the data stack"
                        ),
                    )?;

                    self.push_stack(ip);
                    self.data_stack = DataStack::new(expected);
                }

                ControlOp::EndWhile => {
                    let TypeBlock(expected, do_op) = self.block_stack_pop();

                    self.expect_stack_arity(
                        &expected,
                        loc,
                        concat!(
                            "Do block is not allowed to alter ",
                            "the types of the arguments on the data stack"
                        ),
                    )?;

                    let TypeBlock(old_stack, start_op) = self.block_stack_pop();

                    let updater = |control_op: &mut OpType| {
                        let OpType::ControlOp(_, operand) = control_op else {
                            panic!("ICE");
                        };

                        *operand = ip - start_op;
                    };

                    ctx.update_op(start_op, updater);
                    ctx.update_op(ip, updater);

                    let ins = (self.data_stack.min()).min(expected.min()).abs();
                    let out = (self.data_stack.count()).max(expected.count()) + ins;
                    let contr = (ins.unsigned_abs(), out.unsigned_abs());

                    ctx.block_contracts()
                        .extend([(do_op, contr), (start_op, contr)]);

                    let old_count = old_stack.count();
                    self.data_stack.set_count(old_count - ins, old_count);
                }

                ControlOp::BindStack => {
                    let proc = self
                        .current_proc(ctx)
                        .expect("Expected to be used inside a procedure");

                    let Binds(bindings) = &proc.binds[index];
                    let mut binds = vec![];

                    for (_, typ) in bindings {
                        if let &Some(data_type) = typ {
                            let type_def = ctx.get_type_descr(data_type);

                            let contract: Vec<_> = type_def.units().map(|u| u.get_type()).collect();
                            self.data_stack.expect_contract_pop(&contract, loc)?;

                            binds.push(((data_type, loc), type_def.size()));
                        } else {
                            let top = self.data_stack.expect_pop(loc)?;
                            binds.push((top, WORD_USIZE));
                        }
                    }

                    self.bind_stack.extend(binds.iter().rev());
                }

                ControlOp::PopBind => {
                    let proc = self
                        .current_proc(ctx)
                        .expect("Expected to be used inside a procedure");

                    let Binds(bindings) = &proc.binds[index];
                    self.bind_stack
                        .truncate(self.bind_stack.len() - bindings.len());
                }

                ControlOp::CaseStart => todo!(),
                ControlOp::CaseMatch => todo!(),
                ControlOp::CaseOption => todo!(),
                ControlOp::EndCase => todo!(),
            },

            OpType::ExpectType(data_type) => {
                self.data_stack
                    .expect_peek(ArityType::Type(data_type), loc)?;
            }
        }
        Ok(())
    }

    fn push_stack(&mut self, ip: usize) {
        self.block_stack
            .push(TypeBlock(self.data_stack.clone(), ip));
    }

    fn pop_push<const N: usize>(
        &mut self, contr: [DataType; N], data_type: DataType, loc: Loc,
    ) -> LazyResult<()> {
        self.data_stack.pop_push(contr, (data_type, loc), loc)
    }

    fn extend_value<const N: usize>(&mut self, value: [DataType; N], loc: Loc) {
        self.data_stack
            .extend(value.map(|data_type| (data_type, loc)));
    }

    fn push_frame(&mut self, data_type: DataType, loc: Loc) {
        self.data_stack.push((data_type, loc));
    }

    fn get_bind_type_offset(&self, operand: usize) -> (DataType, u16) {
        let mut offset = 0;
        for ((_, size), _) in self.bind_stack.iter().rev().zip(0..=operand) {
            offset += size;
        }

        let (frame, _) = self.bind_stack[self.bind_stack.len() - 1 - operand];

        (frame.get_type(), offset)
    }

    fn expect_type_ref(&mut self, ctx: &Ctx, word: Name, loc: Loc) -> LazyResult<(DataType, u16)> {
        let data_type = self.data_stack.expect_pop(loc)?.get_type();

        let TypeDescr::Reference(ptr) = &*ctx.get_type_descr(data_type) else {
            lazybail!(
                |f| "{}Cannot `.` access elements of type: `{}`",
                f(Fmt::Loc(loc)),
                f(Fmt::Dat(data_type))
            )
        };

        match &*ctx.get_type_descr(ptr.ptr_type()) {
            TypeDescr::Structure(StructType(fields, _)) => {
                let struct_name = fields.name();
                fields
                    .get_offset(word)
                    .with_err_ctx(lazyctx!(
                        |f| "{}The struct {} does not contain a member with name: `{}`",
                        f(Fmt::Loc(loc)),
                        f(Fmt::Key(struct_name)),
                        f(Fmt::Key(word))
                    ))
                    .map(|(offset, index)| (fields[index].get_type(), offset * WORD_USIZE))
            }

            TypeDescr::Primitive(_) => todo!(),
            TypeDescr::Reference(_) => todo!(),
        }
    }

    fn expect_stack_arity<'a>(
        &self, expected: &[TypeFrame], loc: Loc, error_text: &'a str,
    ) -> crate::compiler::utils::LazyResult<'a, ()> {
        self.data_stack
            .expect_exact(expected, loc)
            .map_err(move |err| lazyerr!(|f| "{}\n[ERROR] {}", error_text, err.apply(f)))
    }
}

impl Ctx {
    fn update_op(&mut self, ip: usize, setter: impl Fn(&mut OpType)) {
        let (op, _) = &mut self.ops_mut()[ip];
        setter(op);
    }

    pub fn type_check(&mut self) -> Result<&mut Self> {
        info!("Typechecking ctx");

        TypeChecker::new()
            .type_check(self)
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Typechecking done");

        Ok(self)
    }
}
