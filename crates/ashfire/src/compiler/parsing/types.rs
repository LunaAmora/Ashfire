use std::{cell::Ref, collections::HashMap, convert::TryInto};

use ashfire_types::{
    core::*,
    data::*,
    enums::{ControlOp, IntrinsicType, MemOp, OpType},
};
use firelib::{
    lexer::Loc,
    span::{Span, Spanned},
    utils::BoolUtils,
};
use num::iter::range_step_from;

use crate::compiler::{
    ctx::{Ctx, InternalString},
    typechecking::expect::Compare,
};

impl Compare<'_, DataToken> for Vec<DataToken> {}

pub type LocWord = Spanned<Name>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarWordType {
    None,
    Store,
    Pointer,
}

pub enum ParseContext {
    ProcName,
    LocalMem,
    GlobalMem,
    LocalVar,
    GlobalVar,
    ConstStruct,
    Binding,
}

pub type Block = (ControlOp, usize, Loc);

pub struct Scope {
    block: Block,
    names: HashMap<Name, ParseContext>,
}

impl Scope {
    pub fn new(block: Block) -> Self {
        Self { block, names: HashMap::default() }
    }
}

#[derive(Default)]
pub struct NameScopes {
    scopes: Vec<Scope>,
    names: HashMap<Name, ParseContext>,
}

impl NameScopes {
    pub fn lookup(&self, name: Name, ctx: &Ctx) -> Option<&ParseContext> {
        // Todo: there must be a better way to support `.` accessing structs
        if let Some(field_name) = name.as_str(ctx).split('.').next() &&
            let Some(key) = ctx.get_key(field_name)
        {
            return self.lookup(key, ctx);
        }

        for scope in self.scopes.iter().rev() {
            let parse_ctx = scope.names.get(&name);
            if parse_ctx.is_some() {
                return parse_ctx;
            }
        }

        self.names.get(&name)
    }

    pub fn register(&mut self, name: Name, ctx: ParseContext) {
        self.scopes
            .last_mut()
            .map_or_else(|| &mut self.names, |scope| &mut scope.names)
            .insert(name, ctx);
    }

    pub fn push(&mut self, block: Block) {
        self.scopes.push(Scope::new(block));
    }

    pub fn pop(&mut self) -> Option<Block> {
        self.scopes.pop().map(|s| s.block)
    }
}

pub trait StructUtils {
    fn get_offset(&self, word: Name) -> Option<(u16, usize)>;
    fn get_offset_local(&self, word: Name) -> Option<(u16, usize)>;
    fn get_pointer(&self, word: &LocWord, push_type: MemOp, data_type: DataType) -> Vec<Op>;
    fn get_fields(
        &self, word: &LocWord, push_type: MemOp, stk_def: Ref<TypeDescr>, store: bool,
    ) -> Vec<Op>;
}

impl StructUtils for [TypeDescr] {
    fn get_offset(&self, word: Name) -> Option<(u16, usize)> {
        let i = self.iter().position(|stk| word.eq(&stk.name()))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset, i))
    }

    fn get_offset_local(&self, word: Name) -> Option<(u16, usize)> {
        let i = self.iter().position(|stk| word.eq(&stk.name()))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..=i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset - 1, i))
    }

    fn get_pointer(&self, word: &LocWord, push_type: MemOp, data_type: DataType) -> Vec<Op> {
        let &(name, loc) = word;
        let (index, _) = if push_type == MemOp::PushLocal {
            self.get_offset_local(name)
        } else {
            self.get_offset(name)
        }
        .expect("Should return an valid offset if the `name` is valid");

        vec![
            (OpType::MemOp(push_type, index), loc),
            Op::new((IntrinsicType::Cast(data_type), loc)),
        ]
    }

    fn get_fields(
        &self, word: &LocWord, push_type: MemOp, stk_def: Ref<TypeDescr>, store: bool,
    ) -> Vec<Op> {
        let mut result = Vec::new();
        let &(name, loc) = word;
        let (index, _) = self
            .get_offset(name)
            .expect("Should return an valid offset if the `name` is valid");

        let is_local = push_type == MemOp::PushLocal;
        let id_range = if store == is_local {
            range_step_from(index.into(), 1) //Todo: this is so dumb
        } else {
            let count: u16 = stk_def.count().try_into().expect("ICE");
            let start = count + index - 1;
            range_step_from(start.into(), -1)
        };

        let members = match &*stk_def {
            TypeDescr::Structure(StructType(fields, _)) => fields
                .units()
                .conditional_rev(store)
                .map(|v| v.get_type())
                .collect(),
            TypeDescr::Primitive(prim) => vec![prim.get_type()],
            TypeDescr::Reference(ptr) => vec![ptr.get_type()],
        };

        for (operand, data_type) in id_range.zip(members) {
            if store {
                result.push((OpType::ExpectType(data_type), loc));
            }

            let offset: u16 = operand.try_into().expect("ICE");
            result.push((OpType::MemOp(push_type, offset * WORD_USIZE), loc));

            if store {
                result.push(Op::new((IntrinsicType::Store32, loc)));
            } else {
                result.extend([
                    Op::new((IntrinsicType::Load32, loc)),
                    Op::new((IntrinsicType::Cast(data_type), loc)),
                ]);
            }
        }

        result
    }
}
