use std::{collections::HashMap, ops::Deref};

use ashfire_types::{
    core::*,
    data::*,
    enums::{IntrinsicType, OpType},
    num::iter::range_step_from,
};
use either::Either;
use firelib::lexer::Loc;

use crate::compiler::{
    program::{InternalString, Program},
    typechecking::expect::Compare,
};

impl Compare<IRToken> for Vec<IRToken> {}

pub struct LocWord {
    word: StrKey,
    pub loc: Loc,
}

impl Operand for LocWord {
    fn operand(&self) -> i32 {
        self.word.operand()
    }

    fn index(&self) -> usize {
        self.word.index()
    }

    fn str_key(&self) -> StrKey {
        self.word
    }
}

impl Location for LocWord {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl LocWord {
    pub fn new<O: Operand>(index: O, loc: Loc) -> Self {
        Self { word: index.str_key(), loc }
    }
}

impl Deref for LocWord {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

impl From<LocWord> for IRToken {
    fn from(value: LocWord) -> Self {
        Self::new(TokenType::Word, value.operand(), value.loc)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarWordType {
    None,
    Store,
    Pointer,
}

#[allow(dead_code)]
pub enum ParseContext {
    ProcName,
    LocalMem,
    GlobalMem,
    LocalVar,
    GlobalVar,
    ConstStruct,
    Binding,
}

pub struct Scope {
    op: Op,
    names: HashMap<StrKey, ParseContext>,
}

impl Scope {
    pub fn new(op: Op) -> Self {
        Self { op, names: HashMap::default() }
    }
}

#[derive(Default)]
pub struct NameScopes {
    scopes: Vec<Scope>,
    names: HashMap<StrKey, ParseContext>,
}

impl NameScopes {
    pub fn lookup(&self, name: &StrKey, prog: &Program) -> Option<&ParseContext> {
        //Todo: there must be a better way to support `.` accessing structs
        let word = name.as_str(prog);
        if word.contains('.') {
            let name = word.split('.').next().unwrap();
            if let Some(key) = prog.get_key(name) {
                return self.lookup(&key, prog);
            }
        }

        for scope in self.scopes.iter().rev() {
            let ctx = scope.names.get(name);
            if ctx.is_some() {
                return ctx;
            }
        }

        self.names.get(name)
    }

    pub fn register(&mut self, name: &StrKey, ctx: ParseContext) {
        self.scopes
            .last_mut()
            .map_or_else(|| &mut self.names, |scope| &mut scope.names)
            .insert(*name, ctx);
    }

    pub fn push(&mut self, op: Op) {
        self.scopes.push(Scope::new(op));
    }

    pub fn pop(&mut self) -> Option<Op> {
        self.scopes.pop().map(|s| s.op)
    }
}

pub trait StructUtils {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)>;
    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)>;
    fn get_pointer(&self, word: &LocWord, push_type: OpType, stk_id: i32) -> Vec<Op>;
    fn get_fields(
        &self, word: &LocWord, push_type: OpType, stk_def: &StructDef, store: bool,
    ) -> Vec<Op>;
}

impl StructUtils for [StructType] {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)> {
        let i = self.iter().position(|stk| word.eq(stk))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset, i))
    }

    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)> {
        let i = self.iter().position(|stk| word.eq(stk))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..=i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset - 1, i))
    }

    fn get_pointer(&self, word: &LocWord, push_type: OpType, stk_id: i32) -> Vec<Op> {
        let index = if push_type == OpType::PushLocal {
            self.get_offset_local(word).unwrap().0
        } else {
            self.get_offset(word).unwrap().0
        };

        vec![
            Op::new(push_type, index as i32, word.loc),
            Op::from((IntrinsicType::Cast(-stk_id), word.loc)),
        ]
    }

    fn get_fields(
        &self, word: &LocWord, push_type: OpType, stk_def: &StructDef, store: bool,
    ) -> Vec<Op> {
        let mut result = Vec::new();
        let loc = word.loc;
        let index = self.get_offset(word).unwrap().0 as i32;

        let is_local = push_type == OpType::PushLocal;
        let id_range = if store == is_local {
            range_step_from(index, 1)
        } else {
            range_step_from(index + stk_def.count() as i32 - 1, -1)
        };

        let members = stk_def.units();
        let members = if store {
            Either::Left(members.into_iter().rev())
        } else {
            Either::Right(members.into_iter())
        };

        let members = members.map(ValueUnit::value_type).map(Operand::operand);

        for (operand, type_id) in id_range.zip(members) {
            if store {
                result.push(Op::new(OpType::ExpectType, type_id, loc));
            }

            result.push(Op::new(push_type, operand, loc));

            if store {
                result.push(Op::from((IntrinsicType::Store32, loc)));
            } else {
                result.extend([
                    Op::from((IntrinsicType::Load32, loc)),
                    Op::from((IntrinsicType::Cast(type_id), loc)),
                ]);
            }
        }

        result
    }
}

pub fn unpack_struct(
    stk: &StructType, push_type: OpType, mut offset: usize, var_typ: VarWordType, loc: Loc,
) -> Vec<Op> {
    let mut result = Vec::new();
    match stk {
        StructType::Unit(unit) => {
            let type_id = unit.value_type().operand();
            result.push(Op::new(push_type, offset as i32, loc));

            if var_typ == VarWordType::Store {
                result.insert(0, Op::new(OpType::ExpectType, type_id, loc));
                result.push(Op::from((IntrinsicType::Store32, loc)));
            } else if var_typ == VarWordType::Pointer {
                result.push(Op::from((IntrinsicType::Cast(-type_id), loc)));
            } else {
                result.extend([
                    Op::from((IntrinsicType::Load32, loc)),
                    Op::from((IntrinsicType::Cast(type_id), loc)),
                ]);
            }
        }

        StructType::Root(root) => {
            if var_typ == VarWordType::Store {
                todo!();
            }

            if push_type == OpType::PushLocal {
                offset += 1;
            }

            let type_id = root.get_ref_type().operand();

            result.extend([
                Op::new(push_type, offset as i32, loc),
                Op::from((IntrinsicType::Cast(-type_id), loc)),
            ]);

            if var_typ != VarWordType::Pointer {
                result.push((OpType::Unpack, loc).into());
            }
        }
    };
    result
}
