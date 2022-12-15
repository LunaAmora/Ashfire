use std::collections::HashMap;

use ashfire_types::{
    core::*,
    data::*,
    enums::{ControlOp, IndexOp, IntrinsicType, OpType},
    lasso::Key,
    num::iter::range_step_from,
};
use firelib::{lexer::Loc, utils::BoolUtils};

use crate::compiler::{
    program::{InternalString, Program},
    typechecking::expect::Compare,
};

impl Compare<'_, IRToken> for Vec<IRToken> {}

pub struct LocWord(pub Name, pub Loc);

impl LocWord {
    pub fn name(&self) -> Name {
        self.0
    }

    pub fn index(&self) -> usize {
        self.0.into_usize()
    }
}

impl Location for LocWord {
    fn loc(&self) -> Loc {
        self.1
    }
}

impl InternalString for LocWord {
    fn as_str<'p>(&self, prog: &'p Program) -> &'p str {
        self.0.as_str(prog)
    }

    fn as_string(&self, prog: &Program) -> String {
        self.0.as_string(prog)
    }
}

impl PartialEq<Name> for LocWord {
    fn eq(&self, other: &Name) -> bool {
        self.0 == *other
    }
}

impl From<LocWord> for IRToken {
    fn from(word: LocWord) -> Self {
        Self(TokenType::Word, word.0.into_usize() as i32, word.1)
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
    block: (ControlOp, usize, Loc),
    names: HashMap<Name, ParseContext>,
}

impl Scope {
    pub fn new(block: (ControlOp, usize, Loc)) -> Self {
        Self { block, names: HashMap::default() }
    }
}

#[derive(Default)]
pub struct NameScopes {
    scopes: Vec<Scope>,
    names: HashMap<Name, ParseContext>,
}

impl NameScopes {
    pub fn lookup(&self, name: Name, prog: &Program) -> Option<&ParseContext> {
        //Todo: there must be a better way to support `.` accessing structs
        let word = name.as_str(prog);
        if word.contains('.') {
            let name = word.split('.').next().unwrap();
            if let Some(key) = prog.get_key(name) {
                return self.lookup(key, prog);
            }
        }

        for scope in self.scopes.iter().rev() {
            let ctx = scope.names.get(&name);
            if ctx.is_some() {
                return ctx;
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

    pub fn push(&mut self, block: (ControlOp, usize, Loc)) {
        self.scopes.push(Scope::new(block));
    }

    pub fn pop(&mut self) -> Option<(ControlOp, usize, Loc)> {
        self.scopes.pop().map(|s| s.block)
    }
}

pub trait StructUtils {
    fn get_offset(&self, word: Name) -> Option<(usize, usize)>;
    fn get_offset_local(&self, word: Name) -> Option<(usize, usize)>;
    fn get_pointer(&self, word: &LocWord, push_type: IndexOp, type_id: TypeId) -> Vec<Op>;
    fn get_fields(
        &self, word: &LocWord, push_type: IndexOp, stk_def: &TypeDescr, store: bool,
    ) -> Vec<Op>;
}

impl StructUtils for [TypeDescr] {
    fn get_offset(&self, word: Name) -> Option<(usize, usize)> {
        let i = self.iter().position(|stk| word.eq(&stk.name()))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset, i))
    }

    fn get_offset_local(&self, word: Name) -> Option<(usize, usize)> {
        let i = self.iter().position(|stk| word.eq(&stk.name()))?;

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..=i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset - 1, i))
    }

    fn get_pointer(&self, word: &LocWord, push_type: IndexOp, type_id: TypeId) -> Vec<Op> {
        let &LocWord(name, loc) = word;
        let (index, _) = if push_type == IndexOp::PushLocal {
            self.get_offset_local(name).unwrap()
        } else {
            self.get_offset(name).unwrap()
        };

        vec![
            Op(OpType::IndexOp(push_type, index), loc),
            Op::from((IntrinsicType::Cast(type_id), loc)),
        ]
    }

    fn get_fields(
        &self, word: &LocWord, push_type: IndexOp, stk_def: &TypeDescr, store: bool,
    ) -> Vec<Op> {
        let mut result = Vec::new();
        let &LocWord(name, loc) = word;
        let (index, _) = self.get_offset(name).unwrap();

        let is_local = push_type == IndexOp::PushLocal;
        let id_range = if store == is_local {
            range_step_from(index as i32, 1)
        } else {
            range_step_from((index + stk_def.count()) as i32 - 1, -1)
        };

        let members = match stk_def {
            TypeDescr::Structure(StructType(fields, _)) => fields
                .units()
                .conditional_rev(store)
                .map(|v| v.type_id())
                .collect(),
            TypeDescr::Primitive(prm) => vec![prm.type_id()],
            TypeDescr::Reference(ptr) => vec![ptr.type_id()],
        };

        for (operand, type_id) in id_range.zip(members) {
            if store {
                result.push(Op(OpType::ExpectType(type_id), loc));
            }

            result.push(Op(OpType::IndexOp(push_type, operand as usize), loc));

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
