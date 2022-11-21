use std::{collections::HashMap, ops::Deref};

use firelib::lexer::Loc;

use crate::compiler::{
    program::{InternalString, Program},
    typechecking::expect::Compare,
    types::core::*,
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

#[derive(Clone, Copy)]
pub enum VarWordType {
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
        if let Some(scope) = self.scopes.last_mut() {
            scope.names.insert(*name, ctx);
        } else {
            self.names.insert(*name, ctx);
        }
    }

    pub fn push(&mut self, op: Op) {
        self.scopes.push(Scope::new(op));
    }

    pub fn pop(&mut self) -> Option<Op> {
        self.scopes.pop().map(|s| s.op)
    }
}
