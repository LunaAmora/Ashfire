use std::{collections::HashMap, ops::Deref};

use firelib::lexer::Loc;

use crate::compiler::{expect::Expect, types::*};

impl Expect<IRToken> for Vec<IRToken> {}

pub struct LocWord {
    pub name: String,
    pub loc: Loc,
}

impl Location for LocWord {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl PartialEq<str> for LocWord {
    fn eq(&self, other: &str) -> bool {
        self.name == other
    }
}

impl Deref for LocWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl LocWord {
    pub fn new(name: &str, loc: Loc) -> Self {
        Self { name: name.to_owned(), loc }
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
    GlobalMem,
    ConstStruct,
    Variable,
    LocalMem,
    Binding,
}

pub struct Scope {
    op: Op,
    names: HashMap<String, ParseContext>,
}

impl Scope {
    pub fn new(op: Op) -> Self {
        Self { op, names: HashMap::default() }
    }
}

#[derive(Default)]
pub struct NameScopes {
    scopes: Vec<Scope>,
    names: HashMap<String, ParseContext>,
}

impl NameScopes {
    pub fn lookup(&self, name: &str) -> Option<&ParseContext> {
        //Todo: there must be a better way to support `.` accessing structs
        let name = name.split(".").next().unwrap();

        for scope in self.scopes.iter().rev() {
            let ctx = scope.names.get(name);
            if ctx.is_some() {
                return ctx;
            }
        }

        self.names.get(name)
    }

    pub fn register(&mut self, name: String, ctx: ParseContext) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.names.insert(name, ctx);
        } else {
            self.names.insert(name, ctx);
        }
    }

    pub fn push(&mut self, op: Op) {
        self.scopes.push(Scope::new(op))
    }

    pub fn pop(&mut self) -> Option<Op> {
        self.scopes.pop().map(|s| s.op)
    }
}
