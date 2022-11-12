use std::collections::HashMap;

use firelib::lexer::Loc;

use crate::compiler::{expect::Compare, program::Program, types::*};

impl Compare<IRToken> for Vec<IRToken> {}

pub struct LocWord {
    pub index: usize,
    pub loc: Loc,
}

impl Location for LocWord {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl LocWord {
    pub fn new(index: usize, loc: Loc) -> Self {
        Self { index, loc }
    }

    pub fn as_str<'a>(&self, prog: &'a Program) -> &'a str {
        &prog.words[self.index]
    }

    pub fn as_string(&self, prog: &Program) -> String {
        prog.words[self.index].to_owned()
    }

    pub fn operand(&self) -> i32 {
        self.index as i32
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

    pub fn register(&mut self, name: &str, ctx: ParseContext) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.names.insert(name.to_string(), ctx);
        } else {
            self.names.insert(name.to_string(), ctx);
        }
    }

    pub fn push(&mut self, op: Op) {
        self.scopes.push(Scope::new(op))
    }

    pub fn pop(&mut self) -> Option<Op> {
        self.scopes.pop().map(|s| s.op)
    }
}
