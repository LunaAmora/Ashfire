use std::fmt::{self, Display, Formatter, Result};

use itertools::Itertools;

#[derive(Clone, Copy)]
pub enum WasmType {
    I32,
}

impl Display for WasmType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WasmType::I32 => write!(f, "i32"),
        }
    }
}

pub struct WasmValue {
    pub(crate) wasm_type: WasmType,
    pub(crate) value: i32,
    pub(crate) is_mut: bool,
}

pub struct FuncType {
    pub(crate) param: Vec<WasmType>,
    pub(crate) result: Vec<WasmType>,
}

impl Display for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut res = String::new();

        if !self.param.is_empty() {
            res += &format!(" (param {})", self.param.iter().join(" "));
        }

        if !self.result.is_empty() {
            res += &format!(" (result {})", self.result.iter().join(" "))
        }

        write!(f, "{res}")
    }
}

pub struct Memory {
    pages: usize,
}

impl Default for Memory {
    fn default() -> Self {
        Self { pages: 1 }
    }
}

impl Display for Memory {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "(memory {})", self.pages)
    }
}

pub struct Import {
    pub(crate) module: String,
    pub(crate) label: String,
    pub(crate) bind: Bind,
}

pub struct Export {
    pub(crate) label: String,
    pub(crate) bind: Bind,
}

pub enum Ident {
    Id(usize),
    Label(String),
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Ident::Id(id) => write!(f, "{id}"),
            Ident::Label(label) => write!(f, "${label}"),
        }
    }
}

impl From<&str> for Ident {
    fn from(label: &str) -> Self {
        Ident::Label(label.to_owned())
    }
}

pub enum Bind {
    Global(Ident),
    Func(Ident),
    Mem(Ident),
}

pub struct Global {
    pub(crate) value: WasmValue,
}

pub struct Func {
    pub(crate) contract: usize,
    pub(crate) code: Vec<Instruction>,
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Scope {
    local,
    global,
}

pub enum Instruction {
    Block(BlockType, Option<Ident>),
    Get(Scope, Ident),
    Set(Scope, Ident),
    I32(NumMethod),
    Const(i32),
    Call(Ident),
    BrIf(Ident),
    Br(Ident),
    Else,
    Drop,
    End,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let text = match self {
            Instruction::Block(_, _) => unimplemented!("Not possible to format"),
            Instruction::Get(scope, ident) => format!("{:?}.get {ident}", scope),
            Instruction::Set(scope, ident) => format!("{:?}.set {ident}", scope),
            Instruction::I32(num_method) => format!("i32.{:?}", num_method),
            Instruction::Const(value) => format!("i32.const {value}"),
            Instruction::Call(ident) => format!("call {ident}"),
            Instruction::BrIf(_) => todo!(),
            Instruction::Br(_) => todo!(),
            Instruction::Else => "else".to_string(),
            Instruction::Drop => "drop".to_string(),
            Instruction::End => "end".to_string(),
        };
        write!(f, "{text}")
    }
}

pub enum BlockType {
    If,
    Loop(Option<Ident>),
    Block(Option<Ident>),
}

impl Display for BlockType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let text = match self {
            BlockType::If => "if".to_string(),
            BlockType::Loop(_) => todo!(),
            BlockType::Block(_) => todo!(),
        };
        write!(f, "{text}")
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum NumMethod {
    add,
    sub,
    store,
    store8,
    store16,
    load,
    load8_s,
    load16_s,
    or,
    xor,
    and,
    eq,
    lt_s,
    ge_s,
    le_s,
    gt_s,
}
