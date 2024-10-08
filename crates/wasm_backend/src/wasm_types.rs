use std::fmt::{self, Display, Formatter, Result};

use itertools::Itertools;

#[derive(Clone, Copy)]
pub enum WasmType {
    I32,
}

impl Display for WasmType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
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
        if !self.param.is_empty() {
            write!(f, " (param {})", self.param.iter().join(" "))?;
        }

        if !self.result.is_empty() {
            write!(f, " (result {})", self.result.iter().join(" "))?;
        }

        Ok(())
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
            Self::Id(id) => write!(f, "{id}"),
            Self::Label(label) => write!(f, "${label}"),
        }
    }
}

impl From<&str> for Ident {
    fn from(label: &str) -> Self {
        Self::Label(label.to_owned())
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
#[allow(non_camel_case_types, reason = "Debug format used in wasm generation")]
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
        match self {
            Self::Block(_, _) => unimplemented!("Not possible to format"),
            Self::Get(scope, ident) => write!(f, "{scope:?}.get {ident}"),
            Self::Set(scope, ident) => write!(f, "{scope:?}.set {ident}"),
            Self::I32(num_method) => write!(f, "i32.{num_method:?}"),
            Self::Const(value) => write!(f, "i32.const {value}"),
            Self::Call(ident) => write!(f, "call {ident}"),
            Self::BrIf(_) => todo!(),
            Self::Br(ident) => write!(f, "br {ident}"),
            Self::Else => write!(f, "else"),
            Self::Drop => write!(f, "drop"),
            Self::End => write!(f, "end"),
        }
    }
}

pub enum BlockType {
    If,
    Loop(Option<Ident>),
    Block(Option<Ident>),
}

impl Display for BlockType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let (text, id) = match self {
            Self::If => ("if", &None),
            Self::Loop(id) => ("loop", id),
            Self::Block(id) => ("block", id),
        };

        f.write_str(text)?;

        if let Some(id) = id {
            write!(f, " {id}")?;
        }

        Ok(())
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types, reason = "Debug format used in wasm generation")]
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
