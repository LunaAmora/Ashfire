use super::{
    core::{word_aligned, OffsetWord, StrKey, TokenType, WORD_SIZE},
    data::TypeDescr,
};
use crate::data::StructInfo;

#[derive(Default)]
pub struct Data {
    pub local_vars: Vec<TypeDescr>,
    pub local_mems: Vec<OffsetWord>,
    mem_size: usize,
}

impl Data {
    pub fn push_mem(&mut self, word: &StrKey, size: usize) {
        self.mem_size += size;
        self.local_mems
            .push(OffsetWord::new(*word, self.mem_size as i32));
    }

    pub fn total_size(&self) -> i32 {
        word_aligned(self.mem_size) +
            self.local_vars.iter().fold(0, |acc, var| acc + var.size()) as i32
    }

    pub fn var_mem_offset(&self, index: i32) -> i32 {
        word_aligned(self.mem_size) + ((index + 1) * WORD_SIZE)
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ModeType {
    Import,
    Export,
    Declare,
    Inline(usize),
}

pub enum Mode {
    Imported,
    Exported(Data),
    Declared(Data),
    Inlined(usize, usize),
}

impl From<ModeType> for Mode {
    fn from(value: ModeType) -> Self {
        match value {
            ModeType::Import => Self::Imported,
            ModeType::Export => Self::Exported(Data::default()),
            ModeType::Inline(start) => Self::Inlined(start, 0),
            ModeType::Declare => Self::default(),
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Declared(Data::default())
    }
}

#[derive(Default)]
pub struct Binding(pub Vec<(StrKey, Option<usize>)>);

#[derive(Default)]
pub struct Proc {
    pub name: StrKey,
    pub contract: Contract,
    pub mode: Mode,
    pub bindings: Vec<Binding>,
}

impl Proc {
    pub fn new(name: &StrKey, contract: Contract, mode: ModeType) -> Self {
        let mode = Mode::from(mode);
        Self { name: *name, contract, mode, ..Default::default() }
    }

    pub fn get_data(&self) -> Option<&Data> {
        match &self.mode {
            Mode::Declared(data) | Mode::Exported(data) => Some(data),
            _ => None,
        }
    }

    pub fn get_data_mut(&mut self) -> Option<&mut Data> {
        match &mut self.mode {
            Mode::Declared(data) | Mode::Exported(data) => Some(data),
            _ => None,
        }
    }

    pub fn bindings(&self) -> impl Iterator<Item = &(StrKey, Option<usize>)> {
        self.bindings.iter().rev().flat_map(|bind| bind.0.iter())
    }

    pub fn is_import(&self) -> bool {
        matches!(self.mode, Mode::Imported)
    }

    pub fn is_export(&self) -> bool {
        matches!(self.mode, Mode::Exported(_))
    }
}

#[derive(Default)]
pub struct Contract {
    ins: Vec<TokenType>,
    outs: Vec<TokenType>,
}

impl Contract {
    pub fn new(ins: Vec<TokenType>, outs: Vec<TokenType>) -> Self {
        Self { ins, outs }
    }

    pub fn ins(&self) -> &[TokenType] {
        &self.ins
    }

    pub fn outs(&self) -> &[TokenType] {
        &self.outs
    }

    pub fn size(&self) -> (usize, usize) {
        (self.ins.len(), self.outs.len())
    }
}

impl From<&Contract> for (usize, usize) {
    fn from(contr: &Contract) -> Self {
        (contr.ins.len(), contr.outs.len())
    }
}
