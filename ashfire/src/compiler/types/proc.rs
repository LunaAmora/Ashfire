use super::{
    core::{OffsetWord, StrKey, TokenType, WORD_SIZE},
    data::{StructInfo, StructType},
};

#[derive(Default)]
pub struct ProcData {
    pub bindings: Vec<StrKey>,
    pub local_vars: Vec<StructType>,
    pub local_mems: Vec<OffsetWord>,
    mem_size: usize,
}

impl ProcData {
    pub fn push_mem(&mut self, word: &StrKey, size: usize) {
        self.mem_size += size;
        self.local_mems
            .push(OffsetWord::new(*word, self.mem_size as i32));
    }

    pub fn total_size(&self) -> i32 {
        (self.mem_size + self.local_vars.iter().fold(0, |acc, var| acc + var.size())) as i32
    }

    pub fn var_mem_offset(&self, index: i32) -> i32 {
        self.mem_size as i32 + (index + 1) * WORD_SIZE
    }
}

pub enum ProcType {
    Inline(usize, usize),
    Declare(ProcData),
}

impl Default for ProcType {
    fn default() -> Self {
        Self::Declare(ProcData::default())
    }
}

#[derive(Default)]
pub struct Proc {
    pub name: StrKey,
    pub contract: Contract,
    pub data: ProcType,
}

impl Proc {
    pub fn new(name: &StrKey, contract: Contract, inline: Option<usize>) -> Self {
        let data = inline.map_or_else(ProcType::default, |start| ProcType::Inline(start, 0));
        Self { name: *name, contract, data }
    }

    pub fn get_data(&self) -> Option<&ProcData> {
        match &self.data {
            ProcType::Inline(..) => None,
            ProcType::Declare(data) => Some(data),
        }
    }

    pub fn get_data_mut(&mut self) -> Option<&mut ProcData> {
        match &mut self.data {
            ProcType::Inline(..) => None,
            ProcType::Declare(data) => Some(data),
        }
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
