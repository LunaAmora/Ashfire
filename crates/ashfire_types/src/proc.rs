use crate::{
    core::{Name, OffsetWord, WORD_USIZE, word_aligned},
    data::{DataType, StructInfo, TypeDescr},
};

#[derive(Default)]
pub struct Data {
    pub local_vars: Vec<TypeDescr>,
    pub local_mems: Vec<OffsetWord>,
    mem_size: u16,
}

impl Data {
    pub fn push_mem(&mut self, word: Name, size: u16) {
        self.mem_size += size;
        self.local_mems.push(OffsetWord::new(word, self.mem_size));
    }

    pub fn total_size(&self) -> u16 {
        word_aligned(self.mem_size) + self.local_vars.iter().fold(0, |acc, var| acc + var.size())
    }

    pub fn var_mem_offset(&self, offset: u16) -> u16 {
        word_aligned(self.mem_size) + WORD_USIZE + offset
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ModeType {
    Import,
    Export,
    Declare,
    Inline(usize),
}

pub enum ModeData {
    Imported,
    Exported(Data),
    Declared(Data),
    Inlined(usize, usize),
}

impl From<ModeType> for ModeData {
    fn from(value: ModeType) -> Self {
        match value {
            ModeType::Import => Self::Imported,
            ModeType::Export => Self::Exported(Data::default()),
            ModeType::Inline(start) => Self::Inlined(start, 0),
            ModeType::Declare => Self::default(),
        }
    }
}

impl Default for ModeData {
    fn default() -> Self {
        Self::Declared(Data::default())
    }
}

#[derive(Default)]
pub struct Binds(pub Vec<(Name, Option<DataType>)>);

#[derive(Default)]
pub struct Proc {
    pub name: Name,
    pub contract: Contract,
    pub mode_data: ModeData,
    pub binds: Vec<Binds>,
}

impl Proc {
    pub fn new(name: Name, contract: Contract, mode: ModeType) -> Self {
        let mode_data = ModeData::from(mode);
        Self { name, contract, mode_data, ..Default::default() }
    }

    pub fn get_data(&self) -> Option<&Data> {
        match &self.mode_data {
            ModeData::Declared(data) | ModeData::Exported(data) => Some(data),
            _ => None,
        }
    }

    pub fn get_data_mut(&mut self) -> Option<&mut Data> {
        match &mut self.mode_data {
            ModeData::Declared(data) | ModeData::Exported(data) => Some(data),
            _ => None,
        }
    }

    pub fn bindings(&self) -> impl Iterator<Item = &(Name, Option<DataType>)> {
        self.binds.iter().rev().flat_map(|Binds(bind)| bind.iter())
    }

    pub fn is_import(&self) -> bool {
        matches!(self.mode_data, ModeData::Imported)
    }

    pub fn is_export(&self) -> bool {
        matches!(self.mode_data, ModeData::Exported(_))
    }
}

#[derive(Default)]
pub struct Contract {
    ins: Vec<DataType>,
    outs: Vec<DataType>,
}

impl Contract {
    pub fn new(ins: Vec<DataType>, outs: Vec<DataType>) -> Self {
        Self { ins, outs }
    }

    pub fn ins(&self) -> &[DataType] {
        &self.ins
    }

    pub fn outs(&self) -> &[DataType] {
        &self.outs
    }

    pub fn size(&self) -> (usize, usize) {
        (self.ins.len(), self.outs.len())
    }

    pub fn as_vec<I: Copy, O: Copy>(&self, in_type: I, out_type: O) -> (Vec<I>, Vec<O>) {
        let (ins, outs) = self.size();
        (vec![in_type; ins], vec![out_type; outs])
    }
}

impl From<&Contract> for (usize, usize) {
    fn from(contr: &Contract) -> Self {
        (contr.ins.len(), contr.outs.len())
    }
}
