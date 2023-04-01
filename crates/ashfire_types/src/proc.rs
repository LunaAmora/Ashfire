use crate::{
    core::{word_aligned, Name, OffsetWord, WORD_SIZE},
    data::{DataType, StructInfo, TypeDescr},
};

#[derive(Default)]
pub struct Data {
    pub local_vars: Vec<TypeDescr>,
    pub local_mems: Vec<OffsetWord>,
    mem_size: usize,
}

impl Data {
    pub fn push_mem(&mut self, word: Name, size: usize) {
        self.mem_size += size;
        self.local_mems.push(OffsetWord::new(word, self.mem_size));
    }

    pub fn total_size(&self) -> i32 {
        word_aligned(self.mem_size) +
            self.local_vars.iter().fold(0, |acc, var| acc + var.size()) as i32
    }

    pub fn var_mem_offset(&self, index: usize) -> i32 {
        word_aligned(self.mem_size) + ((index as i32 + 1) * WORD_SIZE)
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
pub struct Binds(pub Vec<(Name, Option<DataType>)>);

#[derive(Default)]
pub struct Proc {
    pub name: Name,
    pub contract: Contract,
    pub mode: Mode,
    pub binds: Vec<Binds>,
}

impl Proc {
    pub fn new(name: Name, contract: Contract, mode: ModeType) -> Self {
        let mode = Mode::from(mode);
        Self { name, contract, mode, ..Default::default() }
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

    pub fn bindings(&self) -> impl Iterator<Item = &(Name, Option<DataType>)> {
        self.binds.iter().rev().flat_map(|Binds(bind)| bind.iter())
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
