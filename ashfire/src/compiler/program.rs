use std::collections::HashMap;

use firelib::{lazy, lexer::Loc};
use lasso::Rodeo;

use super::types::{
    core::*,
    data::*,
    enums::IntrinsicType,
    proc::{Data, Proc},
};

pub type OptionErr<T, E = Fmt> = ashlib::OptionErr<T, E>;
pub type LazyResult<T, E = Fmt> = lazy::LazyResult<T, E>;
pub type LazyError<E = Fmt> = lazy::LazyError<E>;

pub trait InternalString {
    fn as_str<'a>(&self, prog: &'a Program) -> &'a str;
    fn as_string(&self, prog: &Program) -> String;
}

impl InternalString for StrKey {
    fn as_str<'a>(&self, prog: &'a Program) -> &'a str {
        prog.interner.resolve(self)
    }

    fn as_string(&self, prog: &Program) -> String {
        prog.interner.resolve(self).to_owned()
    }
}

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub consts: Vec<StructType>,
    pub global_vars: Vec<StructType>,
    pub structs_types: Vec<StructDef>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    pub included_files: Vec<StrKey>,
    interner: Rodeo,
    mem_size: usize,
    memory: Vec<OffsetWord>,
    data_size: usize,
    data: Vec<OffsetData>,
}

impl Program {
    pub fn new() -> Self {
        let mut interner = Rodeo::default();
        interner.get_or_intern(String::new());

        let structs_types = vec![
            (interner.get_or_intern("int"), Value::Int).into(),
            (interner.get_or_intern("bool"), Value::Bool).into(),
            (interner.get_or_intern("ptr"), Value::Ptr).into(),
            StructDef::new(&interner.get_or_intern("str"), vec![
                StructType::Unit(ValueUnit::from_type(
                    &interner.get_or_intern("len"),
                    ValueType::Typ(Value::Int),
                )),
                StructType::Unit(ValueUnit::from_type(
                    &interner.get_or_intern("data"),
                    ValueType::Typ(Value::Ptr),
                )),
            ]),
            (interner.get_or_intern("any"), Value::Any).into(),
        ];

        Self { structs_types, interner, ..Default::default() }
    }

    pub fn push_mem(&mut self, word: &StrKey, size: usize) {
        let value = OffsetWord::new(*word, self.mem_size as i32);
        self.memory.push(value);
        self.mem_size += size;
    }

    pub fn push_data(&mut self, mut word: String, size: usize) -> usize {
        if word.ends_with("\\0") {
            word.push('0');
        };

        let name = self.get_or_intern(&word);
        let value = OffsetData::new(name, size as i32, self.data_size as i32);
        self.data.push(value);
        self.data_size += size;
        self.data.len() - 1
    }

    pub fn data_size(&self) -> usize {
        self.data_size
    }

    pub fn data_start(&self) -> i32 {
        word_aligned(self.mem_size)
    }

    pub fn global_vars_start(&self) -> i32 {
        self.data_start() + word_aligned(self.data_size)
    }

    pub fn global_vars_size(&self) -> i32 {
        self.global_vars.iter().fold(0, |acc, var| acc + var.size()) as i32
    }

    pub fn stack_start(&self) -> i32 {
        self.global_vars_start() + self.global_vars_size()
    }

    pub fn get_or_intern(&mut self, word: &str) -> StrKey {
        self.interner.get_or_intern(word)
    }

    pub fn get_key(&self, word: &str) -> Option<StrKey> {
        self.interner.get(word)
    }

    pub fn get_word<O: Operand>(&self, index: O) -> &str {
        self.interner.resolve(&index.str_key())
    }

    pub fn get_data<O: Operand>(&self, index: O) -> &OffsetData {
        &self.data[index.index()]
    }

    pub fn get_data_str<O: Operand>(&self, index: O) -> &str {
        self.get_data(index).as_str(self)
    }

    pub fn get_proc<O: Operand>(&self, index: O) -> &Proc {
        &self.procs[index.index()]
    }

    pub fn get_contract<O: Operand>(&self, index: O) -> (usize, usize) {
        self.block_contracts[&(index.index())]
    }

    pub fn get_all_data(&self) -> &[OffsetData] {
        &self.data
    }

    pub fn get_memory(&self) -> &[OffsetWord] {
        &self.memory
    }

    pub fn current_ip(&self) -> usize {
        self.ops.len()
    }

    fn data_name(&self, value: Value) -> String {
        match value {
            Value::Int => "Integer",
            Value::Bool => "Boolean",
            Value::Ptr => "Pointer",
            Value::Str => "String",
            Value::Any => "Any",
            Value::Type(n) => self.structs_types[n].as_str(self),
        }
        .to_owned()
    }

    pub fn type_name(&self, typ: TokenType) -> String {
        match typ {
            TokenType::Keyword => "Keyword",
            TokenType::Word => "Word or Intrinsic",
            TokenType::Data(data) => match data {
                ValueType::Typ(value) => return self.data_name(value),
                ValueType::Ptr(value) => return self.data_name(value) + " Pointer",
            },
            TokenType::Str => "String",
        }
        .to_owned()
    }

    fn data_display<O: Operand>(value: Value, operand: O) -> String {
        let operand = operand.operand();
        match value {
            Value::Bool => fold_bool!(operand != 0, "True", "False").to_owned(),
            Value::Ptr => format!("*{operand}"),
            Value::Str | Value::Any | Value::Int | Value::Type(_) => operand.to_string(),
        }
    }

    pub fn type_display(&self, tok: IRToken) -> String {
        match tok.token_type {
            TokenType::Keyword => format!("{:?}", tok.as_keyword()),
            TokenType::Word => self.get_word(tok).to_owned(),
            TokenType::Str => self.get_data_str(tok).to_owned(),
            TokenType::Data(data) => Self::data_display(data.get_value(), tok),
        }
    }

    pub fn loc_fmt<L: Location>(&self, loc: L) -> String {
        let loc = loc.loc();
        self.included_files
            .get(loc.file_index)
            .map_or_else(String::new, |l| format!("{}:{}:{} ", l.as_str(self), loc.line, loc.col))
    }

    pub fn format(&self, fmt: Fmt) -> String {
        match fmt {
            Fmt::Loc(loc) => self.loc_fmt(loc),
            Fmt::Tok(tok) => self.type_display(tok),
            Fmt::Typ(typ) => self.type_name(typ),
        }
    }

    pub fn get_intrinsic_type(&self, word: &str) -> Option<IntrinsicType> {
        IntrinsicType::from_str(word).or_else(|| {
            Some(match word.as_bytes() {
                [b'#', b'*', rest @ ..] => IntrinsicType::Cast(-self.get_cast_type(rest)?),
                [b'#', rest @ ..] => IntrinsicType::Cast(self.get_cast_type(rest)?),
                _ => return None,
            })
        })
    }

    fn get_cast_type(&self, rest: &[u8]) -> Option<i32> {
        self.get_key(std::str::from_utf8(rest).ok()?)
            .as_ref()
            .map(|key| self.get_struct_type_id(key))?
            .map(|u| u as i32)
    }

    pub fn get_struct_type_id(&self, word: &StrKey) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|def| word.eq(def))
            .map(|u| u + 1)
    }

    pub fn get_struct_value_id(&self, def: &StructDef) -> Option<Value> {
        self.structs_types
            .iter()
            .position(|id| id.eq(def))
            .map(Value::from)
    }

    /// Searches for a `const` that matches the given [`StrKey`].
    pub fn get_const_by_name(&self, word: &StrKey) -> Option<&StructType> {
        self.consts.iter().find(|cnst| word.eq(cnst))
    }
}

pub trait Visitor {
    fn set_index(&mut self, i: Option<usize>);
    fn get_index(&self) -> Option<usize>;

    fn inside_proc(&self) -> bool {
        self.get_index().is_some()
    }

    fn current_proc<'a>(&self, program: &'a Program) -> Option<&'a Proc> {
        self.get_index().and_then(|i| program.procs.get(i))
    }

    fn current_proc_data<'a>(&self, program: &'a Program) -> Option<&'a Data> {
        self.current_proc(program).and_then(Proc::get_data)
    }

    fn current_proc_mut<'a>(&self, program: &'a mut Program) -> Option<&'a mut Proc> {
        self.get_index().and_then(|i| program.procs.get_mut(i))
    }

    #[track_caller]
    fn visit_proc<'a>(&mut self, program: &'a Program, index: usize) -> &'a Proc {
        self.enter_proc(index);
        &program.procs[index]
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i));
    }

    fn exit_proc(&mut self) {
        self.set_index(None);
    }
}

pub enum Fmt {
    Loc(Loc),
    Typ(TokenType),
    Tok(IRToken),
}
