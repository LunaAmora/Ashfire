use std::collections::HashMap;

use ashlib::from_i32;
use firelib::{lazy, lexer::Loc};
use itertools::Itertools;

use super::types::*;

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub words: Vec<String>,
    pub consts: Vec<StructType>,
    pub global_vars: Vec<StructType>,
    pub structs_types: Vec<StructDef>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    pub included_files: Vec<String>,
    mem_size: i32,
    memory: Vec<SizeWord>,
    data_size: i32,
    data: Vec<OffsetWord>,
}

pub type OptionErr<T, E = Fmt> = ashlib::OptionErr<T, E>;
pub type LazyResult<T, E = Fmt> = lazy::LazyResult<T, E>;
pub type LazyError<E = Fmt> = lazy::LazyError<E>;

impl Program {
    pub fn new() -> Self {
        Self {
            structs_types: vec![
                ("int", Value::Int).into(),
                ("bool", Value::Bool).into(),
                ("ptr", Value::Ptr).into(),
                ("any", Value::Any).into(),
            ],
            ..Default::default()
        }
    }

    pub fn register_string(&mut self, operand: i32) -> i32 {
        if let Some(data) = self.data.get_mut(operand as usize) {
            if data.offset() < 0 {
                data.set_offset(self.data_size);
                self.data_size += data.size();
            }
        }
        operand
    }

    pub fn push_mem(&mut self, word: &str, size: i32) {
        self.memory.push(SizeWord::new(word, self.mem_size));
        self.mem_size += size;
    }

    pub fn push_data(&mut self, word: &str, size: usize) -> usize {
        self.data.push(OffsetWord::new(word, size as i32));
        self.data.len() - 1
    }

    pub fn data_size(&self) -> i32 {
        self.data_size
    }

    pub fn mem_start(&self) -> i32 {
        ((self.data_size + 3) / 4) * 4
    }

    pub fn global_vars_start(&self) -> i32 {
        self.mem_start() + self.mem_size
    }

    pub fn global_vars_size(&self) -> i32 {
        self.global_vars.iter().fold(0, |acc, var| acc + var.size()) as i32
    }

    pub fn stack_start(&self) -> i32 {
        self.global_vars_start() + self.global_vars_size()
    }

    pub fn get_word(&self, index: i32) -> &String {
        &self.words[index as usize]
    }

    pub fn get_string(&self, index: i32) -> &OffsetWord {
        &self.data[index as usize]
    }

    pub fn get_sorted_data(&self) -> Vec<&OffsetWord> {
        self.data
            .iter()
            .filter(|d| d.offset() >= 0)
            .sorted_by_key(|d| d.offset())
            .collect()
    }

    fn data_name(&self, value: Value) -> String {
        match value {
            Value::Int => "Integer",
            Value::Bool => "Boolean",
            Value::Ptr => "Pointer",
            Value::Any => "Any",
            Value::Type(n) => self.structs_types[n].name(),
        }
        .to_owned()
    }

    fn type_name(&self, typ: TokenType) -> String {
        match typ {
            TokenType::Keyword => "Keyword",
            TokenType::Word => "Word or Intrinsic",
            TokenType::DataType(value) => return self.data_name(value),
            TokenType::DataPtr(value) => return self.data_name(value) + " Pointer",
            TokenType::Str => "String",
        }
        .to_owned()
    }

    fn data_display(&self, value: Value, operand: i32) -> String {
        match value {
            Value::Bool => fold_bool!(operand != 0, "True", "False").to_owned(),
            Value::Ptr => format!("*{}", operand),
            Value::Any | Value::Int | Value::Type(_) => operand.to_string(),
        }
    }

    fn type_display(&self, tok: IRToken) -> String {
        match tok.token_type {
            TokenType::Keyword => format!("{:?}", from_i32::<KeywordType>(tok.operand)),
            TokenType::Word => self.get_word(tok.operand).to_owned(),
            TokenType::DataType(value) | TokenType::DataPtr(value) => {
                self.data_display(value, tok.operand)
            }
            TokenType::Str => self.get_string(tok.operand).to_string(),
        }
    }

    pub fn loc_fmt<L: Location>(&self, loc: L) -> String {
        let loc = loc.loc();
        self.included_files
            .get(loc.file_index)
            .map_or_else(String::new, |l| format!("{l}:{}:{} ", loc.line, loc.col))
    }

    pub fn format(&self, fmt: Fmt) -> String {
        match fmt {
            Fmt::Loc(loc) => self.loc_fmt(loc),
            Fmt::Tok(tok) => self.type_display(tok),
            Fmt::Typ(typ) => self.type_name(typ),
        }
    }

    pub fn get_intrinsic_type(&self, word: &str) -> Option<IntrinsicType> {
        Some(match word {
            "+" => IntrinsicType::Plus,
            "-" => IntrinsicType::Minus,
            "*" => IntrinsicType::Times,
            "%" => IntrinsicType::Div,
            ">" => IntrinsicType::Greater,
            ">=" => IntrinsicType::GreaterE,
            "<" => IntrinsicType::Lesser,
            "<=" => IntrinsicType::LesserE,
            "or" => IntrinsicType::Or,
            "and" => IntrinsicType::And,
            "xor" => IntrinsicType::Xor,
            "@8" => IntrinsicType::Load8,
            "!8" => IntrinsicType::Store8,
            "@16" => IntrinsicType::Load16,
            "!16" => IntrinsicType::Store16,
            "@32" => IntrinsicType::Load32,
            "!32" => IntrinsicType::Store32,
            "fd_write" => IntrinsicType::FdWrite,
            _ => match word.as_bytes() {
                [b'#', b'*', rest @ ..] => IntrinsicType::Cast(-self.get_cast_type(rest)?),
                [b'#', rest @ ..] => IntrinsicType::Cast(self.get_cast_type(rest)?),
                _ => return None,
            },
        })
    }

    fn get_cast_type(&self, rest: &[u8]) -> Option<i32> {
        self.get_data_type_id(std::str::from_utf8(rest).ok()?)
            .map(|u| u as i32)
    }

    pub fn get_data_type_id(&self, word: &str) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|s| s.name() == word)
            .map(|u| u + 1)
    }

    pub fn get_struct_value_id(&self, def: &StructDef) -> Option<Value> {
        self.structs_types
            .iter()
            .position(|s| s.name() == def.name())
            .map(|u| Value::from(u))
    }

    /// Searches for a `const` that matches the given `&str`.
    pub fn get_const_by_name(&self, word: &str) -> Option<&StructType> {
        self.consts.iter().find(|cnst| word == cnst.name())
    }

    pub fn get_memory(&self) -> &[SizeWord] {
        &self.memory
    }
}

pub trait ProgramVisitor {
    fn set_index(&mut self, i: Option<usize>);
    fn get_index(&self) -> Option<usize>;

    fn inside_proc(&self) -> bool {
        self.get_index().is_some()
    }

    fn current_proc<'a>(&self, program: &'a Program) -> Option<&'a Proc> {
        if let Some(i) = self.get_index() {
            program.procs.get(i)
        } else {
            None
        }
    }

    fn current_proc_data<'a>(&self, program: &'a Program) -> Option<&'a ProcData> {
        self.current_proc(program).and_then(Proc::get_data)
    }

    fn current_proc_mut<'a>(&self, program: &'a mut Program) -> Option<&'a mut Proc> {
        if let Some(i) = self.get_index() {
            program.procs.get_mut(i)
        } else {
            None
        }
    }

    #[track_caller]
    fn visit_proc<'a>(&mut self, program: &'a Program, index: usize) -> &'a Proc {
        self.enter_proc(index);
        &program.procs[index]
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i))
    }

    fn exit_proc(&mut self) {
        self.set_index(None)
    }
}

pub enum Fmt {
    Loc(Loc),
    Typ(TokenType),
    Tok(IRToken),
}
