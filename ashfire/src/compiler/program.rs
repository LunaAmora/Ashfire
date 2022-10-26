use std::collections::HashMap;

use ashlib::from_i32;
use itertools::Itertools;

use super::{parser::ParseContext, types::*};

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub words: Vec<String>,
    pub consts: Vec<ValueType>,
    pub global_vars: Vec<ValueType>,
    pub structs_types: Vec<StructDef>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    mem_size: i32,
    memory: Vec<SizeWord>,
    data_size: i32,
    data: Vec<OffsetWord>,
}

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

    pub fn push_mem_by_context(
        &mut self, proc_index: Option<usize>, word: &str, size: i32,
    ) -> ParseContext {
        match proc_index.and_then(|i| self.procs.get_mut(i)) {
            Some(proc) => {
                proc.push_mem(word, size);
                ParseContext::LocalMem
            }
            None => {
                self.push_mem(word, size);
                ParseContext::GlobalMem
            }
        }
    }

    pub fn push_mem(&mut self, word: &str, size: i32) {
        self.memory.push(SizeWord(word.to_string(), self.mem_size));
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
        self.global_vars.len() as i32 * 4
    }

    pub fn stack_start(&self) -> i32 {
        self.global_vars_start() + self.global_vars_size()
    }

    pub fn get_word(&self, index: i32) -> &String {
        self.words.get(index as usize).unwrap()
    }

    pub fn get_string(&self, index: i32) -> &OffsetWord {
        self.data.get(index as usize).unwrap()
    }

    pub fn get_sorted_data(&self) -> Vec<&OffsetWord> {
        self.data
            .iter()
            .filter(|d| d.offset() >= 0)
            .sorted_by_key(|d| d.offset())
            .collect()
    }

    pub fn data_name(&self, value: Value) -> String {
        match value {
            Value::Int => "Integer",
            Value::Bool => "Boolean",
            Value::Ptr => "Pointer",
            Value::Any => "Any",
            Value::Type(n) => self.structs_types.get(n).unwrap().name(),
        }
        .to_owned()
    }

    pub fn type_name(&self, typ: TokenType) -> String {
        match typ {
            TokenType::Keyword => "Keyword",
            TokenType::Word => "Word or Intrinsic",
            TokenType::DataType(value) => return self.data_name(value),
            TokenType::DataPtr(value) => return self.data_name(value) + " Pointer",
            TokenType::Str => "String",
        }
        .to_owned()
    }

    pub fn data_display(&self, value: Value, operand: i32) -> String {
        match value {
            Value::Bool => fold_bool!(operand != 0, "True", "False").to_owned(),
            Value::Ptr => format!("*{}", operand),
            Value::Any | Value::Int | Value::Type(_) => operand.to_string(),
        }
    }

    pub fn type_display(&self, typ: TokenType, operand: i32) -> String {
        match typ {
            TokenType::Keyword => format!("{:?}", from_i32::<KeywordType>(operand)),
            TokenType::Word => self.get_word(operand).to_owned(),
            TokenType::DataType(value) | TokenType::DataPtr(value) => {
                self.data_display(value, operand)
            }
            TokenType::Str => self.get_string(operand).to_string(),
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
        self.get_data_type(std::str::from_utf8(rest).ok()?)
            .map(|u| u as i32)
    }

    pub fn get_data_type(&self, word: &str) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|s| s.name() == word)
            .map(|u| u + 1)
    }

    /// Searches for a `const` that matches the given `&str`.
    pub fn get_const_name(&self, word: &str) -> Option<&ValueType> {
        self.consts.iter().find(|cnst| word == cnst.as_str())
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

    fn current_proc_mut<'a>(&self, program: &'a mut Program) -> Option<&'a mut Proc> {
        if let Some(i) = self.get_index() {
            program.procs.get_mut(i)
        } else {
            None
        }
    }

    fn visit_proc<'a>(&mut self, program: &'a Program, index: usize) -> &'a Proc {
        self.enter_proc(index);
        program.procs.get(index).unwrap()
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i))
    }

    fn exit_proc(&mut self) {
        self.set_index(None)
    }
}
