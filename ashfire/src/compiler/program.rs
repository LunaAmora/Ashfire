use std::collections::HashMap;

use ashlib::from_i32;
use firelib::fold_bool;
use itertools::Itertools;

use super::{parser::ParseContext, types::*};

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub words: Vec<String>,
    pub consts: Vec<TypedWord>,
    pub global_vars: Vec<TypedWord>,
    pub structs_types: Vec<StructType>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    mem_size: i32,
    memory: Vec<Word>,
    data_size: i32,
    data: Vec<SizedWord>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            structs_types: vec![
                ("int", ValueType::Int).into(),
                ("bool", ValueType::Bool).into(),
                ("ptr", ValueType::Ptr).into(),
                ("any", ValueType::Any).into(),
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
        self.memory.push(Word::new(word, self.mem_size));
        self.mem_size += size;
    }

    pub fn push_data(&mut self, word: &str, size: usize) -> usize {
        self.data.push(SizedWord::new(Word::new(word, size as i32)));
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

    pub fn get_string(&self, index: i32) -> &SizedWord {
        self.data.get(index as usize).unwrap()
    }

    pub fn get_sorted_data(&self) -> Vec<&SizedWord> {
        self.data
            .iter()
            .filter(|d| d.offset() >= 0)
            .sorted_by_key(|d| d.offset())
            .collect()
    }

    pub fn data_name(&self, value: ValueType) -> String {
        match value {
            ValueType::Int => "Integer",
            ValueType::Bool => "Boolean",
            ValueType::Ptr => "Pointer",
            ValueType::Any => "Any",
            ValueType::Type(n) => self.structs_types.get(n).unwrap().name(),
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

    pub fn data_display(&self, value: ValueType, operand: i32) -> String {
        match value {
            ValueType::Bool => fold_bool!(operand != 0, "True", "False").to_owned(),
            ValueType::Ptr => format!("*{}", operand),
            ValueType::Any | ValueType::Int | ValueType::Type(_) => operand.to_string(),
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
            _ => IntrinsicType::Cast(self.get_cast_type(word.strip_prefix('#')?)?),
        })
    }

    pub fn get_cast_type(&self, word: &str) -> Option<i32> {
        let (word, type_or_pointer) = match word.strip_prefix('*') {
            None => (word, 1),
            Some(word) => (word, -1),
        };
        self.get_data_type(word).map(|u| type_or_pointer * u as i32)
    }

    pub fn get_data_type(&self, word: &str) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|s| s.name() == word)
            .map(|u| u + 1)
    }

    /// Searches for a `const` that matches the given `&str`.
    pub fn get_const_name(&self, word: &str) -> Option<&TypedWord> {
        self.consts.iter().find(|cnst| word == cnst.as_str())
    }

    pub fn get_memory(&self) -> &[Word] {
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
