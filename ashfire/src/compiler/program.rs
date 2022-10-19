use std::collections::HashMap;

use ashlib::from_i32;
use firelib::fold_bool;

use super::types::{KeywordType, Op, Proc, SizedWord, StructType, TokenType, TypedWord, ValueType};

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub words: Vec<String>,
    pub data: Vec<SizedWord>,
    pub mem_size: i32,
    pub data_size: i32,
    pub global_vars: Vec<TypedWord>,
    pub structs_types: Vec<StructType>,
    pub procs: Vec<Proc>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
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

    pub fn final_data_size(&self) -> i32 {
        ((self.data_size + 3) / 4) * 4
    }

    pub fn total_vars_size(&self) -> i32 {
        self.global_vars.len() as i32 * 4
    }

    pub fn get_word(&self, index: i32) -> &String {
        self.words.get(index as usize).unwrap()
    }

    pub fn get_string(&self, index: i32) -> &SizedWord {
        self.data.get(index as usize).unwrap()
    }

    pub fn data_name(&self, value: ValueType) -> String {
        match value {
            ValueType::Int => "Integer",
            ValueType::Bool => "Boolean",
            ValueType::Ptr => "Pointer",
            ValueType::Any => "Any",
            ValueType::Type(n) => {
                return self.structs_types.get(n as usize).unwrap().name.to_owned()
            }
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
