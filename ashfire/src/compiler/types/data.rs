use std::ops::Deref;

use super::core::{Operand, StrKey, TokenType, Typed, WORD_USIZE};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Value {
    Int,
    Bool,
    Ptr,
    Any,
    Type(usize),
}

impl Typed for Value {
    fn get_type(&self) -> TokenType {
        TokenType::Data(Data::Typ(*self))
    }
}

impl Operand for Value {
    fn operand(&self) -> i32 {
        Data::Typ(*self).operand()
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::Int,
            1 => Self::Bool,
            2 => Self::Ptr,
            3 => Self::Any,
            i => Self::Type(i),
        }
    }
}

impl From<Value> for usize {
    fn from(value: Value) -> Self {
        match value {
            Value::Int => 0,
            Value::Bool => 1,
            Value::Ptr => 2,
            Value::Any => 3,
            Value::Type(i) => i,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Data {
    Typ(Value),
    Ptr(Value),
}

impl Data {
    pub fn get_value(self) -> Value {
        match self {
            Self::Typ(val) | Self::Ptr(val) => val,
        }
    }
}

impl Typed for Data {
    fn get_type(&self) -> TokenType {
        TokenType::Data(*self)
    }
}

impl Operand for Data {
    fn operand(&self) -> i32 {
        match self {
            Self::Typ(value) => 1 + usize::from(*value) as i32,
            Self::Ptr(value) => -(1 + usize::from(*value) as i32),
        }
    }

    fn index(&self) -> usize {
        unimplemented!()
    }
}

impl From<i32> for Data {
    fn from(value: i32) -> Self {
        match value {
            0 => unimplemented!("Not a valid value"),
            1.. => Self::Typ(Value::from((value - 1) as usize)),
            _ => Self::Ptr(Value::from((-value - 1) as usize)),
        }
    }
}

#[derive(Clone)]
pub struct ValueType {
    name: StrKey,
    value: i32,
    data_type: Data,
}

impl ValueType {
    pub fn new<T: Typed + Operand>(name: &StrKey, typed: T) -> Self {
        let TokenType::Data(data_type) =  typed.get_type() else {
            unimplemented!()
        };

        Self { name: *name, value: typed.operand(), data_type }
    }

    pub fn value(&self) -> i32 {
        self.value
    }

    pub fn data(&self) -> &Data {
        &self.data_type
    }
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        self.data_type.get_type()
    }
}

impl<T: Typed> From<(StrKey, T)> for ValueType {
    fn from(tuple: (StrKey, T)) -> Self {
        let TokenType::Data(data_type) = tuple.1.get_type() else {
            unimplemented!()
        };

        Self { name: tuple.0, value: 0, data_type }
    }
}

impl Deref for ValueType {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

#[derive(Clone)]
pub struct StructRef {
    data: StructDef,
    reftype: Value,
}

impl Deref for StructRef {
    type Target = StructDef;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl StructRef {
    pub fn new(name: &StrKey, members: Vec<StructType>, reftype: Value) -> Self {
        Self { data: StructDef::new(name, members), reftype }
    }

    pub fn get_ref_type(&self) -> Value {
        self.reftype
    }
}

impl Typed for StructRef {
    fn get_type(&self) -> TokenType {
        self.reftype.get_type()
    }
}

#[derive(Clone)]
pub struct StructDef {
    name: StrKey,
    members: Vec<StructType>,
}

impl StructDef {
    pub fn new(name: &StrKey, members: Vec<StructType>) -> Self {
        Self { name: *name, members }
    }

    pub fn members(&self) -> &[StructType] {
        &self.members
    }

    pub fn units(&self) -> Vec<&ValueType> {
        self.members.iter().flat_map(StructType::units).collect()
    }

    pub fn count(&self) -> usize {
        self.members
            .iter()
            .fold(0, |acc, member| acc + member.count())
    }

    pub fn size(&self) -> usize {
        self.members
            .iter()
            .fold(0, |acc, member| acc + member.size())
    }
}

impl Deref for StructDef {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl From<(StrKey, Value)> for StructDef {
    fn from(tuple: (StrKey, Value)) -> Self {
        Self {
            name: tuple.0,
            members: vec![StructType::Unit((StrKey::default(), tuple.1).into())],
        }
    }
}

#[derive(Clone)]
pub enum StructType {
    Root(StructRef),
    Unit(ValueType),
}

impl StructType {
    pub fn units(&self) -> Vec<&ValueType> {
        match self {
            Self::Root(s) => s.units(),
            Self::Unit(v) => vec![v],
        }
    }

    pub fn count(&self) -> usize {
        match self {
            Self::Root(s) => s.count(),
            Self::Unit(_) => 1,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Root(s) => s.size(),
            Self::Unit(_) => WORD_USIZE,
        }
    }
}

impl Deref for StructType {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Unit(val) => &val.name,
            Self::Root(stk) => &stk.name,
        }
    }
}

impl Typed for StructType {
    fn get_type(&self) -> TokenType {
        match self {
            Self::Root(stk) => stk.get_type(),
            Self::Unit(typ) => typ.get_type(),
        }
    }
}

impl From<ValueType> for StructType {
    fn from(value: ValueType) -> Self {
        Self::Unit(value)
    }
}

pub trait StructUtils {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)>;
    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)>;
}

impl StructUtils for [StructType] {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)> {
        let Some(i) = self.iter().position(|stk| word.eq(stk)) else {
            return None;
        };

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset, i))
    }

    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)> {
        let Some(i) = self.iter().position(|stk| word.eq(stk)) else {
            return None;
        };

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..=i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset - 1, i))
    }
}
