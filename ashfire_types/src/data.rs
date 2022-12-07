use std::{iter::once, ops::Deref};

use firelib::utils::BoolUtils;

use super::core::{Operand, StrKey, TokenType, Typed, WORD_USIZE};
use crate::core::IRToken;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Value(pub usize);

impl Value {
    pub const ANY: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const INT: Self = Self(2);
    pub const PTR: Self = Self(3);
    pub const STR: Self = Self(4);
}

impl Typed for Value {
    fn get_type(&self) -> TokenType {
        TokenType::Data(ValueType::Typ(*self))
    }
}

impl Operand for Value {
    fn operand(&self) -> i32 {
        ValueType::Typ(*self).operand()
    }
}

#[derive(Debug, Eq, Clone, Copy)]
pub enum ValueType {
    Typ(Value),
    Ptr(Value),
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        let ((Self::Ptr(l), Self::Ptr(r)) | (Self::Typ(l), Self::Typ(r))) = (*self, *other) else {
            return false;
        };

        l == r || (l == Value::PTR && r == Value::STR)
    }
}

impl ValueType {
    pub fn get_value(self) -> Value {
        match self {
            Self::Typ(val) | Self::Ptr(val) => val,
        }
    }
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        TokenType::Data(*self)
    }
}

impl Operand for ValueType {
    fn operand(&self) -> i32 {
        match *self {
            Self::Typ(Value(value)) => 1 + value as i32,
            Self::Ptr(Value(value)) => -(1 + value as i32),
        }
    }

    fn index(&self) -> usize {
        unimplemented!()
    }
}

impl From<i32> for ValueType {
    fn from(value: i32) -> Self {
        match value {
            0 => unimplemented!("Not a valid value"),
            1.. => Self::Typ(Value((value - 1) as usize)),
            _ => Self::Ptr(Value((-value - 1) as usize)),
        }
    }
}

#[derive(Clone)]
pub struct ValueUnit(pub StrKey, pub i32, pub ValueType);

impl ValueUnit {
    pub fn new(name: &StrKey, typed: &IRToken) -> Self {
        let TokenType::Data(value_type) = typed.get_type() else {
            unimplemented!()
        };

        Self(*name, typed.operand(), value_type)
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }

    pub fn value(&self) -> i32 {
        self.1
    }

    pub fn value_type(&self) -> &ValueType {
        &self.2
    }

    pub fn name(&self) -> &StrKey {
        &self.0
    }
}

impl Typed for ValueUnit {
    fn get_type(&self) -> TokenType {
        self.2.get_type()
    }
}

#[derive(Clone)]
pub struct StructRef(StructDef, Value);

impl Deref for StructRef {
    type Target = StructDef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StructRef {
    pub fn get_ref_type(&self) -> Value {
        self.1
    }
}

impl Typed for StructRef {
    fn get_type(&self) -> TokenType {
        self.1.get_type()
    }
}

pub trait StructInfo {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &ValueUnit> + '_>;
    fn count(&self) -> usize;
    fn size(&self) -> usize;
}

#[derive(Clone)]
pub struct StructDef(pub StrKey, pub Vec<StructType>);

impl StructDef {
    pub fn new(name: StrKey, value: Value) -> Self {
        Self(name, vec![StructType::from(ValueType::Typ(value))])
    }

    pub fn members(&self) -> &[StructType] {
        &self.1
    }

    pub fn ordered_members(self, rev: bool) -> impl Iterator<Item = StructType> {
        self.1.into_iter().conditional_rev(rev)
    }

    pub fn name(&self) -> &StrKey {
        &self.0
    }
}

impl Deref for StructDef {
    type Target = [StructType];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl StructInfo for [StructType] {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &ValueUnit> + '_> {
        Box::new(self.iter().flat_map(StructInfo::units))
    }

    fn count(&self) -> usize {
        self.iter().fold(0, |acc, member| acc + member.count())
    }

    fn size(&self) -> usize {
        self.iter().fold(0, |acc, member| acc + member.size())
    }
}

#[derive(Clone)]
pub enum StructType {
    Root(StructRef),
    Unit(ValueUnit),
}

impl StructType {
    pub fn unit(name: &StrKey, value_type: ValueType) -> Self {
        Self::Unit(ValueUnit(*name, 0, value_type))
    }

    pub fn root(name: &StrKey, members: Vec<Self>, reftype: Value) -> Self {
        Self::Root(StructRef(StructDef(*name, members), reftype))
    }

    pub fn name(&self) -> &StrKey {
        match self {
            Self::Unit(val) => val.name(),
            Self::Root(stk) => stk.name(),
        }
    }
}

impl From<ValueType> for StructType {
    fn from(value: ValueType) -> Self {
        Self::Unit(ValueUnit(StrKey::default(), 0, value))
    }
}

pub trait Transposer<T, I> {
    type Provider<'a>
    where
        I: 'a,
    = &'a mut (dyn Iterator<Item = I>);
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<T>;
}

impl<I> Transposer<Vec<StructType>, IRToken> for I
where
    I: Iterator<Item = StructType>,
{
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<StructType>> {
        self.map(|member| member.transpose(rev, provider)).collect()
    }
}

impl Transposer<Self, IRToken> for StructType {
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Self> {
        Some(match self {
            Self::Unit(unit) => Self::Unit(ValueUnit::new(unit.name(), &provider.next()?)),

            Self::Root(StructRef(str_def @ StructDef(name, ..), reftype)) => str_def
                .transpose(rev, provider)
                .map(|members| Self::root(&name, members, reftype))?,
        })
    }
}

impl Transposer<Vec<StructType>, IRToken> for StructDef {
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<StructType>> {
        self.ordered_members(rev).transpose(rev, provider)
    }
}

impl StructInfo for StructType {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &ValueUnit> + '_> {
        match self {
            Self::Root(s) => s.units(),
            Self::Unit(v) => Box::new(once(v)),
        }
    }

    fn count(&self) -> usize {
        match self {
            Self::Root(s) => s.count(),
            Self::Unit(_) => 1,
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::Root(s) => s.size(),
            Self::Unit(v) => v.size(),
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

impl From<ValueUnit> for StructType {
    fn from(value: ValueUnit) -> Self {
        Self::Unit(value)
    }
}
