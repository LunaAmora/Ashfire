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
pub struct ValueUnit {
    name: StrKey,
    value: i32,
    value_type: ValueType,
}

impl ValueUnit {
    pub fn new<T: Typed + Operand>(name: &StrKey, typed: T) -> Self {
        let TokenType::Data(value_type) = typed.get_type() else {
            unimplemented!()
        };

        Self { name: *name, value: typed.operand(), value_type }
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }

    pub fn value(&self) -> i32 {
        self.value
    }

    pub fn value_type(&self) -> &ValueType {
        &self.value_type
    }
}

impl Typed for ValueUnit {
    fn get_type(&self) -> TokenType {
        self.value_type.get_type()
    }
}

impl Deref for ValueUnit {
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
    pub fn get_ref_type(&self) -> Value {
        self.reftype
    }
}

impl Typed for StructRef {
    fn get_type(&self) -> TokenType {
        self.reftype.get_type()
    }
}

pub trait StructInfo {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &ValueUnit> + '_>;
    fn count(&self) -> usize;
    fn size(&self) -> usize;
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

    pub fn ordered_members(self, rev: bool) -> impl Iterator<Item = StructType> {
        self.members.into_iter().conditional_rev(rev)
    }
}

impl StructInfo for StructDef {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &ValueUnit> + '_> {
        self.members.units()
    }

    fn count(&self) -> usize {
        self.members.count()
    }

    fn size(&self) -> usize {
        self.members.size()
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

impl Deref for StructDef {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl From<(StrKey, Value)> for StructDef {
    fn from(tuple: (StrKey, Value)) -> Self {
        let (name, value_type) = (tuple.0, ValueType::Typ(tuple.1));
        let members = vec![StructType::unit(&StrKey::default(), value_type)];

        Self { name, members }
    }
}

#[derive(Clone)]
pub enum StructType {
    Root(StructRef),
    Unit(ValueUnit),
}

impl StructType {
    pub fn unit(name: &StrKey, value_type: ValueType) -> Self {
        Self::Unit(ValueUnit { name: *name, value: 0, value_type })
    }

    pub fn root(name: &StrKey, members: Vec<Self>, reftype: Value) -> Self {
        Self::Root(StructRef { data: StructDef::new(name, members), reftype })
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
            Self::Unit(unit) => Self::Unit(ValueUnit::new(&unit, provider.next()?)),

            Self::Root(StructRef { data: str_def @ StructDef { name, .. }, reftype }) => str_def
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

impl From<ValueUnit> for StructType {
    fn from(value: ValueUnit) -> Self {
        Self::Unit(value)
    }
}
