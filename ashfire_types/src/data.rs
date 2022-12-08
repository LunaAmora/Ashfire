use std::{iter::once, ops::Deref};

use firelib::utils::BoolUtils;

use super::core::{Operand, StrKey as Name, TokenType, Typed, WORD_USIZE};
use crate::core::IRToken;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(pub usize);

impl TypeId {
    pub const ANY: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const INT: Self = Self(2);
    pub const PTR: Self = Self(3);
    pub const STR: Self = Self(4);
}

impl Typed for TypeId {
    fn get_type(&self) -> TokenType {
        TokenType::Data(ValueType::Typ(*self))
    }
}

impl Operand for TypeId {
    fn operand(&self) -> i32 {
        self.0.operand()
    }
}

#[derive(Debug, Eq, Clone, Copy)]
pub enum ValueType {
    Typ(TypeId),
    Ptr(TypeId),
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        let ((Self::Ptr(l), Self::Ptr(r)) | (Self::Typ(l), Self::Typ(r))) = (*self, *other) else {
            return false;
        };

        l == r || (l == TypeId::PTR && r == TypeId::STR)
    }
}

impl ValueType {
    pub fn get_value(self) -> TypeId {
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

// impl Operand for ValueType {
//     fn operand(&self) -> i32 {
//         match *self {
//             Self::Typ(Value(value)) => 1 + value as i32,
//             Self::Ptr(Value(value)) => -(1 + value as i32),
//         }
//     }

//     fn index(&self) -> usize {
//         unimplemented!()
//     }
// }

// impl From<i32> for ValueType {
//     fn from(value: i32) -> Self {
//         match value {
//             0 => unimplemented!("Not a valid value"),
//             1.. => Self::Typ(Value((value - 1) as usize)),
//             _ => Self::Ptr(Value((-value - 1) as usize)),
//         }
//     }
// }

#[derive(Clone)]
pub struct Primitive(pub Name, pub i32, pub TypeId);

impl Primitive {
    pub fn new(name: &Name, typed: &IRToken) -> Self {
        let TokenType::Data(value_type) = typed.get_type() else {
            unimplemented!()
        };

        Self(*name, typed.operand(), value_type.get_value())
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }

    pub fn value(&self) -> i32 {
        self.1
    }

    pub fn type_id(&self) -> &TypeId {
        &self.2
    }

    pub fn name(&self) -> &Name {
        &self.0
    }
}

impl Typed for Primitive {
    fn get_type(&self) -> TokenType {
        self.2.get_type()
    }
}

#[derive(Clone)]
pub struct StructType(StructField, TypeId);

impl Deref for StructType {
    type Target = StructField;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StructType {
    pub fn get_ref_type(&self) -> TypeId {
        self.1
    }
}

impl Typed for StructType {
    fn get_type(&self) -> TokenType {
        self.1.get_type()
    }
}

pub trait StructInfo {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &Primitive> + '_>;
    fn count(&self) -> usize;
    fn size(&self) -> usize;
}

#[derive(Clone)]
pub struct StructField(pub Name, pub Vec<TypeDescr>);

impl StructField {
    pub fn new(name: Name, value: TypeId) -> Self {
        Self(name, vec![TypeDescr::from(value)])
    }

    pub fn members(&self) -> &[TypeDescr] {
        &self.1
    }

    pub fn ordered_members(self, rev: bool) -> impl Iterator<Item = TypeDescr> {
        self.1.into_iter().conditional_rev(rev)
    }

    pub fn name(&self) -> &Name {
        &self.0
    }
}

impl Deref for StructField {
    type Target = [TypeDescr];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl StructInfo for [TypeDescr] {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &Primitive> + '_> {
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
pub enum TypeDescr {
    Structure(StructType),
    Primitive(Primitive),
}

impl TypeDescr {
    pub fn unit(name: &Name, value_type: TypeId) -> Self {
        Self::Primitive(Primitive(*name, 0, value_type))
    }

    pub fn root(name: &Name, members: Vec<Self>, reftype: TypeId) -> Self {
        Self::Structure(StructType(StructField(*name, members), reftype))
    }

    pub fn name(&self) -> &Name {
        match self {
            Self::Primitive(val) => val.name(),
            Self::Structure(stk) => stk.name(),
        }
    }
}

impl From<TypeId> for TypeDescr {
    fn from(value: TypeId) -> Self {
        Self::Primitive(Primitive(Name::default(), 0, value))
    }
}

pub trait Transposer<T, I> {
    type Provider<'a>
    where
        I: 'a,
    = &'a mut (dyn Iterator<Item = I>);
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<T>;
}

impl<I> Transposer<Vec<TypeDescr>, IRToken> for I
where
    I: Iterator<Item = TypeDescr>,
{
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<TypeDescr>> {
        self.map(|member| member.transpose(rev, provider)).collect()
    }
}

impl Transposer<Self, IRToken> for TypeDescr {
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Self> {
        Some(match self {
            Self::Primitive(unit) => {
                Self::Primitive(Primitive::new(unit.name(), &provider.next()?))
            }

            Self::Structure(StructType(str_def @ StructField(name, ..), reftype)) => str_def
                .transpose(rev, provider)
                .map(|members| Self::root(&name, members, reftype))?,
        })
    }
}

impl Transposer<Vec<TypeDescr>, IRToken> for StructField {
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<TypeDescr>> {
        self.ordered_members(rev).transpose(rev, provider)
    }
}

impl StructInfo for TypeDescr {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = &Primitive> + '_> {
        match self {
            Self::Structure(s) => s.units(),
            Self::Primitive(v) => Box::new(once(v)),
        }
    }

    fn count(&self) -> usize {
        match self {
            Self::Structure(s) => s.count(),
            Self::Primitive(_) => 1,
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::Structure(s) => s.size(),
            Self::Primitive(v) => v.size(),
        }
    }
}

impl Typed for TypeDescr {
    fn get_type(&self) -> TokenType {
        match self {
            Self::Structure(stk) => stk.get_type(),
            Self::Primitive(typ) => typ.get_type(),
        }
    }
}

impl From<Primitive> for TypeDescr {
    fn from(value: Primitive) -> Self {
        Self::Primitive(value)
    }
}
