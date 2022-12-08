use std::{iter::once, ops::Deref};

use firelib::utils::BoolUtils;

use super::core::{Operand, StrKey as Name, TokenType, Typed, WORD_USIZE};
use crate::core::IRToken;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(pub usize);

impl TypeId {
    pub const ANY: Self = Self(0);
    pub const ANY_PTR: Self = Self(1);
    pub const BOOL: Self = Self(2);
    pub const INT: Self = Self(3);
    pub const PTR: Self = Self(4);
    pub const STR: Self = Self(5);
}

impl Typed for TypeId {
    fn get_type(&self) -> TokenType {
        TokenType::Data(ValueType(*self))
    }
}

#[derive(Debug, Eq, Clone, Copy)]
pub struct ValueType(pub TypeId);

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        let (Self(l), Self(r)) = (*self, *other);

        l == r || (l == TypeId::PTR && r == TypeId::STR)
    }
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        TokenType::Data(*self)
    }
}

#[derive(Clone)]
pub struct Primitive(pub Name, pub i32, pub TypeId);

impl Primitive {
    pub fn new(name: &Name, typed: &IRToken) -> Self {
        let TokenType::Data(ValueType(id)) = typed.get_type() else {
            unimplemented!()
        };

        Self(*name, typed.operand(), id)
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
pub struct PointerType(pub Name, pub TypeId, pub Box<TypeDescr>);

impl PointerType {
    pub fn as_primitive(&self, value: i32) -> Primitive {
        Primitive(self.0, value, self.1)
    }

    pub fn name(&self) -> &Name {
        &self.0
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }
}

impl Typed for PointerType {
    fn get_type(&self) -> TokenType {
        self.1.get_type()
    }
}

#[derive(Clone)]
pub struct StructType(pub StructFields, pub TypeId);

impl StructType {
    fn name(&self) -> &Name {
        self.0.name()
    }
}

impl Typed for StructType {
    fn get_type(&self) -> TokenType {
        self.1.get_type()
    }
}

pub trait StructInfo {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = Primitive> + '_>;
    fn count(&self) -> usize;
    fn size(&self) -> usize;
}

#[derive(Clone)]
pub struct StructFields(pub Name, pub Vec<TypeDescr>);

impl StructFields {
    pub fn new(name: Name, value: TypeId) -> Self {
        Self(name, vec![TypeDescr::from(value)])
    }

    pub fn ordered_members(self, rev: bool) -> impl Iterator<Item = TypeDescr> {
        self.1.into_iter().conditional_rev(rev)
    }

    pub fn name(&self) -> &Name {
        &self.0
    }
}

impl Deref for StructFields {
    type Target = [TypeDescr];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl StructInfo for [TypeDescr] {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = Primitive> + '_> {
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
    Reference(PointerType),
}

impl TypeDescr {
    pub fn primitive(name: Name, value_type: TypeId) -> Self {
        Self::Primitive(Primitive(name, 0, value_type))
    }

    pub fn structure(name: Name, members: Vec<Self>, reftype: TypeId) -> Self {
        Self::Structure(StructType(StructFields(name, members), reftype))
    }

    pub fn members(&self) -> Option<&[Self]> {
        match self {
            Self::Structure(StructType(fields, _)) => Some(fields),
            _ => None,
        }
    }

    pub fn name(&self) -> &Name {
        match self {
            Self::Primitive(val) => val.name(),
            Self::Structure(stk) => stk.name(),
            Self::Reference(ptr) => ptr.name(),
        }
    }

    pub fn type_id(&self) -> TypeId {
        match self {
            Self::Structure(stk) => stk.1,
            Self::Reference(ptr) => ptr.1,
            Self::Primitive(typ) => typ.2,
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
        self.map(|descr| {
            Some(match descr {
                TypeDescr::Primitive(unit) => {
                    TypeDescr::Primitive(Primitive::new(unit.name(), &provider.next()?))
                }

                TypeDescr::Reference(ptr) => {
                    TypeDescr::Primitive(ptr.as_primitive(provider.next()?.operand()))
                }

                TypeDescr::Structure(StructType(field @ StructFields(name, _), reftype)) => field
                    .transpose(rev, provider)
                    .map(|members| TypeDescr::structure(name, members, reftype))?,
            })
        })
        .collect()
    }
}

impl Transposer<Vec<TypeDescr>, IRToken> for StructFields {
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<TypeDescr>> {
        self.ordered_members(rev).transpose(rev, provider)
    }
}

impl StructInfo for TypeDescr {
    fn units(&self) -> Box<dyn DoubleEndedIterator<Item = Primitive> + '_> {
        match self {
            Self::Structure(StructType(fields, _)) => fields.units(),
            Self::Primitive(prim) => Box::new(once(prim.clone())),
            Self::Reference(ptr) => Box::new(once(ptr.as_primitive(0))),
        }
    }

    fn count(&self) -> usize {
        match self {
            Self::Structure(StructType(fields, _)) => fields.count(),
            _ => 1,
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::Structure(StructType(fields, _)) => fields.size(),
            Self::Primitive(v) => v.size(),
            Self::Reference(_) => WORD_USIZE,
        }
    }
}

impl From<Primitive> for TypeDescr {
    fn from(value: Primitive) -> Self {
        Self::Primitive(value)
    }
}
