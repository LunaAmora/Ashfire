use std::{iter::once, ops::Deref};

use firelib::utils::BoolUtils;

use crate::core::{DataToken, Name, Typed, Value, WORD_USIZE};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(usize);

impl TypeId {
    pub const ANY: Self = Self(0);
    pub const ANY_PTR: Self = Self(1);
    pub const BOOL: Self = Self(2);
    pub const INT: Self = Self(3);
    pub const PTR: Self = Self(4);
    pub const STR: Self = Self(5);
}

impl Typed for TypeId {
    fn get_type(&self) -> DataType {
        DataType(*self)
    }
}

#[derive(Debug, Eq, Clone, Copy)]
pub struct DataType(pub TypeId);

impl DataType {
    pub fn new(id: usize) -> Self {
        Self(TypeId(id))
    }

    pub fn id(&self) -> usize {
        let &Self(TypeId(id)) = self;
        id
    }
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        let (Self(l), Self(r)) = (*self, *other);

        l == r || (l == TypeId::PTR && r == TypeId::STR)
    }
}

impl Typed for DataType {
    fn get_type(&self) -> DataType {
        *self
    }
}

#[derive(Clone)]
pub struct Primitive {
    name: Name,
    value: Value,
}

impl Primitive {
    pub fn new(name: Name, DataToken(value, _): DataToken) -> Self {
        Self { name, value }
    }

    pub fn name(&self) -> Name {
        self.name
    }

    pub fn value(&self) -> i32 {
        self.value.1
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }
}

impl Typed for Primitive {
    fn get_type(&self) -> DataType {
        self.value.get_type()
    }
}

#[derive(Clone)]
pub struct PointerType {
    name: Name,
    data_type: DataType,
    ptr_type: DataType,
}

impl PointerType {
    pub fn as_primitive(&self, value: i32) -> Primitive {
        Primitive {
            name: self.name,
            value: Value(self.data_type, value),
        }
    }

    pub fn name(&self) -> Name {
        self.name
    }

    pub fn ptr_type(&self) -> DataType {
        self.ptr_type
    }

    pub fn size(&self) -> usize {
        WORD_USIZE
    }
}

impl Typed for PointerType {
    fn get_type(&self) -> DataType {
        self.data_type.get_type()
    }
}

#[derive(Clone)]
pub struct StructType(pub StructFields, pub DataType);

impl StructType {
    fn name(&self) -> Name {
        self.0.name()
    }
}

impl Typed for StructType {
    fn get_type(&self) -> DataType {
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
    pub fn ordered_members(self, rev: bool) -> impl Iterator<Item = TypeDescr> {
        self.1.into_iter().conditional_rev(rev)
    }

    pub fn name(&self) -> Name {
        self.0
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
    Primitive(Primitive),
    Structure(StructType),
    Reference(PointerType),
}

impl TypeDescr {
    pub fn primitive(name: Name, data_type: DataType) -> Self {
        Self::Primitive(Primitive { name, value: Value(data_type, 0) })
    }

    pub fn structure(name: Name, members: Vec<Self>, reftype: DataType) -> Self {
        Self::Structure(StructType(StructFields(name, members), reftype))
    }

    pub fn reference(name: Name, data_type: DataType, ptr_type: DataType) -> Self {
        Self::Reference(PointerType { name, data_type, ptr_type })
    }

    pub fn name(&self) -> Name {
        match self {
            Self::Primitive(val) => val.name,
            Self::Structure(stk) => stk.name(),
            Self::Reference(ptr) => ptr.name,
        }
    }
}

impl Typed for TypeDescr {
    fn get_type(&self) -> DataType {
        match self {
            Self::Structure(stk) => stk.get_type(),
            Self::Reference(ptr) => ptr.get_type(),
            Self::Primitive(typ) => typ.get_type(),
        }
    }
}

pub trait Transposer<T, I> {
    type Provider<'i>
    where
        I: 'i,
    = &'i mut (dyn Iterator<Item = I>);
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<T>;
}

impl<I> Transposer<Vec<TypeDescr>, DataToken> for I
where
    I: Iterator<Item = TypeDescr>,
{
    fn transpose(self, rev: bool, provider: Self::Provider<'_>) -> Option<Vec<TypeDescr>> {
        self.map(|descr| {
            Some(match descr {
                TypeDescr::Primitive(prim) => {
                    TypeDescr::Primitive(Primitive::new(prim.name, provider.next()?))
                }

                TypeDescr::Reference(ptr) => {
                    let DataToken(Value(_, operand), _) = provider.next()?;
                    TypeDescr::Primitive(ptr.as_primitive(operand))
                }

                TypeDescr::Structure(StructType(field @ StructFields(name, _), reftype)) => field
                    .transpose(rev, provider)
                    .map(|members| TypeDescr::structure(name, members, reftype))?,
            })
        })
        .collect()
    }
}

impl Transposer<Vec<TypeDescr>, DataToken> for StructFields {
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
