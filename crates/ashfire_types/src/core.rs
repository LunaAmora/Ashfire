use std::{ops::Deref, usize};

use firelib::lexer::Loc;
use lasso::Key;
use num::FromPrimitive;

use super::{data::*, enums::*};

pub type Name = lasso::Spur;

pub fn name_from_usize(value: usize) -> Name {
    Name::try_from_usize(value).expect("Value should not be greater than `u32::MAX - 1`")
}

pub const WORD_SIZE: i32 = 4;
pub const WORD_USIZE: usize = 4;

pub fn word_aligned(value: usize) -> i32 {
    ((value as i32 + WORD_SIZE - 1) / WORD_SIZE) * WORD_SIZE
}

pub trait Typed {
    fn get_type(&self) -> TokenType;
}

impl<T: Typed> Typed for &T {
    fn get_type(&self) -> TokenType {
        (*self).get_type()
    }
}

pub trait Location {
    fn loc(&self) -> Loc;
}

impl<T: Location> Location for &T {
    fn loc(&self) -> Loc {
        (*self).loc()
    }
}

impl Location for Loc {
    fn loc(&self) -> Loc {
        *self
    }
}

pub const ANY: TokenType = TokenType::Data(ValueType(TypeId::ANY));
pub const ANY_PTR: TokenType = TokenType::Data(ValueType(TypeId::ANY_PTR));
pub const BOOL: TokenType = TokenType::Data(ValueType(TypeId::BOOL));
pub const INT: TokenType = TokenType::Data(ValueType(TypeId::INT));
pub const PTR: TokenType = TokenType::Data(ValueType(TypeId::PTR));
pub const STR: TokenType = TokenType::Data(ValueType(TypeId::STR));

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    Data(ValueType),
}

impl Typed for TokenType {
    fn get_type(&self) -> TokenType {
        *self
    }
}

impl PartialEq<ValueType> for TokenType {
    fn eq(&self, other: &ValueType) -> bool {
        match self {
            Self::Data(data) => data == other,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct IRToken(pub TokenType, pub i32, pub Loc);

impl IRToken {
    pub fn operand(&self) -> i32 {
        self.1
    }

    pub fn index(&self) -> usize {
        self.1 as usize
    }

    pub fn name(&self) -> Name {
        name_from_usize(self.index())
    }
}

impl Typed for IRToken {
    fn get_type(&self) -> TokenType {
        self.0
    }
}

impl Location for IRToken {
    fn loc(&self) -> Loc {
        self.2
    }
}

impl IRToken {
    pub fn get_keyword(&self) -> Option<KeywordType> {
        if self == TokenType::Keyword {
            FromPrimitive::from_i32(self.1)
        } else {
            None
        }
    }

    /// # Panics
    ///
    /// Will panic if the operand is not a valid `KeywordType`.
    pub fn as_keyword(&self) -> KeywordType {
        FromPrimitive::from_i32(self.1).expect("IRToken is not a valid `KeywordType`")
    }
}

impl Deref for IRToken {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl PartialEq<KeywordType> for &IRToken {
    fn eq(&self, other: &KeywordType) -> bool {
        self.get_keyword().map_or(false, |key| other == &key)
    }
}

impl PartialEq<TokenType> for &IRToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.0 == other
    }
}

impl PartialEq<TypeId> for &IRToken {
    fn eq(&self, other: &TypeId) -> bool {
        self.0 == ValueType(*other)
    }
}

#[derive(Clone)]
pub struct Op(pub OpType, pub Loc);

impl Op {
    pub fn set_index(&mut self, value: usize) {
        match &mut self.0 {
            OpType::IndexOp(_, index) | OpType::ControlOp(_, index) => *index = value,
            _ => todo!(),
        }
    }
}

impl From<(&Primitive, Loc)> for Op {
    fn from(value: (&Primitive, Loc)) -> Self {
        let (prim, loc) = value;
        Self(OpType::PushData(prim.type_id(), prim.value()), loc)
    }
}

impl<T> From<(T, Loc)> for Op
where
    OpType: From<T>,
{
    fn from(value: (T, Loc)) -> Self {
        let (from, loc) = value;
        Self(from.into(), loc)
    }
}

pub struct Wrapper<T, O>(T, O);

impl<T, O: Copy> Wrapper<T, O> {
    pub fn value(&self) -> O {
        self.1
    }
}

impl<T, O> Deref for Wrapper<T, O> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type OffsetData = Wrapper<OffsetWord, i32>;

impl OffsetData {
    pub fn new(name: Name, size: usize, offset: i32) -> Self {
        Self(OffsetWord::new(name, size), offset)
    }

    pub fn data(&self) -> (usize, i32) {
        (self.size(), self.value())
    }

    pub fn size(&self) -> usize {
        self.0.value()
    }
}

pub type OffsetWord = Wrapper<Name, usize>;

impl OffsetWord {
    pub fn new(name: Name, offset: usize) -> Self {
        Self(name, offset)
    }
}

pub type TypedWord = Wrapper<Name, TypeId>;

impl TypedWord {
    pub fn new(name: Name, type_id: TypeId) -> Self {
        Self(name, type_id)
    }
}
