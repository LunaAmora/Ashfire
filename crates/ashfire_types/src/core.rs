use std::{ops::Deref, usize};

use firelib::lexer::Loc;
use lasso::Key;

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

pub const ANY: TokenType = TokenType::Type(ValueType(TypeId::ANY));
pub const ANY_PTR: TokenType = TokenType::Type(ValueType(TypeId::ANY_PTR));
pub const BOOL: TokenType = TokenType::Type(ValueType(TypeId::BOOL));
pub const INT: TokenType = TokenType::Type(ValueType(TypeId::INT));
pub const PTR: TokenType = TokenType::Type(ValueType(TypeId::PTR));
pub const STR: TokenType = TokenType::Type(ValueType(TypeId::STR));

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Keyword(KeywordType),
    Word(Name),
    Str(usize),
    Type(ValueType),
    Data(ValueType, i32),
}

impl Typed for TokenType {
    fn get_type(&self) -> TokenType {
        *self
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(l0) | Self::Data(l0, _), Self::Type(r0) | Self::Data(r0, _)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct IRToken(pub TokenType, pub Loc);

impl IRToken {
    pub fn data(id: TypeId, value: i32, loc: Loc) -> Self {
        Self(TokenType::Data(ValueType(id), value), loc)
    }

    pub fn get_word(&self) -> Option<Name> {
        match self.0 {
            TokenType::Word(name) => Some(name),
            _ => None,
        }
    }

    pub fn get_data(&self) -> Option<i32> {
        match self.0 {
            TokenType::Data(_, value) => Some(value),
            _ => None,
        }
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self.0, TokenType::Keyword(_))
    }

    pub fn get_keyword(&self) -> Option<KeywordType> {
        match self.0 {
            TokenType::Keyword(key) => Some(key),
            _ => None,
        }
    }

    /// # Panics
    ///
    /// Will panic if the operand is not a valid `KeywordType`.
    pub fn as_keyword(&self) -> KeywordType {
        self.get_keyword()
            .expect("IRToken is not a valid `KeywordType`")
    }
}

impl Typed for IRToken {
    fn get_type(&self) -> TokenType {
        self.0
    }
}

impl Location for IRToken {
    fn loc(&self) -> Loc {
        self.1
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
        match self.0 {
            TokenType::Type(id) | TokenType::Data(id, _) => id == ValueType(*other),
            _ => false,
        }
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
