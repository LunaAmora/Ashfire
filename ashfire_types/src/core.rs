use std::ops::Deref;

use firelib::lexer::Loc;
use lasso::Key;
use num::FromPrimitive;

use super::{data::*, enums::*};

pub type Name = lasso::Spur;

pub const WORD_SIZE: i32 = 4;
pub const WORD_USIZE: usize = 4;

pub fn word_aligned<O: Operand>(value: O) -> i32 {
    ((value.operand() + WORD_SIZE - 1) / WORD_SIZE) * WORD_SIZE
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

pub trait Operand {
    fn operand(&self) -> i32;

    fn index(&self) -> usize {
        self.operand() as usize
    }

    fn name(&self) -> Name {
        Name::try_from_usize(self.index()).unwrap()
    }
}

impl<T: Operand> Operand for &T {
    fn operand(&self) -> i32 {
        (*self).operand()
    }
}

impl Operand for i32 {
    fn operand(&self) -> i32 {
        *self
    }
}

impl Operand for usize {
    fn operand(&self) -> i32 {
        *self as i32
    }
}

impl Operand for Name {
    fn index(&self) -> usize {
        self.into_usize()
    }

    fn operand(&self) -> i32 {
        self.index() as i32
    }

    fn name(&self) -> Name {
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

impl PartialEq<TypeId> for TokenType {
    fn eq(&self, other: &TypeId) -> bool {
        match self {
            Self::Data(ValueType(typ)) => typ == other,
            _ => false,
        }
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

impl Typed for IRToken {
    fn get_type(&self) -> TokenType {
        self.0
    }
}

impl Operand for IRToken {
    fn operand(&self) -> i32 {
        self.1
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
        FromPrimitive::from_i32(self.1).unwrap()
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
        &self.0 == other
    }
}

#[derive(Clone)]
pub struct Op(pub OpType, pub i32, pub Loc);

impl Operand for Op {
    fn operand(&self) -> i32 {
        self.1
    }
}

impl Op {
    pub fn set_operand(&mut self, value: i32) {
        self.1 = value;
    }
}

impl From<(&Primitive, Loc)> for Op {
    fn from(value: (&Primitive, Loc)) -> Self {
        let (prim, loc) = value;
        Self(OpType::PushData(prim.type_id()), prim.value(), loc)
    }
}

impl From<(IntrinsicType, Loc)> for Op {
    fn from(value: (IntrinsicType, Loc)) -> Self {
        let (intrinsic, loc) = value;
        Self(OpType::Intrinsic, usize::from(intrinsic).operand(), loc)
    }
}

pub struct Wrapper<T, O>(T, O);

impl<T, O: Copy> Wrapper<T, O> {
    pub fn get_value(&self) -> O {
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
    pub fn new(name: Name, size: i32, offset: i32) -> Self {
        Self(OffsetWord::new(name, size), offset)
    }

    pub fn data(&self) -> (i32, i32) {
        (self.size(), self.get_value())
    }

    pub fn size(&self) -> i32 {
        self.0.get_value()
    }
}

pub type OffsetWord = Wrapper<Name, i32>;

impl OffsetWord {
    pub fn new(name: Name, offset: i32) -> Self {
        Self(name, offset)
    }
}

pub type TypedWord = Wrapper<Name, TypeId>;

impl TypedWord {
    pub fn new(name: Name, type_id: TypeId) -> Self {
        Self(name, type_id)
    }
}
