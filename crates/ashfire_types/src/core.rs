use std::ops::Deref;

use firelib::{lexer::Loc, span::Spanned};
use lasso::Key;

use super::{data::*, enums::*};

pub type Name = lasso::Spur;

pub fn name_from_usize(value: usize) -> Name {
    Name::try_from_usize(value).expect("Value should not be greater than `u32::MAX - 1`")
}

pub const WORD_SIZE: i32 = 4;
pub const WORD_USIZE: u16 = 4;

pub fn word_aligned(value: u16) -> u16 {
    value.div_ceil(WORD_USIZE) * WORD_USIZE
}

pub trait Typed {
    fn get_type(&self) -> DataType;
}

impl<T: Typed> Typed for &T {
    fn get_type(&self) -> DataType {
        (*self).get_type()
    }
}

impl Typed for DataType {
    fn get_type(&self) -> DataType {
        *self
    }
}

impl<T: Typed> Typed for Spanned<T> {
    fn get_type(&self) -> DataType {
        self.0.get_type()
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

impl<T> Location for Spanned<T> {
    fn loc(&self) -> Loc {
        self.1
    }
}

pub const ANY: DataType = DataType(TypeId::ANY);
pub const ANY_PTR: DataType = DataType(TypeId::ANY_PTR);
pub const BOOL: DataType = DataType(TypeId::BOOL);
pub const INT: DataType = DataType(TypeId::INT);
pub const PTR: DataType = DataType(TypeId::PTR);
pub const STR: DataType = DataType(TypeId::STR);

#[derive(Debug, Clone, Copy)]
pub struct Value(pub DataType, pub i32);

impl Value {
    pub fn value(&self) -> i32 {
        self.1
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Typed for Value {
    fn get_type(&self) -> DataType {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DataKey(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Keyword(KeywordType),
    Word(Name),
    Str(DataKey),
    Data(Value),
    Type(DataType),
}

impl From<Name> for TokenType {
    fn from(name: Name) -> Self {
        Self::Word(name)
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Type(l0) | Self::Data(Value(l0, _)),
                Self::Type(r0) | Self::Data(Value(r0, _)),
            ) => l0 == r0,
            _ => false,
        }
    }
}

impl PartialEq<DataType> for TokenType {
    fn eq(&self, other: &DataType) -> bool {
        match self {
            Self::Type(data_type) => data_type == other,
            _ => false,
        }
    }
}

pub type IRToken = Spanned<TokenType>;

#[ext(IRTokenExt)]
impl IRToken {
    pub fn data(id: TypeId, value: i32, loc: Loc) -> Self {
        (TokenType::Data(Value(DataType(id), value)), loc)
    }

    pub fn get_word(&self) -> Option<Name> {
        match self.0 {
            TokenType::Word(name) => Some(name),
            _ => None,
        }
    }

    pub fn get_data(&self, data_type: DataType) -> Option<i32> {
        match self.0 {
            TokenType::Data(Value(typ, value)) if typ == data_type => Some(value),
            _ => None,
        }
    }

    pub fn token_type(&self) -> TokenType {
        self.0
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

impl PartialEq<KeywordType> for &IRToken {
    fn eq(&self, other: &KeywordType) -> bool {
        self.get_keyword().is_some_and(|key| other == &key)
    }
}

impl PartialEq<TokenType> for &IRToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.0 == other
    }
}

impl PartialEq<DataType> for &IRToken {
    fn eq(&self, other: &DataType) -> bool {
        match self.0 {
            TokenType::Type(id) | TokenType::Data(Value(id, _)) => id == *other,
            _ => false,
        }
    }
}

pub type DataToken = Spanned<Value>;

#[ext(DataTokenExt)]
impl DataToken {
    pub fn new(id: DataType, value: i32, loc: Loc) -> Self {
        (Value(id, value), loc)
    }

    pub fn value(&self) -> i32 {
        self.0.value()
    }

    pub fn add(self, (Value(_, r_value), _): Self, loc: Loc) -> Self {
        let (Value(id, value), _) = self;
        (Value(id, value + r_value), loc)
    }

    pub fn sub(self, (Value(_, r_value), _): Self, loc: Loc) -> Self {
        let (Value(id, value), _) = self;
        (Value(id, value - r_value), loc)
    }
}

impl PartialEq<DataType> for DataToken {
    fn eq(&self, other: &DataType) -> bool {
        self.get_type() == *other
    }
}

pub type Op = Spanned<OpType>;

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

pub type OffsetData = Wrapper<OffsetWord, u16>;

impl OffsetData {
    pub fn new(name: Name, size: u16, offset: u16) -> Self {
        Self(OffsetWord::new(name, size), offset)
    }

    pub fn data(&self) -> (u16, u16) {
        (self.size(), self.value())
    }

    pub fn size(&self) -> u16 {
        self.0.value()
    }
}

pub type OffsetWord = Wrapper<Name, u16>;

impl OffsetWord {
    pub fn new(name: Name, offset: u16) -> Self {
        Self(name, offset)
    }
}

pub type TypedWord = Wrapper<Name, DataType>;

impl TypedWord {
    pub fn new(name: Name, data_type: DataType) -> Self {
        Self(name, data_type)
    }
}
