use std::ops::Deref;

use firelib::lexer::Loc;
use lasso::Key;
use num::FromPrimitive;

use super::{data::*, enums::*};

pub type StrKey = lasso::Spur;

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

    fn str_key(&self) -> StrKey {
        StrKey::try_from_usize(self.index()).unwrap()
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

impl Operand for StrKey {
    fn index(&self) -> usize {
        self.into_usize()
    }

    fn operand(&self) -> i32 {
        self.index() as i32
    }

    fn str_key(&self) -> StrKey {
        *self
    }
}

pub const ANY: TokenType = TokenType::Data(ValueType::Typ(Value::ANY));
pub const BOOL: TokenType = TokenType::Data(ValueType::Typ(Value::BOOL));
pub const INT: TokenType = TokenType::Data(ValueType::Typ(Value::INT));
pub const PTR: TokenType = TokenType::Data(ValueType::Typ(Value::PTR));
pub const STR: TokenType = TokenType::Data(ValueType::Typ(Value::STR));

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

impl PartialEq<Value> for TokenType {
    fn eq(&self, other: &Value) -> bool {
        match self {
            Self::Data(ValueType::Typ(typ)) => typ == other,
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
        self.0 == TokenType::Keyword && other == &self.as_keyword()
    }
}

impl PartialEq<TokenType> for &IRToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.0 == other
    }
}

impl PartialEq<Value> for &IRToken {
    fn eq(&self, other: &Value) -> bool {
        &self.0 == other
    }
}

impl From<(&StructType, Loc)> for IRToken {
    fn from(value: (&StructType, Loc)) -> Self {
        match value {
            (StructType::Root(_), _) => todo!("Support const use on other consts"),
            (StructType::Unit(u), loc) => Self(u.value_type().get_type(), u.value(), loc),
        }
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

impl From<(&ValueUnit, Loc)> for Op {
    fn from(tuple: (&ValueUnit, Loc)) -> Self {
        let ValueType::Typ(typ) = tuple.0.value_type() else {
            unimplemented!("Conversion not supported for `ValueType::Ptr`")
        };

        Self(OpType::PushData(*typ), tuple.0.value(), tuple.1)
    }
}

impl From<(IntrinsicType, Loc)> for Op {
    fn from(value: (IntrinsicType, Loc)) -> Self {
        Self(OpType::Intrinsic, i32::from(value.0), value.1)
    }
}

pub struct Offset<T, O = i32>(pub T, pub O);

impl<T, O: Copy> Offset<T, O> {
    pub fn offset(&self) -> O {
        self.1
    }
}

impl<T, O> Deref for Offset<T, O> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type OffsetData = Offset<OffsetWord>;

impl OffsetData {
    pub fn new(name: StrKey, size: i32, offset: i32) -> Self {
        Self(OffsetWord::new(name, size), offset)
    }

    pub fn data(&self) -> (i32, i32) {
        (self.size(), self.offset())
    }

    pub fn size(&self) -> i32 {
        self.0.offset()
    }
}

pub type OffsetWord = Offset<StrKey>;

impl OffsetWord {
    pub fn new(name: StrKey, offset: i32) -> Self {
        Self(name, offset)
    }
}

pub type IndexWord = Offset<StrKey, usize>;

impl Operand for IndexWord {
    fn operand(&self) -> i32 {
        self.1 as i32
    }

    fn index(&self) -> usize {
        self.1
    }
}

impl IndexWord {
    pub fn new<O: Operand>(name: &StrKey, index: O) -> Self {
        Self(*name, index.index())
    }
}
