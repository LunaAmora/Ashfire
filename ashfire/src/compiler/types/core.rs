use std::{fmt, ops::Deref};

use firelib::{anyhow::Result, lexer::Loc};
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

    #[track_caller]
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

pub const INT: TokenType = TokenType::Data(Data::Typ(Value::Int));
pub const BOOL: TokenType = TokenType::Data(Data::Typ(Value::Bool));
pub const PTR: TokenType = TokenType::Data(Data::Typ(Value::Ptr));
pub const ANY: TokenType = TokenType::Data(Data::Typ(Value::Any));

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    Data(Data),
}

impl Typed for TokenType {
    fn get_type(&self) -> TokenType {
        *self
    }
}

impl From<IRToken> for TokenType {
    fn from(tok: IRToken) -> Self {
        tok.token_type
    }
}

impl PartialEq<Value> for TokenType {
    fn eq(&self, other: &Value) -> bool {
        match self {
            Self::Data(Data::Typ(typ)) => typ == other,
            _ => false,
        }
    }
}

impl PartialEq<Data> for TokenType {
    fn eq(&self, other: &Data) -> bool {
        match self {
            Self::Data(data) => data == other,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct IRToken {
    pub token_type: TokenType,
    pub operand: i32,
    pub loc: Loc,
}

impl Typed for IRToken {
    fn get_type(&self) -> TokenType {
        self.token_type
    }
}

impl Operand for IRToken {
    fn operand(&self) -> i32 {
        self.operand
    }
}

impl Location for IRToken {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl IRToken {
    pub fn new<O: Operand>(token_type: TokenType, operand: O, loc: Loc) -> Self {
        Self { token_type, operand: operand.operand(), loc }
    }

    pub fn get_keyword(&self) -> Option<KeywordType> {
        if self == TokenType::Keyword {
            FromPrimitive::from_i32(self.operand)
        } else {
            None
        }
    }

    #[track_caller]
    pub fn as_keyword(&self) -> KeywordType {
        FromPrimitive::from_i32(self.operand).unwrap()
    }
}

impl Deref for IRToken {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        &self.operand
    }
}

impl PartialEq<KeywordType> for &IRToken {
    #[track_caller]
    fn eq(&self, other: &KeywordType) -> bool {
        self.token_type == TokenType::Keyword && other == &self.as_keyword()
    }
}

impl PartialEq<TokenType> for &IRToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.token_type == other
    }
}

impl PartialEq<Value> for &IRToken {
    fn eq(&self, other: &Value) -> bool {
        &self.token_type == other
    }
}

impl From<(&StructType, Loc)> for IRToken {
    fn from(value: (&StructType, Loc)) -> Self {
        match value.0 {
            StructType::Root(_) => todo!("Support const use on other consts"),
            StructType::Unit(u) => (u, value.1).into(),
        }
    }
}

impl From<(&ValueType, Loc)> for IRToken {
    fn from(tuple: (&ValueType, Loc)) -> Self {
        Self::new(tuple.0.data().get_type(), tuple.0.value(), tuple.1)
    }
}

#[derive(Clone)]
pub struct Op {
    pub op_type: OpType,
    pub operand: i32,
    pub loc: Loc,
}

impl Operand for Op {
    fn operand(&self) -> i32 {
        self.operand
    }
}

impl Op {
    pub fn new(op_type: OpType, operand: i32, loc: Loc) -> Self {
        Self { op_type, operand, loc }
    }

    pub fn set_operand(&mut self, value: i32) {
        self.operand = value;
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op_type {
            OpType::Intrinsic => {
                write!(f, "Intrinsic {:?}", IntrinsicType::from(self.operand))
            }
            _ => write!(f, "{:?} [{}]", self.op_type, self.operand),
        }
    }
}

impl<O: Operand> From<(OpType, O, Loc)> for Op {
    fn from(tuple: (OpType, O, Loc)) -> Self {
        let (op_type, operand, loc) = (tuple.0, tuple.1.operand(), tuple.2);
        Self { op_type, operand, loc }
    }
}

impl From<(OpType, Loc)> for Op {
    fn from(tuple: (OpType, Loc)) -> Self {
        Self { op_type: tuple.0, operand: 0, loc: tuple.1 }
    }
}

impl From<(&StructType, Loc)> for Op {
    #[track_caller]
    fn from(tuple: (&StructType, Loc)) -> Self {
        let StructType::Unit(val) = tuple.0 else {
            unimplemented!("Conversion supported only for Unit Types")
        };

        (val, tuple.1).into()
    }
}

impl From<(&ValueType, Loc)> for Op {
    #[track_caller]
    fn from(tuple: (&ValueType, Loc)) -> Self {
        let Data::Typ(typ) = tuple.0.data() else {
            unimplemented!("Conversion supported only for DataTypes")
        };

        Self::new(OpType::PushData(*typ), tuple.0.value(), tuple.1)
    }
}

impl From<(IntrinsicType, Loc)> for Op {
    fn from(value: (IntrinsicType, Loc)) -> Self {
        Self::new(OpType::Intrinsic, value.0.into(), value.1)
    }
}

impl From<Op> for Vec<Op> {
    fn from(op: Op) -> Self {
        vec![op]
    }
}

impl From<Op> for Result<Option<Vec<Op>>> {
    fn from(op: Op) -> Self {
        Ok(Some(op.into()))
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
