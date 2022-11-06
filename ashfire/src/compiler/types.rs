use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

use firelib::{
    anyhow::Result,
    lexer::{Loc, Token},
};
use num::FromPrimitive;

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
        self.to_owned()
    }
}

impl Location for &Token {
    fn loc(&self) -> Loc {
        self.loc.to_owned()
    }
}

pub trait Operand {
    fn as_operand(&self) -> i32;
}

impl<T: Operand> Operand for &T {
    fn as_operand(&self) -> i32 {
        (*self).as_operand()
    }
}

impl Operand for i32 {
    fn as_operand(&self) -> i32 {
        *self
    }
}

#[derive(Default)]
pub struct Proc {
    pub name: String,
    pub contract: Contract,
    pub bindings: Vec<String>,
    pub local_vars: Vec<StructType>,
    pub local_mem_names: Vec<SizeWord>,
    mem_size: i32,
}

impl Proc {
    pub fn new(name: &str, contract: Contract) -> Self {
        Self {
            name: name.to_owned(),
            contract,
            ..Default::default()
        }
    }

    pub fn push_mem(&mut self, word: &str, size: i32) {
        self.mem_size += size;
        self.local_mem_names
            .push(SizeWord::new(word, self.mem_size));
    }

    pub fn get_label(&self) -> &str {
        &self.name
    }

    pub fn total_size(&self) -> i32 {
        self.mem_size + (self.local_vars.iter().fold(0, |acc, var| acc + var.size()) as i32)
    }

    pub fn var_mem_offset(&self, index: i32) -> i32 {
        self.mem_size + (index + 1) * 4
    }
}

#[derive(Default)]
pub struct Contract {
    ins: Vec<TokenType>,
    outs: Vec<TokenType>,
}

impl Contract {
    pub fn new(ins: Vec<TokenType>, outs: Vec<TokenType>) -> Self {
        Self { ins, outs }
    }

    pub fn ins(&self) -> &[TokenType] {
        &self.ins
    }

    pub fn outs(&self) -> &[TokenType] {
        &self.outs
    }

    pub fn size(&self) -> (usize, usize) {
        (self.ins.len(), self.outs.len())
    }
}

impl From<&Contract> for (usize, usize) {
    fn from(contr: &Contract) -> Self {
        (contr.ins.len(), contr.outs.len())
    }
}

#[derive(Clone)]
pub struct Op {
    pub op_type: OpType,
    pub operand: i32,
    pub loc: Loc,
}

impl Op {
    pub fn new(op_type: OpType, operand: i32, loc: Loc) -> Self {
        Self { op_type, operand, loc }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.op_type {
            OpType::Intrinsic => {
                write!(f, "Intrinsic {:?}", IntrinsicType::from(self.operand))
            }
            _ => write!(f, "{:?} [{}]", self.op_type, self.operand),
        }
    }
}

impl From<(OpType, i32, Loc)> for Op {
    fn from(tuple: (OpType, i32, Loc)) -> Self {
        Self { op_type: tuple.0, operand: tuple.1, loc: tuple.2 }
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
        let TokenType::DataType(typ) = tuple.0.token_type else {
            unimplemented!("Conversion supported only for DataTypes")
        };

        Op::new(OpType::PushData(typ), tuple.0.value, tuple.1)
    }
}

impl From<(IntrinsicType, Loc)> for Op {
    fn from(value: (IntrinsicType, Loc)) -> Self {
        Op::new(OpType::Intrinsic, value.0.into(), value.1)
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

#[derive(Clone, Copy)]
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
    fn as_operand(&self) -> i32 {
        self.operand
    }
}

impl Location for IRToken {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl IRToken {
    pub fn new(token_type: TokenType, operand: i32, loc: Loc) -> Self {
        Self { token_type, operand, loc }
    }

    pub fn get_keyword(&self) -> Option<KeywordType> {
        if self == TokenType::Keyword {
            FromPrimitive::from_i32(self.operand)
        } else {
            None
        }
    }
}

impl PartialEq<KeywordType> for &IRToken {
    #[track_caller]
    fn eq(&self, other: &KeywordType) -> bool {
        self.token_type == TokenType::Keyword &&
            other == &FromPrimitive::from_i32(self.operand).unwrap()
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
        IRToken::new(tuple.0.token_type, tuple.0.value, tuple.1)
    }
}

#[derive(Clone)]
pub enum StructType {
    Root(StructDef),
    Unit(ValueType),
}

impl StructType {
    #[track_caller]
    pub fn get_value(&self) -> &ValueType {
        match self {
            StructType::Unit(val) => val,
            _ => unimplemented!("only supported for Unit Types"),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            StructType::Unit(val) => &val.name,
            StructType::Root(stk) => &stk.name,
        }
    }

    pub fn units(&self) -> Vec<&ValueType> {
        match self {
            StructType::Root(s) => s.units(),
            StructType::Unit(v) => vec![v],
        }
    }

    pub fn size(&self) -> usize {
        match self {
            StructType::Root(s) => s.size(),
            StructType::Unit(_) => 4,
        }
    }
}

impl Typed for StructType {
    fn get_type(&self) -> TokenType {
        match self {
            StructType::Unit(val) => val.token_type,
            _ => unimplemented!("Temporary hack"),
        }
    }
}

impl From<ValueType> for StructType {
    fn from(value: ValueType) -> Self {
        StructType::Unit(value)
    }
}

impl From<StructDef> for StructType {
    fn from(value: StructDef) -> Self {
        StructType::Root(value)
    }
}

#[derive(Clone)]
pub struct StructDef {
    name: String,
    members: Vec<StructType>,
}

impl StructDef {
    pub fn new(name: String, members: Vec<StructType>) -> Self {
        Self { name, members }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn members(&self) -> &[StructType] {
        &self.members
    }

    pub fn units(&self) -> Vec<&ValueType> {
        self.members.iter().flat_map(|m| m.units()).collect()
    }

    pub fn size(&self) -> usize {
        self.members
            .iter()
            .fold(0, |acc, member| acc + member.size())
    }
}

impl From<(&str, Value)> for StructDef {
    fn from(tuple: (&str, Value)) -> Self {
        Self {
            name: tuple.0.to_string(),
            members: vec![StructType::Unit((String::new(), tuple.1).into())],
        }
    }
}

#[derive(Clone)]
pub struct ValueType {
    name: String,
    value: i32,
    token_type: TokenType,
}

impl ValueType {
    pub fn new<T: Typed + Operand>(name: String, typed: T) -> Self {
        Self {
            name,
            value: typed.as_operand(),
            token_type: typed.get_type(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        self.token_type
    }
}

impl Operand for ValueType {
    fn as_operand(&self) -> i32 {
        self.value
    }
}

impl<T: Typed> From<(String, T)> for ValueType {
    fn from(tuple: (String, T)) -> Self {
        Self {
            name: tuple.0,
            value: 0,
            token_type: tuple.1.get_type(),
        }
    }
}

impl Deref for ValueType {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

pub struct Offset<T, O = i32>(T, O);

impl<T, O: Copy> Offset<T, O> {
    pub fn offset(&self) -> O {
        self.1
    }

    pub fn set_offset(&mut self, offset: O) {
        self.1 = offset
    }
}

impl<T, O> Deref for Offset<T, O> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type SizeWord = Offset<String>;

impl SizeWord {
    pub fn new(name: &str, offset: i32) -> Self {
        Self(name.to_string(), offset)
    }

    pub fn size(&self) -> i32 {
        self.1
    }
}

pub type OffsetWord = Offset<SizeWord>;

impl OffsetWord {
    pub fn new(name: &str, value: i32) -> Self {
        Self(SizeWord::new(name, value), -1)
    }
}

pub type IndexWord = Offset<String, usize>;

impl IndexWord {
    pub fn new(name: &str, index: usize) -> Self {
        Self(name.to_owned(), index)
    }

    pub fn index(&self) -> usize {
        self.1
    }
}

#[derive(Clone, Copy)]
pub struct TypeFrame(TokenType, Loc);

impl TypeFrame {
    pub fn new<T: Typed + Location>(tok: T) -> Self {
        Self(tok.get_type(), tok.loc().to_owned())
    }
}

impl Typed for TypeFrame {
    fn get_type(&self) -> TokenType {
        self.0
    }
}

impl Location for TypeFrame {
    fn loc(&self) -> Loc {
        self.1
    }
}

impl<T: Typed, L: Location> From<(T, L)> for TypeFrame {
    fn from(tuple: (T, L)) -> Self {
        Self(tuple.0.get_type(), tuple.1.loc().to_owned())
    }
}

#[allow(dead_code)]
pub struct CaseOption {
    pub case_type: CaseType,
    pub values: Vec<i32>,
}

pub const INT: TokenType = TokenType::DataType(Value::Int);
pub const BOOL: TokenType = TokenType::DataType(Value::Bool);
pub const PTR: TokenType = TokenType::DataType(Value::Ptr);
pub const ANY: TokenType = TokenType::DataType(Value::Any);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(Value),
    DataPtr(Value),
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

impl From<TypeFrame> for TokenType {
    fn from(frame: TypeFrame) -> Self {
        (&frame).get_type()
    }
}

impl From<TokenType> for i32 {
    fn from(tok: TokenType) -> Self {
        match tok {
            TokenType::DataType(value) => 1 + usize::from(value) as i32,
            TokenType::DataPtr(value) => -(1 + usize::from(value) as i32),
            _ => 0,
        }
    }
}

impl PartialEq<Value> for TokenType {
    fn eq(&self, other: &Value) -> bool {
        match self {
            Self::DataType(typ) => typ == other,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Value {
    Int,
    Bool,
    Ptr,
    Any,
    Type(usize),
}

impl Typed for Value {
    fn get_type(&self) -> TokenType {
        TokenType::DataType(*self)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        match value {
            0 => Value::Int,
            1 => Value::Bool,
            2 => Value::Ptr,
            3 => Value::Any,
            i => Value::Type(i),
        }
    }
}

impl From<Value> for usize {
    fn from(value: Value) -> Self {
        match value {
            Value::Int => 0,
            Value::Bool => 1,
            Value::Ptr => 2,
            Value::Any => 3,
            Value::Type(i) => i,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpType {
    PushData(Value),
    PushStr,
    PushLocalMem,
    PushGlobalMem,
    PushLocal,
    PushGlobal,
    OffsetLoad,
    Offset,
    Intrinsic,
    Drop,
    Dup,
    Swap,
    Over,
    Rot,
    Call,
    Equal,
    PrepProc,
    IfStart,
    Else,
    EndIf,
    EndElse,
    EndProc,
    BindStack,
    PushBind,
    PopBind,
    While,
    Do,
    EndWhile,
    Unpack,
    ExpectType,
    CaseStart,
    CaseMatch,
    CaseOption,
    EndCase,
}

#[derive(Debug)]
pub enum IntrinsicType {
    Plus,
    Minus,
    Times,
    Div,
    Greater,
    GreaterE,
    Lesser,
    LesserE,
    And,
    Or,
    Xor,
    Load8,
    Load16,
    Load32,
    Store8,
    Store16,
    Store32,
    FdWrite,
    Cast(i32),
}

impl const From<i32> for IntrinsicType {
    fn from(value: i32) -> Self {
        match value {
            0 => IntrinsicType::Plus,
            1 => IntrinsicType::Minus,
            2 => IntrinsicType::Times,
            3 => IntrinsicType::Div,
            4 => IntrinsicType::Greater,
            5 => IntrinsicType::GreaterE,
            6 => IntrinsicType::Lesser,
            7 => IntrinsicType::LesserE,
            8 => IntrinsicType::And,
            9 => IntrinsicType::Or,
            10 => IntrinsicType::Xor,
            11 => IntrinsicType::Load8,
            12 => IntrinsicType::Store8,
            13 => IntrinsicType::Load16,
            14 => IntrinsicType::Store16,
            15 => IntrinsicType::Load32,
            16 => IntrinsicType::Store32,
            17 => IntrinsicType::FdWrite,
            n if n.abs() <= CAST_BASE => IntrinsicType::Cast(0), // invalid cast
            n => IntrinsicType::Cast(fold_bool!(n.is_positive(), -CAST_BASE, CAST_BASE) + n),
        }
    }
}

const CAST_BASE: i32 = i32::from(IntrinsicType::Cast(0));

impl const From<IntrinsicType> for i32 {
    fn from(intrinsic: IntrinsicType) -> Self {
        match intrinsic {
            IntrinsicType::Plus => 0,
            IntrinsicType::Minus => 1,
            IntrinsicType::Times => 2,
            IntrinsicType::Div => 3,
            IntrinsicType::Greater => 4,
            IntrinsicType::GreaterE => 5,
            IntrinsicType::Lesser => 6,
            IntrinsicType::LesserE => 7,
            IntrinsicType::And => 8,
            IntrinsicType::Or => 9,
            IntrinsicType::Xor => 10,
            IntrinsicType::Load8 => 11,
            IntrinsicType::Store8 => 12,
            IntrinsicType::Load16 => 13,
            IntrinsicType::Store16 => 14,
            IntrinsicType::Load32 => 15,
            IntrinsicType::Store32 => 16,
            IntrinsicType::FdWrite => 17,
            IntrinsicType::Cast(n) => 18 * fold_bool!(n >= 0, 1, -1) + n,
        }
    }
}

#[derive(FromPrimitive, Debug, PartialEq, Eq)]
pub enum KeywordType {
    If,
    Else,
    End,
    Arrow,
    Drop,
    Dup,
    Swap,
    Over,
    Rot,
    Colon,
    Equal,
    Proc,
    Mem,
    Struct,
    Let,
    While,
    Do,
    At,
    Include,
    Case,
}

#[allow(dead_code)]
pub enum CaseType {
    None,
    Equal,
    Match,
    Range,
    Lesser,
    LesserE,
    Greater,
    GreaterE,
    BitAnd,
    Default,
}

#[cfg(test)]
mod tests {
    use super::{IntrinsicType, CAST_BASE};
    const RANGE: i32 = 30;

    #[test]
    fn intrinsic_type_conversion() {
        for n in -RANGE..=RANGE {
            let i = i32::from(IntrinsicType::from(n));

            if !(-CAST_BASE..0).contains(&n) {
                assert_eq!(n, i);
            } else {
                assert_eq!(i, CAST_BASE);
            }
        }
    }
}
