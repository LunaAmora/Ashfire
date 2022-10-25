use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

use anyhow::Result;
use firelib::{fold_bool, lexer::Loc};
use num::FromPrimitive;

pub trait Typed {
    fn get_type(&self) -> TokenType;
}

pub trait Location {
    fn loc(&self) -> &Loc;
}

impl Location for Loc {
    fn loc(&self) -> &Loc {
        self
    }
}

pub trait Operand {
    fn as_operand(&self) -> i32;
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
    pub local_vars: Vec<TypedWord>,
    pub local_mem_names: Vec<Word>,
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
        self.local_mem_names.push(Word::new(word, self.mem_size));
    }

    pub fn get_label(&self) -> &str {
        &self.name
    }

    pub fn total_size(&self) -> i32 {
        self.mem_size + (self.local_vars.len() as i32 * 4)
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
    pub fn new(op_type: OpType, operand: i32, loc: &Loc) -> Self {
        Self { op_type, operand, loc: loc.clone() }
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

impl<T: Typed + Operand> From<(&T, &Loc)> for Op {
    fn from(tuple: (&T, &Loc)) -> Self {
        let TokenType::DataType(typ) = tuple.0.get_type() else {
            unreachable!("Conversion supported only for DataTypes")
        };

        Op::new(OpType::PushData(typ), tuple.0.as_operand(), tuple.1)
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
    fn as_operand(&self) -> i32 {
        self.operand
    }
}

impl Location for IRToken {
    fn loc(&self) -> &Loc {
        &self.loc
    }
}

impl IRToken {
    pub fn new(token_type: TokenType, operand: i32, loc: &Loc) -> Self {
        Self { token_type, operand, loc: loc.to_owned() }
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

impl<T: Typed + Operand> From<(&T, &Loc)> for IRToken {
    fn from(tuple: (&T, &Loc)) -> Self {
        IRToken::new(tuple.0.get_type(), tuple.0.as_operand(), &tuple.1.to_owned())
    }
}

#[derive(Clone)]
pub struct StructType {
    name: String,
    members: Vec<StructMember>,
}

impl StructType {
    pub fn new(name: String, members: Vec<StructMember>) -> Self {
        Self { name, members }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn members(&self) -> &[StructMember] {
        &self.members
    }
}

impl From<(&str, ValueType)> for StructType {
    fn from(tuple: (&str, ValueType)) -> Self {
        Self {
            name: tuple.0.to_string(),
            members: vec![(String::new(), &tuple.1).into()],
        }
    }
}

#[derive(Clone)]
pub struct StructMember {
    name: String,
    token_type: TokenType,
    default_value: i32,
}

impl StructMember {
    pub fn new(name: String, token_type: TokenType, default_value: i32) -> Self {
        Self { name, token_type, default_value }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Typed for StructMember {
    fn get_type(&self) -> TokenType {
        self.token_type
    }
}

impl Operand for StructMember {
    fn as_operand(&self) -> i32 {
        self.default_value
    }
}

impl<T: Typed> From<(String, &T)> for StructMember {
    fn from(tuple: (String, &T)) -> Self {
        Self::new(tuple.0, tuple.1.get_type(), 0)
    }
}

#[derive(Clone)]
pub struct Word {
    name: String,
    value: i32,
}

impl Word {
    pub fn new(name: &str, value: i32) -> Self {
        Self { name: name.to_string(), value }
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

impl Deref for Word {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

#[derive(Clone)]
pub struct SizedWord {
    word: Word,
    offset: i32,
}

impl SizedWord {
    pub fn new(word: Word) -> Self {
        Self { word, offset: -1 }
    }

    pub fn size(&self) -> i32 {
        self.word.value
    }

    pub fn offset(&self) -> i32 {
        self.offset
    }

    pub fn set_offset(&mut self, offset: i32) {
        self.offset = offset
    }
}

impl Deref for SizedWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

#[derive(Clone)]
pub struct TypedWord {
    word: Word,
    token_type: TokenType,
}

impl TypedWord {
    pub fn new<T: Typed + Operand>(name: String, typed: T) -> Self {
        Self {
            word: Word { name, value: typed.as_operand() },
            token_type: typed.get_type(),
        }
    }

    pub fn value(&self) -> i32 {
        self.word.value
    }
}

impl Typed for TypedWord {
    fn get_type(&self) -> TokenType {
        self.token_type
    }
}

impl Operand for TypedWord {
    fn as_operand(&self) -> i32 {
        self.value()
    }
}

impl Deref for TypedWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

#[derive(Clone)]
pub struct TypeFrame {
    token_type: TokenType,
    loc: Loc,
}

impl Typed for TypeFrame {
    fn get_type(&self) -> TokenType {
        self.token_type
    }
}

impl Location for TypeFrame {
    fn loc(&self) -> &Loc {
        &self.loc
    }
}

impl<T: Typed + Location> From<&T> for TypeFrame {
    fn from(tok: &T) -> Self {
        Self {
            token_type: tok.get_type(),
            loc: tok.loc().to_owned(),
        }
    }
}

impl<T: Typed, L: Location> From<(T, &L)> for TypeFrame {
    fn from(tuple: (T, &L)) -> Self {
        Self {
            token_type: tuple.0.get_type(),
            loc: tuple.1.loc().to_owned(),
        }
    }
}

#[allow(dead_code)]
pub struct CaseOption {
    pub case_type: CaseType,
    pub values: Vec<i32>,
}

pub const INT: TokenType = TokenType::DataType(ValueType::Int);
pub const BOOL: TokenType = TokenType::DataType(ValueType::Bool);
pub const PTR: TokenType = TokenType::DataType(ValueType::Ptr);
pub const ANY: TokenType = TokenType::DataType(ValueType::Any);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(ValueType),
    DataPtr(ValueType),
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
        frame.token_type
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

impl PartialEq<ValueType> for TokenType {
    fn eq(&self, other: &ValueType) -> bool {
        match self {
            Self::DataType(typ) => typ == other,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ValueType {
    Int,
    Bool,
    Ptr,
    Any,
    Type(usize),
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        TokenType::DataType(*self)
    }
}

impl From<usize> for ValueType {
    fn from(value: usize) -> Self {
        match value {
            0 => ValueType::Int,
            1 => ValueType::Bool,
            2 => ValueType::Ptr,
            3 => ValueType::Any,
            i => ValueType::Type(i),
        }
    }
}

impl From<ValueType> for usize {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::Int => 0,
            ValueType::Bool => 1,
            ValueType::Ptr => 2,
            ValueType::Any => 3,
            ValueType::Type(i) => i,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OpType {
    PushData(ValueType),
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
