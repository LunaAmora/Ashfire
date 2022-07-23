use num::FromPrimitive;
use std::{
    fmt::{self, write, Debug, Display, Formatter, Result},
    ops::Deref,
};

#[derive(Default)]
pub struct Proc {
    pub name: String,
    pub contract: Contract,
    pub bindings: Vec<String>,
    pub local_vars: Vec<TypedWord>,
    pub local_mem_names: Vec<Word>,
    pub mem_size: i32,
}

impl Proc {
    pub fn new(name: &str, contract: Contract) -> Self {
        Self {
            name: name.to_owned(),
            contract,
            ..Default::default()
        }
    }
}

#[derive(Default)]
pub struct Contract {
    pub ins: Vec<TokenType>,
    pub outs: Vec<TokenType>,
}

#[derive(Clone)]
pub struct Op {
    pub typ: OpType,
    pub operand: i32,
    pub loc: Loc,
}

impl Op {
    pub fn new(typ: OpType, operand: i32, loc: &Loc) -> Self {
        Self { typ, operand, loc: loc.clone() }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.typ {
            OpType::Intrinsic => {
                write!(f, "Intrinsic {:?}", IntrinsicType::from(self.operand))
            }
            _ => write!(f, "{:?} [{}]", self.typ, self.operand),
        }
    }
}

impl From<(OpType, i32, Loc)> for Op {
    fn from(tuple: (OpType, i32, Loc)) -> Self {
        Self { typ: tuple.0, operand: tuple.1, loc: tuple.2 }
    }
}

impl From<(OpType, Loc)> for Op {
    fn from(tuple: (OpType, Loc)) -> Self {
        Self { typ: tuple.0, operand: 0, loc: tuple.1 }
    }
}

impl From<Op> for Vec<Op> {
    fn from(op: Op) -> Self {
        vec![op]
    }
}

impl From<Op> for anyhow::Result<Option<Vec<Op>>> {
    fn from(op: Op) -> Self {
        Ok(Some(op.into()))
    }
}

#[derive(Clone, Default)]
pub struct Loc {
    pub file: String,
    pub line: i32,
    pub col: i32,
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.file.is_empty() {
            Ok(())
        } else {
            write!(f, "{}:{}:{}: ", self.file, self.line, self.col)
        }
    }
}

#[derive(Clone)]
pub struct IRToken {
    pub typ: TokenType,
    pub operand: i32,
    pub loc: Loc,
}

impl From<(i32, Loc)> for IRToken {
    fn from(tuple: (i32, Loc)) -> Self {
        let (operand, loc) = tuple;
        Self { typ: ValueType::Int.into(), operand, loc }
    }
}

impl IRToken {
    pub fn new(typ: TokenType, operand: i32, loc: &Loc) -> Self {
        Self { typ, operand, loc: loc.to_owned() }
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
        self.typ == TokenType::Keyword &&
            other == &FromPrimitive::from_i32(self.operand).expect("unreachable")
    }
}

impl PartialEq<TokenType> for &IRToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.typ == other
    }
}

impl From<(&TypedWord, &Loc)> for IRToken {
    fn from(tuple: (&TypedWord, &Loc)) -> Self {
        let (typ, operand, loc) = (tuple.0.typ, tuple.0.word.value, tuple.1);
        Self { typ, operand, loc: loc.to_owned() }
    }
}

#[derive(Clone)]
pub struct StructType {
    pub name: String,
    pub members: Vec<StructMember>,
}

impl StructType {
    pub fn new(name: String, members: Vec<StructMember>) -> Self {
        Self { name, members }
    }
}

impl From<(&str, ValueType)> for StructType {
    fn from(tuple: (&str, ValueType)) -> Self {
        Self {
            name: tuple.0.to_string(),
            members: vec![tuple.1.into()],
        }
    }
}

#[derive(Clone)]
pub struct StructMember {
    pub name: String,
    pub typ: TokenType,
    pub default_value: i32,
}

impl From<(String, TokenType)> for StructMember {
    fn from(tuple: (String, TokenType)) -> Self {
        Self { name: tuple.0, typ: tuple.1, default_value: 0 }
    }
}

impl From<ValueType> for StructMember {
    fn from(typ: ValueType) -> Self {
        (String::new(), TokenType::DataType(typ)).into()
    }
}

#[derive(Clone)]
pub struct Word {
    pub name: String,
    pub value: i32,
}

impl Word {
    pub fn new(name: &str, value: i32) -> Self {
        Self { name: name.to_string(), value }
    }
}

impl Deref for Word {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

pub struct SizedWord {
    pub word: Word,
    pub offset: i32,
}

impl Deref for SizedWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

impl SizedWord {
    pub fn size(&self) -> i32 {
        self.word.value
    }
}

impl From<Word> for SizedWord {
    fn from(word: Word) -> Self {
        Self { word, offset: -1 }
    }
}

#[derive(Clone)]
pub struct TypedWord {
    pub word: Word,
    pub typ: TokenType,
}

impl Deref for TypedWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.word
    }
}

impl From<(String, i32, TokenType)> for TypedWord {
    fn from(tuple: (String, i32, TokenType)) -> Self {
        Self {
            word: Word { name: tuple.0, value: tuple.1 },
            typ: tuple.2,
        }
    }
}

pub struct LocWord {
    pub name: String,
    pub loc: Loc,
}

impl PartialEq<String> for LocWord {
    fn eq(&self, other: &String) -> bool {
        self.name == *other
    }
}

impl PartialEq<str> for LocWord {
    fn eq(&self, other: &str) -> bool {
        self.name == other
    }
}

impl Deref for LocWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl LocWord {
    pub fn new(name: &String, loc: Loc) -> Self {
        Self { name: name.to_owned(), loc }
    }
}

pub struct TypeFrame {
    typ: TokenType,
    loc: Loc,
}

pub struct CaseOption {
    typ: CaseType,
    values: Vec<i32>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(ValueType),
    DataPtr(ValueType),
}

impl PartialEq<ValueType> for TokenType {
    fn eq(&self, other: &ValueType) -> bool {
        match (self, other) {
            (Self::DataType(typ), _) => typ == other,
            _ => false,
        }
    }
}
impl From<ValueType> for TokenType {
    fn from(value: ValueType) -> Self {
        TokenType::DataType(value)
    }
}

impl From<TokenType> for i32 {
    fn from(tok: TokenType) -> Self {
        match tok {
            TokenType::Keyword => 0,
            TokenType::Word => 1,
            TokenType::Str => 2,
            TokenType::DataType(value) => 3 + i32::from(value),
            TokenType::DataPtr(value) => -(3 + i32::from(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ValueType {
    Int,
    Bool,
    Ptr,
    Any,
    Type(i32),
}
impl From<usize> for ValueType {
    fn from(value: usize) -> Self {
        match value {
            0 => ValueType::Int,
            1 => ValueType::Bool,
            2 => ValueType::Ptr,
            3 => ValueType::Any,
            i => ValueType::Type(i as i32),
        }
    }
}

impl From<ValueType> for i32 {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::Int => 0,
            ValueType::Bool => 1,
            ValueType::Ptr => 2,
            ValueType::Any => 3,
            ValueType::Type(i) => 4 + i,
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
    Dup,
    Drop,
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
    Store8,
    Load16,
    Store16,
    Load32,
    Store32,
    FdWrite,
    Cast(i32),
}

impl From<i32> for IntrinsicType {
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
            16 => IntrinsicType::Load32,
            17 => IntrinsicType::Store32,
            19 => IntrinsicType::FdWrite,
            n if n >= 20 => IntrinsicType::Cast(n - 20),
            n => IntrinsicType::Cast(n + 20),
        }
    }
}

impl From<IntrinsicType> for i32 {
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
            IntrinsicType::Load32 => 16,
            IntrinsicType::Store32 => 17,
            IntrinsicType::FdWrite => 19,
            IntrinsicType::Cast(n) => n + if n >= 0 { 20 } else { -20 },
        }
    }
}

#[derive(FromPrimitive, Debug, PartialEq, Eq)]
pub enum KeywordType {
    If,
    Else,
    End,
    Arrow,
    Dup,
    Drop,
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

impl From<KeywordType> for i32 {
    fn from(k: KeywordType) -> Self {
        k as i32
    }
}

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

#[derive(Clone, Copy)]
pub enum VarWordType {
    Store,
    Pointer,
}
