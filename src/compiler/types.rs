use std::fmt::{Display, Formatter, Result, write, self};
use anyhow::Result as anyResult;

pub struct Proc {
    name: String,
    contract: Contract,
}

pub struct Contract {
    ins:  Vec<TokenType>,
    outs: Vec<TokenType>,
}

#[derive(Clone)]
pub struct Op {
    pub typ:     OpType,
    pub operand: i32,
    pub loc:     Loc,
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

impl From<Op> for anyResult<Option<Op>> {
    fn from(op: Op) -> Self {
        Ok(Some(op))
    }
}

#[derive(Clone)]
pub struct Loc { //does not change
    pub file: String,
    pub line: i32,
    pub col:  i32
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}:{}:{}:", self.file, self.line, self.col)
    }
}

pub struct IRToken  { //does not change
    pub typ: TokenType,
    pub operand: i32,
    pub loc: Loc
}

impl Display for IRToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?} [{}]", self.typ, self.operand)
    }
}

pub struct StructType {
    name:    String,
    members: Vec<StructMember>
}

pub struct StructMember {
    name: String,
    typ:  TokenType,
    default_value: i32 // default 0
}

pub struct Word {
    pub name:  String,
    pub value: i32
}

pub struct SizedWord {
    pub word:   Word,
    pub offset: i32 // default -1
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

pub struct TypedWord {
    word: Word,
    typ:  TokenType
}

pub struct TypeFrame {
    typ: TokenType,
    loc: Loc
}

pub struct CaseOption {
    typ:    CaseType,
    values: Vec<i32>
}

#[derive(Debug)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(ValueType),
    DataPtr(ValueType),
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Int,
    Bool,
    Ptr,
    Any,
    Type(i32)
}

#[derive(Debug, Clone)]
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
    Load(DataSizes),
    Store(DataSizes),
    FdWrite,
    Cast(i32)
}

pub enum DataSizes {
    I8,
    I16,
    I32
}

#[derive(FromPrimitive, Debug)]
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

impl From<KeywordType> for i32{
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
