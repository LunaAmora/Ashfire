use std::fmt::{Display, Formatter, Result, write, self};
use num::FromPrimitive;

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

impl From<Op> for Vec<Op> {
    fn from(op: Op) -> Self {
        vec![op]
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

impl From<Op> for anyhow::Result<Option<Vec<Op>>> {
    fn from(op: Op) -> Self {
        Ok(Some(op.into()))
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

pub struct IRToken  {
    pub typ: TokenType,
    pub operand: i32,
    pub loc: Loc
}

impl From<(TypedWord, Loc)> for IRToken {
    fn from(tuple: (TypedWord, Loc)) -> Self {
        let (typ, operand, loc) = (tuple.0.typ, tuple.0.word.value, tuple.1);
        Self { typ, operand, loc }
    }
}

impl IRToken {
    pub fn is_keyword(&self, expected: KeywordType) -> bool{
        self.typ == TokenType::Keyword &&
        expected == FromPrimitive::from_i32(self.operand).expect("unreachable")
    }
}

impl Display for IRToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?} [{}]", self.typ, self.operand)
    }
}

pub struct StructType {
    pub name:    String,
    pub members: Vec<StructMember>
}

impl From<(&str, ValueType)> for StructType {
    fn from(tuple: (&str, ValueType)) -> Self {
        Self { name: tuple.0.to_string(), members: vec![tuple.1.into()] }
    }
}

pub struct StructMember {
    name: String,
    typ:  TokenType,
    default_value: i32
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
    pub name:  String,
    pub value: i32
}

pub struct SizedWord {
    pub word:   Word,
    pub offset: i32
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
    pub typ:  TokenType
}

impl TypedWord {
    pub fn name(&self) -> &str {
        self.word.name.as_str()
    }
}

pub struct TypeFrame {
    typ: TokenType,
    loc: Loc
}

pub struct CaseOption {
    typ:    CaseType,
    values: Vec<i32>
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(ValueType),
    DataPtr(ValueType),
}

#[derive(Debug, Clone, PartialEq)]
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
    Load8,
    Store8,
    Load16,
    Store16,
    Load32,
    Store32,
    FdWrite,
    Cast(i32)
}

impl From<IntrinsicType> for i32 {
    fn from(intrinsic: IntrinsicType) -> Self {
        match intrinsic {
            IntrinsicType::Plus     => 0,
            IntrinsicType::Minus    => 1,
            IntrinsicType::Times    => 2,
            IntrinsicType::Div      => 3,
            IntrinsicType::Greater  => 4,
            IntrinsicType::GreaterE => 5,
            IntrinsicType::Lesser   => 6,
            IntrinsicType::LesserE  => 7,
            IntrinsicType::And      => 8,
            IntrinsicType::Or       => 9,
            IntrinsicType::Xor      => 10,
            IntrinsicType::Load8    => 11,
            IntrinsicType::Store8   => 12,
            IntrinsicType::Load16   => 13,
            IntrinsicType::Store16  => 14,
            IntrinsicType::Load32   => 16,
            IntrinsicType::Store32  => 17,
            IntrinsicType::FdWrite  => 19,
            IntrinsicType::Cast(n)  => 20 + n,
        }
    }
}

#[derive(FromPrimitive, Debug, PartialEq)]
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
