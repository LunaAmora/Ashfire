use std::fmt::{Display, Formatter, Result, write, self};

pub struct Proc {
    name: String,
    contract: Contract,
}

pub struct Contract {
    ins:  Vec<TokenType>,
    outs: Vec<TokenType>,
}

pub struct Op {
    op_type: OpType,
    loc:     Loc,
    operand: i32 //optional, default 0, can change
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
    word:   Word,
    offset: i32 // default -1
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
    DataType(i32),
    DataPtr(i32),
}

pub enum OpType {
    PushInt,
    PushBool,
    PushPtr,
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
