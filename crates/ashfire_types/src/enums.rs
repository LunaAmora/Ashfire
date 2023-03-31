use std::str::FromStr;

use super::data::TypeId;

#[derive(Debug, Clone, Copy)]
pub enum OpType {
    PushData(TypeId, i32),
    ExpectType(TypeId),

    IndexOp(IndexOp, usize),
    ControlOp(ControlOp, usize),

    StackOp(StackOp),
    Intrinsic(IntrinsicType),
}

impl From<IndexOp> for OpType {
    fn from(value: IndexOp) -> Self {
        Self::IndexOp(value, 0)
    }
}

impl From<ControlOp> for OpType {
    fn from(value: ControlOp) -> Self {
        Self::ControlOp(value, 0)
    }
}

impl From<StackOp> for OpType {
    fn from(value: StackOp) -> Self {
        Self::StackOp(value)
    }
}

impl From<IntrinsicType> for OpType {
    fn from(value: IntrinsicType) -> Self {
        Self::Intrinsic(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexOp {
    PushStr,
    PushLocalMem,
    PushGlobalMem,
    PushLocal,
    PushGlobal,

    Offset,
    Unpack,

    Call,
    CallInline,

    PushBind,
    LoadBind,
}

#[derive(Debug, Clone, Copy)]
pub enum ControlOp {
    PrepProc,
    PrepInline,

    EndProc,
    EndInline,

    IfStart,
    Else,
    EndIf,
    EndElse,

    While,
    Do,
    EndWhile,

    BindStack,
    PopBind,

    CaseStart,
    CaseMatch,
    CaseOption,
    EndCase,
}

#[derive(Debug, Clone, Copy)]
pub enum StackOp {
    Drop,
    Dup,
    Swap,
    Over,
    Rot,
    Equal,
}

#[derive(Debug, Clone, Copy)]
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
    Cast(TypeId),
}

impl FromStr for IntrinsicType {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(match value {
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Times,
            "%" => Self::Div,
            ">" => Self::Greater,
            ">=" => Self::GreaterE,
            "<" => Self::Lesser,
            "<=" => Self::LesserE,
            "or" => Self::Or,
            "and" => Self::And,
            "xor" => Self::Xor,
            "@8" => Self::Load8,
            "!8" => Self::Store8,
            "@16" => Self::Load16,
            "!16" => Self::Store16,
            "@32" => Self::Load32,
            "!32" => Self::Store32,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    In,
    While,
    Do,
    At,
    Dot,
    Ref,
    Inline,
    Include,
    Import,
    Export,
    Case,
}

impl FromStr for KeywordType {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(match value {
            "dup" => Self::Dup,
            "swap" => Self::Swap,
            "drop" => Self::Drop,
            "over" => Self::Over,
            "rot" => Self::Rot,
            "if" => Self::If,
            "else" => Self::Else,
            "end" => Self::End,
            "proc" => Self::Proc,
            "->" => Self::Arrow,
            "mem" => Self::Mem,
            ":" => Self::Colon,
            "=" => Self::Equal,
            "let" => Self::Let,
            "in" => Self::In,
            "do" => Self::Do,
            "@" => Self::At,
            "." => Self::Dot,
            "*" => Self::Ref,
            "case" => Self::Case,
            "while" => Self::While,
            "struct" => Self::Struct,
            "inline" => Self::Inline,
            "include" => Self::Include,
            "import" => Self::Import,
            "export" => Self::Export,
            _ => return Err(()),
        })
    }
}
