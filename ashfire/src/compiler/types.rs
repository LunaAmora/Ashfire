use std::{
    fmt::{Debug, Display, Formatter, Result},
    ops::Deref,
};

use firelib::{expect_get, fold_bool};
use num::FromPrimitive;

pub trait ProgramVisitor {
    fn set_index(&mut self, i: Option<usize>);
    fn get_index(&self) -> Option<usize>;

    fn inside_proc(&self) -> bool {
        self.get_index().is_some()
    }

    fn current_proc<'a>(&self, program: &'a Program) -> Option<&'a Proc> {
        if let Some(i) = self.get_index() {
            program.procs.get(i)
        } else {
            None
        }
    }

    fn current_proc_mut<'a>(&self, program: &'a mut Program) -> Option<&'a mut Proc> {
        if let Some(i) = self.get_index() {
            program.procs.get_mut(i)
        } else {
            None
        }
    }

    fn visit_proc<'a>(&mut self, program: &'a Program, index: usize) -> &'a Proc {
        self.enter_proc(index);
        program.procs.get(index).unwrap()
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i))
    }

    fn exit_proc(&mut self) {
        self.set_index(None)
    }
}

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub words: Vec<String>,
    pub data: Vec<SizedWord>,
    pub mem_size: i32,
    pub data_size: i32,
    pub global_vars: Vec<TypedWord>,
    pub structs_types: Vec<StructType>,
    pub procs: Vec<Proc>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            structs_types: vec![
                ("int", ValueType::Int).into(),
                ("bool", ValueType::Bool).into(),
                ("ptr", ValueType::Ptr).into(),
                ("any", ValueType::Any).into(),
            ],
            ..Default::default()
        }
    }

    pub fn final_data_size(&self) -> i32 {
        ((self.data_size + 3) / 4) * 4
    }

    pub fn total_vars_size(&self) -> i32 {
        self.global_vars.len() as i32 * 4
    }

    pub fn get_word(&self, index: i32) -> &String {
        expect_get(&self.words, index as usize)
    }

    pub fn get_string(&self, index: i32) -> &SizedWord {
        expect_get(&self.data, index as usize)
    }

    pub fn data_name(&self, value: ValueType) -> String {
        match value {
            ValueType::Int => "Integer",
            ValueType::Bool => "Boolean",
            ValueType::Ptr => "Pointer",
            ValueType::Any => "Any",
            ValueType::Type(n) => {
                return expect_get(&self.structs_types, n as usize).name.to_owned()
            }
        }
        .to_owned()
    }

    pub fn data_display(&self, value: ValueType, operand: i32) -> String {
        match value {
            ValueType::Bool => fold_bool!(operand != 0, "True", "False").to_owned(),
            ValueType::Ptr => format!("*{}", operand),
            ValueType::Any | ValueType::Int | ValueType::Type(_) => operand.to_string(),
        }
    }

    pub fn type_name(&self, typ: TokenType) -> String {
        match typ {
            TokenType::Keyword => "Keyword",
            TokenType::Word => "Word or Intrinsic",
            TokenType::DataType(value) => return self.data_name(value),
            TokenType::DataPtr(value) => return self.data_name(value) + " Pointer",
            TokenType::Str => "String",
        }
        .to_owned()
    }

    pub fn type_display(&self, typ: TokenType, operand: i32) -> String {
        match typ {
            TokenType::Keyword => format!("{:?}", from_i32::<KeywordType>(operand)),
            TokenType::Word => self.get_word(operand).to_owned(),
            TokenType::DataType(value) | TokenType::DataPtr(value) => {
                self.data_display(value, operand)
            }
            TokenType::Str => self.get_string(operand).to_string(),
        }
    }
}

pub fn from_i32<T: FromPrimitive>(value: i32) -> T {
    FromPrimitive::from_i32(value).unwrap()
}

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

impl From<&Contract> for (usize, usize) {
    fn from(contr: &Contract) -> Self {
        (contr.ins.len(), contr.outs.len())
    }
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
        Self { typ: INT, operand, loc }
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
        self.typ == TokenType::Keyword && other == &FromPrimitive::from_i32(self.operand).unwrap()
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

#[derive(Clone)]
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

impl TypedWord {
    pub fn value(&self) -> i32 {
        self.word.value
    }
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

#[derive(Clone)]
pub struct TypeFrame {
    pub typ: TokenType,
    pub loc: Loc,
}

impl From<&IRToken> for TypeFrame {
    fn from(tok: &IRToken) -> Self {
        Self { typ: tok.typ, loc: tok.loc.to_owned() }
    }
}

impl From<(ValueType, &Loc)> for TypeFrame {
    fn from(t: (ValueType, &Loc)) -> Self {
        TypeFrame { typ: t.0.into(), loc: t.1.to_owned() }
    }
}

impl From<(TokenType, &Loc)> for TypeFrame {
    fn from(t: (TokenType, &Loc)) -> Self {
        TypeFrame { typ: t.0, loc: t.1.to_owned() }
    }
}

pub struct CaseOption {
    pub typ: CaseType,
    pub values: Vec<i32>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Keyword,
    Word,
    Str,
    DataType(ValueType),
    DataPtr(ValueType),
}

pub const INT: TokenType = TokenType::DataType(ValueType::Int);
pub const BOOL: TokenType = TokenType::DataType(ValueType::Bool);
pub const PTR: TokenType = TokenType::DataType(ValueType::Ptr);
pub const ANY: TokenType = TokenType::DataType(ValueType::Any);

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
            TokenType::DataType(value) => 1 + usize::from(value) as i32,
            TokenType::DataPtr(value) => -(1 + usize::from(value) as i32),
            _ => 0,
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
