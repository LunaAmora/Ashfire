use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

use firelib::{anyhow::Result, lexer::Loc};
use lasso::Key;
use num::FromPrimitive;

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

#[derive(Default)]
pub struct ProcData {
    pub bindings: Vec<StrKey>,
    pub local_vars: Vec<StructType>,
    pub local_mems: Vec<OffsetWord>,
    mem_size: usize,
}

impl ProcData {
    pub fn push_mem(&mut self, word: &StrKey, size: usize) {
        self.mem_size += size;
        self.local_mems
            .push(OffsetWord::new(*word, self.mem_size as i32));
    }

    pub fn total_size(&self) -> i32 {
        (self.mem_size + self.local_vars.iter().fold(0, |acc, var| acc + var.size())) as i32
    }

    pub fn var_mem_offset(&self, index: i32) -> i32 {
        self.mem_size as i32 + (index + 1) * WORD_SIZE
    }
}

pub enum ProcType {
    Inline(usize, usize),
    Declare(ProcData),
}

impl Default for ProcType {
    fn default() -> Self {
        Self::Declare(ProcData::default())
    }
}

#[derive(Default)]
pub struct Proc {
    pub name: StrKey,
    pub contract: Contract,
    pub data: ProcType,
}

impl Proc {
    pub fn new(name: &StrKey, contract: Contract, inline: Option<usize>) -> Self {
        let data = inline.map_or_else(ProcType::default, |start| ProcType::Inline(start, 0));
        Self { name: *name, contract, data }
    }

    pub fn get_data(&self) -> Option<&ProcData> {
        match &self.data {
            ProcType::Inline(..) => None,
            ProcType::Declare(data) => Some(data),
        }
    }

    pub fn get_data_mut(&mut self) -> Option<&mut ProcData> {
        match &mut self.data {
            ProcType::Inline(..) => None,
            ProcType::Declare(data) => Some(data),
        }
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
        let Data::Typ(typ) = tuple.0.data_type else {
            unimplemented!("Conversion supported only for DataTypes")
        };

        Self::new(OpType::PushData(typ), tuple.0.value, tuple.1)
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
        Self::new(tuple.0.data_type.get_type(), tuple.0.value, tuple.1)
    }
}

#[derive(Clone)]
pub enum StructType {
    Root(StructRef),
    Unit(ValueType),
}

impl StructType {
    pub fn units(&self) -> Vec<&ValueType> {
        match self {
            Self::Root(s) => s.units(),
            Self::Unit(v) => vec![v],
        }
    }

    pub fn count(&self) -> usize {
        match self {
            Self::Root(s) => s.count(),
            Self::Unit(_) => 1,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Root(s) => s.size(),
            Self::Unit(_) => WORD_USIZE,
        }
    }
}

impl Deref for StructType {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Unit(val) => &val.name,
            Self::Root(stk) => &stk.name,
        }
    }
}

impl Typed for StructType {
    fn get_type(&self) -> TokenType {
        match self {
            Self::Root(stk) => stk.get_type(),
            Self::Unit(typ) => typ.get_type(),
        }
    }
}

impl From<ValueType> for StructType {
    fn from(value: ValueType) -> Self {
        Self::Unit(value)
    }
}

pub trait OffsetByKey {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)>;
    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)>;
}

impl OffsetByKey for [StructType] {
    fn get_offset(&self, word: &StrKey) -> Option<(usize, usize)> {
        let Some(i) = self.iter().position(|stk| word.eq(stk)) else {
            return None;
        };

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset, i))
    }

    fn get_offset_local(&self, word: &StrKey) -> Option<(usize, usize)> {
        let Some(i) = self.iter().position(|stk| word.eq(stk)) else {
            return None;
        };

        let mut offset = 0;
        for (var, _) in self.iter().zip(0..=i) {
            offset += var.size() / WORD_USIZE;
        }

        Some((offset - 1, i))
    }
}

#[derive(Clone)]
pub struct StructDef {
    name: StrKey,
    members: Vec<StructType>,
}

impl StructDef {
    pub fn new(name: &StrKey, members: Vec<StructType>) -> Self {
        Self { name: *name, members }
    }

    pub fn members(&self) -> &[StructType] {
        &self.members
    }

    pub fn units(&self) -> Vec<&ValueType> {
        self.members.iter().flat_map(StructType::units).collect()
    }

    pub fn count(&self) -> usize {
        self.members
            .iter()
            .fold(0, |acc, member| acc + member.count())
    }

    pub fn size(&self) -> usize {
        self.members
            .iter()
            .fold(0, |acc, member| acc + member.size())
    }
}

impl Deref for StructDef {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl From<(StrKey, Value)> for StructDef {
    fn from(tuple: (StrKey, Value)) -> Self {
        Self {
            name: tuple.0,
            members: vec![StructType::Unit((StrKey::default(), tuple.1).into())],
        }
    }
}

#[derive(Clone)]
pub struct StructRef {
    data: StructDef,
    reftype: Value,
}

impl Deref for StructRef {
    type Target = StructDef;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl StructRef {
    pub fn new(name: &StrKey, members: Vec<StructType>, reftype: Value) -> Self {
        Self { data: StructDef::new(name, members), reftype }
    }

    pub fn get_ref_type(&self) -> Value {
        self.reftype
    }
}

impl Typed for StructRef {
    fn get_type(&self) -> TokenType {
        self.reftype.get_type()
    }
}

#[derive(Clone)]
pub struct ValueType {
    name: StrKey,
    value: i32,
    data_type: Data,
}

impl ValueType {
    pub fn new<T: Typed + Operand>(name: &StrKey, typed: T) -> Self {
        let TokenType::Data(data_type) =  typed.get_type() else {
            unimplemented!()
        };

        Self { name: *name, value: typed.operand(), data_type }
    }

    pub fn value(&self) -> i32 {
        self.value
    }

    pub fn data(&self) -> &Data {
        &self.data_type
    }
}

impl Typed for ValueType {
    fn get_type(&self) -> TokenType {
        self.data_type.get_type()
    }
}

impl<T: Typed> From<(StrKey, T)> for ValueType {
    fn from(tuple: (StrKey, T)) -> Self {
        let TokenType::Data(data_type) = tuple.1.get_type() else {
            unimplemented!()
        };

        Self { name: tuple.0, value: 0, data_type }
    }
}

impl Deref for ValueType {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.name
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

#[derive(Clone, Copy)]
pub struct TypeFrame(TokenType, Loc);

impl TypeFrame {
    pub fn new<T: Typed + Location>(tok: T) -> Self {
        Self(tok.get_type(), tok.loc())
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
        Self(tuple.0.get_type(), tuple.1.loc())
    }
}

#[allow(dead_code)]
pub struct CaseOption {
    pub case_type: CaseType,
    pub values: Vec<i32>,
}

pub const INT: TokenType = TokenType::Data(Data::Typ(Value::Int));
pub const BOOL: TokenType = TokenType::Data(Data::Typ(Value::Bool));
pub const PTR: TokenType = TokenType::Data(Data::Typ(Value::Ptr));
pub const ANY: TokenType = TokenType::Data(Data::Typ(Value::Any));

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Data {
    Typ(Value),
    Ptr(Value),
}

impl Data {
    pub fn get_value(self) -> Value {
        match self {
            Self::Typ(val) | Self::Ptr(val) => val,
        }
    }
}

impl Typed for Data {
    fn get_type(&self) -> TokenType {
        TokenType::Data(*self)
    }
}

impl Operand for Data {
    fn operand(&self) -> i32 {
        match self {
            Self::Typ(value) => 1 + usize::from(*value) as i32,
            Self::Ptr(value) => -(1 + usize::from(*value) as i32),
        }
    }

    fn index(&self) -> usize {
        unimplemented!()
    }
}

impl From<i32> for Data {
    fn from(value: i32) -> Self {
        match value {
            0 => unimplemented!("Not a valid value"),
            1.. => Self::Typ(Value::from((value - 1) as usize)),
            _ => Self::Ptr(Value::from((-value - 1) as usize)),
        }
    }
}

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

impl From<TypeFrame> for TokenType {
    fn from(frame: TypeFrame) -> Self {
        frame.get_type()
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
        TokenType::Data(Data::Typ(*self))
    }
}

impl Operand for Value {
    fn operand(&self) -> i32 {
        Data::Typ(*self).operand()
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::Int,
            1 => Self::Bool,
            2 => Self::Ptr,
            3 => Self::Any,
            i => Self::Type(i),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    CallInline,
    Equal,
    PrepProc,
    PrepInline,
    IfStart,
    Else,
    EndIf,
    EndElse,
    EndProc,
    EndInline,
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

impl IntrinsicType {
    pub fn from_str(value: &str) -> Option<Self> {
        Some(match value {
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
            "fd_write" => Self::FdWrite,
            _ => return None,
        })
    }
}

impl const From<i32> for IntrinsicType {
    fn from(value: i32) -> Self {
        match value {
            0 => Self::Plus,
            1 => Self::Minus,
            2 => Self::Times,
            3 => Self::Div,
            4 => Self::Greater,
            5 => Self::GreaterE,
            6 => Self::Lesser,
            7 => Self::LesserE,
            8 => Self::And,
            9 => Self::Or,
            10 => Self::Xor,
            11 => Self::Load8,
            12 => Self::Store8,
            13 => Self::Load16,
            14 => Self::Store16,
            15 => Self::Load32,
            16 => Self::Store32,
            17 => Self::FdWrite,
            n if n.abs() <= CAST_BASE => Self::Cast(0), // invalid cast
            n => Self::Cast(fold_bool!(n.is_positive(), -CAST_BASE, CAST_BASE) + n),
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

#[derive(FromPrimitive, Clone, Copy, Debug, PartialEq, Eq)]
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
    Dot,
    Ref,
    Inline,
    Include,
    Case,
}

impl KeywordType {
    pub fn from_str(value: &str) -> Option<Self> {
        Some(match value {
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
            "do" => Self::Do,
            "@" => Self::At,
            "." => Self::Dot,
            "*" => Self::Ref,
            "case" => Self::Case,
            "while" => Self::While,
            "struct" => Self::Struct,
            "inline" => Self::Inline,
            "include" => Self::Include,
            _ => return None,
        })
    }
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
