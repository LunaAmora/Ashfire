use std::str::FromStr;

use super::data::Value;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpType {
    PushData(Value),
    PushStr,
    PushLocalMem,
    PushGlobalMem,
    PushLocal,
    PushGlobal,
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
    LoadBind,
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
    Cast(i32),
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
            IntrinsicType::Cast(n) => 17 * fold_bool!(n >= 0, 1, -1) + n,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{IntrinsicType, CAST_BASE};
    const RANGE: i32 = 30;

    #[test]
    fn intrinsic_type_conversion() {
        for n in -RANGE..=RANGE {
            let i = i32::from(IntrinsicType::from(n));

            if (-CAST_BASE..0).contains(&n) {
                assert_eq!(i, CAST_BASE);
            } else {
                assert_eq!(n, i);
            }
        }
    }
}
