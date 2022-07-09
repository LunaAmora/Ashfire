use std::{io::BufReader, fs::File, path::PathBuf, collections::VecDeque};
use anyhow::{Context, Result, ensure, anyhow, bail};
use super::{lexer::Lexer, types::*};
use num::FromPrimitive;
use firelib::*;

#[derive(Default)]
pub struct Parser<'a> {
    pub word_list: Vec<String>,
    pub data_list: Vec<SizedWord>,
    pub total_data_size: i32,
    pub struct_list : Vec<StructType>,
    ir_tokens:  VecDeque<IRToken>,
    const_list: Vec<TypedWord>,
    program:   Vec<Op>,
    op_blocks: Vec<Op>,
    current_proc: Option<&'a Proc>,
}

impl Parser<'_> {
    pub fn new() -> Self {
        Self {
            struct_list : vec![
                ("int",  ValueType::Int).into(),
                ("bool", ValueType::Bool).into(),
                ("ptr",  ValueType::Ptr).into(),
                ("any",  ValueType::Any).into()],
            ..Default::default()
        }
    }
    
    fn inside_proc(&self) -> bool {
        self.current_proc.is_some()
    }

    fn next_irtoken(&mut self) -> Option<IRToken> {
        self.ir_tokens.pop_front()
    }

    fn parse_tokens(&mut self) -> Result<()> {
        while let Some(token) = self.next_irtoken() {
            if let Some(mut op) = self.define_op(token)? {
                self.program.append(&mut op)
            }
        }
        Ok(())
    }

    fn define_op(&mut self, tok: IRToken) -> Result<Option<Vec<Op>>> {
        ensure!(matches!(tok.typ, TokenType::Keyword | TokenType::Word) || self.inside_proc(),
            "{} Token type cannot be used outside of a procedure: `{:?}", tok.loc, tok.typ);
        
        Op::from(match tok.typ {
            TokenType::Keyword => return self.define_keyword_op(tok.operand, tok.loc),
            TokenType::Str => (OpType::PushStr, self.register_string(tok.operand), tok.loc),
            TokenType::DataType(typ) => {
                match typ {
                    ValueType::Type(_) => bail!("{} Value type not valid here: `{:?}`", tok.loc, typ),
                    _ => (OpType::PushData(typ), tok.operand, tok.loc),
                }
            },
            TokenType::DataPtr(typ) => bail!("{} Data pointer type not valid here: `{:?}`", tok.loc, typ),
            TokenType::Word => {
                let word = self.word_list.get(tok.operand as usize).expect("unreachable").to_owned();
                return choice!(
                    self.get_const_struct(&word, tok.loc.to_owned()),
                    self.get_offset(&word, tok.operand, tok.loc.to_owned()),
                    self.get_binding(&word, tok.loc.to_owned()),
                    self.get_intrinsic(&word, tok.loc.to_owned()),
                    self.get_local_mem(&word, tok.loc.to_owned()),
                    self.get_global_mem(&word, tok.loc.to_owned()),
                    self.get_proc_name(&word, tok.loc.to_owned()),
                    self.get_const_name(&word, tok.loc.to_owned()),
                    self.get_variable(&word, tok.loc.to_owned()),
                    self.define_context(&word, tok.loc.to_owned()),
                    Err(anyhow!("{} Word was not declared on the program: `{}`", tok.loc, word)))
            },
        }).into()
    }

    fn define_keyword_op(&mut self, operand: i32, loc: Loc) -> Result<Option<Vec<Op>>> {
        let key = FromPrimitive::from_i32(operand).expect("unreachable, lexer error");

        match key {
            KeywordType::Dup   => (OpType::Dup, loc).into(),
            KeywordType::Drop  => (OpType::Drop, loc).into(),
            KeywordType::Swap  => (OpType::Swap, loc).into(),
            KeywordType::Over  => (OpType::Over, loc).into(),
            KeywordType::Rot   => (OpType::Rot,  loc).into(),
            KeywordType::Equal => (OpType::Equal,loc).into(),
            KeywordType::At    => (OpType::Unpack, loc).into(),
            KeywordType::While => self.push_block((OpType::While, loc).into()),
            KeywordType::Do    => match self.pop_block(&loc, key)? {
                Op {typ: OpType::While, ..} => (OpType::Do, loc).into(),
                Op {typ: OpType::CaseMatch, operand, ..} => (OpType::CaseOption, operand, loc).into(),
                block => invalid_block(&loc, block, "`do` can only come in a `while` or `case` block")?
            },
            KeywordType::Let   => todo!(),
            KeywordType::Case  => todo!(),
            KeywordType::Colon => match self.pop_block(&loc, key)? {
                Op {typ: OpType::CaseStart, ..} =>  todo!(),
                block => invalid_block(&loc, block, "`:` can only be used on word or `case` block definition")?
            },
            KeywordType::If    => self.push_block((OpType::IfStart, loc).into()),
            KeywordType::Else  => match self.pop_block(&loc, key)? {
                Op {typ: OpType::IfStart, ..}    =>  todo!(),
                Op {typ: OpType::CaseOption, ..} =>  todo!(),
                block => invalid_block(&loc, block, "`else` can only come in a `if` or `case` block")?
            },
            KeywordType::End   => match self.pop_block(&loc, key)? {
                Op {typ: OpType::IfStart, ..} => (OpType::EndIf, loc).into(),
                Op {typ: OpType::Else, ..}    => (OpType::EndElse, loc).into(),
                Op {typ: OpType::Do, ..}      => (OpType::EndWhile, loc).into(),
                Op {typ: OpType::BindStack, ..}  => todo!(),
                Op {typ: OpType::CaseOption, ..} => todo!(),
                Op {typ: OpType::PrepProc, operand, ..} => (OpType::EndProc, operand, loc).into(),
                block => invalid_block(&loc, block, "Expected `end` to close a valid block")?
            },
            KeywordType::Include| KeywordType::Arrow | KeywordType::Proc | KeywordType::Mem |
            KeywordType::Struct => bail!("{} Keyword type is not valid here: `{:?}`", loc, key),
        }.into()
        
    }

    fn register_string(&mut self, operand: i32) -> i32 {
        if let Some(data) = self.data_list.get_mut(operand as usize){
            if data.offset == -1 {
                data.offset = self.total_data_size;
                self.total_data_size += data.size();
            }
        }
        operand
    }

    fn pop_block(&mut self, loc: &Loc, closing_type: KeywordType) -> Result<Op> {
        self.op_blocks.pop()
            .with_context(|| format!("{} There are no open blocks to close with `{:?}`", loc, closing_type))
    }

    fn push_block(&mut self, op: Op) -> Op {
        self.op_blocks.push(op.clone());
        op
    }

    fn get_intrinsic(&self, word: &str, loc: Loc) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word)
            .map(|i| vec![(OpType::Intrinsic, i.into(), loc).into()])
    }

    fn get_intrinsic_type(&self, word: &str) -> Option<IntrinsicType> {
        Some(match word {
            "+"   => IntrinsicType::Plus,
            "-"   => IntrinsicType::Minus,
            "*"   => IntrinsicType::Times,
            "%"   => IntrinsicType::Div,
            ">"   => IntrinsicType::Greater,
            ">="  => IntrinsicType::GreaterE,
            "<"   => IntrinsicType::Lesser,
            "<="  => IntrinsicType::LesserE,
            "or"  => IntrinsicType::Or,
            "and" => IntrinsicType::And,
            "xor" => IntrinsicType::Xor,
            "@8"  => IntrinsicType::Load8,
            "!8"  => IntrinsicType::Store8,
            "@16" => IntrinsicType::Load16,
            "!16" => IntrinsicType::Store16,
            "@32" => IntrinsicType::Load32,
            "!32" => IntrinsicType::Store32,
            "fd_write" => IntrinsicType::FdWrite,
            _ => IntrinsicType::Cast(self.parse_cast_type(word.strip_prefix('#')?)?)
        })
    }
    
    fn parse_cast_type(&self, word: &str) -> Option<i32> {
        self.parse_data_type(match word.strip_prefix('*') {
            Some(word) => word,
            None => word,
        })
    }
    
    fn parse_data_type(&self, word: &str) -> Option<i32> {
        self.struct_list.iter()
            .position(|s| s.name == word)
            .map(|u| u as i32)
    }

    #[allow(dead_code)]
    fn get_type_name(&self, word: &str) -> Option<&StructType> {
        self.struct_list.iter()
            .find(|s| s.name == word)
    }

    fn get_const_struct(&mut self, word: &str, loc: Loc) -> Result<Option<Vec<Op>>> {
        Ok(empty_or_some(
            flatten(
                self.def_ops_from_fields(self.get_const_struct_fields(word, loc))?
            )
        ))
    }

    fn get_const_struct_fields(&self, word: &str, loc: Loc) -> Vec<IRToken> {
        self.const_list.iter()
            .filter(|cnst| cnst.name().starts_with(&format!("{}.", word)))
            .map(|tword| (tword.clone(), loc.clone()).into())
            .collect()
    }

    fn def_ops_from_fields(&mut self, toks: Vec<IRToken>) -> Result<Vec<Vec<Op>>> {
        toks.into_iter()
            .filter_map(|tok| self.define_op(tok).transpose())
            .collect()
    }

    fn get_offset(&self, _word: &str, _operand: i32, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn get_binding(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn get_local_mem(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn get_global_mem(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn get_proc_name(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn get_const_name(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }
    
    fn get_variable(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn define_context(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn lex_file(&mut self, path: PathBuf) -> Result<&mut Self> {
        let reader = BufReader::new(File::open(&path)
            .with_context(|| format!("could not read file `{:?}`", &path))?);

        let current = path.to_owned();
        let mut lex = Lexer::new(reader, path);

        while let Some(token) = lex.lex_next_token(self)? {
            if !token.is_keyword(KeywordType::Include) {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = lex.lex_next_token(self)?
                .expect_by(|t| -> bool {t.typ == TokenType::Str}, "include file name")?;
                
            let include = get_dir(&current)
                .with_context(|| "failed to get file directory path")?
                .join(self.data_list.get(tok.operand as usize)
                .expect("unreachable, lexer error")
                .word.name.as_str());

            info!("Including file: {:?}", include);
            self.lex_file(include)?;
        };
        Ok(self)
    }
}

impl ExpectBy<IRToken> for Option<IRToken> {
    fn expect_by(self, pred: impl FnOnce(&IRToken) -> bool, desc: &str) -> Result<IRToken>{
        match self {
            Some(tok) if pred(&tok) => Ok(tok),
            Some(tok) => bail!("{} Expected to find {}, but found: `{}`", tok.loc, desc, tok),
            None => bail!("Expected to find {}, but found nothing", desc),
        }
    }
}

fn invalid_block(loc: &Loc, block: Op, error: &str) -> Result<Op> {
    bail!("{} {}, but found a `{:?}` block instead\n{} [INFO] The found block started here", loc, error, block.typ, block.loc)
}

pub fn compile_file(path: PathBuf) -> Result<()> {
    info!("Compiling file: {:?}", path);
    Parser::new()
        .lex_file(path)?
        .parse_tokens()
}
