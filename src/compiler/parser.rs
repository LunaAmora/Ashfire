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
    mem_list: Vec<Word>,
    proc_list: Vec<Proc>,
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
        let key = from_primitive(operand);

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

    fn get_type_name(&self, word: &str) -> Option<&StructType> {
        self.struct_list.iter()
            .find(|s| s.name == word)
    }

    fn get_data_pointer(&self, _found_word: &str) -> Option<Vec<Op>> {
        todo!()
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

    fn get_offset(&self, word: &str, operand: i32, loc: Loc) -> Option<Vec<Op>> {
        word.strip_prefix('.').map(|word| Op::from(
            word.strip_prefix('*').map_or_else(|| 
                (OpType::OffsetLoad, operand, loc.clone()), |_|
                (OpType::Offset, operand, loc.clone())
            )
        ).into())
    }

    fn get_binding(&self, word: &str, loc: Loc) -> Option<Vec<Op>> {
        self.current_proc
            .and_then(|proc| proc.bindings.iter().position(|bind| bind.eq(word))
            .map(|index| (proc.bindings.len() - 1 - index) as i32))
            .map(|index| Op::from((OpType::PushBind, index, loc)).into())
    }

    fn get_local_mem(&self, word: &str, loc: Loc) -> Option<Vec<Op>> {
        self.current_proc
            .and_then(|proc| proc.local_mem_names.iter().find(|mem| mem.name.eq(word)))
            .map(|local| Op::from((OpType::PushLocalMem, local.value, loc)).into())
    }

    fn get_global_mem(&self, word: &str, loc: Loc) -> Option<Vec<Op>> {
        self.mem_list.iter()
            .find(|mem| mem.name.eq(word))
            .map(|global| Op::from((OpType::PushGlobalMem, global.value, loc)).into())
    }

    fn get_proc_name(&self, word: &str, loc: Loc) -> Option<Vec<Op>> {
        self.proc_list.iter()
            .position(|proc| proc.name.eq(word))
            .map(|index| Op::from((OpType::Call, index as i32, loc)).into())
    }

    fn get_const_name(&mut self, word: &str, loc: Loc) -> Result<Option<Vec<Op>>> {
        self.const_list.iter()
            .find(|cnst| cnst.name().eq(word))
            .map(|tword| (tword.clone(), loc).into())
            .map_or_else(|| Ok(None), |tok| self.define_op(tok))
    }
    
    fn get_variable(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn define_context(&mut self, word: &str, loc: Loc) -> Result<Option<Vec<Op>>> {
        let mut i = 0;
        let mut colon_count = 0;

        while let Some(tok) = self.ir_tokens.get(i) { i += 1;
            match (colon_count, &tok.typ) {
                (0, _) => {
                    if tok.typ == TokenType::Word {
                        if let Some(stk) = self.get_type_name(self.get_word(tok.operand)).cloned() {
                            let operand = tok.operand;
                            self.next_irtoken();
                            return self.parse_const_or_var(word, loc, operand, stk);
                        }
                    }
                    return Ok(None);
                },
                (1, TokenType::DataType(ValueType::Int)) => {
                    return self.parse_context(KeywordType::Mem, word, loc)
                },
                (1, TokenType::Word) => {
                    let found_word = self.get_word(tok.operand);

                    if self.get_type_name(found_word).is_some() ||
                        self.get_data_pointer(found_word).is_some() {
                        return self.parse_context(KeywordType::Proc, word, loc)
                    }
                    
                    if let (Some(n1), Some(n2)) = (self.ir_tokens.get(i+1), self.ir_tokens.get(i+2)){
                        if n1.typ == TokenType::Word && self.get_type_name(self.get_word(n1.operand)).is_some() &&
                            ((n2.typ == TokenType::Keyword && KeywordType::End == from_primitive(n2.operand)) ||
                                n2.typ == TokenType::Word) {
                            return self.parse_context(KeywordType::Struct, word, loc)
                        }
                    }

                    return self.invalid_token(tok, "context declaration");
                },
                (_, TokenType::Keyword) => {
                    let context = from_primitive(tok.operand);
                    match context {
                        KeywordType::Proc | KeywordType::Mem | KeywordType::Struct if colon_count == 0 => {
                            self.next_irtoken();
                            return self.parse_context(context, word, loc)
                        },
                        KeywordType::End => {
                            bail!("{} Missing body or contract necessary to infer the type of the word: `{word}`", loc)
                        },
                        KeywordType::Equal if colon_count == 1 =>
                        {
                            // self.next_irtoken();
                            // self.next_irtoken();
                            todo!("Compile eval");
                            // return self.invalid_token(token, "context declaration");
                        },
                        KeywordType::Colon => colon_count += 1,
                        _ => {}
                    }

                    if colon_count == 2 {
                        if i != 1 {
                            return self.parse_context(KeywordType::Proc, word, loc)
                        }

                        // self.next_irtoken();
                        // self.next_irtoken();
                        todo!("Compile eval or proc definition");
                    }
                },
                _ => return self.invalid_token(tok, "context declaration")
            }
        }
        Ok(None)
    }

    fn invalid_token(&self, tok: &IRToken, error_context: &str) -> Result<Option<Vec<Op>>> {
        let (found_desc, found_name) = match tok.typ {
            TokenType::Keyword => ("keyword", format!("{:?}", from_primitive::<KeywordType>(tok.operand))),
            TokenType::Word    => ("word or intrinsic", self.get_word(tok.operand).to_owned()),
            _ => ("token", format!("{:?}", tok.typ))
        };
        bail!("{} Invalid {found_desc} found on {error_context}: `{found_name}`", tok.loc)
    }

    fn get_word(&self, value: i32) -> &String {
        self.word_list.get(value as usize).expect("unreachable, lexer error")
    }

    fn parse_context(&mut self, context: KeywordType, word: &str, loc: Loc) -> Result<Option<Vec<Op>>> {
        match context {
            KeywordType::Proc   => self.parse_procedure(word, Op::from((OpType::PrepProc, loc))),
            KeywordType::Mem    => self.parse_memory(word, loc),
            KeywordType::Struct => self.parse_struct(word, loc),
            _ => Ok(None)
        }
    }

    fn parse_procedure(&self, _word: &str, _loc: Op) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn parse_memory(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn parse_struct(&self, _word: &str, _loc: Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn parse_const_or_var(&mut self, _word: &str, _loc: Loc, _operand: i32, _stk: StructType) -> Result<Option<Vec<Op>>> {
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

fn from_primitive<T: FromPrimitive>(value: i32) -> T {
    FromPrimitive::from_i32(value).expect("unreachable, lexer error")
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
