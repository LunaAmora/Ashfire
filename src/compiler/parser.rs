use std::{io::BufReader, fs::File, path::PathBuf, collections::VecDeque};
use anyhow::{Context, Result, ensure, anyhow, bail};
use super::{lexer::Lexer, types::*};
use num::FromPrimitive;
use firelib::*;

#[derive(Default)]
pub struct Parser {
    pub word_list: Vec<String>,
    pub data_list: Vec<SizedWord>,
    pub total_data_size: i32,
    pub struct_list: Vec<StructType>,
    ir_tokens:    VecDeque<IRToken>,
    const_list:   Vec<TypedWord>,
    global_vars:  Vec<TypedWord>,
    program:      Vec<Op>,
    op_blocks:    Vec<Op>,
    struct_names: Vec<Word>,
    mem_list:     Vec<Word>,
    proc_list:    Vec<Proc>,
    current_proc: Option<usize>,
}

impl Parser {
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

    fn current_proc(&self) -> Option<&Proc> {
        self.current_proc.and_then(|index| self.proc_list.get(index))
    }

    fn current_proc_mut(&mut self) -> Option<&mut Proc> {
        self.current_proc.and_then(|index| self.proc_list.get_mut(index))
    }

    fn next_irtoken(&mut self) -> Option<IRToken> {
        self.ir_tokens.pop_front()
    }

    fn next_irtokens(&mut self, n: usize) -> Option<IRToken> {
        let mut result = None;
        for _ in 0..n {
            result = self.ir_tokens.pop_front()
        }
        result
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
                let word = &self.word_list.get(tok.operand as usize).expect("unreachable").to_owned();
                let loc = &tok.loc;
                return choice!(
                    self.get_const_struct(word, loc),
                    self.get_offset(word, tok.operand, loc),
                    self.get_binding(word, loc),
                    self.get_intrinsic(word, loc),
                    self.get_local_mem(word, loc),
                    self.get_global_mem(word, loc),
                    self.get_proc_name(word, loc),
                    self.get_const_name(word, loc),
                    self.get_variable(word, loc),
                    self.define_context(word, loc),
                    Err(anyhow!("{} Word was not declared on the program: `{}`", loc, word)))
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

    fn get_intrinsic(&self, word: &str, loc: &Loc) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word)
            .map(|i| vec![Op::new(OpType::Intrinsic, i.into(), loc)])
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
    
    fn get_word(&self, value: i32) -> String {
        self.word_list.get(value as usize).expect("unreachable, lexer error").to_owned()
    }

    fn try_get_word(&self, tok: &IRToken) -> Option<String> {
        if tok == TokenType::Word {
            Some(self.get_word(tok.operand))
        } else {
            None
        }
    }

    fn get_type_name(&self, word: &str) -> Option<&StructType> {
        self.struct_list.iter()
            .find(|s| s.name == word)
    }

    fn get_struct_type(&self, tok: &IRToken) -> Option<&StructType> {
        if tok == TokenType::Word {
            self.get_type_name(&self.get_word(tok.operand))
        } else {
            None
        }
    }

    fn get_data_pointer(&self, _found_word: &str) -> Option<TokenType> {
        todo!()
    }

    fn get_const_struct(&mut self, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        Ok(empty_or_some(
            flatten(
                self.def_ops_from_fields(self.get_const_struct_fields(word, loc))?
            )
        ))
    }

    fn get_const_struct_fields(&self, word: &str, loc: &Loc) -> Vec<IRToken> {
        self.const_list.iter()
            .filter(|cnst| cnst.name().starts_with(&format!("{}.", word)))
            .map(|tword| (tword, loc).into())
            .collect()
    }

    fn def_ops_from_fields(&mut self, toks: Vec<IRToken>) -> Result<Vec<Vec<Op>>> {
        toks.into_iter()
            .filter_map(|tok| self.define_op(tok).transpose())
            .collect()
    }

    fn get_offset(&self, word: &str, operand: i32, loc: &Loc) -> Option<Vec<Op>> {
        word.strip_prefix('.').map(|word| Op::from(
            word.strip_prefix('*').map_or_else(|| 
                (OpType::OffsetLoad, operand, loc.clone()), |_|
                (OpType::Offset, operand, loc.clone())
            )
        ).into())
    }

    fn get_binding(&self, word: &str, loc: &Loc) -> Option<Vec<Op>> {
        self.current_proc()
            .and_then(|proc| proc.bindings.iter().position(|bind| bind.eq(word))
            .map(|index| (proc.bindings.len() - 1 - index) as i32))
            .map(|index| Op::new(OpType::PushBind, index, loc).into())
    }

    fn get_local_mem(&self, word: &str, loc: &Loc) -> Option<Vec<Op>> {
        self.current_proc()
            .and_then(|proc| proc.local_mem_names.iter().find(|mem| mem.name.eq(word)))
            .map(|local| Op::new(OpType::PushLocalMem, local.value, loc).into())
    }

    fn get_global_mem(&self, word: &str, loc: &Loc) -> Option<Vec<Op>> {
        self.mem_list.iter()
            .find(|mem| mem.name.eq(word))
            .map(|global| Op::new(OpType::PushGlobalMem, global.value, loc).into())
    }

    fn get_proc_name(&self, word: &str, loc: &Loc) -> Option<Vec<Op>> {
        self.proc_list.iter()
            .position(|proc| proc.name.eq(word))
            .map(|index| Op::new(OpType::Call, index as i32, loc).into())
    }

    fn get_const_name(&mut self, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        self.const_list.iter()
            .find(|cnst| cnst.name().eq(word))
            .map(|tword| (tword, loc).into())
            .map_or_else(|| Ok(None), |tok| self.define_op(tok))
    }
    
    fn get_variable(&mut self, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        let (word, word_type) =
            word.strip_prefix('!').map_or_else(|| 
            word.strip_prefix('*').map_or_else(|| 
                (word, None), |word|
                (word, Some(VarWordType::Pointer))), |word|
                (word, Some(VarWordType::Store)));
        
        choice!(self.current_proc()
                .map(|proc| proc.local_vars.clone())
                .map_or(Ok(None),|proc_vars|
            self.try_get_var(word, loc, proc_vars, true, word_type)),
            self.try_get_var(word, loc, self.global_vars.clone(), false, word_type))
    }

    fn try_get_var(&mut self, _word: &str, _loc: &Loc, _vars: Vec<TypedWord>, _local: bool, _word_type: Option<VarWordType>) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn define_context(&mut self, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        let mut i = 0;
        let mut colons = 0;

        while let Some(tok) = self.ir_tokens.get(i).cloned() { i += 1;
            match (colons, &tok.typ) {
                (1, TokenType::DataType(ValueType::Int))
                    => return self.parse_memory(word, loc),
                (1, TokenType::Word) => return choice!(
                    self.parse_proc_ctx(self.get_word(tok.operand), word, loc),
                    self.parse_struct_ctx(i, word, loc),
                    map_res(self.invalid_token(tok, "context declaration"))
                ),
                (_, TokenType::Keyword) => match choice!(
                            self.parse_keyword_ctx(&mut colons, word, tok, loc),
                            self.parse_end_ctx(colons, i, word, loc)) {
                        Ok(None) => {},
                        result => return result
                },
                (0, _) => return self.parse_word_ctx(&tok, word, loc),
                _ => return map_res(self.invalid_token(tok, "context declaration"))
            }
        }
        Ok(None)
    }

    fn parse_proc_ctx(&mut self, found_word: String, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        if self.get_type_name(&found_word).is_some() ||
            self.get_data_pointer(&found_word).is_some() {
            return self.parse_procedure(word, loc);
        }
        Ok(None)
    }

    fn parse_struct_ctx(&mut self, top_index: usize, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        if let (Some(n1), Some(n2)) = (self.ir_tokens.get(top_index+1), self.ir_tokens.get(top_index+2)) {
            if self.get_struct_type(n1).is_some() &&
                equals_any!(n2, KeywordType::End, TokenType::Word) {
                return self.parse_struct(word, loc);
            }
        }
        Ok(None)
    }

    fn parse_keyword_ctx(&mut self, colons: &mut i32, word: &str, tok: IRToken, loc: &Loc) -> Result<Option<Vec<Op>>> {
        match (*colons, from_primitive(tok.operand)) {
            (0, KeywordType::Mem) => {
                self.next_irtoken();
                self.parse_memory(word, loc)
            },
            (0, KeywordType::Struct) => {
                self.next_irtoken();
                self.parse_struct(word, loc)
            },
            (0, KeywordType::Proc) => {
                self.next_irtoken();
                self.parse_procedure(word, loc)
            },
            (1, KeywordType::Equal) => {
                self.next_irtokens(2);
                if let Some((eval, skip)) = self.compile_eval() {
                    ensure!(eval.typ != ValueType::Any, "{} Undefined variable value is not allowed", loc);
                    self.next_irtokens(skip);
                    self.register_var((word, eval.operand, eval.typ).into());
                    return map_res(Ok(()))
                }
                map_res(self.invalid_token(tok, "context declaration")) 
            },
            (_, KeywordType::Colon) => {
                *colons += 1;
                Ok(None)
            },
            (_, KeywordType::End) => {
                bail!("{} Missing body or contract necessary to infer the type of the word: `{word}`", loc)
            },
            _ => Ok(None)
        }
    }

    fn parse_end_ctx(&mut self, colons: i32, ctx_size: usize, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        if colons != 2 { return Ok(None)}
        choice!(self.parse_static_ctx(ctx_size, word, loc),
            map_res_t(self.define_proc(word, loc, Contract::default())))
    }

    fn parse_static_ctx(&mut self, ctx_size: usize, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        if ctx_size != 1 {
            return self.parse_procedure(word, loc);
        }
        
        self.next_irtokens(2);
        if let Some((eval, skip)) = self.compile_eval() {
            if eval.typ != ValueType::Any {
                self.next_irtokens(skip);
                self.const_list.push((word, eval.operand, eval.typ).into());
                return map_res(Ok(()));
            }
        }
        Ok(None)
    }

    fn parse_word_ctx(&mut self, tok: &IRToken, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        if tok == TokenType::Word {
            if let Some(stk) = self.get_struct_type(tok).cloned() {
                self.next_irtoken();
                return map_res(self.parse_const_or_var(word, loc, tok.operand, stk));
            }
        }
        Ok(None)
    }

    fn define_proc(&mut self, name: &str, loc: &Loc, contract: Contract) -> Result<Op> {
        ensure!(self.inside_proc(), "{} Cannot define a procedure inside of another procedure", loc);
        
        let proc = Proc::new(name, contract);
        let operand = self.proc_list.len();
        self.current_proc = Some(operand);
        self.proc_list.push(proc);

        Ok(self.push_block(Op::new(OpType::PrepProc, operand  as i32, loc)))
    }

    fn compile_eval_n(&mut self, _n: usize) -> Option<(Vec<IRToken>, usize)> {
        warn!("Todo: Compile time evaluation not implemented yet");
        None
    }
    
    fn compile_eval(&mut self) -> Option<(IRToken, usize)> {
        warn!("Todo: Compile time evaluation not implemented yet");
        None
    }
    
    fn invalid_token(&self, tok: IRToken, error_context: &str) -> Result<()> {
        let (found_desc, found_name) = match tok.typ {
            TokenType::Keyword => ("keyword", format!("{:?}", from_primitive::<KeywordType>(tok.operand))),
            TokenType::Word    => ("word or intrinsic", self.get_word(tok.operand)),
            _ => ("token", format!("{:?}", tok.typ))
        };
        bail!("{} Invalid {found_desc} found on {error_context}: `{found_name}`", tok.loc)
    }

    fn parse_procedure(&mut self, word: &str, loc: &Loc) -> Result<Option<Vec<Op>>> {
        let mut ins  = Vec::new();
        let mut outs = Vec::new();
        let mut found_arrow = false;

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `proc`", loc)?;
        let error_text = "Expected proc contract or `:` after procedure definition, but found";

        while let Some(tok) = self.next_irtoken() {
            if let Some(key) = tok.get_keyword() {
                match key {
                    KeywordType::Arrow => {
                        ensure!(!found_arrow, "{} Duplicated `->` found on procedure definition", loc);
                        found_arrow = true;
                    },
                    KeywordType::Colon => {
                        return map_res_t(self.define_proc(word, loc, Contract {ins, outs}));
                    },
                    _ => bail!("{}, {}: `{:?}`", loc, error_text, key),
                }
            } else if let Some(found_word) = self.try_get_word(&tok) {
                if let Some(stk) = self.get_type_name(&found_word) {
                    for member in stk.members.iter() {
                        push_by_condition(found_arrow, member.typ, &mut outs, &mut ins);
                    }
                } else if let Some(type_ptr) = self.get_data_pointer(&found_word) {
                    push_by_condition(found_arrow, type_ptr, &mut outs, &mut ins);
                } else {
                    bail!("{}, {} the Word: `{:?}`", loc, error_text, found_word);
                }
            } else {
                bail!("{}, {}: `{:?}`", loc, error_text, tok.typ);
            }
        }
        bail!("{}, {} nothing", loc, error_text);
    }

    fn expect_keyword(&mut self, key: KeywordType, error_text: &str, loc: &Loc) -> Result<IRToken> {
        self.expect_next_by(|tok| tok == key, error_text, loc)
    }

    fn expect_next_by(&mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: &str, loc: &Loc) -> Result<IRToken> {
        let tok = self.next_irtoken();
        let none = tok.is_none();
        tok.expect_by(pred, error_text).map_err(|err| 
            if none {
                let context = format!("{} {}", loc, err);
                err.context(context)
            } else {
                err
            }
        )
    }

    fn parse_memory(&mut self, _word: &str, _loc: &Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn parse_struct(&mut self, _word: &str, _loc: &Loc) -> Result<Option<Vec<Op>>> {
        todo!()
    }

    fn parse_const_or_var(&mut self, word: &str, loc: &Loc, operand: i32, stk: StructType) -> Result<()> {
        self.expect_keyword(KeywordType::Colon,  "`:` after variable type definition" , loc)?;

        let assign = self.expect_next_by(|tok|
            equals_any!(tok, KeywordType::Colon, KeywordType::Equal),
            "`:` or `=` after keyword `:`", loc)?
            .get_keyword().expect("unreachable");
        
        
        let (mut result, skip) = self.compile_eval_n(stk.members.len())
            .with_context(|| format!("{} Failed to parse an valid struct value at compile-time evaluation", loc))?;

        let end_token = self.next_irtokens(skip).expect("unreachable");
        let mut members = stk.members;
        members.reverse();

        if result.len() == 1 {
            if let Some(eval) = result.pop() {
                if eval.typ == ValueType::Any {
                    for member in members {
                        let name = format!("{word}.{}", member.name);
                        let struct_word = (name.as_str(), member.default_value, member.typ).into();
                        self.register_typed_word(&assign, struct_word);
                    }
                } else {
                    let member_type = members.pop().expect("unreachable").typ;
                    ensure!(equals_any!(member_type, ValueType::Any, eval.typ),
                            "{} Expected type `{:?}` on the stack at the end of the compile-time evaluation, but found: `{:?}`",
                            end_token.loc, member_type, eval.typ);
                    
                    let struct_word = (word, eval.operand, member_type).into();
                    self.register_typed_word(&assign, struct_word);
                }
            }
        } else {
            warn!("Todo: Typechecking stack elements is not implemented yet");

            if !self.inside_proc() {
                members.reverse();
                result.reverse();
            }

            for index in 0..members.len() {
                if let (Some(member), Some(item)) = (members.get(index), result.get(index)) {
                    let name = format!("{word}.{}", member.name);
                    let struct_word = (name.as_str(), item.operand, item.typ).into();
                    self.register_typed_word(&assign, struct_word);
                }
            }
        }
    
        self.struct_names.push(Word::new(word, operand));
        Ok(())
    }

    fn register_typed_word(&mut self, assign: &KeywordType, struct_word: TypedWord) {
        match assign {
            KeywordType::Colon => self.const_list.push(struct_word),
            _                  => self.register_var(struct_word),
        }
    }

    fn register_var(&mut self, struct_word: TypedWord) {
        match self.current_proc_mut() {
            Some(proc) => proc.local_vars.push(struct_word),
            _          => self.global_vars.push(struct_word)
        }
    }

    fn lex_file(&mut self, path: PathBuf) -> Result<&mut Self> {
        let reader = BufReader::new(File::open(&path)
            .with_context(|| format!("could not read file `{:?}`", &path))?);
        let mut lex = Lexer::new(reader, &path);

        while let Some(token) = lex.lex_next_token(self)? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = lex.lex_next_token(self)?
                .expect_by(|tok| tok == TokenType::Str, "include file name")?;
                
            let include = get_dir(&path)
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
