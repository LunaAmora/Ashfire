use super::{lexer::Lexer, types::*};
use anyhow::{anyhow, bail, ensure, Context, Result};
use firelib::*;
use num::FromPrimitive;
use std::{collections::VecDeque, fs::File, io::BufReader, path::PathBuf};

#[derive(Default)]
pub struct Parser {
    pub word_list: Vec<String>,
    pub data_list: Vec<SizedWord>,
    pub total_mem_size: i32,
    pub total_data_size: i32,
    pub struct_list: Vec<StructType>,
    ir_tokens: VecDeque<IRToken>,
    const_list: Vec<TypedWord>,
    global_vars: Vec<TypedWord>,
    program: Vec<Op>,
    op_blocks: Vec<Op>,
    struct_names: Vec<Word>,
    mem_list: Vec<Word>,
    proc_list: Vec<Proc>,
    current_proc: Option<usize>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            struct_list: vec![
                ("int", ValueType::Int).into(),
                ("bool", ValueType::Bool).into(),
                ("ptr", ValueType::Ptr).into(),
                ("any", ValueType::Any).into(),
            ],
            ..Default::default()
        }
    }

    fn inside_proc(&self) -> bool {
        self.current_proc.is_some()
    }

    fn current_proc(&self) -> Option<&Proc> {
        self.current_proc
            .and_then(|index| self.proc_list.get(index))
    }

    fn current_proc_mut(&mut self) -> Option<&mut Proc> {
        self.current_proc
            .and_then(|index| self.proc_list.get_mut(index))
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
        ensure!(
            matches!(tok.typ, TokenType::Keyword | TokenType::Word) || self.inside_proc(),
            "{} Token type cannot be used outside of a procedure: `{:?}",
            tok.loc,
            tok.typ
        );

        Op::from(match tok.typ {
            TokenType::Keyword => return self.define_keyword_op(tok.operand, tok.loc),
            TokenType::Str => (OpType::PushStr, self.register_string(tok.operand), tok.loc),
            TokenType::DataType(typ) => match typ {
                ValueType::Type(_) => bail!("{} Value type not valid here: `{:?}`", tok.loc, typ),
                _ => (OpType::PushData(typ), tok.operand, tok.loc),
            },
            TokenType::DataPtr(typ) =>
                bail!("{} Data pointer type not valid here: `{:?}`", tok.loc, typ),
            TokenType::Word => {
                let loc_word = &LocWord::new(
                    tok.loc,
                    self.word_list
                        .get(tok.operand as usize)
                        .expect("unreachable")
                        .to_owned(),
                );
                return choice!(
                    self.get_const_struct(loc_word),
                    self.get_offset(loc_word, tok.operand),
                    self.get_binding(loc_word),
                    self.get_intrinsic(loc_word),
                    self.get_local_mem(loc_word),
                    self.get_global_mem(loc_word),
                    self.get_proc_name(loc_word),
                    self.get_const_name(loc_word),
                    self.get_variable(loc_word),
                    self.define_context(loc_word),
                    Err(anyhow!(
                        "{} Word was not declared on the program: `{}`",
                        loc_word.loc,
                        loc_word.name
                    ))
                );
            }
        })
        .into()
    }

    fn define_keyword_op(&mut self, operand: i32, loc: Loc) -> Result<Option<Vec<Op>>> {
        let key = from_primitive(operand);

        match key {
            KeywordType::Dup => (OpType::Dup, loc).into(),
            KeywordType::Drop => (OpType::Drop, loc).into(),
            KeywordType::Swap => (OpType::Swap, loc).into(),
            KeywordType::Over => (OpType::Over, loc).into(),
            KeywordType::Rot => (OpType::Rot, loc).into(),
            KeywordType::Equal => (OpType::Equal, loc).into(),
            KeywordType::At => (OpType::Unpack, loc).into(),
            KeywordType::While => self.push_block((OpType::While, loc).into()),
            KeywordType::Do => match self.pop_block(&loc, key)? {
                Op { typ: OpType::While, .. } => (OpType::Do, loc).into(),
                Op { typ: OpType::CaseMatch, operand, .. } =>
                    (OpType::CaseOption, operand, loc).into(),
                block =>
                    invalid_block(&loc, block, "`do` can only come in a `while` or `case` block")?,
            },
            KeywordType::Let => todo!(),
            KeywordType::Case => todo!(),
            KeywordType::Colon => match self.pop_block(&loc, key)? {
                Op { typ: OpType::CaseStart, .. } => todo!(),
                block => invalid_block(
                    &loc,
                    block,
                    "`:` can only be used on word or `case` block definition",
                )?,
            },
            KeywordType::If => self.push_block((OpType::IfStart, loc).into()),
            KeywordType::Else => match self.pop_block(&loc, key)? {
                Op { typ: OpType::IfStart, .. } => todo!(),
                Op { typ: OpType::CaseOption, .. } => todo!(),
                block =>
                    invalid_block(&loc, block, "`else` can only come in a `if` or `case` block")?,
            },
            KeywordType::End => match self.pop_block(&loc, key)? {
                Op { typ: OpType::IfStart, .. } => (OpType::EndIf, loc).into(),
                Op { typ: OpType::Else, .. } => (OpType::EndElse, loc).into(),
                Op { typ: OpType::Do, .. } => (OpType::EndWhile, loc).into(),
                Op { typ: OpType::BindStack, .. } => todo!(),
                Op { typ: OpType::CaseOption, .. } => todo!(),
                Op { typ: OpType::PrepProc, operand, .. } =>
                    self.exit_proc((OpType::EndProc, operand, loc).into()),
                block => invalid_block(&loc, block, "Expected `end` to close a valid block")?,
            },
            KeywordType::Include |
            KeywordType::Arrow |
            KeywordType::Proc |
            KeywordType::Mem |
            KeywordType::Struct => bail!("{} Keyword type is not valid here: `{:?}`", loc, key),
        }
        .into()
    }

    fn register_string(&mut self, operand: i32) -> i32 {
        if let Some(data) = self.data_list.get_mut(operand as usize) {
            if data.offset == -1 {
                data.offset = self.total_data_size;
                self.total_data_size += data.size();
            }
        }
        operand
    }

    fn pop_block(&mut self, loc: &Loc, closing_type: KeywordType) -> Result<Op> {
        self.op_blocks.pop().with_context(|| {
            format!("{} There are no open blocks to close with `{:?}`", loc, closing_type)
        })
    }

    fn push_block(&mut self, op: Op) -> Op {
        self.op_blocks.push(op.clone());
        op
    }

    fn exit_proc(&mut self, op: Op) -> Op {
        self.current_proc = None;
        op
    }

    fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word)
            .map(|i| vec![Op::new(OpType::Intrinsic, i.into(), &word.loc)])
    }

    fn get_intrinsic_type(&self, word: &str) -> Option<IntrinsicType> {
        Some(match word {
            "+" => IntrinsicType::Plus,
            "-" => IntrinsicType::Minus,
            "*" => IntrinsicType::Times,
            "%" => IntrinsicType::Div,
            ">" => IntrinsicType::Greater,
            ">=" => IntrinsicType::GreaterE,
            "<" => IntrinsicType::Lesser,
            "<=" => IntrinsicType::LesserE,
            "or" => IntrinsicType::Or,
            "and" => IntrinsicType::And,
            "xor" => IntrinsicType::Xor,
            "@8" => IntrinsicType::Load8,
            "!8" => IntrinsicType::Store8,
            "@16" => IntrinsicType::Load16,
            "!16" => IntrinsicType::Store16,
            "@32" => IntrinsicType::Load32,
            "!32" => IntrinsicType::Store32,
            "fd_write" => IntrinsicType::FdWrite,
            _ => IntrinsicType::Cast(self.parse_cast_type(word.strip_prefix('#')?)?),
        })
    }

    fn parse_cast_type(&self, word: &str) -> Option<i32> {
        match word.strip_prefix('*') {
            Some(_) => todo!("cast as typed pointer not implemented"),
            None => self.parse_data_type(word).map(|u| (u as i32)),
        }
    }

    fn parse_data_type(&self, word: &str) -> Option<usize> {
        self.struct_list.iter().position(|s| s.name == word)
    }

    fn get_word(&self, value: i32) -> String {
        self.word_list
            .get(value as usize)
            .expect("unreachable, lexer error")
            .to_owned()
    }

    fn try_get_word(&self, tok: &IRToken) -> Option<String> {
        if tok == TokenType::Word {
            Some(self.get_word(tok.operand))
        } else {
            None
        }
    }

    fn get_type_name(&self, word: &str) -> Option<&StructType> {
        self.struct_list.iter().find(|s| s.name == word)
    }

    fn get_struct_type(&self, tok: &IRToken) -> Option<&StructType> {
        if tok == TokenType::Word {
            self.get_type_name(&self.get_word(tok.operand))
        } else {
            None
        }
    }

    fn get_data_pointer(&self, word: &str) -> Option<TokenType> {
        word.strip_prefix('*')
            .and_then(|word| self.parse_data_type(word))
            .map(|i| TokenType::DataPtr(ValueType::from(i)))
    }

    fn get_const_struct(&mut self, word: &LocWord) -> Result<Option<Vec<Op>>> {
        Ok(empty_or_some(flatten(
            self.def_ops_from_fields(self.get_const_struct_fields(word))?,
        )))
    }

    fn get_const_struct_fields(&self, word: &LocWord) -> Vec<IRToken> {
        self.const_list
            .iter()
            .filter(|cnst| cnst.starts_with(&format!("{}.", word.name)))
            .map(|tword| (tword, &word.loc).into())
            .collect()
    }

    fn def_ops_from_fields(&mut self, toks: Vec<IRToken>) -> Result<Vec<Vec<Op>>> {
        toks.into_iter()
            .filter_map(|tok| self.define_op(tok).transpose())
            .collect()
    }

    fn get_offset(&self, word: &LocWord, operand: i32) -> Option<Vec<Op>> {
        word.strip_prefix('.').map(|strip| {
            Op::from(strip.strip_prefix('*').map_or_else(
                || (OpType::OffsetLoad, operand, word.loc.clone()),
                |_| (OpType::Offset, operand, word.loc.clone()),
            ))
            .into()
        })
    }

    fn get_binding(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.current_proc()
            .and_then(|proc| {
                proc.bindings
                    .iter()
                    .position(|bind| word == bind)
                    .map(|index| (proc.bindings.len() - 1 - index) as i32)
            })
            .map(|index| Op::new(OpType::PushBind, index, &word.loc).into())
    }

    fn get_local_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.current_proc()
            .and_then(|proc| proc.local_mem_names.iter().find(|mem| word == &mem.name))
            .map(|local| Op::new(OpType::PushLocalMem, local.value, &word.loc).into())
    }

    fn get_global_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.mem_list
            .iter()
            .find(|mem| word == &mem.name)
            .map(|global| Op::new(OpType::PushGlobalMem, global.value, &word.loc).into())
    }

    fn get_proc_name(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.proc_list
            .iter()
            .position(|proc| word == &proc.name)
            .map(|index| Op::new(OpType::Call, index as i32, &word.loc).into())
    }

    fn try_get_const_name(&self, word: &LocWord) -> Option<&TypedWord> {
        self.const_list.iter().find(|cnst| word == cnst.as_str())
    }

    fn get_const_name(&mut self, word: &LocWord) -> Result<Option<Vec<Op>>> {
        self.try_get_const_name(word)
            .map(|tword| (tword, &word.loc).into())
            .map_or_else(|| Ok(None), |tok| self.define_op(tok))
    }

    fn get_variable(&mut self, loc_word: &LocWord) -> Result<Option<Vec<Op>>> {
        let (word, var_typ) = match loc_word.split_at(1) {
            ("!", rest) => (rest, Some(VarWordType::Store)),
            ("*", rest) => (rest, Some(VarWordType::Pointer)),
            _ => (loc_word.as_str(), None),
        };

        let word = LocWord { name: word.to_string(), loc: loc_word.loc.clone() };
        choice!(
            self.current_proc()
                .map(|proc| proc.local_vars.clone())
                .map_or(Ok(None), |vars| self.try_get_var(&word, vars, true, var_typ)),
            self.try_get_var(&word, self.global_vars.clone(), false, var_typ)
        )
    }

    fn try_get_var(
        &mut self, word: &LocWord, vars: Vec<TypedWord>, local: bool, var_typ: Option<VarWordType>,
    ) -> Result<Option<Vec<Op>>> {
        let mut result = Vec::new();
        let loc = &word.loc;

        let push_type = if local {
            OpType::PushLocal
        } else {
            OpType::PushGlobal
        };

        let (store, pointer) = match var_typ {
            Some(VarWordType::Store) => (true, false),
            Some(VarWordType::Pointer) => (false, true),
            _ => Default::default(),
        };

        if let Some(index) = vars.iter().position(|name| *word == **name) {
            let typ = vars.get(index).expect("unreachable").typ;
            if store {
                result.push(Op::new(OpType::ExpectType, typ.into(), loc))
            }

            result.push(Op::new(push_type, index as i32, loc));

            if store {
                result.push(Op::new(OpType::Intrinsic, IntrinsicType::Store32.into(), loc))
            } else if pointer {
                let ptr_typ = IntrinsicType::Cast(-i32::from(typ) - 1);
                result.push(Op::new(OpType::Intrinsic, ptr_typ.into(), loc));
            } else {
                let data_typ = IntrinsicType::Cast(typ.into());
                result.push(Op::new(OpType::Intrinsic, IntrinsicType::Load32.into(), loc));
                result.push(Op::new(OpType::Intrinsic, data_typ.into(), loc));
            }

            return Ok(Some(result));
        }

        if let Some(struct_type) = self.try_get_struct_type(word) {
            let mut member = struct_type.members.first().expect("unreachable");
            if pointer {
                let pattern = &format!("{}.{}", word.name, member.name);
                let index = vars
                    .iter()
                    .position(|name| name.eq(pattern))
                    .expect("unreachable");

                let stk_id = self
                    .parse_data_type(struct_type.name.as_str())
                    .expect("unreachable");

                let ptr_typ = IntrinsicType::Cast(-(stk_id as i32) - 1);

                result.push(Op::new(push_type, index as i32, loc));
                result.push(Op::new(OpType::Intrinsic, ptr_typ.into(), loc));
            } else {
                let mut members = struct_type.members.clone();
                if store {
                    members.reverse();
                    member = struct_type.members.last().expect("unreachable");
                }

                let pattern = &format!("{}.{}", word.name, member.name);
                let index = vars
                    .iter()
                    .position(|name| name.eq(pattern))
                    .expect("unreachable") as i32;

                for (i, member) in (0_i32..).zip(members.into_iter()) {
                    let operand = index + if local == store { i } else { -i };

                    if store {
                        result.push(Op::new(OpType::ExpectType, member.typ.into(), loc))
                    }

                    result.push(Op::new(push_type, operand, loc));

                    if store {
                        result.push(Op::new(OpType::Intrinsic, IntrinsicType::Store32.into(), loc))
                    } else {
                        let data_typ = IntrinsicType::Cast(member.typ.into());
                        result.push(Op::new(OpType::Intrinsic, IntrinsicType::Load32.into(), loc));
                        result.push(Op::new(OpType::Intrinsic, data_typ.into(), loc));
                    }
                }
            }

            return Ok(Some(result));
        }

        Ok(None)
    }

    fn try_get_struct_type(&self, word: &LocWord) -> Option<&StructType> {
        self.struct_names
            .iter()
            .find(|stk| *word == stk.name)
            .and_then(|stk| {
                self.word_list
                    .get(stk.value as usize)
                    .and_then(|stk| self.get_type_name(stk))
            })
    }

    fn define_context(&mut self, word: &LocWord) -> Result<Option<Vec<Op>>> {
        let mut i = 0;
        let mut colons = 0;
        while let Some(tok) = self.ir_tokens.get(i).cloned() {
            match (colons, &tok.typ) {
                (1, TokenType::DataType(ValueType::Int)) =>
                    return map_res(self.parse_memory(word)),
                (1, TokenType::Word) =>
                    return choice!(
                        self.parse_proc_ctx(self.get_word(tok.operand), word),
                        self.parse_struct_ctx(i, word),
                        map_res(self.invalid_token(tok, "context declaration"))
                    ),
                (_, TokenType::Keyword) => match choice!(
                    self.parse_keyword_ctx(&mut colons, word, tok),
                    self.parse_end_ctx(colons, i, word)
                ) {
                    Ok(None) => {}
                    result => return result,
                },
                (0, _) => return self.parse_word_ctx(&tok, word),
                _ => return map_res(self.invalid_token(tok, "context declaration")),
            }
            i += 1;
        }
        Ok(None)
    }

    fn parse_proc_ctx(&mut self, found_word: String, word: &LocWord) -> Result<Option<Vec<Op>>> {
        if self.get_type_name(&found_word).is_some() || self.get_data_pointer(&found_word).is_some()
        {
            return self.parse_procedure(word);
        }
        Ok(None)
    }

    fn parse_struct_ctx(&mut self, top_index: usize, word: &LocWord) -> Result<Option<Vec<Op>>> {
        if let (Some(n1), Some(n2)) =
            (self.ir_tokens.get(top_index + 1), self.ir_tokens.get(top_index + 2))
        {
            if self.get_struct_type(n1).is_some() &&
                equals_any!(n2, KeywordType::End, TokenType::Word)
            {
                return self.parse_struct(word);
            }
        }
        Ok(None)
    }

    fn parse_keyword_ctx(
        &mut self, colons: &mut i32, word: &LocWord, tok: IRToken,
    ) -> Result<Option<Vec<Op>>> {
        match (*colons, from_primitive(tok.operand)) {
            (0, KeywordType::Mem) => {
                self.next_irtoken();
                map_res(self.parse_memory(word))
            }
            (0, KeywordType::Struct) => {
                self.next_irtoken();
                self.parse_struct(word)
            }
            (0, KeywordType::Proc) => {
                self.next_irtoken();
                self.parse_procedure(word)
            }
            (1, KeywordType::Equal) => {
                self.next_irtokens(2);
                if let Ok((eval, skip)) = self.compile_eval() {
                    ensure!(
                        eval.typ != ValueType::Any,
                        "{} Undefined variable value is not allowed",
                        &word.loc
                    );
                    self.next_irtokens(skip);
                    self.register_var((word.to_string(), eval.operand, eval.typ).into());
                    return map_res(Ok(()));
                }
                map_res(self.invalid_token(tok, "context declaration"))
            }
            (_, KeywordType::Colon) => {
                *colons += 1;
                Ok(None)
            }
            (_, KeywordType::End) => {
                bail!(
                    "{} Missing body or contract necessary to infer the type of the word: `{}`",
                    word.loc,
                    word.name
                )
            }
            _ => Ok(None),
        }
    }

    fn parse_end_ctx(
        &mut self, colons: i32, ctx_size: usize, word: &LocWord,
    ) -> Result<Option<Vec<Op>>> {
        if colons != 2 {
            return Ok(None);
        }
        choice!(
            self.parse_static_ctx(ctx_size, word),
            map_res_t(self.define_proc(word, Contract::default()))
        )
    }

    fn parse_static_ctx(&mut self, ctx_size: usize, word: &LocWord) -> Result<Option<Vec<Op>>> {
        if ctx_size != 1 {
            return self.parse_procedure(word);
        }

        self.next_irtokens(2);
        if let Ok((eval, skip)) = self.compile_eval() {
            if eval.typ != ValueType::Any {
                self.next_irtokens(skip);
                self.const_list
                    .push((word.to_string(), eval.operand, eval.typ).into());
                return map_res(Ok(()));
            }
        }
        Ok(None)
    }

    fn parse_word_ctx(&mut self, tok: &IRToken, word: &LocWord) -> Result<Option<Vec<Op>>> {
        if tok == TokenType::Word {
            if let Some(stk) = self.get_struct_type(tok).cloned() {
                self.next_irtoken();
                return map_res(self.parse_const_or_var(word, tok.operand, stk));
            }
        }
        Ok(None)
    }

    fn define_proc(&mut self, name: &LocWord, contract: Contract) -> Result<Op> {
        ensure!(
            !self.inside_proc(),
            "{} Cannot define a procedure inside of another procedure",
            &name.loc
        );

        let proc = Proc::new(name, contract);
        let operand = self.proc_list.len();
        self.current_proc = Some(operand);
        self.proc_list.push(proc);

        Ok(self.push_block(Op::new(OpType::PrepProc, operand as i32, &name.loc)))
    }

    fn compile_eval(&mut self) -> Result<(IRToken, usize), IRToken> {
        match self.compile_eval_n(1) {
            Ok((mut result, skip)) => Ok((result.pop().expect("unreachable"), skip)),
            Err(tok) => Err(tok),
        }
    }

    fn compile_eval_n(&mut self, n: usize) -> Result<(Vec<IRToken>, usize), IRToken> {
        let mut result = Vec::new();
        let mut i = 0;

        while let Some(tok) = self.ir_tokens.get(i).cloned() {
            if &tok == KeywordType::End {
                if result.is_empty() && i == 0 {
                    result.push(IRToken::new(ValueType::Any.into(), 0, &tok.loc));
                } else if result.len() != n {
                    todo!("Handle mismath of expected types")
                }
                break;
            }

            self.eval_token(tok, &mut result)?;

            i += 1;
        }
        Ok((result, i + 1))
    }

    fn eval_token(&mut self, tok: IRToken, result: &mut Vec<IRToken>) -> Result<(), IRToken> {
        match tok.typ {
            TokenType::Keyword => {
                let key = FromPrimitive::from_i32(tok.operand).expect("unreachable");
                match key {
                    KeywordType::Dup => todo!(),
                    KeywordType::Drop => todo!(),
                    KeywordType::Swap => todo!(),
                    KeywordType::Over => todo!(),
                    KeywordType::Rot => todo!(),
                    KeywordType::Equal => todo!(),
                    _ => return Err(IRToken::new(TokenType::Keyword, tok.operand, &tok.loc)),
                }
            }
            TokenType::Word => {
                let word = self
                    .word_list
                    .get(tok.operand as usize)
                    .expect("unreachable");

                let loc_word = LocWord::new(tok.loc.clone(), word.to_string());
                match self.get_intrinsic_type(&loc_word) {
                    Some(intrinsic) => match intrinsic {
                        IntrinsicType::Plus => todo!(),
                        IntrinsicType::Minus => todo!(),
                        IntrinsicType::Cast(n) => {
                            let a = result.pop().expect("Todo:: report error");
                            let cast = if n >= 0 {
                                ValueType::from(n as usize).into()
                            } else {
                                todo!("Todo:: casting to ptr type not implemented yet");
                            };

                            result.push(IRToken::new(cast, a.operand, &tok.loc));
                        }
                        _ => return Err(IRToken::new(TokenType::Word, tok.operand, &tok.loc)),
                    },
                    None =>
                        if let Some(cnst) = self.try_get_const_name(&loc_word) {
                            result.push(IRToken::new(cnst.typ, cnst.word.value, &tok.loc));
                        } else {
                            return Err(tok);
                        },
                }
            }
            TokenType::Str => {
                self.register_string(tok.operand);
                let data = self
                    .data_list
                    .get(tok.operand as usize)
                    .expect("unreachable");

                result.push(IRToken::new(
                    TokenType::DataType(ValueType::Int),
                    data.size(),
                    &tok.loc,
                ));
                result.push(IRToken::new(
                    TokenType::DataType(ValueType::Ptr),
                    data.offset,
                    &tok.loc,
                ));
            }
            TokenType::DataType(value) => match value {
                ValueType::Int | ValueType::Bool | ValueType::Ptr => result.push(tok),
                _ => return Err(tok),
            },
            _ => return Err(tok),
        }
        Ok(())
    }

    fn invalid_token(&self, tok: IRToken, error_context: &str) -> Result<()> {
        let (found_desc, found_name) = match tok.typ {
            TokenType::Keyword =>
                ("keyword", format!("{:?}", from_primitive::<KeywordType>(tok.operand))),
            TokenType::Word => ("word or intrinsic", self.get_word(tok.operand)),
            _ => ("token", format!("{:?}", tok.typ)),
        };
        bail!("{} Invalid {found_desc} found on {error_context}: `{found_name}`", tok.loc)
    }

    fn parse_procedure(&mut self, word: &LocWord) -> Result<Option<Vec<Op>>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = &word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `proc`", loc)?;
        let error_text =
            format!("{loc} Expected proc contract or `:` after procedure definition, but found");

        while let Some(tok) = self.next_irtoken() {
            if let Some(key) = tok.get_keyword() {
                match key {
                    KeywordType::Arrow => {
                        ensure!(!arrow, "{loc} Duplicated `->` found on procedure definition");
                        arrow = true;
                    }
                    KeywordType::Colon => {
                        return map_res_t(self.define_proc(word, Contract { ins, outs }));
                    }
                    _ => bail!("{loc}, {error_text}: `{:?}`", key),
                }
            } else if let Some(found_word) = self.try_get_word(&tok) {
                if let Some(stk) = self.get_type_name(&found_word) {
                    for member in stk.members.iter() {
                        push_by_condition(arrow, member.typ, &mut outs, &mut ins);
                    }
                } else if let Some(type_ptr) = self.get_data_pointer(&found_word) {
                    push_by_condition(arrow, type_ptr, &mut outs, &mut ins);
                } else {
                    bail!("{error_text} the Word: `{found_word}`");
                }
            } else {
                bail!("{error_text}: `{:?}`", tok.typ);
            }
        }
        bail!("{error_text} nothing");
    }

    fn expect_keyword(&mut self, key: KeywordType, error_text: &str, loc: &Loc) -> Result<IRToken> {
        self.expect_next_by(|tok| tok == key, error_text, loc)
    }

    fn expect_next_by(
        &mut self, pred: impl FnOnce(&IRToken) -> bool, error_text: &str, loc: &Loc,
    ) -> Result<IRToken> {
        let tok = self.next_irtoken();
        let none = tok.is_none();
        tok.expect_by(pred, error_text).map_err(|err| {
            if none {
                let context = format!("{} {}", loc, err);
                err.context(context)
            } else {
                err
            }
        })
    }

    fn parse_memory(&mut self, word: &LocWord) -> Result<()> {
        let loc = &word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after `mem`", loc)?;
        let value_token =
            self.expect_next_by(|tok| tok.typ == ValueType::Int, "memory size after `:`", loc)?;
        self.expect_keyword(KeywordType::End, "`end` after memory size", loc)?;

        let size = ((value_token.operand + 3) / 4) * 4;
        if let Some(proc) = self.current_proc_mut() {
            proc.mem_size += size;
            proc.local_mem_names.push(Word::new(word, proc.mem_size));
        } else {
            self.mem_list.push(Word::new(word, self.total_mem_size));
            self.total_mem_size += size;
        }
        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord) -> Result<Option<Vec<Op>>> {
        let mut members = Vec::new();
        let loc = &word.loc;
        self.expect_keyword(KeywordType::Colon, "`:` after keyword `struct`", loc)?;
        let error_text = "Expected struct member type but found";

        while let Some(tok) = self.ir_tokens.get(0) {
            if tok.typ == TokenType::Keyword {
                self.expect_keyword(KeywordType::End, "`end` after struct declaration", loc)?;
                self.struct_list
                    .push(StructType::new(word.to_string(), members));
                return map_res(Ok(()));
            }

            let next =
                self.expect_next_by(|n| n.typ == TokenType::Word, "struct member name", loc)?;

            let found_word = self
                .word_list
                .get(next.operand as usize)
                .expect("unreachable")
                .to_owned();

            if let Some(name_type) = self.next_irtoken() {
                if name_type.typ == TokenType::Word {
                    let found_type = self
                        .word_list
                        .get(name_type.operand as usize)
                        .expect("unreachable");

                    if let Some(stk_typ) = self.get_type_name(found_type) {
                        if stk_typ.members.len() == 1 {
                            let typ = stk_typ.members.first().expect("unreachable").typ;
                            members.push((found_word, typ).into());
                        } else {
                            for member in &stk_typ.members {
                                let member_name = format!("{found_word}.{}", member.name);
                                members.push((member_name, member.typ).into());
                            }
                        }
                    } else if let Some(typ_ptr) = self.get_data_pointer(found_type) {
                        members.push((found_word, typ_ptr).into());
                    } else {
                        bail!("{loc} {error_text} the Word: `{found_type}`");
                    }
                } else {
                    bail!("{loc} {error_text}: {}", name_type);
                }
            }
        }

        bail!("{loc} Expected struct members or `end` after struct declaration");
    }

    fn parse_const_or_var(&mut self, word: &LocWord, operand: i32, stk: StructType) -> Result<()> {
        let loc = &word.loc;
        self.expect_keyword(KeywordType::Colon, "`:` after variable type definition", loc)?;

        let assign = self
            .expect_next_by(
                |tok| equals_any!(tok, KeywordType::Colon, KeywordType::Equal),
                "`:` or `=` after keyword `:`",
                loc,
            )?
            .get_keyword()
            .expect("unreachable");

        let (mut result, skip) = match self.compile_eval_n(stk.members.len()) {
            Ok((result, skip)) => (result, skip),
            _ => bail!("Failed to parse an valid struct value at compile-time evaluation"),
        };

        let end_token = self.next_irtokens(skip).expect("unreachable");
        let mut members = stk.members;
        members.reverse();
        result.reverse(); //Todo:: check if this is correct

        if result.len() == 1 {
            if let Some(eval) = result.pop() {
                if eval.typ == ValueType::Any {
                    for member in members {
                        let name = format!("{}.{}", word.name, member.name);
                        let struct_word = (name, member.default_value, member.typ).into();
                        self.register_typed_word(&assign, struct_word);
                    }
                } else {
                    let member_type = members.pop().expect("unreachable").typ;
                    ensure!(
                        equals_any!(member_type, ValueType::Any, eval.typ),
                        "{} Expected type `{:?}` on the stack at the end of the compile-time \
                         evaluation, but found: `{:?}`",
                        end_token.loc,
                        member_type,
                        eval.typ
                    );

                    let struct_word = (word.to_string(), eval.operand, member_type).into();
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
                    let name = format!("{}.{}", word.name, member.name);
                    let struct_word = (name, item.operand, item.typ).into();
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
            _ => self.register_var(struct_word),
        }
    }

    fn register_var(&mut self, struct_word: TypedWord) {
        match self.current_proc_mut() {
            Some(proc) => proc.local_vars.push(struct_word),
            _ => self.global_vars.push(struct_word),
        }
    }

    fn lex_file(&mut self, path: PathBuf) -> Result<&mut Self> {
        let reader = BufReader::new(
            File::open(&path).with_context(|| format!("could not read file `{:?}`", &path))?,
        );
        let mut lex = Lexer::new(reader, &path);

        while let Some(token) = lex.lex_next_token(self)? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = lex
                .lex_next_token(self)?
                .expect_by(|tok| tok == TokenType::Str, "include file name")?;

            let include_path = self
                .data_list
                .get(tok.operand as usize)
                .expect("unreachable, lexer error")
                .as_str();

            let include = get_dir(&path)
                .with_context(|| "failed to get file directory path")?
                .join(include_path);

            info!("Including file: {:?}", include);
            self.lex_file(include)?;
        }
        Ok(self)
    }
}

fn from_primitive<T: FromPrimitive>(value: i32) -> T {
    FromPrimitive::from_i32(value).expect("unreachable, lexer error")
}

impl ExpectBy<IRToken> for Option<IRToken> {
    fn expect_by(self, pred: impl FnOnce(&IRToken) -> bool, desc: &str) -> Result<IRToken> {
        match self {
            Some(tok) if pred(&tok) => Ok(tok),
            Some(tok) => bail!("{} Expected to find {}, but found: {}", tok.loc, desc, tok),
            None => bail!("Expected to find {}, but found nothing", desc),
        }
    }
}

fn invalid_block(loc: &Loc, block: Op, error: &str) -> Result<Op> {
    bail!(
        "{} {}, but found a `{:?}` block instead\n{} [INFO] The found block started here",
        loc,
        error,
        block.typ,
        block.loc
    )
}

pub fn compile_file(path: PathBuf) -> Result<()> {
    info!("Compiling file: {:?}", path);
    let mut parser = Parser::new();
    parser.lex_file(path)?.parse_tokens()?;

    for op in parser.program {
        info!("{} {}", op.loc, op)
    }

    Ok(())
}
