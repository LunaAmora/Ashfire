use std::{io::BufReader, fs::File, path::PathBuf, collections::VecDeque};
use anyhow::{Context, Result, ensure, bail};
use super::{lexer::Lexer, types::*};
use num::FromPrimitive;

#[derive(Default)]
pub struct Parser {
    pub word_list: Vec<String>,
    pub data_list: Vec<SizedWord>,
    pub total_data_size: i32,
    ir_tokens: VecDeque<IRToken>,
    program:   Vec<Op>,
    op_blocks: Vec<Op>,
    current_proc: Option<Proc>,
}

impl Parser {
    pub fn new() -> Self { Default::default() }
    
    fn inside_proc(&self) -> bool {
        self.current_proc.is_some()
    }

    fn parse_tokens(&mut self) -> Result<()> {
        while let Some(token) = self.ir_tokens.pop_front() {
            if let Some(op) = self.define_op(token)? {
                self.program.push(op)
            }
        }
        Ok(())
    }

    fn define_op(&mut self, tok: IRToken) -> Result<Option<Op>> {
        ensure!(matches!(tok.typ, TokenType::Keyword | TokenType::Word) || self.inside_proc(),
            "{} Token type cannot be used outside of a procedure: `{:?}", tok.loc, tok.typ);
        
        Op::from(match tok.typ {
            TokenType::Keyword => return self.define_keyword_op(tok.operand, tok.loc),
            TokenType::Word => todo!(),
            TokenType::Str => (OpType::PushStr, self.register_string(tok.operand), tok.loc),
            TokenType::DataType(typ) => {
                match typ {
                    ValueType::Type(_) => bail!("{} Value type not valid here: `{:?}`", tok.loc, typ),
                    _ => (OpType::PushData(typ), tok.operand, tok.loc),
                }
            },
            TokenType::DataPtr(typ) => bail!("{} Data pointer type not valid here: `{:?}`", tok.loc, typ),
        }).into()
    }

    fn define_keyword_op(&mut self, operand: i32, loc: Loc) -> Result<Option<Op>> {
        let key = FromPrimitive::from_i32(operand).expect("unreachable, lexer error");

        match key {
            KeywordType::Dup   => (OpType::Dup, loc).into(),
            KeywordType::Drop  => (OpType::Drop, loc).into(),
            KeywordType::Swap  => (OpType::Swap, loc).into(),
            KeywordType::Over  => (OpType::Over, loc).into(),
            KeywordType::Rot   => (OpType::Rot,  loc).into(),
            KeywordType::Equal => (OpType::Equal,loc).into(),
            KeywordType::At    => (OpType::Unpack, loc).into(),
            KeywordType::Include => todo!(),
            KeywordType::While   => todo!(),
            KeywordType::Do      => todo!(),
            KeywordType::Let     => todo!(),
            KeywordType::Case    => todo!(),
            KeywordType::Colon   => todo!(),
            KeywordType::If      => todo!(),
            KeywordType::Else    => todo!(),
            KeywordType::End     => match self.pop_block(&loc, key)? {
                Op { op_type: OpType::PrepProc, operand, ..} => Op::from((OpType::EndProc, operand, loc)),
                block => invalid_block(&loc, block, "Expected `end` to close a valid block")?
            },
            KeywordType::Arrow | KeywordType::Proc | KeywordType::Mem |
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
}

fn invalid_block(loc: &Loc, block: Op, error: &str) -> Result<Op> {
    bail!("{} {}, but found a `{:?}` block instead\n{} [INFO] The found block started here", loc, error, block.op_type, block.loc)
}

pub fn compile_file(path: PathBuf) -> Result<()> {
    let f =  File::open(&path)
        .with_context(|| format!("could not read file `{:?}`", path))?;
    log::info!("Compiling file: {:?}", path);

    let mut parser = Parser::new();
    let mut lex = Lexer::new(BufReader::new(f), path);

    while let Some(token) = lex.lex_next_token(&mut parser)? {
        parser.ir_tokens.push_back(token)
    }

    parser.parse_tokens()
}
