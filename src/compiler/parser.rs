use super::{lexer::Lexer, types::SizedWord};
use std::{io::BufReader, fs::File, path::PathBuf};
use anyhow::{Context, Result};

pub struct Parser {
    pub word_list: Vec<String>,
    pub data_list: Vec<SizedWord>
}

impl Parser {
    pub fn new() -> Self { Self { word_list: Vec::new(), data_list: Vec::new() } }
}

pub fn compile_file(path: PathBuf) -> Result<()> {
    let f =  File::open(&path)
        .with_context(|| format!("could not read file `{:?}`", path))?;
    log::info!("Compiling file: {:?}", path);

    let mut parser = Parser::new();
    let mut lex = Lexer::new(BufReader::new(f), path);

    while let Some(token) = lex.lex_next_token(&mut parser)? {
        info!("Lexed token: {:?}", token);
    }

    Ok(())
}
