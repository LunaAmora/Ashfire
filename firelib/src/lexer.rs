use std::{
    collections::HashMap,
    io::{BufRead, BufReader, Read},
};

use crate::utils::strip_trailing_newline;

pub struct LexerBuilder {
    matches: Vec<Match>,
    separators: Vec<char>,
    comments: Option<String>,
}

impl LexerBuilder {
    fn new() -> Self {
        Self {
            matches: vec![],
            separators: vec![],
            comments: None,
        }
    }

    #[must_use]
    /// Separators are not included in other tokens, unless inside a [`Match`].
    pub fn with_separators<const N: usize>(mut self, sep: [char; N]) -> Self {
        self.separators.extend(sep);
        self
    }

    #[must_use]
    /// Captures everything betwen a pair of chars or until the end of the line.
    pub fn with_matches<const N: usize>(mut self, matches: [Match; N]) -> Self {
        self.matches.extend(matches);
        self
    }

    #[must_use]
    /// Makes the rest of the line be ignored by the lexer.
    pub fn with_comments(mut self, comment: &str) -> Self {
        self.comments = Some(comment.to_owned());
        self
    }

    /// Tries to build an `Lexer` with the given `Read` and parameters.
    pub fn build(self, index: usize, read: impl Read + 'static) -> Lexer {
        let mut matches = HashMap::with_capacity(self.matches.len());

        for value in &self.matches {
            match value {
                Match::Same(start) => matches.insert(*start, *start),
                Match::Pair(start, end) => matches.insert(*start, *end),
            };
        }

        Lexer::new(Box::new(BufReader::new(read)), index, self.separators, self.comments, matches)
    }
}

pub enum Match {
    Same(char),
    Pair(char, char),
}

enum Predicate {
    Char(char),
    Whitespace,
    Separators,
}

pub struct Lexer {
    buffer: Vec<char>,
    reader: Box<dyn BufRead>,
    separators: Vec<char>,
    comments: Option<String>,
    matches: HashMap<char, char>,
    file_index: usize,
    lex_pos: usize,
    col_num: usize,
    line_num: usize,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Lexer {
    pub fn new(
        reader: Box<dyn BufRead>, file_index: usize, separators: Vec<char>,
        comments: Option<String>, matches: HashMap<char, char>,
    ) -> Self {
        Self {
            reader,
            file_index,
            separators,
            comments,
            matches,
            buffer: Vec::new(),
            lex_pos: 0,
            col_num: 0,
            line_num: 0,
        }
    }

    /// Returns an builder object for working with the `Lexer`.
    pub fn builder() -> LexerBuilder {
        LexerBuilder::new()
    }

    fn next_token(&mut self) -> Option<Token> {
        self.seek_next_token()
            .then(|| Token::new(self.read_token(), self.current_loc()))
    }

    fn read_token(&mut self) -> String {
        self.read_by_predicate(Predicate::Separators)
    }

    fn current_loc(&self) -> Loc {
        Loc::new(self.file_index, self.line_num, self.col_num + 1)
    }

    fn seek_next_token(&mut self) -> bool {
        self.trim_left() || self.read_line()
    }

    fn trim_left(&mut self) -> bool {
        !self.buffer_done_or_empty() && self.check_for_comments()
    }

    fn buffer_done_or_empty(&self) -> bool {
        self.lex_pos + 1 > self.buffer.len() || self.read_from_pos().trim().is_empty()
    }

    fn check_for_comments(&mut self) -> bool {
        self.advance_by_predicate(Predicate::Whitespace);
        self.col_num = self.lex_pos;
        self.comments
            .as_ref()
            .map_or(false, |pattern| !self.read_from_pos().starts_with(pattern))
    }

    fn read_from_pos(&self) -> String {
        self.buffer.iter().skip(self.lex_pos).collect()
    }

    fn read_line(&mut self) -> bool {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(read) if read > 0 => self.setup_buffer(line),
            _ => {
                self.buffer = Vec::new();
                false
            }
        }
    }

    fn setup_buffer(&mut self, line: String) -> bool {
        self.line_num += 1;
        (line.trim().is_empty() && self.read_line()) || self.setup_cursor_and_trim(line)
    }

    fn setup_cursor_and_trim(&mut self, line: String) -> bool {
        self.buffer = strip_trailing_newline(&line).chars().collect();
        self.col_num = 0;
        self.lex_pos = 0;
        self.seek_next_token()
    }

    fn read_by_predicate(&mut self, pred: Predicate) -> String {
        self.advance_by_predicate(pred);

        self.buffer
            .iter()
            .skip(self.col_num)
            .take(self.lex_pos - self.col_num)
            .collect()
    }

    fn advance_by_predicate(&mut self, pred: Predicate) {
        while let Some(&buf) = self.buffer.get(self.lex_pos) {
            match pred {
                Predicate::Char(char) if buf == char => return,
                Predicate::Whitespace if buf != ' ' => return,
                Predicate::Separators if buf == ' ' => return,

                Predicate::Separators if self.is_comment(buf) => return,

                Predicate::Separators if self.separators.contains(&buf) => {
                    if self.lex_pos == self.col_num {
                        self.lex_pos += 1;
                    }
                    return;
                }

                Predicate::Separators => match self.matches.get(&buf) {
                    Some(&end) => {
                        self.lex_pos += 1;
                        self.advance_by_predicate(Predicate::Char(end));

                        if end != ' ' {
                            self.lex_pos += 1;
                        }

                        return;
                    }
                    _ => self.lex_pos += 1,
                },

                _ => self.lex_pos += 1,
            };
        }
    }

    fn is_comment(&self, buf: char) -> bool {
        match &self.comments {
            Some(comment) if comment.starts_with(buf) => self.check_match_next(comment),
            _ => false,
        }
    }

    fn check_match_next(&self, matcher: &str) -> bool {
        for (index, c) in matcher.char_indices().skip(1) {
            if !self
                .buffer
                .get(self.lex_pos + index)
                .map_or(false, |&next| c == next)
            {
                return false;
            }
        }
        true
    }
}

pub struct Token {
    pub name: String,
    pub loc: Loc,
}

impl Token {
    fn new(name: String, loc: Loc) -> Self {
        Self { name, loc }
    }
}

#[derive(Clone, Copy, Default)]
pub struct Loc {
    pub file_index: usize,
    pub line: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(file_index: usize, line: usize, col: usize) -> Self {
        Self { file_index, line, col }
    }
}
