use std::collections::BTreeMap;
use std::rc::Rc;

use thiserror::Error;

use crate::lexer::token::{Punctuation, Token, TokenKind, TokenKindDesc};

use crate::ast::{Module, UsePath, UseSource, Used};
#[derive(Debug, Clone, PartialEq, Error)]
pub enum ParserError {
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] at {}:{}:{}", tok.token, tok.contents, filename, tok.line, tok.col)]
    UnexpectedToken {
        tok: Token,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok, filename, tok.line, tok.col)]
    UnexpectedTokenEx {
        tok: Token,
        ex_tok: TokenKindDesc,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok_kind, filename, tok.line, tok.col)]
    UnexpectedTokenExKind{
        tok: Token,
        ex_tok_kind: TokenKind,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected end of Token Stream at {filename}:{line}:{col}")]
    UnexpectedEos {
        line: u32,
        col: u32,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected end of Token Stream, expected: [{ex:?}] at {line}:{col}:{filename}")]
    UnexpectedEosEx {
        ex: TokenKindDesc,
        line: u32,
        col: u32,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected end of Token Stream, expected: [{ex:?}] at {filename}:{line}:{col}")]
    UnexpectedEosExKind {
        ex: TokenKind,
        line: u32,
        col: u32,
        filename: String,
    },
}
pub type ParserResult<T> = std::result::Result<T, ParserError>;
pub struct Parser {
    pos: usize,
    token_stream: Rc<[Token]>,
    filename: Rc<str>,
}

impl Parser {
    pub fn new(token_stream: impl Into<Rc<[Token]>>, filename: Option<impl Into<Rc<str>>>) -> Self {
        Self {
            pos: 0,
            token_stream: token_stream.into(),
            filename: filename.map(|f| f.into()).unwrap_or("code".into()),
        }
    }
    pub fn parse_module(mut self) -> ParserResult<Module> {
        let end = self.token_stream.len();
        
        let mut sub_modules: Vec<String> = vec![];
        let mut mappings: BTreeMap<String, String> = BTreeMap::new();
        let mut uses: Vec<(UseSource, UsePath, Used)> = vec![];

        while self.can_parse() && self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Mod) {
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Mod))?;
            self.eat_ex_kind(end, TokenKind::ID("declare".into()))?;
            let tok = self.eat_ex(end, TokenKindDesc::ID)?;
            let TokenKind::ID(ref id) = tok.token else { unreachable!() };
            match id.as_ref() {
                "mod" => {
                    let TokenKind::ID(module_name) = self.eat_ex(end, TokenKindDesc::ID)?.token else { unreachable!() };
                    sub_modules.push(module_name.to_string());
                },
                "map" => {
                    let TokenKind::ID(target) = self.eat_ex(end, TokenKindDesc::ID)?.token else { unreachable!() };
                    self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Colon))?;
                    let TokenKind::ID(source) = self.eat_ex(end, TokenKindDesc::ID)?.token else { unreachable!() };
                    mappings.insert(target.to_string(), source.to_string());
                },
                "use" => todo!(),
                _ => return Err(ParserError::UnexpectedToken { tok, filename: self.filename.to_string() })
            }
            self.eat_ex_kind(end, TokenKind::Punctuation(Punctuation::Semicolon))?;
        }
        todo!()
    }

    fn can_parse(&self) -> bool {
        self.token_stream.len() - self.pos > 0
    }

    fn to_first(&self, _end: usize, _tok: TokenKind) -> ParserResult<usize> {
        let mut end = self.pos;
        let mut o = 0;
        loop {
            if end >= _end {
                break;
            }
            let tok = self.peek(o, _end)?;
            if tok.token == _tok {
                break;
            }
            o += 1;
            end += 1;
        }
        Ok(end)
    }
    fn to_first_minding_blocks(&self, _end: usize, _tok: TokenKind) -> ParserResult<usize> {
        let mut end = self.pos;
        let mut o = 0;
        let mut i = 0;
        loop {
            if end >= _end {
                break;
            }
            let tok = self.peek(o, _end)?;
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }
            if tok.token == _tok && i == 0 {
                break;
            }
            o += 1;
            end += 1;
        }
        Ok(end)
    }
    fn to_last(&self, _end: usize, _tok: TokenKind) -> ParserResult<usize> {
        let mut end = self.pos;
        let mut o = 0;
        loop {
            if end >= _end || self.pos + o >= _end {
                break;
            }
            let tok = self.peek(o as isize, _end)?;
            if tok.token == _tok {
                end = self.pos + o;
            }
            o += 1;
        }
        Ok(end)
    }
    fn to_last_minding_blocks(&self, _end: usize, _tok: TokenKind) -> ParserResult<usize> {
        let mut end = self.pos;
        let mut o = 0;
        let mut i = 0;
        loop {
            if end >= _end || self.pos + o >= _end {
                break;
            }
            let tok = self.peek(o as isize, _end)?;
            if tok.token == _tok && i == 0 {
                end = self.pos + o;
            }
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }

            o += 1;
        }
        Ok(end)
    }
    fn isolate_block(&self, _end: usize) -> ParserResult<usize> {
        let mut i = 1;
        let mut end = self.pos - 1;
        let mut o = 0;
        loop {
            if i == 0 {
                break;
            }
            let tok = self.peek(o, _end)?;
            if TokenKindDesc::Punctuation == tok.token {
                match tok.token {
                    TokenKind::Punctuation(punct) => match punct {
                        Punctuation::LeftBracket => i += 1,
                        Punctuation::RightBracket => i -= 1,
                        Punctuation::LeftSBracket => i += 1,
                        Punctuation::RightSBracket => i -= 1,
                        Punctuation::LeftParen => i += 1,
                        Punctuation::RightParen => i -= 1,
                        _ => {}
                    },
                    _ => {}
                }
            }
            end += 1;
            o += 1;
        }
        Ok(end)
    }

    fn eat(&mut self, end: usize) -> ParserResult<Token> {
        if end < self.pos || end - self.pos <= 0 {
            self.peek(0, self.token_stream.len())?;
        }
        self.pos += 1;
        Ok(self.token_stream[self.pos - 1].clone())
    }
    
    fn eat_ex(&mut self, end: usize, ex_tok: TokenKindDesc) -> ParserResult<Token> {
        let tok = self.eat(end)?;
        if ex_tok != tok.token {
            self.pos -= 1;
            return Err(ParserError::UnexpectedTokenEx {
                tok,
                ex_tok,
                filename: self.filename.to_string(),
            });
        }
        Ok(tok)
    }

    fn eat_ex_kind(&mut self, end: usize, ex_tok_kind: TokenKind) -> ParserResult<Token> {
        let tok = self.eat(end)?;
        if ex_tok_kind != tok.token {
            self.pos -= 1;
            return Err(ParserError::UnexpectedTokenExKind {
                tok,
                ex_tok_kind,
                filename: self.filename.to_string(),
            });
        }
        Ok(tok)
    }

    fn peek(&self, offset: isize, end: usize) -> ParserResult<Token> {
        if end as isize - (self.pos as isize + offset) <= 0 {
            let tk = self.peek(offset - 1, self.token_stream.len())?;
            return Err(ParserError::UnexpectedEos {
                line: tk.line,
                col: tk.col,
                filename: self.filename.to_string(),
            });
        }
        Ok(self.token_stream[(self.pos as isize + offset) as usize].clone())
    }
}