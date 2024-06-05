use std::collections::BTreeMap;
use std::rc::Rc;

use thiserror::Error;

use crate::lexer::token::{Number, Punctuation, Token, TokenKind, TokenKindDesc};

use crate::ast::{
    Block, Entry, Extern, Function, Literal, Module, Statement, Type, TypeSignature, Use, UseSource,
};
use crate::Str;
#[derive(Debug, Clone, PartialEq, Error)]
pub enum ParserError {
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] at {}:{}:{}", tok.token, tok.contents, filename, tok.line, tok.col)]
    UnexpectedToken { tok: Token, filename: String },
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok, filename, tok.line, tok.col)]
    UnexpectedTokenEx {
        tok: Token,
        ex_tok: TokenKindDesc,
        filename: String,
    },
    #[error("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok_kind, filename, tok.line, tok.col)]
    UnexpectedTokenExKind {
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
    #[error("Parser errored:\n\tUnexpected end of Token Stream, expected: [{ex:?}] at {filename}:{line}:{col}")]
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
    filename: Str,
}

impl Parser {
    pub fn new(token_stream: impl Into<Rc<[Token]>>, filename: Option<impl Into<Str>>) -> Self {
        Self {
            pos: 0,
            token_stream: token_stream.into(),
            filename: filename.map(|f| f.into()).unwrap_or("code".into()),
        }
    }
    pub fn parse_module(mut self) -> ParserResult<Module> {
        let end = self.token_stream.len();

        let mut sub_modules: Vec<String> = vec![];
        let mut uses: Vec<Use> = vec![];
        let mut extern_uses: Vec<Extern> = vec![];

        while self.can_parse()
            && self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Mod)
        {
            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Mod), end)?;
            self.eat_ex_kind(TokenKind::ID("declare".into()), end)?;
            let tok = self.eat_ex(TokenKindDesc::ID, end)?;
            let TokenKind::ID(ref id) = tok.token else {
                unreachable!()
            };
            match id.as_ref() {
                "mod" => {
                    let TokenKind::ID(module_name) = self.eat_ex(TokenKindDesc::ID, end)?.token
                    else {
                        unreachable!()
                    };
                    sub_modules.push(module_name.to_string());
                }
                "use" => {
                    let source_tok = self.peek(0, end)?;
                    let source = match source_tok.token {
                        TokenKind::ID(id) => {
                            if id.as_ref() == "self" {
                                self.eat_ex_kind(TokenKind::ID("self".into()), end)?;
                                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Slash), end)?;
                                UseSource::Internal
                            } else {
                                UseSource::External
                            }
                        }
                        _ => {
                            return Err(ParserError::UnexpectedToken {
                                tok: source_tok,
                                filename: self.filename.to_string(),
                            })
                        }
                    };
                    let mut path = Vec::new();
                    while self.peek(1, end)?.token == TokenKind::Punctuation(Punctuation::Slash) {
                        let e_tok = self.eat_ex(TokenKindDesc::ID, end)?;

                        let TokenKind::ID(e) = e_tok.token else {
                            unreachable!()
                        };
                        path.push(e);

                        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Slash), end)?;
                    }

                    let mut used = Vec::new();
                    if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::LeftBracket)
                    {
                        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftBracket), end)?;
                        loop {
                            let TokenKind::ID(e) = self.eat_ex(TokenKindDesc::ID, end)?.token
                            else {
                                unreachable!()
                            };
                            used.push(e);

                            if self.peek(0, end)?.token
                                == TokenKind::Punctuation(Punctuation::RightBracket)
                            {
                                self.eat_ex_kind(
                                    TokenKind::Punctuation(Punctuation::RightBracket),
                                    end,
                                )?;
                                break;
                            } else {
                                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Comma), end)?;
                            }
                        }
                    } else {
                        let TokenKind::ID(e) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                            unreachable!()
                        };
                        used.push(e);
                    }

                    uses.push(Use {
                        source,
                        path: path.into(),
                        used: used.into(),
                    });
                }
                "extern" => {
                    let TokenKind::ID(name) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                        unreachable!()
                    };
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Mod), end)?;

                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftParen), end)?;

                    let mut args = vec![];
                    let mut arg_list;
                    loop {
                        arg_list =
                            self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Dot);
                        if arg_list {
                            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Dot), end)?;
                            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Dot), end)?;
                            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Dot), end)?;
                            break;
                        }

                        args.push(self.parse_type(end)?);

                        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Comma) {
                            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Comma), end)?;
                        } else {
                            break;
                        }
                    }

                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightParen), end)?;

                    let ret_type =
                        if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon) {
                            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Colon), end)?;
                            self.parse_type(end)?
                        } else {
                            Type::Null
                        };

                    extern_uses.push(Extern {
                        name,
                        args: args.into(),
                        ret_type,
                        arg_list,
                    });
                }
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        tok,
                        filename: self.filename.to_string(),
                    })
                }
            }
            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Semicolon), end)?;
        }

        let mut entries = BTreeMap::new();
        let mut functions = BTreeMap::new();

        while self.can_parse() {
            let tok = self.eat_ex(TokenKindDesc::ID, end)?;
            let TokenKind::ID(ref t) = tok.token else {
                unreachable!()
            };
            match t.as_ref() {
                "entry" => {
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Mod), end)?;

                    let TokenKind::ID(name) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                        unreachable!()
                    };

                    let type_sig = self.parse_type_signature(end)?;

                    let block = self.parse_block(end)?;

                    entries.insert(name, Entry { type_sig, block });
                }
                "fn" => {
                    let TokenKind::ID(name) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                        unreachable!()
                    };

                    let type_sig = self.parse_type_signature(end)?;

                    let block = self.parse_block(end)?;

                    functions.insert(
                        name,
                        Function {
                            type_sig,
                            block,
                            id: 0, // TODO,
                        },
                    );
                }
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        tok,
                        filename: self.filename.to_string(),
                    })
                }
            }
        }

        Ok(Module {
            sub_modules,
            uses,
            extern_uses,
            entries,
            functions,
        })
    }

    fn parse_standalone_statement(&mut self, end: usize) -> ParserResult<Statement> {
        let pos = self.pos;

        if let Ok(_let) = self.parse_let(end) {
            return Ok(_let);
        } else {
            self.pos = pos;
        }

        if let Ok(call) = self.parse_call(end, true) {
            return Ok(call);
        } else {
            self.pos = pos;
        }

        let tok = self.peek(0, end)?;

        Ok(match &tok.token {
            TokenKind::ID(id) => match &(**id) {
                "return" => self.parse_return(end)?,
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        tok,
                        filename: self.filename.to_string(),
                    })
                }
            },
            _ => {
                return Err(ParserError::UnexpectedToken {
                    tok,
                    filename: self.filename.to_string(),
                })
            }
        })
    }

    fn parse_return(&mut self, end: usize) -> ParserResult<Statement> {
        self.eat_ex_kind(TokenKind::ID("return".into()), end)?;

        let _end =
            self.to_first_minding_blocks(TokenKind::Punctuation(Punctuation::Semicolon), end)?;

        let value = self.parse_statement(_end)?;

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Semicolon), end)?;

        Ok(Statement::Return(Box::new(value)))
    }

    fn parse_let(&mut self, end: usize) -> ParserResult<Statement> {
        let _end =
            self.to_first_minding_blocks(TokenKind::Punctuation(Punctuation::Semicolon), end)?;

        let TokenKind::ID(name) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
            unreachable!()
        };

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Colon), end)?;

        let _type = if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Equals)
            && self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Semicolon)
        {
            Some(self.parse_type(end)?)
        } else {
            None
        };

        let value = if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Equals) {
            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Equals), end)?;
            Some(Box::new(self.parse_statement(_end)?))
        } else {
            None
        };

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Semicolon), end)?;

        Ok(Statement::Let { name, _type, value })
    }

    fn parse_statement(&mut self, end: usize) -> ParserResult<Statement> {
        let pos = self.pos;

        if let Ok(call) = self.parse_call(end, false) {
            return Ok(call);
        } else {
            self.pos = pos;
        }

        let tok = self.peek(0, end)?;
        let stmt = match &tok.token {
            TokenKind::Number(_) | TokenKind::String(_) => self.parse_literal(end)?,
            TokenKind::ID(id) => match &(**id) {
                "false" | "true" => self.parse_literal(end)?,
                _ => {
                    let TokenKind::ID(id) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                        unreachable!()
                    };
                    Statement::Get(id)
                }
            },
            TokenKind::Punctuation(Punctuation::Minus) => self.parse_literal(end)?,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    tok,
                    filename: self.filename.to_string(),
                })
            }
        };
        Ok(stmt)
    }

    fn parse_call(&mut self, end: usize, standalone: bool) -> ParserResult<Statement> {
        let TokenKind::ID(called) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
            unreachable!()
        };

        let entry = self
            .eat_ex_kind(TokenKind::Punctuation(Punctuation::Mod), end)
            .is_ok();

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftParen), end)?;

        let _end = self.isolate_block(end)?;

        let mut args = vec![];
        loop {
            let __end =
                self.to_first_minding_blocks(TokenKind::Punctuation(Punctuation::Comma), _end)?;

            args.push(self.parse_statement(__end)?);

            if self.peek(0, end)?.token != TokenKind::Punctuation(Punctuation::Comma) {
                break;
            } else {
                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Comma), end)?;
            }
        }

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightParen), end)?;

        if standalone {
            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Semicolon), end)?;
        }

        Ok(Statement::Call {
            called,
            args,
            entry,
        })
    }

    fn parse_literal(&mut self, end: usize) -> ParserResult<Statement> {
        Ok(match self.peek(0, end)?.token {
            TokenKind::Number(num) => {
                self.eat_ex(TokenKindDesc::Number, end)?;
                Statement::Literal(match num {
                    Number::Float(f) => Literal::Float(f),
                    Number::Int(i) => Literal::Integer(i as i128),
                    Number::Hex(h) => Literal::Integer(h as i128),
                })
            }
            TokenKind::String(s) => {
                self.eat_ex(TokenKindDesc::String, end)?;
                Statement::Literal(Literal::String(s))
            }
            TokenKind::ID(id) => match &(*id) {
                "true" => {
                    self.eat_ex(TokenKindDesc::ID, end)?;
                    Statement::Literal(Literal::Boolean(true))
                }
                "false" => {
                    self.eat_ex(TokenKindDesc::ID, end)?;
                    Statement::Literal(Literal::Boolean(false))
                }
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        tok: self.peek(0, end)?,
                        filename: self.filename.to_string(),
                    })
                }
            },
            TokenKind::Punctuation(Punctuation::Minus) => {
                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Minus), end)?;
                let TokenKind::Number(num) = self.eat_ex(TokenKindDesc::Number, end)?.token else {
                    unreachable!()
                };
                Statement::Literal(match num {
                    Number::Float(f) => Literal::Float(-f),
                    Number::Int(i) => Literal::Integer(-(i as i128)),
                    Number::Hex(h) => Literal::Integer(-(h as i128)),
                })
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    tok: self.peek(0, end)?,
                    filename: self.filename.to_string(),
                })
            }
        })
    }

    fn parse_type_signature(&mut self, end: usize) -> ParserResult<TypeSignature> {
        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftParen), end)?;

        let mut args = vec![];
        loop {
            let TokenKind::ID(arg_name) = self.eat_ex(TokenKindDesc::ID, end)?.token else {
                unreachable!()
            };

            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Colon), end)?;

            let _type = self.parse_type(end)?;

            args.push((arg_name, _type));

            if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Comma) {
                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Comma), end)?;
            } else {
                break;
            }
        }

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightParen), end)?;

        let ret_type = if self.peek(0, end)?.token == TokenKind::Punctuation(Punctuation::Colon) {
            self.eat_ex_kind(TokenKind::Punctuation(Punctuation::Colon), end)?;
            self.parse_type(end)?
        } else {
            Type::Null
        };

        Ok(TypeSignature {
            args: args.into(),
            ret_type,
        })
    }

    fn parse_type(&mut self, end: usize) -> ParserResult<Type> {
        let tok = self.eat(end)?;
        Ok(match tok.token {
            TokenKind::Punctuation(Punctuation::LeftSBracket) => {
                let _end = self.isolate_block(end)?;
                let _type = self.parse_type(_end)?;
                self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightSBracket), end)?;
                Type::Slice(Box::new(_type))
            }
            TokenKind::ID(id) => match &(*id) {
                "f32" => Type::F32,
                "f64" => Type::F64,
                "i8" => Type::I8,
                "i16" => Type::I16,
                "i32" => Type::I32,
                "i64" => Type::I64,
                "u8" => Type::U8,
                "u16" => Type::U16,
                "u32" => Type::U32,
                "u64" => Type::U64,
                "char" => Type::Char,
                "ref" => {
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftSBracket), end)?;
                    let _end = self.isolate_block(end)?;
                    let _type = self.parse_type(_end)?;
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightSBracket), end)?;
                    Type::Ref(Box::new(_type))
                }
                "ptr" => {
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftSBracket), end)?;
                    let _end = self.isolate_block(end)?;
                    let _type = self.parse_type(_end)?;
                    self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightSBracket), end)?;
                    Type::Ptr(Box::new(_type))
                }
                _ => Type::Struct(id),
            },
            _ => {
                return Err(ParserError::UnexpectedToken {
                    tok,
                    filename: self.filename.to_string(),
                })
            }
        })
    }

    fn parse_block(&mut self, end: usize) -> ParserResult<Block> {
        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::LeftBracket), end)?;
        let _end = self.isolate_block(end)?;

        let mut statements = vec![];
        while _end - self.pos > 0 {
            statements.push(self.parse_standalone_statement(_end)?);
        }

        self.eat_ex_kind(TokenKind::Punctuation(Punctuation::RightBracket), end)?;

        Ok(Block(statements.into()))
    }

    fn can_parse(&self) -> bool {
        self.token_stream.len() - self.pos > 0
    }

    fn to_first(&self, _tok: TokenKind, _end: usize) -> ParserResult<usize> {
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
    fn to_first_minding_blocks(&self, _tok: TokenKind, _end: usize) -> ParserResult<usize> {
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
    fn to_last(&self, _tok: TokenKind, _end: usize) -> ParserResult<usize> {
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
    fn to_last_minding_blocks(&self, _tok: TokenKind, _end: usize) -> ParserResult<usize> {
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

    fn eat_ex(&mut self, ex_tok: TokenKindDesc, end: usize) -> ParserResult<Token> {
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

    fn eat_ex_kind(&mut self, ex_tok_kind: TokenKind, end: usize) -> ParserResult<Token> {
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
