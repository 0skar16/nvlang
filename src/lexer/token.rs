use std::fmt::{Debug, Write};
use std::{ops::Range, rc::Rc};

use super::Lexer;

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token: TokenKind,
    pub contents: Rc<str>,
    pub pos: Range<usize>,
    pub line: u32,
    pub col: u32,
    pub filename: Rc<str>,
}
impl Token {
    pub fn new(lexer: &Lexer, token: TokenKind, contents: Rc<str>, filename: Rc<str>) -> Token {
        Token {
            token,
            pos: lexer.pos..lexer.pos + contents.len(),
            contents,
            line: lexer.line,
            col: lexer.col,
            filename,
        }
    }
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.contents)
    }
}
pub(crate) struct TokenStream(Rc<[Token]>);
#[allow(dead_code)]
pub(crate) trait ToTokenStream {
    fn to_token_stream(self) -> TokenStream;
}
impl<T: Iterator<Item = Token>> ToTokenStream for T {
    fn to_token_stream(self) -> TokenStream {
        TokenStream(self.collect())
    }
}
impl Debug for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('"')?;
        f.write_char(' ')?;
        for t in self.0.iter() {
            t.fmt(f)?;
            f.write_char(' ')?;
        }
        f.write_char('"')?;
        Ok(())
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    ID(Rc<str>),
    String(Rc<str>),
    Punctuation(Punctuation),
    Number(Number),
    _Eof,
}
impl TokenKind {
    pub fn kind(&self) -> TokenKindDesc {
        match self {
            TokenKind::ID(_) => TokenKindDesc::ID,
            TokenKind::String(_) => TokenKindDesc::String,
            TokenKind::Punctuation(_) => TokenKindDesc::Punctuation,
            TokenKind::Number(_) => TokenKindDesc::Number,
            TokenKind::_Eof => unreachable!(),
        }
    }
}
impl PartialEq<TokenKind> for TokenKindDesc {
    fn eq(&self, other: &TokenKind) -> bool {
        match other {
            TokenKind::ID(_) => *self == TokenKindDesc::ID,
            TokenKind::String(_) => *self == TokenKindDesc::String,
            TokenKind::Punctuation(_) => *self == TokenKindDesc::Punctuation,
            TokenKind::Number(_) => *self == TokenKindDesc::Number,
            TokenKind::_Eof => false,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenKindDesc {
    ID,
    String,
    Punctuation,
    Number,
}
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Punctuation {
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftSBracket,
    RightSBracket,
    Dot,
    Colon,
    Semicolon,
    Mod,
    Asterisk,
    And,
    Or,
    Comma,
    Minus,
    Slash,
    Plus,
    Exp,
    Not,
    LesserThan,
    GreaterThan,
    Equals,
    EqualsTo,
    BitwiseAnd,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
}
impl Punctuation {
    pub fn is_operation(&self) -> bool {
        match self {
            Punctuation::LeftBracket => false,
            Punctuation::RightBracket => false,
            Punctuation::LeftParen => false,
            Punctuation::RightParen => false,
            Punctuation::LeftSBracket => false,
            Punctuation::RightSBracket => false,
            Punctuation::Colon => false,
            Punctuation::Semicolon => false,
            Punctuation::Dot => false,
            Punctuation::Comma => false,
            Punctuation::Equals => false,
            _ => true,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Float(f64),
    Int(u64),
    Hex(u64),
}
