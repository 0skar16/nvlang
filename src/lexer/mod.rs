pub mod token;
use token::*;

use std::{rc::Rc, str::Chars};

use thiserror::Error;

use crate::{throw_unexpected_char, token, try_next_char};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum LexerError {
    #[error("Unexpected char [{char}] at {filename}:{line}:{col}")]
    UnexpectedChar {
        char: char,
        line: u32, 
        col: u32,
        filename: String,
    },
    #[error("Unexpected end of file at {filename}:{line}:{col}")]
    UnexpectedEof {
        line: u32, 
        col: u32,
        filename: String,
    },
    #[error("Expected char [{char}], but didn't get one at {filename}:{line}:{col}")]
    ExpectedCharNotExisting {
        char: char,
        line: u32, 
        col: u32,
        filename: String,
    },
}
pub type LexerResult<T> = std::result::Result<T, LexerError>;

pub struct Lexer {
    // the characters used, funnily enough, not only is this better than an Iterator because it can be looked up faster, a Peekable iterator won't let us look backwards, needed for fast string parsing
    chars: Rc<[char]>,
    line: u32,
    col: u32,
    // position in the character array
    pos: usize,
    // the output
    token_stream: Vec<Token>,
    filename: Rc<str>,
}

impl Lexer {
    // we take in a character iterator because it's more convenient and a filename for error creation
    pub fn new(chars: Chars<'_>, filename: Option<impl Into<Rc<str>>>) -> Lexer {
        Self {
            chars: chars.collect(),
            token_stream: vec![],
            line: 1,
            col: 1,
            pos: 0,
            filename: filename.map(|f| f.into()).unwrap_or("code".into()),
        }
    }
    pub fn tokenize(mut self) -> LexerResult<Rc<[Token]>> {
        while let (tok, is_eof) = self.next_token()
            && !is_eof
        {
            self.token_stream.push(tok?);
        }
        Ok(self.token_stream.into())
    }
    // and now the fun begins
    fn next_token(&mut self) -> (LexerResult<Token>, bool) {
        // we return an EoF token as this is not erroneous and an expected behaviour
        self.skip_unneeded();
        if self.chars.len() - self.pos <= 0 {
            return (
                Ok(Token::new(
                    self,
                    TokenKind::_Eof,
                    "".into(),
                    self.filename.clone(),
                )),
                true,
            );
        }
        let tok = match try_next_char!(self) {
            '"' => {
                let s: Rc<str> = {
                    let mut out = String::new();
                    loop {
                        let char = self.next_char_unfiltered();

                        let char = if let Ok(char) = char {
                            char
                        } else {
                            let Err(e) = char else {
                                unreachable!()
                            };
                            return (LexerResult::Err(e), false);
                        };


                        if char == '"' {
                            // end string if the quote is string-ending
                            // if Lexer gets '\"', it skips it
                            if self.peek_char(-2) != Some('\\') {
                                break;
                            }
                            // we don't add it to the output because it has already been added by the backslash character
                        } else {
                            if char == '\\' {
                                match try_next_char!(self) {
                                    // just a zero byte
                                    '0' => out.push('\0'),
                                    
                                    // newline
                                    'n' => out.push('\n'),
                                    // tab
                                    't' => out.push('\t'),
                                    
                                    // not string-ending quote
                                    '"' => out.push('"'),
                                    // backslash
                                    '\\' => out.push('\\'),
                                    
                                    // throw an error because something else after a backslash is not expected
                                    c => throw_unexpected_char!(c, self),
                                }
                            }else {
                                out.push(char);
                            }
                        }
                    }
                    out
                }.into();
                token!(self; TokenKind::String(s.clone()), format!("\"{}\"", s))
            },
            '/' => token!(self;
                _ => TokenKind::Punctuation(Punctuation::Slash), "/";
                // these are comments, as such, they need different behaviour than the default token one
                b '*' => {
                    try_next_char!(self);
                    self.take_until("*/");
                    return self.next_token();
                };
                b '/' => {
                    try_next_char!(self);
                    self.take_until("\n");
                    return self.next_token();
                }
            ),
            '&' => token!(self;
                _ => TokenKind::Punctuation(Punctuation::BitwiseAnd), "&";
                '&' => TokenKind::Punctuation(Punctuation::And), "&&";
            ),
            '<' => token!(self;
                _ => TokenKind::Punctuation(Punctuation::LesserThan), "<";
                '<' => TokenKind::Punctuation(Punctuation::ShiftLeft), "<<";
            ),
            '>' => token!(self;
                _ => TokenKind::Punctuation(Punctuation::GreaterThan), ">";
                '>' => TokenKind::Punctuation(Punctuation::ShiftRight), ">>";
            ),
            '=' => token!(self;
                _ => TokenKind::Punctuation(Punctuation::Equals), "=";
                '=' => TokenKind::Punctuation(Punctuation::EqualsTo), "==";
            ),
            '|' => token!(self; TokenKind::Punctuation(Punctuation::BitwiseOr), "|"),
            '-' => token!(self; TokenKind::Punctuation(Punctuation::Minus), "-"),
            '{' => token!(self; TokenKind::Punctuation(Punctuation::LeftBracket), "{"),
            '}' => token!(self; TokenKind::Punctuation(Punctuation::RightBracket), "}"),
            '(' => token!(self; TokenKind::Punctuation(Punctuation::LeftParen), "("),
            ')' => token!(self; TokenKind::Punctuation(Punctuation::RightParen), ")"),
            '[' => token!(self; TokenKind::Punctuation(Punctuation::LeftSBracket), "["),
            ']' => token!(self; TokenKind::Punctuation(Punctuation::RightSBracket), "]"),
            '.' => token!(self; TokenKind::Punctuation(Punctuation::Dot), "."),
            ':' => token!(self; TokenKind::Punctuation(Punctuation::Colon), ":"),
            ';' => token!(self; TokenKind::Punctuation(Punctuation::Semicolon), ";"),
            '%' => token!(self; TokenKind::Punctuation(Punctuation::Mod), "%"),
            '*' => token!(self; TokenKind::Punctuation(Punctuation::Asterisk), "*"),
            ',' => token!(self; TokenKind::Punctuation(Punctuation::Comma), ","),
            '+' => token!(self; TokenKind::Punctuation(Punctuation::Plus), "+"),
            '^' => token!(self; TokenKind::Punctuation(Punctuation::Exp), "^"),
            '!' => token!(self; TokenKind::Punctuation(Punctuation::Not), "!"),
            c => {
                
                // Numeric tokens
                // we don't tokenize negative numbers yet because at this stage they can be mistaken with a subtraction operation

                // if next two characters are 0x start get a hex number
                if c == '0'
                    && let Some(char) = self.peek_char(0)
                    && (char == 'x' || char == 'X')
                {
                    // we have already peeked at the character, as such we know what it is and don't do anything with the output
                    let _ = self.next_char_unfiltered();
                    let mut number: String = "0x".into();
                    number.push_str(&self.take_while(|c| c.is_ascii_hexdigit()));
                    token!(self;
                        TokenKind::Number(Number::Hex(
                            u64::from_str_radix(&number[2..], 16).unwrap(),
                        )),
                        number
                    );
                }

                // knowing that it isn't a hex number, we get a decimal one
                if c.is_numeric() {
                    let mut number: String = c.into();
                    number.push_str(&self.take_while(|c| c.is_numeric() || c == '.'));
                    let mut found_dot = false;
                    let chars = number.chars().collect::<Rc<_>>();
                    let mut i = 0;
                    let chars_len = chars.len();
                    loop {
                        // break loop if ran out of characters
                        if i == chars.len() {
                            break;
                        }

                        // get current character
                        let char = chars[i];

                        // increase the index
                        i += 1;

                        if char == '.' {
                            // if already found dot, decrease the index and decrease the pos by the number of not read charactesr
                            if found_dot {
                                i -= 1;
                                self.pos -= chars_len - i;
                                break;
                            }

                            // if is a dot, set found_dot to true to indicate it's a floating-point number
                            found_dot = true;
                        }

                    }
                    // set the number string to the slice of number that has been marked out as the number
                    let number = String::from_iter(&chars[..i]);

                    if found_dot {
                        // if there's been a dot in the number, tokenize it as a float
                        if let Ok(num) = number.parse() {
                            return (
                                Ok(Token::new(
                                    self,
                                    TokenKind::Number(Number::Float(num)),
                                    number.into(),
                                    self.filename.clone(),
                                )),
                                false,
                            );
                        }
                    } else {
                        // if not, tokenize it as an integer
                        if let Ok(num) = number.parse() {
                            return (
                                Ok(Token::new(
                                    self,
                                    TokenKind::Number(Number::Int(num)),
                                    number.into(),
                                    self.filename.clone(),
                                )),
                                false,
                            );
                        }
                    }
                }

                // not a number, so it is text thus we tokenize it as an identifier
                if c.is_alphabetic() || c == '_' {
                    let mut id: String = c.into();
                    id.push_str(&self.take_while(|c| c.is_alphanumeric() || c == '_'));
                    let id: Rc<str> = id.into();
                    return (Ok(token!(self; TokenKind::ID(id.clone()), id)), false);
                }
                throw_unexpected_char!(c, self)
            }
        };
        (Ok(tok), false)
    }
    fn skip_unneeded(&mut self) {
        while let Some(char) = self.peek_char(0) {
            if char == ' ' || char == '\t' || char == '\n' {
                let _ = self.next_char_unfiltered();
            } else {
                break;
            }
        }
    }
    fn take_until(&mut self, pattern: &str) -> Rc<str> {
        let pattern: Vec<char> = pattern.chars().collect();
        let mut pos = 0;
        let mut out = String::new();
        let mut buf = String::new();
        loop {
            if pattern.len() == pos {
                break;
            }
            if let Ok(char) = self.next_char_unfiltered() {
                if char == pattern[pos] {
                    buf.push(char);
                    pos += 1;
                } else {
                    out.push_str(&buf);
                    pos = 0;
                    out.push(char);
                    buf.clear();
                }
            } else {
                break;
            }
        }
        out.into()
    }
    fn take_while(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut out = String::new();
        while let Some(char) = self.peek_char(0)
            && condition(char)
        {
            out.push(self.next_char_unfiltered().unwrap());
        }
        out
    }
    fn next_char_unfiltered(&mut self) -> LexerResult<char> {
        if self.chars.len() - self.pos <= 0 {
            return Err(LexerError::UnexpectedEof{
                line: self.line,
                col: self.col - 1,
                filename: self.filename.to_string(),
            });
        }
        let char = self.chars[self.pos];
        self.pos += 1;
        self.col += 1;
        if char == '\n' {
            self.col = 1;
            self.line += 1;
        }
        Ok(char)
    }
    fn next_char(&mut self) -> LexerResult<char> {
        let char = self.next_char_unfiltered();
        if let Ok(char) = char {
            if char == '\n' || char == '\t' || char == ' ' {
                return self.next_char();
            }
        }
        char
    }
    fn peek_char(&self, offset: i32) -> Option<char> {
        if (self.chars.len() - self.pos) as i32 - offset <= 0 {
            return None;
        }
        if self.pos as i32 + offset < 0 {
            return None;
        }
        Some(self.chars[(self.pos as i32 + offset) as usize])
    }
}

#[macro_export]
macro_rules! try_next_char {
    ($self:expr) => {{
        let next_char = $self.next_char();
        if let Ok(char) = next_char {
            char
        } else {
            return (Err(next_char.err().unwrap()), false);
        }
    }};
}
#[macro_export]
macro_rules! throw_unexpected_char {
    ($char:expr, $lex:expr) => {
        return (
            Err(LexerError::UnexpectedChar{
                char: $char,
                line: $lex.line,
                col: $lex.col - 1,
                filename: $lex.filename.to_string(),
            }),
            false,
        )
    };
}

#[macro_export]
macro_rules! token {
    ($self:expr; $tk:expr, $contents:expr) => {
        Token::new(
            $self,
            $tk,
            $contents.into(),
            $self.filename.clone(),
        )
    };
    ($self:expr; _ => $tk:expr, $contents:expr; $($char:literal => $_tk:expr, $_contents:expr;)* $( b $char2:literal => $block:block);*) => {{
        match $self.peek_char(0) {
            $(Some($char) => {
                $crate::try_next_char!($self);
                $crate::token!($self; $_tk, $_contents)
            },)*
            $(Some($char2) => $block,)*
            _ => $crate::token!($self; $tk, $contents),
        }
    }};
}
