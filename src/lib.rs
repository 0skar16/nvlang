#![feature(let_chains)]

use std::rc::Rc;
pub mod ast;
pub mod parser;
pub mod lexer;

type Str = Rc<str>;