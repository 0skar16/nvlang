
use std::rc::Rc;
pub mod ast;
pub mod parser;
pub mod lexer;
pub mod compiler;

type Str = Rc<str>;