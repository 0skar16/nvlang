use std::rc::Rc;
pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;

type Str = Rc<str>;
