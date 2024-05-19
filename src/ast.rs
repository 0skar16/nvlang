use std::{
    collections::BTreeMap, hash::Hash, ops::{Deref, DerefMut}, rc::Rc
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Module {
    pub sub_modules: Vec<String>,
    pub uses: Vec<Use>,
    pub mapping: BTreeMap<String, String>,
    pub entries: BTreeMap<String, Entry>,
    pub function: BTreeMap<String, Function>,
}

pub struct Use {
    source: UseSource,
    path: Vec<String>,
    used: Vec<String>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UseSource {
    Internal,
    External,
    CExternal,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Entry {
    pub args: Vec<(String, Type)>,
    pub ret_type: Type,
    pub block: Block,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub args: Vec<(String, Type)>,
    pub ret_type: Type,
    pub id: u64,
    pub block: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Call(Box<Statement>, Vec<Statement>),
    Get(Rc<str>),
    Child(Box<Statement>, Rc<str>),
    Method(Box<Statement>, Rc<str>),
    Let(Rc<str>, Type, Option<Box<Statement>>),
    Assignment(Box<Statement>, Box<Statement>),
    Operation(Box<Statement>, Operation, Option<Box<Statement>>),
    OperationAssignment(Box<Statement>, Operation, Box<Statement>),
    Literal(Literal),
    Return(Box<Statement>),
    For(Box<[Statement; 3]>, Block),
    While(Box<Statement>, Block),
    If(
        Box<Statement>,
        Block,
        Vec<(Statement, Block)>,
        Option<Block>,
    ),
    Paren(Box<Statement>),
    Index(Box<Statement>, Box<Statement>),
    LoopOperation(LoopOp),
    Ref(Box<Statement>),
    Null,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Char,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Ref(Rc<Type>),
    Slice(Rc<Type>),
    Ptr(Rc<Type>),
    Struct(Rc<str>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MatchOutput {
    Block(Block),
    Statement(Box<Statement>),
}
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum LoopOp {
    Break,
    Continue,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block(pub Vec<Statement>);

impl DerefMut for Block {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Deref for Block {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Operation {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    Lesser,
    GE,
    LE,
    Exp,
    Not,
    BitwiseOr,
    BitwiseAnd,
    ShiftLeft,
    ShiftRight,

}
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    String(Rc<str>),
    Boolean(bool),
    Slice(Vec<Statement>),
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::Float(f) => f.to_bits().hash(state),
            Literal::String(s) => s.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Slice(s) => s.hash(state),
            Literal::Integer(i) => i.hash(state),
        }
    }
}