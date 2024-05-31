use std::{
    collections::BTreeMap, hash::Hash, ops::{Deref, DerefMut}, rc::Rc
};
use crate::Str;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Module {
    pub sub_modules: Vec<String>,
    pub uses: Vec<Use>,
    pub mappings: BTreeMap<String, String>,
    pub entries: BTreeMap<Str, Entry>,
    pub functions: BTreeMap<Str, Function>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Use {
    pub source: UseSource,
    pub path: Rc<[Str]>,
    pub used: Rc<[Str]>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UseSource {
    Internal,
    External,
    CExternal,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Entry {
    pub type_sig: TypeSignature,
    pub block: Block,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub type_sig: TypeSignature,
    pub id: u64,
    pub block: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeSignature {
    pub args: Rc<[(Str, Type)]>,
    pub ret_type: Type,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Call {
        called: Box<Statement>, 
        args: Vec<Statement>,
        entry: bool,
    },
    Get(Str),
    Child{
        parent: Box<Statement>,
        child: Str,
    },
    Let {
        name: Str, 
        _type: Option<Type>, 
        value: Option<Box<Statement>>,
    },
    Assignment {
        target: Box<Statement>, 
        value: Box<Statement>,
    },
    Operation{
        operand: Box<Statement>,
        operation: Operation, 
        operand2: Option<Box<Statement>>,
    },
    OperationAssignment{
        target: Box<Statement>, 
        operation: Operation, 
        operand: Box<Statement>,
    },
    Literal(Literal),
    Return(Box<Statement>),
    For{
        init: Box<Statement>,
        condition: Box<Statement>,
        change: Box<Statement>,
        block: Block,
    },
    While{
        condition: Box<Statement>, 
        block: Block,
    },
    If{
        condition: Box<Statement>,
        block: Block,
        else_ifs: Vec<(Statement, Block)>,
        _else: Option<Block>,
    },
    Paren(Box<Statement>),
    Index{
        source: Box<Statement>, 
        index: Box<Statement>,
    },
    LoopOperation(LoopOp),
    Ref(Box<Statement>),
    Null,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
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
    Ref(Box<Type>),
    Slice(Box<Type>),
    Ptr(Box<Type>),
    Struct(Str),
    Null,
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
pub struct Block(pub Rc<[Statement]>);

impl DerefMut for Block {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Deref for Block {
    type Target = Rc<[Statement]>;

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
    String(Str),
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