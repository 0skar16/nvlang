use std::{collections::BTreeMap, rc::Rc};

use inkwell::values::{BasicValueEnum, FunctionValue};

use crate::{ast::Type, Str};

#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    pub entries: BTreeMap<Str, (Rc<[Type]>, Type, bool, FunctionValue<'a>)>,
    pub functions: BTreeMap<Str, (Rc<[Type]>, Type, FunctionValue<'a>)>,
    pub variables: BTreeMap<Str, (Type, Option<BasicValueEnum<'a>>)>,
}
