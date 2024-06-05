use std::collections::BTreeMap;

use crate::{
    ast::{Entry, Extern, Function, Use},
    Str,
};

pub struct ExpandedModuleTree {
    pub modules: BTreeMap<String, ExpandedModuleTree>,
    pub uses: Vec<Use>,
    pub extern_uses: Vec<Extern>,
    pub entries: BTreeMap<Str, Entry>,
    pub functions: BTreeMap<Str, Function>,
}
