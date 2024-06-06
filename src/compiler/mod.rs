use std::{
    cell::{RefCell, RefMut},
    collections::BTreeMap,
};

use frame::StackFrame;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::BasicMetadataTypeEnum,
    values::{AnyValue, BasicValue, BasicValueEnum, FunctionValue},
};
use module::ExpandedModuleTree;
use thiserror::Error;
use translation::{IntoBasicValueEnumOption, IntoTypeOptionBasicValue};

use crate::{
    ast::{BasicType, Block, Literal, Operation, Statement, Type},
    Str,
};

pub mod frame;
pub mod module;
pub mod translation;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum CompilerError {
    #[error("Compiler errored:\n\tEntry/extern under the name [{0}] already exists")]
    EntryAlreadyExistsInFrame(Str),
    #[error("Compiler errored:\n\t[{0:?}] type is not expected in this case")]
    UnexpectedType(Type),
    #[error("Compiler errored:\n\tTried to assign a negative value to an unsigned value")]
    NegativeTypeInUnsigned,
    #[error("Compiler errored:\n\tVariable [{0}] not found in this context")]
    VariableNotFound(Str),
    #[error("Compiler errored:\n\tVariable [{0}] doesn't have a value assigned")]
    VariableNoValueAssigned(Str),
}
pub type CompilerResult<T> = std::result::Result<T, CompilerError>;

pub struct Compiler {
    module_tree: ExpandedModuleTree,
    llvm_ctx: Context,
}

impl Compiler {
    pub fn new(module_tree: ExpandedModuleTree) -> Self {
        Self {
            module_tree,
            llvm_ctx: Context::create(),
        }
    }
    pub fn build<'a>(&'a mut self) -> CompilerResult<Vec<Module<'a>>> {
        let mut paths = vec![vec![]];
        for (sub_module_name, sub_module) in &self.module_tree.modules {
            Self::walk_tree(sub_module, sub_module_name.clone(), &vec![], &mut paths);
        }
        let mut modules = vec![];

        for path in paths {
            modules.push(self.build_module(path)?);
        }

        Ok(modules)
    }
    fn walk_tree(
        module: &ExpandedModuleTree,
        name: String,
        parent: &Vec<String>,
        paths: &mut Vec<Vec<String>>,
    ) {
        let mut path = parent.clone();
        path.push(name);
        paths.push(path.clone());
        for (sub_module_name, sub_module) in &module.modules {
            Self::walk_tree(sub_module, sub_module_name.clone(), &path, paths);
        }
    }
    fn build_module(&self, mut path: Vec<String>) -> CompilerResult<Module> {
        let builder = self.llvm_ctx.create_builder();
        let mut expanded_module = &self.module_tree;
        for path_el in &path {
            let Some(_expanded_module) = expanded_module.modules.get(path_el) else {
                unreachable!("internal module search failed")
            };
            expanded_module = _expanded_module;
        }

        let mut frame = StackFrame {
            entries: BTreeMap::new(),
            functions: BTreeMap::new(),
            variables: BTreeMap::new(),
        };

        //frame.functions.insert(name.clone(), (function.type_sig.args.iter().map(|(_, ty)| ty.clone()).collect(), function.type_sig.ret_type.clone()));

        path.insert(0, "self".to_string());

        let llvm_module = self.llvm_ctx.create_module(&path.join("/"));

        for extern_use in &expanded_module.extern_uses {
            let args: Option<Vec<BasicMetadataTypeEnum>> = extern_use
                .args
                .iter()
                .map(|arg_type| arg_type.to_basic_type(&self.llvm_ctx).map(|t| t.into()))
                .collect();
            let Some(args) = args else {
                return Err(todo!());
            };

            let Some(fn_type) =
                extern_use
                    .ret_type
                    .fn_type(&self.llvm_ctx, &args, extern_use.arg_list)
            else {
                return Err(todo!());
            };

            let fn_value =
                llvm_module.add_function(&*extern_use.name, fn_type, Some(Linkage::External));

            let None = frame.entries.insert(
                extern_use.name.clone(),
                (
                    extern_use.args.clone(),
                    extern_use.ret_type.clone(),
                    extern_use.arg_list,
                    fn_value,
                ),
            ) else {
                return Err(CompilerError::EntryAlreadyExistsInFrame(
                    extern_use.name.clone(),
                ));
            };
        }

        for (name, entry) in &expanded_module.entries {
            let args: Option<Vec<BasicMetadataTypeEnum>> = entry
                .type_sig
                .args
                .iter()
                .map(|(_, arg_type)| arg_type.to_basic_type(&self.llvm_ctx).map(|t| t.into()))
                .collect();
            let Some(args) = args else {
                return Err(todo!());
            };

            let Some(fn_type) = entry
                .type_sig
                .ret_type
                .fn_type(&self.llvm_ctx, &args, false)
            else {
                return Err(todo!());
            };

            let function = llvm_module.add_function(&name, fn_type, None);

            let None = frame.entries.insert(
                name.clone(),
                (
                    entry
                        .type_sig
                        .args
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .collect(),
                    entry.type_sig.ret_type.clone(),
                    false,
                    function,
                ),
            ) else {
                return Err(CompilerError::EntryAlreadyExistsInFrame(name.clone()));
            };

            let mut frame = frame.clone();
            frame.variables.extend(
                entry
                    .type_sig
                    .args
                    .iter()
                    .zip(function.get_param_iter())
                    .map(|((name, ty), val)| (name.clone(), (ty.clone(), Some(val.into())))),
            );

            self.build_block(&frame, &builder, &entry.block, "entry".into(), function)?;
        }

        println!("{}", llvm_module.print_to_string().to_string());

        Ok(llvm_module)
    }

    fn build_block<'a>(
        &'a self,
        frame: &StackFrame<'a>,
        builder: &'a Builder<'a>,
        block: &Block,
        name: String,
        function: FunctionValue<'a>,
    ) -> CompilerResult<()> {
        let frame = RefCell::new(frame.clone());

        let basic_block = self.llvm_ctx.append_basic_block(function, &name);
        builder.position_at_end(basic_block);
        for statement in block.iter() {
            self.build_standalone_statement(frame.borrow_mut(), builder, statement)?;
        }
        Ok(())
    }
    fn build_standalone_statement<'a>(
        &'a self,
        mut frame: RefMut<'a, StackFrame<'a>>,
        builder: &'a Builder<'a>,
        statement: &Statement,
    ) -> CompilerResult<()> {
        match statement {
            crate::ast::Statement::Call {
                called,
                args,
                entry,
            } => {
                self.build_call(&*frame, builder, called.clone(), args.clone(), *entry)?;
            }
            crate::ast::Statement::Let { name, _type, value } => {
                self.build_let(frame, builder, name.clone(), _type.clone(), value.clone())?
            }
            crate::ast::Statement::Assignment {
                target: _,
                value: _,
            } => todo!(),
            crate::ast::Statement::OperationAssignment {
                target: _,
                operation: _,
                operand: _,
            } => todo!(),
            crate::ast::Statement::Return(value) => {
                let v = self.build_statement(&*frame, builder, None, &value)?.1;
                if let Some(v) = v {
                    builder.build_return(Some(&v)).unwrap();
                } else {
                    builder.build_return(None).unwrap();
                }
            }
            crate::ast::Statement::For {
                init: _,
                condition: _,
                change: _,
                block: _,
            } => todo!(),
            crate::ast::Statement::While {
                condition: _,
                block: _,
            } => todo!(),
            crate::ast::Statement::If {
                condition: _,
                block: _,
                else_ifs: _,
                _else,
            } => todo!(),
            crate::ast::Statement::LoopOperation(_) => todo!(),
            _ => unreachable!(),
        };
        Ok(())
    }
    fn build_statement<'a>(
        &'a self,
        frame: &StackFrame<'a>,
        builder: &'a Builder<'a>,
        ty: Option<Type>,
        statement: &Statement,
    ) -> CompilerResult<(Type, Option<BasicValueEnum>)> {
        Ok(match statement {
            Statement::Literal(lit) => self
                .build_literal(builder, ty.clone(), lit.clone())?
                .into_type_option_basic_value(),
            Statement::Get(name) => self
                .build_get(&*frame, ty.clone(), name.clone())?
                .into_type_option_basic_value(),
            Statement::Call {
                called,
                args,
                entry,
            } => self.build_call(&*frame, builder, called.clone(), args.clone(), *entry)?,
            Statement::Child { parent, child } => todo!(),
            Statement::Operation {
                operand,
                operation,
                operand2,
            } => {
                let (operation_type, Some(operand)) =
                    self.build_statement(frame, builder, None, &operand)?
                else {
                    todo!()
                };

                let (_, Some(operand2)) = self.build_statement(
                    frame,
                    builder,
                    Some(operation_type.clone()),
                    &operand2.clone().unwrap(),
                )?
                else {
                    todo!()
                };

                let out: BasicValueEnum = match BasicType::from(operation_type.clone()) {
                    BasicType::Integer => match operation {
                        Operation::Add => builder
                            .build_int_add(
                                operand.into_int_value(),
                                operand2.into_int_value(),
                                "add",
                            )
                            .unwrap()
                            .into(),
                        Operation::Sub => builder
                            .build_int_sub(
                                operand.into_int_value(),
                                operand2.into_int_value(),
                                "sub",
                            )
                            .unwrap()
                            .into(),
                        Operation::Mul => builder
                            .build_int_mul(
                                operand.into_int_value(),
                                operand2.into_int_value(),
                                "mul",
                            )
                            .unwrap()
                            .into(),
                        _ => todo!(),
                    },
                    _ => todo!(),
                };

                (operation_type, Some(out))
            }
            Statement::Paren(_) => todo!(),
            Statement::Index { source, index } => todo!(),
            Statement::Ref(_) => todo!(),
            Statement::Null => todo!(),
            _ => unreachable!(),
        })
    }
    fn build_let<'a>(
        &'a self,
        mut frame: RefMut<'a, StackFrame<'a>>,
        builder: &'a Builder<'a>,
        name: Str,
        ty: Option<Type>,
        value: Option<Box<Statement>>,
    ) -> CompilerResult<()> {
        let (ty, value) = if let Some(value) = value {
            let (ty, value) = match *value {
                Statement::Literal(lit) => self.build_literal(builder, ty.clone(), lit)?,
                Statement::Get(name) => self.build_get(&*frame, ty.clone(), name)?,
                _ => todo!(),
            };
            (ty, Some(value))
        } else {
            (ty.unwrap_or(Type::I32), None)
        };

        frame.variables.insert(name, (ty, value));
        Ok(())
    }
    fn build_literal<'a>(
        &'a self,
        builder: &'a Builder<'a>,
        ty: Option<Type>,
        lit: Literal,
    ) -> CompilerResult<(Type, BasicValueEnum)> {
        Ok(match lit {
            Literal::Integer(int) => {
                let sign_extend = int.is_negative();
                let int = int.abs() as u64;

                let ty = ty.unwrap_or(Type::I32);

                (
                    ty.clone(),
                    match ty {
                        Type::Char | Type::I8 => {
                            self.llvm_ctx.i8_type().const_int(int, sign_extend)
                        }
                        Type::U8 => {
                            if sign_extend {
                                return Err(CompilerError::NegativeTypeInUnsigned);
                            } else {
                                self.llvm_ctx.i8_type().const_int(int, false)
                            }
                        }
                        Type::I16 => self.llvm_ctx.i16_type().const_int(int, sign_extend),
                        Type::U16 => {
                            if sign_extend {
                                return Err(CompilerError::NegativeTypeInUnsigned);
                            } else {
                                self.llvm_ctx.i16_type().const_int(int, false)
                            }
                        }
                        Type::I32 => self.llvm_ctx.i32_type().const_int(int, sign_extend),
                        Type::U32 => {
                            if sign_extend {
                                return Err(CompilerError::NegativeTypeInUnsigned);
                            } else {
                                self.llvm_ctx.i32_type().const_int(int, false)
                            }
                        }
                        Type::I64 => self.llvm_ctx.i64_type().const_int(int, sign_extend),
                        Type::U64 => {
                            if sign_extend {
                                return Err(CompilerError::NegativeTypeInUnsigned);
                            } else {
                                self.llvm_ctx.i64_type().const_int(int, false)
                            }
                        }
                        _ => unreachable!(),
                    }
                    .into(),
                )
            }
            Literal::String(s) => {
                let string = unsafe { builder.build_global_string(&s, "string_").unwrap() };
                let value = unsafe {
                    builder
                        .build_gep(
                            string.as_basic_value_enum().get_type(),
                            string.as_pointer_value(),
                            &[
                                self.llvm_ctx.i64_type().const_int(0, false),
                                self.llvm_ctx.i64_type().const_int(0, false),
                            ],
                            "cast",
                        )
                        .unwrap()
                };
                (Type::Ptr(Box::new(Type::Char)), value.into())
            }
            _ => todo!(),
        })
    }
    fn build_get<'a>(
        &'a self,
        frame: &StackFrame<'a>,
        ty: Option<Type>,
        name: Str,
    ) -> CompilerResult<(Type, BasicValueEnum<'a>)> {
        let Some((ty, value)) = frame.variables.get(&name) else {
            return Err(CompilerError::VariableNotFound(name));
        };

        let Some(value) = value else {
            return Err(CompilerError::VariableNoValueAssigned(name));
        };

        Ok((ty.clone(), *value))
    }
    fn build_call<'a>(
        &'a self,
        frame: &StackFrame<'a>,
        builder: &'a Builder<'a>,
        called: Str,
        args: Vec<Statement>,
        entry: bool,
    ) -> CompilerResult<(Type, Option<BasicValueEnum<'a>>)> {
        let (arg_types, ret_type, function, arg_list) = if entry {
            let Some((args, ret_type, arg_list, value)) = frame.entries.get(&called) else {
                todo!()
            };
            (args.clone(), ret_type.clone(), value.clone(), *arg_list)
        } else {
            let Some((args, ret_type, value)) = frame.functions.get(&called) else {
                todo!()
            };
            (args.clone(), ret_type.clone(), value.clone(), false)
        };

        let mut llvm_args = vec![];
        let mut i = 0;
        for arg in args {
            let ty = if i < arg_types.len() {
                Some(arg_types[i].clone())
            } else {
                if arg_list {
                    None
                } else {
                    todo!()
                }
            };
            llvm_args.push(
                self.build_statement(frame, builder, ty, &arg)?
                    .1
                    .unwrap()
                    .into(),
            );
            i += 1;
        }

        let value = builder
            .build_direct_call(function, &llvm_args, &format!("{called}_call_return"))
            .unwrap();
        Ok((
            ret_type,
            value.as_any_value_enum().into_basic_value_enum_opt(),
        ))
    }
}
