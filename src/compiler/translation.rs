use inkwell::{
    context::Context,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{AnyValueEnum, BasicValueEnum},
    AddressSpace,
};

use crate::ast::Type;

impl Type {
    pub(super) fn to_any_llvm_type<'a>(&'a self, llvm_ctx: &'a Context) -> Option<AnyTypeEnum<'a>> {
        Some(match self {
            Type::Char => Type::I8.to_any_llvm_type(llvm_ctx)?,
            Type::F32 => todo!(),
            Type::F64 => todo!(),
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64 => self.to_basic_type(llvm_ctx)?.into_int_type().into(),
            Type::Ref(_) => todo!(),
            Type::Slice(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Ptr(ty) => ty
                .to_basic_type(llvm_ctx)?
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Null => AnyTypeEnum::VoidType(llvm_ctx.void_type()),
            _ => {
                return None;
            }
        })
    }
    pub(super) fn to_basic_type<'a>(&'a self, llvm_ctx: &'a Context) -> Option<BasicTypeEnum<'a>> {
        Some(match self {
            Type::Char => Type::I8.to_basic_type(llvm_ctx)?,
            Type::F32 => todo!(),
            Type::F64 => todo!(),
            Type::I8 => llvm_ctx.i8_type().into(),
            Type::I16 => todo!(),
            Type::I32 => llvm_ctx.i32_type().into(),
            Type::I64 => todo!(),
            Type::U8 => todo!(),
            Type::U16 => todo!(),
            Type::U32 => todo!(),
            Type::U64 => todo!(),
            Type::Ref(_) => todo!(),
            Type::Slice(_) => todo!(),
            Type::Ptr(_) => llvm_ctx.ptr_type(AddressSpace::default()).into(),
            Type::Struct(_) => todo!(),
            _ => return None,
        })
    }
    pub(super) fn fn_type<'a>(
        &'a self,
        llvm_ctx: &'a Context,
        args: &[BasicMetadataTypeEnum<'a>],
        arg_list: bool,
    ) -> Option<FunctionType<'a>> {
        Some(match self.to_any_llvm_type(llvm_ctx)? {
            inkwell::types::AnyTypeEnum::FloatType(_) => todo!(),
            inkwell::types::AnyTypeEnum::IntType(ty) => ty.fn_type(args, arg_list),
            inkwell::types::AnyTypeEnum::PointerType(_) => todo!(),
            inkwell::types::AnyTypeEnum::StructType(_) => todo!(),
            inkwell::types::AnyTypeEnum::VoidType(ty) => ty.fn_type(args, arg_list),
            _ => unreachable!(),
        })
    }
}

pub(super) trait IntoBasicValueEnumOption<'a> {
    fn into_basic_value_enum_opt(self) -> Option<BasicValueEnum<'a>>;
}

impl<'a> IntoBasicValueEnumOption<'a> for AnyValueEnum<'a> {
    fn into_basic_value_enum_opt(self) -> Option<BasicValueEnum<'a>> {
        Some(match self {
            AnyValueEnum::ArrayValue(v) => v.into(),
            AnyValueEnum::IntValue(v) => v.into(),
            AnyValueEnum::FloatValue(v) => v.into(),
            AnyValueEnum::PointerValue(v) => v.into(),
            AnyValueEnum::StructValue(v) => v.into(),
            AnyValueEnum::VectorValue(v) => v.into(),
            _ => {
                return None;
            }
        })
    }
}

pub(super) trait IntoTypeOptionBasicValue<'a> {
    fn into_type_option_basic_value(self) -> (Type, Option<BasicValueEnum<'a>>);
}

impl<'a> IntoTypeOptionBasicValue<'a> for (Type, BasicValueEnum<'a>) {
    fn into_type_option_basic_value(self) -> (Type, Option<BasicValueEnum<'a>>) {
        (self.0, Some(self.1))
    }
}
