use koopa::ir::builder_traits::*;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, TypeKind};

use crate::ast::*;

pub trait Generate_Program<'ast> {
    type Output;
    fn generate_program(&'ast self, program: &mut Program) -> Result<Self::Output>;
}

impl<'ast> Generate_Program<'ast> for CompUnit {
    type Output = ();
    fn generate_program(&'ast self, program: &mut Program) -> Result<Self::Output>{
        self.func_def.generate_program(program);
        Ok(())
    }
}

impl<'ast> Generate_Program<'ast> for FuncDef {
    type Output = ();
    fn generate_program(&'ast self, program: &mut Program) -> Result<Self::Output>{
        let func = program.new_func(FunctionData::new(
            format!("@{}", self.ident),
            Vec::new(),
            match self.func_type{
                FuncType::Int => Type::get_i32(),
            }
        ));

    }
}

impl<'ast> Generate_Program<'ast> for FuncType {
    type Output = Type;
    fn generate_program(&'ast self, program: &mut Program) -> Result<Self::Output>{
        Ok(match self{
            FuncType::Int => Type::get_i32(),
        
        })
    }
}

impl<'ast> Generate_Program<'ast> for Block {
    type Output = ();
    fn generate_program(&self, program: &mut Program) {
        self.stmt.generate_program(program);
    }
}