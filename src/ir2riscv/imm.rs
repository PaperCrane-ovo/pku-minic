#![allow(dead_code)]

use std::{fmt::Display, ops::Neg};


use super::{register::RegId, riscv::RiscvInst, riscvgen::AsmProgram};
pub enum I12pos{
    Imm12(I12),
    RegId(RegId),
}
impl I12pos{
    pub fn get_value(&self) -> I12{
        match self{
            I12pos::Imm12(i12) => *i12,
            I12pos::RegId(regid) => panic!("I12Pos::get_value() called on RegId({:?})",regid),
        }
    }
    pub fn get_regid(&self) -> RegId{
        match self{
            I12pos::Imm12(i12) => panic!("I12Pos::get_regid() called on Imm12({:?})",i12),
            I12pos::RegId(regid) => *regid,
        }
    }
}
#[derive(Debug,Clone,PartialEq,Eq,Hash,Copy)]
pub struct I12{
    pub value: i32,
} 
impl I12{
    pub fn build(value: i32,asm:&mut AsmProgram) -> I12pos{
        if value >= -2048 && value <= 2047 {
            I12pos::Imm12(I12{value})
        }else{
            asm.push(super::riscv::TempRiscv::Inst(RiscvInst::Li(RegId::T2,value)));
            I12pos::RegId(RegId::T2)
            
        }
    }
    
}
impl Display for I12{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Neg for I12{
    type Output = Self;
    fn neg(self) -> Self::Output {
        I12{value: -self.value}
    }
}

