use std::fmt::Display;

use super::{imm::I12, register::RegId};
#[allow(unused)]

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
pub enum RiscvInst{

    // Arithmetic
    Add(RegId,RegId,RegId),
    Addi(RegId,RegId,I12),
    Sub(RegId,RegId,RegId),
    Mul(RegId,RegId,RegId),
    Div(RegId,RegId,RegId),
    Neg(RegId,RegId),
    Rem(RegId,RegId,RegId),


    // Load immediate value
    Li(RegId,i32),
    /// Move
    Mv(RegId,RegId),

    // Bitwise
    And(RegId,RegId,RegId),
    Andi(RegId,RegId,I12),
    Not(RegId,RegId),
    Or(RegId,RegId,RegId),
    Ori(RegId,RegId,I12),
    Xor(RegId,RegId,RegId),
    Xori(RegId,RegId,I12),

    // Compare
    /// Set less than
    /// rd = (rs1 < rs2) ? 1 : 0
    Slt(RegId,RegId,RegId),
    /// Set less than immediate
    /// rd = (rs1 < imm) ? 1 : 0
    Slti(RegId,RegId,I12),
    /// Set less than unsigned
    /// rd = (rs1 < rs2) ? 1 : 0
    Sltu(RegId,RegId,RegId),
    /// Set less than immediate unsigned
    /// rd = (rs1 < imm) ? 1 : 0
    Sltiu(RegId,RegId,I12),
    /// Set equal to zero
    /// rd = (rs1 == 0) ? 1 : 0
    Seqz(RegId,RegId),
    /// Set not equal to zero
    /// rd = (rs1 != 0) ? 1 : 0
    Snez(RegId,RegId),
    /// Set less than zero
    /// rd = (rs1 < 0) ? 1 : 0
    Sltz(RegId,RegId),
    /// Set greater than zero
    /// rd = (rs1 > 0) ? 1 : 0
    Sgtz(RegId,RegId),
    /// Set greater than
    /// rd = (rs1 > rs2) ? 1 : 0
    Sgt(RegId,RegId,RegId),

    // Shift
    // TODO

    // Memory
    /// Load word
    /// rd = memory[rs1 + imm]
    Lw(RegId,I12,RegId),
    /// Store word
    /// memory[rs1 + imm] = rs2
    Sw(RegId,I12,RegId),

    // Control flow
    /// Jump 
    /// TODO: Use Label instead of String
    J(String),
    /// Branch if equal
    /// if rs1 == rs2 then jump to imm
    Beq(RegId,RegId,String),
    /// Branch if equal zero
    /// if rs1 == 0 then jump to imm
    Beqz(RegId,String),
    /// Branch if not equal
    /// if rs1 != rs2 then jump to imm
    Bne(RegId,RegId,String),
    /// Branch if not equal zero
    /// if rs1 != 0 then jump to imm
    Bnez(RegId,String),

    // TODO
    Ret,

    Nop,


}

impl Display for RiscvInst{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            RiscvInst::Add(rd,rs1,rs2) => write!(f,"add {},{},{}",rd,rs1,rs2),
            RiscvInst::Addi(rd,rs1,imm) => write!(f,"addi {},{},{}",rd,rs1,imm),
            RiscvInst::Sub(rd,rs1,rs2) => write!(f,"sub {},{},{}",rd,rs1,rs2),
            RiscvInst::Mul(rd,rs1,rs2) => write!(f,"mul {},{},{}",rd,rs1,rs2),
            RiscvInst::Div(rd,rs1,rs2) => write!(f,"div {},{},{}",rd,rs1,rs2),
            RiscvInst::Neg(rd,rs1) => write!(f,"neg {},{}",rd,rs1),
            RiscvInst::Rem(rd,rs1,rs2) => write!(f,"rem {},{},{}",rd,rs1,rs2),
            RiscvInst::Li(rd,imm) => write!(f,"li {},{}",rd,imm),
            RiscvInst::Mv(rd,rs1) => write!(f,"mv {},{}",rd,rs1),
            RiscvInst::And(rd,rs1,rs2) => write!(f,"and {},{},{}",rd,rs1,rs2),
            RiscvInst::Andi(rd,rs1,imm) => write!(f,"andi {},{},{}",rd,rs1,imm),
            RiscvInst::Not(rd,rs1) => write!(f,"not {},{}",rd,rs1),
            RiscvInst::Or(rd,rs1,rs2) => write!(f,"or {},{},{}",rd,rs1,rs2),
            RiscvInst::Ori(rd,rs1,imm) => write!(f,"ori {},{},{}",rd,rs1,imm),
            RiscvInst::Xor(rd,rs1,rs2) => write!(f,"xor {},{},{}",rd,rs1,rs2),
            RiscvInst::Xori(rd,rs1,imm) => write!(f,"xori {},{},{}",rd,rs1,imm),
            RiscvInst::Slt(rd,rs1,rs2) => write!(f,"slt {},{},{}",rd,rs1,rs2),
            RiscvInst::Slti(rd,rs1,imm) => write!(f,"slti {},{},{}",rd,rs1,imm),
            RiscvInst::Sltu(rd,rs1,rs2) => write!(f,"sltu {},{},{}",rd,rs1,rs2),
            RiscvInst::Sltiu(rd,rs1,imm) => write!(f,"sltiu {},{},{}",rd,rs1,imm),
            RiscvInst::Seqz(rd,rs1) => write!(f,"seqz {},{}",rd,rs1),
            RiscvInst::Snez(rd,rs1) => write!(f,"snez {},{}",rd,rs1),
            RiscvInst::Sltz(rd,rs1) => write!(f,"sltz {},{}",rd,rs1),
            RiscvInst::Sgtz(rd,rs1) => write!(f,"sgtz {},{}",rd,rs1),
            RiscvInst::Sgt(rd,rs1,rs2) => write!(f,"sgt {},{},{}",rd,rs1,rs2),
            RiscvInst::Lw(rd,imm,rs1) => write!(f,"lw {},{}({})",rd,imm,rs1),
            RiscvInst::Sw(rd,imm,rs1) => write!(f,"sw {},{}({})",rd,imm,rs1),
            RiscvInst::J(label) => write!(f,"j {}",label),
            RiscvInst::Beq(rs1,rs2,label) => write!(f,"beq {},{},{}",rs1,rs2,label),
            RiscvInst::Beqz(rs1,label) => write!(f,"beqz {},{}",rs1,label),
            RiscvInst::Bne(rs1,rs2,label) => write!(f,"bne {},{},{}",rs1,rs2,label),
            RiscvInst::Bnez(rs1,label) => write!(f,"bnez {},{}",rs1,label),
            RiscvInst::Ret => write!(f,"ret"),
            RiscvInst::Nop => write!(f,"nop"),
        }
    }
}

pub struct Block{
    pub label: Option<String>,
    pub insts: Vec<RiscvInst>,
}

impl Display for Block{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = &self.label{
            write!(f,"{}:\n",label)?;
        }
        for inst in &self.insts{
            write!(f,"    {}\n",inst)?;
        }
        Ok(())
    }
}
impl Block{
    pub fn push_inst_back(&mut self,inst: RiscvInst){
        self.insts.push(inst);
    }
    pub fn push_inst_front(&mut self,inst: RiscvInst){
        self.insts.insert(0,inst);
    }
    pub fn new(label:Option<String> ) -> Self{
        Block{
            label,
            insts: Vec::new(),
        }
    }
}

pub struct Function{
    pub name: String,
    pub blocks: Vec<Block>,
    // pub stack_frame: StackFrame,
    // pub ret_type: String, // TODO: Use Type instead of String
}

impl Display for Function{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}:\n",self.name)?;
        for block in &self.blocks{
            write!(f,"{}",block)?;
        }
        Ok(())
    }
}

impl Function{
    pub fn push_block_back(&mut self,block: Block){
        self.blocks.push(block);
    }
    pub fn push_block_front(&mut self,block: Block){
        self.blocks.insert(0,block);
    }
    pub fn new(name: String) -> Self{
        Function{
            name,
            blocks: Vec::new(),
            // stack_frame: StackFrame::new(),
            // ret_type,
        }
    }
    pub fn get_front_block(&self) -> Option<&Block>{
        self.blocks.first()
    }
}

pub struct Program{
    pub functions: Vec<Function>,
}

impl Display for Program{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions{
            write!(f,"{}",func)?;
        }
        Ok(())
    }
}
impl Program{
    pub fn push_func_back(&mut self,func: Function){
        self.functions.push(func);
    }
    pub fn push_func_front(&mut self,func: Function){
        self.functions.insert(0,func);
    }
    pub fn new() -> Self{
        Program{
            functions: Vec::new(),
        }
    }
}


pub enum TempRiscv{
    Segment(String),
    Label(String),
    Inst(RiscvInst),
}

impl Display for TempRiscv{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            TempRiscv::Segment(segment) => write!(f,"{}",segment),
            TempRiscv::Label(label) => write!(f,"{}:",label),
            TempRiscv::Inst(inst) => write!(f,"    {}",inst),
        }
    }
}