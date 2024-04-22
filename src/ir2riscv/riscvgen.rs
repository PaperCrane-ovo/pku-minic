
use koopa::ir::{dfg::DataFlowGraph, BinaryOp, FunctionData, Program, Value, ValueKind};

use crate::utils::is_const;

use super::register::{reg_cache, temp_reg, CUR_REG};


// 比较丑陋的实现：给koopa扩展特征，函数签名不同需要不同的函数。
pub trait GenerateAsm {
    fn generate (&self,asm: &mut String){}

}
pub trait GenerateAsmValue{
    fn load_imm(&self, asm: &mut String, dfg: &DataFlowGraph)-> Option<&str>{None}
    fn generate(&self,asm: &mut String,dfg: &DataFlowGraph){}
    fn load_value(&self,asm: &mut String,dfg: &DataFlowGraph) -> Option<&str>{
        None
    }
}

pub trait GenerateAsmValueData{
    fn generate(&self,asm: &mut String){}

}



impl GenerateAsm for Program{
    fn generate (&self,asm: &mut String) {
        for &func in self.func_layout() {
            let func_data = self.func(func);
            func_data.generate(asm);
        }
    }
}

impl GenerateAsm for FunctionData{
    fn generate (&self,asm: &mut String){
        let name = &self.name()[1..];
        asm.push_str("    .text\n");
        asm.push_str(&format!("    .globl {}\n", name));
        asm.push_str(&format!("{}:\n", name));
        
        let dfg = self.dfg();
        for (&bb,node) in self.layout().bbs(){
            // dbg!(node.insts().len());
            // dbg!(dfg.values());
            for &inst in node.insts().keys(){
                inst.generate(asm,dfg);
            }
        }
    }
}

// 既然实现了ValueData还要实现Value，不如直接实现Value

// impl GenerateAsm for ValueData{
//     fn generate (&self,asm: &mut String) {
//         match self.kind(){
//             ValueKind::Integer(i) => {
//                 asm.push_str(&format!("    li a0, {}\n", i.value()));
//             }
//             ValueKind::Return(ret) => {
//                 if let Some(ret_value) = ret.value(){
//                     ret_value.generate(asm);
//                 }
//                 asm.push_str(&format!("    ret\n"));
//             }
//             _ => {}
//         }
//     }
// }

// impl GenerateAsm for Value{
//     fn generate (&self,asm: &mut String) {
//         match self.kind(){
//             ValueKind::Integer(i) => {
//                 asm.push_str(&format!("    li a0, {}\n", i.value()));
//             }
//             _ => {}
//         }
//     }
// }

fn op_to_str(op: BinaryOp) -> &'static str{
    match op{
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "div",
        BinaryOp::Mod => "rem",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::Xor => "xor",
        BinaryOp::Gt => "sgt",
        BinaryOp::Lt => "slt",

        _ => "unknown"
        
    }
}

impl GenerateAsmValue for Value{
    fn generate (&self,asm: &mut String,dfg: &DataFlowGraph) {
        let value_data = dfg.value(*self);
        match value_data.kind(){
            ValueKind::Integer(i) => {
                unreachable!("Integer value should not be generated directly in lv3.");
                //asm.push_str(&format!("    li a0, {}\n", i.value()));
            }
            ValueKind::Return(ret) => {
                if let Some(ret_value) = ret.value(){
                    // dbg!("ret value entered\n",asm.as_str());

                    // ret_value.generate(asm,dfg);
                    let value_reg = unsafe{reg_cache.get(&ret_value)};
                    if let Some(value_reg) = value_reg{
                        asm.push_str(&format!("    mv a0, {}\n",value_reg));
                    }else{
                        let reg = ret_value.load_value(asm,dfg);
                        if let Some(reg) = reg{
                            asm.push_str(&format!("    mv a0, {}\n",reg));
                        }
                    }
                }else{
                    asm.push_str(&format!("    li a0, 0\n"));
                }
                asm.push_str(&format!("    ret\n"));
            }
            ValueKind::Binary(bin) => {
                let rhs = bin.rhs();
                let rhs_value = rhs.load_value(asm, dfg);
                let rhs = rhs_value.unwrap();
                let lhs = bin.lhs();
                let lhs_value = lhs.load_value(asm, dfg);
                let lhs = lhs_value.unwrap();
                
                let op = bin.op();

                // 比较复杂的情况，无法直接生成
                if op_to_str(op) == "unknown" {
                    match op {
                        BinaryOp::Eq => {
                            asm.push_str(&format!("    xor {},{},{}\n",unsafe{temp_reg[CUR_REG as usize]},lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",unsafe{temp_reg[CUR_REG as usize]},unsafe{temp_reg[CUR_REG as usize]}));
                            unsafe{
                                reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                                CUR_REG += 1;
                            }
                        }
                        BinaryOp::NotEq => {
                            asm.push_str(&format!("    xor {},{},{}\n",unsafe{temp_reg[CUR_REG as usize]},lhs,rhs));
                            asm.push_str(&format!("    snez {},{}\n",unsafe{temp_reg[CUR_REG as usize]},unsafe{temp_reg[CUR_REG as usize]}));
                            unsafe{
                                reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                                CUR_REG += 1;
                            }
                        }
                        BinaryOp::Le => {
                            asm.push_str(&format!("    sgt {},{},{}\n",unsafe{temp_reg[CUR_REG as usize]},lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",unsafe{temp_reg[CUR_REG as usize]},unsafe{temp_reg[CUR_REG as usize]}));
                            unsafe{
                                reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                                CUR_REG += 1;
                            }
                        }
                        BinaryOp::Ge => {
                            asm.push_str(&format!("    slt {},{},{}\n",unsafe{temp_reg[CUR_REG as usize]},lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",unsafe{temp_reg[CUR_REG as usize]},unsafe{temp_reg[CUR_REG as usize]}));
                            unsafe{
                                reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                                CUR_REG += 1;
                            }
                        }
                        _ => {}
                    }
                }else{
                    asm.push_str(&format!("    {} {},{},{}\n",op_to_str(op),unsafe{temp_reg[CUR_REG as usize]},lhs,rhs));
                    unsafe{
                        reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                        CUR_REG += 1;
                    }
                }
            

            }
            _ => {}
        }
    }
    fn load_imm(&self,asm: &mut String,dfg: &DataFlowGraph) -> Option<&str>{
        let value_data = dfg.value(*self);
        match value_data.kind(){
            ValueKind::Integer(i) => {
                // TODO: 0的处理
                if i.value() == 0{
                    return Some("x0");
                }


                asm.push_str(&format!("    li {}, {}\n", unsafe{temp_reg[CUR_REG as usize]}, i.value()));

                unsafe{
                    reg_cache.insert(*self, temp_reg[CUR_REG as usize].to_string());
                    CUR_REG += 1;
                }

                Some(unsafe{reg_cache.get(self).unwrap()})

            }
            _ => None
        }
    }
    fn load_value(&self,asm: &mut String,dfg: &DataFlowGraph) -> Option<&str> {
        let value_data = dfg.value(*self);
        if is_const(value_data){
            return self.load_imm(asm,dfg);
        }else{
            let reg = unsafe{
                reg_cache.get(self)
            };
            if let Some(reg) = reg{
                return Some(&reg[0..]);
            }
        }
        None
    }
    
}