
use koopa::ir::{dfg::DataFlowGraph, BinaryOp, FunctionData, Program, Value, ValueKind};

use crate::utils::is_const;

use super::{register::RegId, stack::StackFrame};

use miette::Result;

// 比较丑陋的实现：给koopa扩展特征，函数签名不同需要不同的函数。
pub trait GenerateAsm {
    fn generate (&self,_asm: &mut String){}

}
pub trait GenerateAsmValue{
    fn load_imm(&self, _asm: &mut String,_dfg: &DataFlowGraph,_dest_reg:RegId)-> Result<RegId>{Ok(RegId::T0)}
    fn generate(&self,_asm: &mut String,_dfg: &DataFlowGraph,_stack_frame:&mut StackFrame){}
    fn load_value(&self,_asm: &mut String,_dfg: &DataFlowGraph,_stack_frame:&mut StackFrame,_dest_reg:RegId) -> Result<RegId>{
        Ok(RegId::T0)
    }
    fn store_value(&self,_asm: &mut String,_dfg: &DataFlowGraph,_stack_frame:&mut StackFrame,_src_reg:RegId){}
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

        // 计算栈帧大小

        let mut stack_frame = StackFrame::new();
        let stack_size = stack_frame.get_stack_size(self);

        asm.push_str(&format!("    addi sp, sp, -{}\n", stack_size));




        
        let dfg = self.dfg();
        for (&_bb,node) in self.layout().bbs(){
            // dbg!(node.insts().len());
            // dbg!(dfg.values());
            for &inst in node.insts().keys(){
                inst.generate(asm,dfg,&mut stack_frame);
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
    fn generate (&self,asm: &mut String,dfg: &DataFlowGraph,stack_frame: &mut StackFrame) {
        let value_data = dfg.value(*self);
        match value_data.kind(){
            ValueKind::Integer(_) => {
                unreachable!("Integer value should not be generated directly in lv3.");
            }
            ValueKind::Return(ret) => {
                if let Some(ret_value) = ret.value(){
                    // dbg!("ret value entered\n",asm.as_str());

                    // ret_value.generate(asm,dfg);
                    let value_reg = ret_value.load_value(asm,dfg,stack_frame,RegId::A0);
                    // TODO: 指令优化
                    if let Ok(value_reg) = value_reg{
                        asm.push_str(&format!("    mv a0, {}\n",value_reg));
                    }
                }else{
                    asm.push_str(&format!("    li a0, 0\n"));
                }
                asm.push_str(&format!("    addi sp, sp, {}\n", stack_frame.size));

                asm.push_str(&format!("    ret\n"));
            }
            ValueKind::Binary(bin) => {
                let rhs = bin.rhs();
                let rhs_value = rhs.load_value(asm, dfg,stack_frame,RegId::T1);
                let rhs = rhs_value.unwrap();
                let lhs = bin.lhs();
                let lhs_value = lhs.load_value(asm, dfg,stack_frame,RegId::T0);
                let lhs = lhs_value.unwrap();
                
                let op = bin.op();

                // 比较复杂的情况，无法直接生成
                if op_to_str(op) == "unknown" {
                    let reg = RegId::T0;
                    match op {
                        BinaryOp::Eq => {
                            asm.push_str(&format!("    xor {},{},{}\n",reg,lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                            self.store_value(asm, dfg, stack_frame, reg);
                        }
                        BinaryOp::NotEq => {
                            asm.push_str(&format!("    xor {},{},{}\n",reg,lhs,rhs));
                            asm.push_str(&format!("    snez {},{}\n",reg,reg));
                            self.store_value(asm, dfg, stack_frame, reg);
                        }
                        BinaryOp::Le => {
                            asm.push_str(&format!("    sgt {},{},{}\n",reg,lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                            // unsafe{
                            //     REG_CACHE.insert(*self, reg);
                            // }
                            // stack_frame.insert(*self);
                            self.store_value(asm, dfg, stack_frame, reg);
                        }
                        BinaryOp::Ge => {
                            asm.push_str(&format!("    slt {},{},{}\n",reg,lhs,rhs));
                            asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                            // unsafe{
                            //     REG_CACHE.insert(*self, reg);
                            // }
                            // stack_frame.insert(*self);
                            self.store_value(asm, dfg, stack_frame, reg);
                        }
                        _ => {}
                    }
                }else{
                    let reg = RegId::T0;
                    asm.push_str(&format!("    {} {},{},{}\n",op_to_str(op),reg,lhs,rhs));
                    // unsafe{
                    //     REG_CACHE.insert(*self, reg);
                    // }
                    // stack_frame.insert(*self);
                    self.store_value(asm, dfg, stack_frame, reg);
                }
            

            }
            ValueKind::Alloc(_alloc) => {
            }
            ValueKind::Store(store) => {
                let dest = store.dest();
                let val = store.value();
                let val_reg = val.load_value(asm, dfg,stack_frame,RegId::T1).unwrap();
                dest.store_value(asm, dfg, stack_frame, val_reg);
            }
            ValueKind::Load(load) => {
                let src = load.src();
                let reg = src.load_value(asm, dfg, stack_frame, RegId::T0).unwrap();
                self.store_value(asm, dfg, stack_frame, reg);
            }
            _ => {}
        }
    }
    fn load_imm(&self,asm: &mut String,dfg: &DataFlowGraph,dest_reg:RegId) -> Result<RegId>{
        let value_data = dfg.value(*self);
        match value_data.kind(){
            ValueKind::Integer(i) => {
                // TODO: 0的处理
                if i.value() == 0{
                    Ok(RegId::X0)
                }else{
                    asm.push_str(&format!("    li {}, {}\n",dest_reg,i.value()));
                    Ok(dest_reg)
                }

            }
            ValueKind::ZeroInit(_) => {
                Ok(RegId::X0)
            }
            _ => unreachable!("load_imm called on non-integer value"),
        }
    }
    fn load_value(&self,asm: &mut String,dfg: &DataFlowGraph,stack_frame: &mut StackFrame,dest_reg:RegId) -> Result<RegId> {
        let value_data = dfg.value(*self);
        if is_const(value_data){
            self.load_imm(asm,dfg,dest_reg)
        }else{
            // 从栈帧中加载
            let offset = stack_frame.get(*self);
            asm.push_str(&format!("    lw {}, {}(sp)\n",dest_reg,offset));
            Ok(dest_reg)
        }
        
    }    
    fn store_value(&self,asm: &mut String,_dfg: &DataFlowGraph,stack_frame:&mut StackFrame,src_reg:RegId) {
        if stack_frame.contains(*self){
            let offset = stack_frame.get(*self);
            asm.push_str(&format!("    sw {}, {}(sp)\n",src_reg,offset));
        }else{
            stack_frame.insert(*self);
            let offset = stack_frame.get(*self);
            asm.push_str(&format!("    sw {}, {}(sp)\n",src_reg,offset));
        }
    }
}