use koopa::ir::{
    dfg::DataFlowGraph, BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind,
};

use crate::utils::is_const;

use super::{
    imm::{I12pos, I12},
    register::RegId,
    riscv::{RiscvInst, TempRiscv},
    stack::StackFrame,
};

use miette::Result;

pub type AsmProgram = Vec<TempRiscv>;

// 比较丑陋的实现：给koopa扩展特征，函数签名不同需要不同的函数。
pub trait GenerateAsm {
    fn generate(&self, _asm: &mut AsmProgram) {}
}
pub trait GenerateAsmBlock {
    fn generate(&self, _asm: &mut AsmProgram, _dfg: &DataFlowGraph, _stack_frame: &mut StackFrame) {
    }
}

pub trait GenerateAsmValue {
    fn load_imm(
        &self,
        _asm: &mut AsmProgram,
        _dfg: &DataFlowGraph,
        _dest_reg: RegId,
    ) -> Result<RegId> {
        Ok(RegId::T0)
    }
    fn generate(&self, _asm: &mut AsmProgram, _dfg: &DataFlowGraph, _stack_frame: &mut StackFrame) {
    }
    fn load_value(
        &self,
        _asm: &mut AsmProgram,
        _dfg: &DataFlowGraph,
        _stack_frame: &mut StackFrame,
        _dest_reg: RegId,
    ) -> Result<RegId> {
        Ok(RegId::T0)
    }
    fn store_value(
        &self,
        _asm: &mut AsmProgram,
        _dfg: &DataFlowGraph,
        _stack_frame: &mut StackFrame,
        _src_reg: RegId,
    ) {
    }
}

impl GenerateAsm for Program {
    fn generate(&self, asm: &mut AsmProgram) {
        asm.push(TempRiscv::Segment(String::from(".text")));
        for &func in self.func_layout() {
            let func_data = self.func(func);
            func_data.generate(asm);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, asm: &mut AsmProgram) {
        let name = self.name()[1..].to_string();
        // asm.push_str("    .text\n");
        // asm.push_str(&format!("    .globl {}\n", name));
        asm.push(TempRiscv::Segment(format!(".globl {}", name)));
        asm.push(TempRiscv::Label(format!("{}", name)));

        // asm.push_str(&format!("{}:\n", name));

        // 计算栈帧大小

        let mut stack_frame = StackFrame::new();
        let stack_size = stack_frame.get_stack_size(self);

        // TODO : when stack_size is larger than i12
        let stack_size = I12::build(-(stack_size as i32), asm);
        match stack_size {
            I12pos::Imm12(stack_size) => {
                // asm.push_str(&format!("    addi sp, sp, {}\n", stack_size));
                asm.push(TempRiscv::Inst(RiscvInst::Addi(
                    RegId::SP,
                    RegId::SP,
                    stack_size,
                )));
            }
            I12pos::RegId(stack_size) => {
                // asm.push_str(&format!("    add sp, sp, {}\n", stack_size));
                asm.push(TempRiscv::Inst(RiscvInst::Add(
                    RegId::SP,
                    RegId::SP,
                    stack_size,
                )));
            }
        }

        let dfg = self.dfg();
        for (&_bb, node) in self.layout().bbs() {
            _bb.generate(asm, dfg, &mut stack_frame);
            for &inst in node.insts().keys() {
                inst.generate(asm, dfg, &mut stack_frame);
            }
        }
    }
}
impl GenerateAsmBlock for BasicBlock {
    fn generate(&self, asm: &mut AsmProgram, dfg: &DataFlowGraph, _stack_frame: &mut StackFrame) {
        let name = dfg.bb(*self).name().as_deref().unwrap().to_string();
        asm.push(TempRiscv::Label(format!("L{}", &name[1..])));
    }
}



impl GenerateAsmValue for Value {
    fn generate(&self, asm: &mut AsmProgram, dfg: &DataFlowGraph, stack_frame: &mut StackFrame) {
        let value_data = dfg.value(*self);
        match value_data.kind() {
            ValueKind::Integer(_) => {
                unreachable!("Integer value should not be generated directly in lv3.");
            }
            ValueKind::Return(ret) => {
                if let Some(ret_value) = ret.value() {
                    // dbg!("ret value entered\n",asm.as_str());

                    // ret_value.generate(asm,dfg);
                    let value_reg = ret_value.load_value(asm, dfg, stack_frame, RegId::A0);
                    // TODO: 指令优化
                    if let Ok(value_reg) = value_reg {
                        asm.push(TempRiscv::Inst(RiscvInst::Mv(RegId::A0, value_reg)));
                    }
                } else {
                    asm.push(TempRiscv::Inst(RiscvInst::Li(RegId::A0, 0)));
                }
                // asm.push_str(&format!("    addi sp, sp, {}\n", stack_frame.size));
                let stack_size = I12::build(stack_frame.size as i32, asm);
                match stack_size {
                    I12pos::Imm12(stack_size) => {
                        // asm.push_str(&format!("    addi sp, sp, {}\n", stack_size));
                        asm.push(TempRiscv::Inst(RiscvInst::Addi(
                            RegId::SP,
                            RegId::SP,
                            stack_size,
                        )));
                    }
                    I12pos::RegId(stack_size) => {
                        // asm.push_str(&format!("    add sp, sp, {}\n", stack_size));
                        asm.push(TempRiscv::Inst(RiscvInst::Add(
                            RegId::SP,
                            RegId::SP,
                            stack_size,
                        )));
                    }
                }

                // asm.push_str(&format!("    ret\n"));
                asm.push(TempRiscv::Inst(RiscvInst::Ret));
            }
            ValueKind::Binary(bin) => {
                let rhs = bin.rhs();
                let rhs_value = rhs.load_value(asm, dfg, stack_frame, RegId::T1);
                let rhs = rhs_value.unwrap();
                let lhs = bin.lhs();
                let lhs_value = lhs.load_value(asm, dfg, stack_frame, RegId::T0);
                let lhs = lhs_value.unwrap();

                let op = bin.op();

                let reg = RegId::T0;
                match op {
                    BinaryOp::Eq => {
                        // asm.push_str(&format!("    xor {},{},{}\n",reg,lhs,rhs));
                        // asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                        asm.push(TempRiscv::Inst(RiscvInst::Xor(reg, lhs, rhs)));
                        asm.push(TempRiscv::Inst(RiscvInst::Seqz(reg, reg)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::NotEq => {
                        // asm.push_str(&format!("    xor {},{},{}\n",reg,lhs,rhs));
                        // asm.push_str(&format!("    snez {},{}\n",reg,reg));
                        asm.push(TempRiscv::Inst(RiscvInst::Xor(reg, lhs, rhs)));
                        asm.push(TempRiscv::Inst(RiscvInst::Snez(reg, reg)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Le => {
                        // asm.push_str(&format!("    sgt {},{},{}\n",reg,lhs,rhs));
                        // asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                        // unsafe{
                        //     REG_CACHE.insert(*self, reg);
                        // }
                        // stack_frame.insert(*self);
                        asm.push(TempRiscv::Inst(RiscvInst::Sgt(reg, lhs, rhs)));
                        asm.push(TempRiscv::Inst(RiscvInst::Seqz(reg, reg)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Ge => {
                        // asm.push_str(&format!("    slt {},{},{}\n",reg,lhs,rhs));
                        // asm.push_str(&format!("    seqz {},{}\n",reg,reg));
                        // unsafe{
                        //     REG_CACHE.insert(*self, reg);
                        // }
                        // stack_frame.insert(*self);
                        asm.push(TempRiscv::Inst(RiscvInst::Slt(reg, lhs, rhs)));
                        asm.push(TempRiscv::Inst(RiscvInst::Seqz(reg, reg)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Add => {
                        // asm.push_str(&format!("    add {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Add(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Sub => {
                        // asm.push_str(&format!("    sub {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Sub(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Mul => {
                        // asm.push_str(&format!("    mul {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Mul(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Div => {
                        // asm.push_str(&format!("    div {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Div(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Mod => {
                        // asm.push_str(&format!("    rem {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Rem(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::And => {
                        // asm.push_str(&format!("    and {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::And(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Or => {
                        // asm.push_str(&format!("    or {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Or(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Xor => {
                        // asm.push_str(&format!("    xor {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Xor(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Gt => {
                        // asm.push_str(&format!("    sgt {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Sgt(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    BinaryOp::Lt => {
                        // asm.push_str(&format!("    slt {},{},{}\n",reg,lhs,rhs));
                        asm.push(TempRiscv::Inst(RiscvInst::Slt(reg, lhs, rhs)));
                        self.store_value(asm, dfg, stack_frame, reg);
                    }
                    _ => {}
                }
            }
            ValueKind::Alloc(_alloc) => {}
            ValueKind::Store(store) => {
                let dest = store.dest();
                let val = store.value();
                let val_reg = val.load_value(asm, dfg, stack_frame, RegId::T1).unwrap();
                dest.store_value(asm, dfg, stack_frame, val_reg);
            }
            ValueKind::Load(load) => {
                let src = load.src();
                let reg = src.load_value(asm, dfg, stack_frame, RegId::T0).unwrap();
                self.store_value(asm, dfg, stack_frame, reg);
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let cond_reg = cond.load_value(asm, dfg, stack_frame, RegId::T0).unwrap();
                let true_block = branch.true_bb();
                let false_block = branch.false_bb();
                let true_block_label_name =
                    dfg.bb(true_block).name().as_deref().unwrap().to_string();
                let false_block_label_name =
                    dfg.bb(false_block).name().as_deref().unwrap().to_string();
                let true_block_label = format!("L{}", &true_block_label_name[1..]);
                let false_block_label = format!("L{}", &false_block_label_name[1..]);
                asm.push(TempRiscv::Inst(RiscvInst::Bnez(cond_reg, format!("jto{}", true_block_label))));
                asm.push(TempRiscv::Inst(RiscvInst::J(false_block_label)));
                asm.push(TempRiscv::Label(format!("jto{}", true_block_label)));
                asm.push(TempRiscv::Inst(RiscvInst::J(true_block_label)));
            }
            ValueKind::Jump(jump) => {
                let target = jump.target();
                let target_label_name = dfg.bb(target).name().as_deref().unwrap().to_string();
                let target_label = format!("L{}", &target_label_name[1..]);
                asm.push(TempRiscv::Inst(RiscvInst::J(target_label)));
            }

            _ => {}
        }
    }
    fn load_imm(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        dest_reg: RegId,
    ) -> Result<RegId> {
        let value_data = dfg.value(*self);
        match value_data.kind() {
            ValueKind::Integer(i) => {
                // TODO: 0的处理
                if i.value() == 0 {
                    Ok(RegId::X0)
                } else {
                    // asm.push_str(&format!("    li {}, {}\n",dest_reg,i.value()));
                    asm.push(TempRiscv::Inst(RiscvInst::Li(dest_reg, i.value())));
                    Ok(dest_reg)
                }
            }
            ValueKind::ZeroInit(_) => Ok(RegId::X0),
            _ => unreachable!("load_imm called on non-integer value"),
        }
    }
    fn load_value(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        stack_frame: &mut StackFrame,
        dest_reg: RegId,
    ) -> Result<RegId> {
        let value_data = dfg.value(*self);
        if is_const(value_data) {
            self.load_imm(asm, dfg, dest_reg)
        } else {
            // 从栈帧中加载
            let offset = stack_frame.get(*self);
            // asm.push_str(&format!("    lw {}, {}(sp)\n",dest_reg,offset));
            // asm.push(TempRiscv::Inst(RiscvInst::Lw(
            //     dest_reg,
            //     I12::build(offset as i32).unwrap(),
            //     RegId::SP,
            // )));
            let i12_offset = I12::build(offset as i32, asm);
            match i12_offset {
                I12pos::Imm12(i12_offset) => {
                    asm.push(TempRiscv::Inst(RiscvInst::Lw(dest_reg, i12_offset, RegId::SP)));
                }
                I12pos::RegId(i12_offset) => {
                    asm.push(TempRiscv::Inst(RiscvInst::Add(dest_reg, RegId::SP, i12_offset)));
                    asm.push(TempRiscv::Inst(RiscvInst::Lw(dest_reg, I12 { value: (0) }, dest_reg)));
                }
            }
            Ok(dest_reg)
        }
    }
    fn store_value(
        &self,
        asm: &mut AsmProgram,
        _dfg: &DataFlowGraph,
        stack_frame: &mut StackFrame,
        src_reg: RegId,
    ) {
        if !stack_frame.contains(*self) {
            stack_frame.insert(*self);
        }
        let offset = stack_frame.get(*self);
        // asm.push_str(&format!("    sw {}, {}(sp)\n",src_reg,offset));
        // asm.push(TempRiscv::Inst(RiscvInst::Sw(
        //     src_reg,
        //     I12::build(offset as i32).unwrap(),
        //     RegId::SP,
        // )));
        let i12_offset = I12::build(offset as i32, asm);
        match i12_offset {
            I12pos::Imm12(i12_offset) => {
                asm.push(TempRiscv::Inst(RiscvInst::Sw(src_reg, i12_offset, RegId::SP)));
            }
            I12pos::RegId(i12_offset) => {
                asm.push(TempRiscv::Inst(RiscvInst::Add(RegId::T4, RegId::SP, i12_offset)));
                asm.push(TempRiscv::Inst(RiscvInst::Sw(src_reg, I12 { value: (0) }, RegId::T4)));
            }
        }
    }
}
