
use koopa::ir::{
    dfg::DataFlowGraph, BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind,
};

use crate::utils::is_const;

use super::{
    analyzer::Analyzer, imm::{I12pos, I12}, register::RegId, riscv::{RiscvInst, TempRiscv}, stack::StackFrame
};

use miette::Result;

pub type AsmProgram = Vec<TempRiscv>;

// 重构一下
pub struct RiscvGen<T>(pub T);

impl RiscvGen<&Program> {
    pub fn generate(&mut self, asm: &mut AsmProgram) {

        let mut analyzer = Analyzer::new();
        analyzer.analyze_global(self.0);
        analyzer.analyze_function(self.0);

        // 构建全局变量
        asm.push(TempRiscv::Segment(String::from(".data")));

        for global in analyzer.global_iter(){
            let name = global.name.as_deref().unwrap();
            asm.push(TempRiscv::Segment(format!(".global {}",name)));
            asm.push(TempRiscv::Label(name.to_string()));
            if let Some(init) = global.init{
                asm.push(TempRiscv::Segment(format!(".word {}",init))); // 数组lv9
            }else{
                asm.push(TempRiscv::Segment(format!(".zero {}",4)));
            }

        }
        

        asm.push(TempRiscv::Segment(String::from(".text")));

        for &func in self.0.func_layout() {
            let func_data = self.0.func(func);

            RiscvGen(func_data).generate(asm, &mut analyzer);
        }
    }
}

impl RiscvGen<&FunctionData> {
    pub fn generate(&self, asm: &mut AsmProgram, analyzer:&mut Analyzer) {
        if self.0.layout().entry_bb().is_none() {
            return;
        }
        let name = self.0.name()[1..].to_string();
        asm.push(TempRiscv::Segment(format!(".globl {}", name)));
        asm.push(TempRiscv::Label(format!("{}", name)));

        let mut stack_frame = StackFrame::new();
        let stack_size = stack_frame.get_stack_size(&self.0);

        let stack_size = I12::build(-(stack_size as i32), asm);
        match stack_size {
            I12pos::Imm12(stack_size) => {
                asm.push(TempRiscv::Inst(RiscvInst::Addi(
                    RegId::SP,
                    RegId::SP,
                    stack_size,
                )));
            }
            I12pos::RegId(stack_size) => {
                asm.push(TempRiscv::Inst(RiscvInst::Add(
                    RegId::SP,
                    RegId::SP,
                    stack_size,
                )));
            }
        }
        // 保存 ra
        let pos = stack_frame.save_reg(RegId::RA);
        let pos = I12::build(pos as i32, asm);
        match pos {
            I12pos::Imm12(pos) => {
                asm.push(TempRiscv::Inst(RiscvInst::Sw(RegId::RA, pos, RegId::SP)));
            }
            I12pos::RegId(pos) => {
                asm.push(TempRiscv::Inst(RiscvInst::Add(RegId::T4, RegId::SP, pos)));
                asm.push(TempRiscv::Inst(RiscvInst::Sw(
                    RegId::RA,
                    I12 { value: 0 },
                    RegId::T4,
                )));
            }
        }
        // 保存参数
        // 这部分工作好像交给inst去做了
        let mut param = 0;
        let params = self.0.params();

        let dfg = self.0.dfg();
        for (_bb, node) in self.0.layout().bbs() {
            RiscvGen(_bb).generate(asm, dfg, &mut stack_frame);
            for inst in node.insts().keys() {
                RiscvGen(inst).generate(asm, dfg, &mut stack_frame, analyzer, &mut param, params);
            }
        }
    }
}

impl RiscvGen<&BasicBlock> {
    pub fn generate(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        _stack_frame: &mut StackFrame,
    ) {
        let name = dfg.bb(*self.0).name().as_deref().unwrap().to_string();
        asm.push(TempRiscv::Label(format!("L{}", &name[1..])));
    }
}

impl RiscvGen<&Value> {
    pub fn generate(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        stack_frame: &mut StackFrame,
        analyzer: &mut Analyzer,
        param: &mut usize,
        params: &[Value],
    ) {
        let value_data = dfg.value(*self.0);
        match value_data.kind() {
            ValueKind::Integer(_) => {
                unreachable!("Integer value should not be generated directly in lv3.");
            }
            ValueKind::Return(ret) => {
                if let Some(ret_value) = ret.value() {
                    let value_reg = RiscvGen(&ret_value)
                        .load_value(asm, dfg, stack_frame, RegId::A0, analyzer)
                        .unwrap();
                    if value_reg != RegId::A0 {
                        asm.push(TempRiscv::Inst(RiscvInst::Mv(RegId::A0, value_reg)));
                    }
                } else {
                    asm.push(TempRiscv::Inst(RiscvInst::Li(RegId::A0, 0)));
                }

                let pos = stack_frame.get_reg_pos(RegId::RA);
                let pos = I12::build(pos as i32, asm);
                match pos {
                    I12pos::Imm12(pos) => {
                        asm.push(TempRiscv::Inst(RiscvInst::Lw(RegId::RA, pos, RegId::SP)));
                    }
                    I12pos::RegId(pos) => {
                        asm.push(TempRiscv::Inst(RiscvInst::Add(RegId::T4, RegId::SP, pos)));
                        asm.push(TempRiscv::Inst(RiscvInst::Lw(
                            RegId::RA,
                            I12 { value: 0 },
                            RegId::T4,
                        )));
                    }
                }

                let stack_size = I12::build(stack_frame.size as i32, asm);
                match stack_size {
                    I12pos::Imm12(stack_size) => {
                        asm.push(TempRiscv::Inst(RiscvInst::Addi(
                            RegId::SP,
                            RegId::SP,
                            stack_size,
                        )));
                    }
                    I12pos::RegId(stack_size) => {
                        asm.push(TempRiscv::Inst(RiscvInst::Add(
                            RegId::SP,
                            RegId::SP,
                            stack_size,
                        )));
                    }
                }

                asm.push(TempRiscv::Inst(RiscvInst::Ret));
            }
            ValueKind::Binary(bin) => {
                let rhs = bin.rhs();
                let rhs_value = RiscvGen(&rhs)
                    .load_value(asm, dfg, stack_frame, RegId::T1, analyzer)
                    .unwrap();
                let rhs = rhs_value;
                let lhs = bin.lhs();
                let lhs_value = RiscvGen(&lhs)
                    .load_value(asm, dfg, stack_frame, RegId::T0, analyzer)
                    .unwrap();
                let lhs = lhs_value;

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
            // 在这里有可能会有参数插入
            ValueKind::Store(store) => {
                let dest = store.dest();
                let val = store.value();



                // global value 
                if analyzer.has_global(dest) {
                    let dest_name = analyzer.get_global(dest).unwrap().name.clone();
                    asm.push(TempRiscv::Inst(RiscvInst::La(RegId::A0,dest_name.unwrap())));
                    RiscvGen(&val).load_value(asm, dfg, stack_frame, RegId::A1,analyzer).unwrap();
                    asm.push(TempRiscv::Inst(RiscvInst::Sw(RegId::A1, I12 { value: 0 }, RegId::A0)));
                } else if params.contains(&val){
                    // param value
                    if *param < 8 {
                        // 需要从寄存器中读取
                        let src_reg: RegId = format!("a{}", *param).into();
                        RiscvGen(&dest).store_value(asm, dfg, stack_frame, src_reg);
                        dbg!("stored param");
                        *param += 1;
                    } else {
                        // 需要从栈中读取
                        let offset =
                            I12::build(stack_frame.size as i32 + (*param as i32 - 8) * 4, asm);
                        match offset {
                            I12pos::Imm12(offset) => {
                                asm.push(TempRiscv::Inst(RiscvInst::Lw(
                                    RegId::T1,
                                    offset,
                                    RegId::SP,
                                )));
                            }
                            I12pos::RegId(offset) => {
                                asm.push(TempRiscv::Inst(RiscvInst::Add(
                                    RegId::T1,
                                    RegId::SP,
                                    offset,
                                )));
                                asm.push(TempRiscv::Inst(RiscvInst::Lw(
                                    RegId::T1,
                                    I12 { value: 0 },
                                    RegId::T1,
                                )));
                            }
                        }
                        RiscvGen(&dest).store_value(asm, dfg, stack_frame, RegId::T1);
                        *param += 1;
                    }

                } 
                else {
                    let val_reg = RiscvGen(&val)
                        .load_value(asm, dfg, stack_frame, RegId::T1, analyzer)
                        .unwrap();
                    RiscvGen(&dest).store_value(asm, dfg, stack_frame, val_reg);
                }
            }
            ValueKind::Load(load) => {
                let src = load.src();

                
                if analyzer.has_global(src) {
                    let src_name = analyzer.get_global(src).unwrap().name.clone();
                    asm.push(TempRiscv::Inst(RiscvInst::La(RegId::A0,src_name.unwrap())));
                    asm.push(TempRiscv::Inst(RiscvInst::Lw(RegId::A0, I12 { value: 0 }, RegId::A0)));
                    self.store_value(asm, dfg, stack_frame, RegId::A0);
                }else{
                    let reg = RiscvGen(&src).load_value(asm, dfg, stack_frame, RegId::A0,analyzer).unwrap();
                    self.store_value(asm, dfg, stack_frame, reg);
                }
                
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let cond_reg = RiscvGen(&cond)
                    .load_value(asm, dfg, stack_frame, RegId::T0,analyzer)
                    .unwrap();
                let true_block = branch.true_bb();
                let false_block = branch.false_bb();
                let true_block_label_name =
                    dfg.bb(true_block).name().as_deref().unwrap().to_string();
                let false_block_label_name =
                    dfg.bb(false_block).name().as_deref().unwrap().to_string();
                let true_block_label = format!("L{}", &true_block_label_name[1..]);
                let false_block_label = format!("L{}", &false_block_label_name[1..]);
                asm.push(TempRiscv::Inst(RiscvInst::Bnez(
                    cond_reg,
                    format!("{}", true_block_label),
                )));
                asm.push(TempRiscv::Inst(RiscvInst::J(false_block_label)));
                // asm.push(TempRiscv::Label(format!("jto{}", true_block_label)));
                // asm.push(TempRiscv::Inst(RiscvInst::J(true_block_label)));
            }
            ValueKind::Jump(jump) => {
                let target = jump.target();
                let target_label_name = dfg.bb(target).name().as_deref().unwrap().to_string();
                let target_label = format!("L{}", &target_label_name[1..]);
                asm.push(TempRiscv::Inst(RiscvInst::J(target_label)));
            }
            ValueKind::Call(call) => {
                let func = call.callee();
                let args = call.args();
                for (i, arg) in args.iter().enumerate() {
                    if i < 8 {
                        RiscvGen(arg)
                            .load_value(asm, dfg, stack_frame, format!("a{}", i).into(),analyzer)
                            .expect("load arg to reg failed");
                    } else {
                        stack_frame.insert_param(*arg);
                        RiscvGen(arg)
                            .load_value(asm, dfg, stack_frame, RegId::T0,analyzer)
                            .expect("load arg to stack failed");
                        let pos = stack_frame.get(*arg);
                        let i12_pos = I12::build(pos as i32, asm);
                        match i12_pos {
                            I12pos::Imm12(i12_pos) => {
                                asm.push(TempRiscv::Inst(RiscvInst::Sw(
                                    RegId::T0,
                                    i12_pos,
                                    RegId::SP,
                                )));
                            }
                            I12pos::RegId(i12_pos) => {
                                asm.push(TempRiscv::Inst(RiscvInst::Add(
                                    RegId::T4,
                                    RegId::SP,
                                    i12_pos,
                                )));
                                asm.push(TempRiscv::Inst(RiscvInst::Sw(
                                    RegId::T0,
                                    I12 { value: 0 },
                                    RegId::T4,
                                )));
                            }
                        }
                    }
                }
                let func_name = analyzer.functions[&func].clone();
                asm.push(TempRiscv::Inst(RiscvInst::Call(func_name)));

                if !value_data.ty().is_unit() {
                    self.store_value(asm, dfg, stack_frame, RegId::A0);
                }
            }

            _ => {}
        }
    }
    pub fn load_imm(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        dest_reg: RegId,
    ) -> Result<RegId> {
        let value_data = dfg.value(*self.0);
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
    pub fn load_value(
        &self,
        asm: &mut AsmProgram,
        dfg: &DataFlowGraph,
        stack_frame: &mut StackFrame,
        dest_reg: RegId,
        _analyzer: &mut Analyzer,
    ) -> Result<RegId> {
        let value_data = dfg.value(*self.0);
        if is_const(value_data) {
            RiscvGen(self.0).load_imm(asm, dfg, dest_reg)
        } else {
            let offset = stack_frame.get(*self.0);
            let i12_offset = I12::build(offset as i32, asm);
            match i12_offset {
                I12pos::Imm12(i12_offset) => {
                    asm.push(TempRiscv::Inst(RiscvInst::Lw(
                        dest_reg,
                        i12_offset,
                        RegId::SP,
                    )));
                }
                I12pos::RegId(i12_offset) => {
                    asm.push(TempRiscv::Inst(RiscvInst::Add(
                        dest_reg,
                        RegId::SP,
                        i12_offset,
                    )));
                    asm.push(TempRiscv::Inst(RiscvInst::Lw(
                        dest_reg,
                        I12 { value: 0 },
                        dest_reg,
                    )));
                }
            }
            Ok(dest_reg)
        }
    }
    pub fn store_value(
        &self,
        asm: &mut AsmProgram,
        _dfg: &DataFlowGraph,
        stack_frame: &mut StackFrame,
        src_reg: RegId,
    ) {
        if !stack_frame.contains(*self.0) {
            stack_frame.insert(*self.0);
        }
        let offset = stack_frame.get(*self.0);
        let i12_offset = I12::build(offset as i32, asm);
        match i12_offset {
            I12pos::Imm12(i12_offset) => {
                asm.push(TempRiscv::Inst(RiscvInst::Sw(
                    src_reg,
                    i12_offset,
                    RegId::SP,
                )));
            }
            I12pos::RegId(i12_offset) => {
                asm.push(TempRiscv::Inst(RiscvInst::Add(
                    RegId::T4,
                    RegId::SP,
                    i12_offset,
                )));
                asm.push(TempRiscv::Inst(RiscvInst::Sw(
                    src_reg,
                    I12 { value: 0 },
                    RegId::T4,
                )));
            }
        }
    }
}
