use koopa::ir::{builder::{LocalInstBuilder, ValueBuilder}, BasicBlock, FunctionData, Value, ValueKind};

/// This module is responsible for examining the basic blocks to make sure that they are all ending with a terminator instruction.
pub struct BBExaminer {}
impl BBExaminer {
    pub fn new() -> Self {
        BBExaminer {}
    }
    pub fn examine_ret(&self, func_data: &mut FunctionData) {
        let zero = func_data.dfg_mut().new_value().integer(0);
        let ret = func_data.dfg_mut().new_value().ret(Some(zero));
        let mut vector = Vec::new();
        let layout = func_data.layout_mut();
        let blocks = layout.bbs_mut().iter();
        for block in blocks {
            if block.1.insts().is_empty() {
                vector.push(*block.0);
            }
        }
        for block in vector {
            layout.bb_mut(block).insts_mut().push_key_back(ret).unwrap()
        }
    }
    pub fn examine_bb_name(&self, func_data: &mut FunctionData) {
        let name = func_data.name()[1..].to_string();

        let layout = func_data.layout_mut();
        let blocks = layout.bbs_mut().iter();
        let mut vector = Vec::new();
        let mut count = 0;
        for block in blocks {
            vector.push(*block.0);
        }
        let dfg = func_data.dfg_mut();
        for block in vector {
            let bb = dfg.bb_mut(block);
            if let None = bb.name() {
                bb.set_name(Some(format!("%{}_{}", name, count)));
                count += 1;
            }
        }
    }
    pub fn is_terminated(&self, func_data: &mut FunctionData, bb: BasicBlock) -> bool {
        let layout = func_data.layout_mut();
        let bb = layout.bb_mut(bb);
        let mut terminated = false;
        let mut vector = Vec::new();
        for inst in bb.insts().iter() {
            vector.push(*inst.0);
        }
        let dfg = func_data.dfg_mut();
        for inst in vector {
            match dfg.value(inst).kind() {
                ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
                    terminated = true;
                    break;
                }
                _ => {}
            }
        }
        terminated
    }
    
    pub fn is_terminated_static(func_data: &mut FunctionData, bb: BasicBlock) -> bool {
        let layout = func_data.layout_mut();
        let bb = layout.bb_mut(bb);
        let mut terminated = false;
        let mut vector = Vec::new();
        for inst in bb.insts().iter() {
            vector.push(*inst.0);
        }
        let dfg = func_data.dfg_mut();
        for inst in vector {
            match dfg.value(inst).kind() {
                ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
                    terminated = true;
                }
                _ => {}
            }
        }
        terminated
    }
    pub fn clean_extra_inst(func_data: &mut FunctionData, bb: BasicBlock) {
        let dfg = func_data.dfg();

        let layout = func_data.layout(); // 获取 func_data 的可变引用
        let insts = layout.bbs().node(&bb).unwrap().insts();

        let mut it: Option<Value> = None;

        for i in insts.iter() {
            match dfg.value(*i.0).kind() {
                ValueKind::Return(_) | ValueKind::Jump(_) | ValueKind::Branch(_) => {
                    it = Some(*i.0);
                    break;
                }
                _ => {}
            }
        }

        let insts = func_data.layout_mut().bb_mut(bb).insts_mut();

        while *insts.back_key().unwrap() != it.unwrap() {
            insts.pop_back();
        }
    }
    pub fn clean_all_extra_inst(func_data: &mut FunctionData) {
        let layout = func_data.layout();
        let blocks = layout.bbs().iter();
        let mut vector = Vec::new();
        for block in blocks{
            vector.push(*block.0);
        }
        for block in vector{
            BBExaminer::clean_extra_inst(func_data, block);
        }
    }
}
