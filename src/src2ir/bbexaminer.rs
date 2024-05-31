use koopa::ir::{BasicBlock, Value, ValueKind};


use crate::ast::BType;

use super::core::{Core, InstType};

#[allow(unused)]

/// This module is responsible for examining the basic blocks to make sure that they are all ending with a terminator instruction.

pub struct BBExaminer {}
impl BBExaminer {
    pub fn new() -> Self {
        BBExaminer {}
    }
    pub fn examine_ret(&self, core:&mut Core) {
        let ret = match core.ty() {
            BType::Int => {
                let zero = core.new_int(0);
                core.new_value(InstType::Ret(Some(zero)))
            }
            BType::Void => {
                core.new_value(InstType::Ret(None))
            }
        };

        let mut vector = Vec::new();
        let layout = core.layout_mut();
        let blocks = layout.bbs_mut().iter();
        for block in blocks {
            vector.push(*block.0);
        }
        for block in vector {
            if !self.is_terminated(core, block) {
                core.temporarily_use_block(block, |core| {
                    core.push_inst(ret);
                });
            }
        }
    }
    pub fn examine_bb_name(&self, core: &mut Core) {
        let name = core.func_data().name()[1..].to_string();

        let layout = core.layout_mut();
        let blocks = layout.bbs_mut().iter();
        let mut vector = Vec::new();
        let mut count = 0;
        for block in blocks {
            vector.push(*block.0);
        }
        let dfg = core.dfg_mut();
        for block in vector {
            let bb = dfg.bb_mut(block);
            if let None = bb.name() {
                bb.set_name(Some(format!("%{}_{}", name, count)));
                count += 1;
            }
        }
    }
    pub fn is_terminated(&self, core:&mut Core, bb: BasicBlock) -> bool {
        let layout = core.layout_mut();
        let bb = layout.bb_mut(bb);
        let mut terminated = false;
        let mut vector = Vec::new();
        for inst in bb.insts().iter() {
            vector.push(*inst.0);
        }
        let dfg = core.dfg_mut();
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
    
    // pub fn is_terminated_static(core:&mut Core, bb: BasicBlock) -> bool {
    //     let layout = core.layout_mut();
    //     let bb = layout.bb_mut(bb);
    //     let mut terminated = false;
    //     let mut vector = Vec::new();
    //     for inst in bb.insts().iter() {
    //         vector.push(*inst.0);
    //     }
    //     let dfg = core.dfg_mut();
    //     for inst in vector {
    //         match dfg.value(inst).kind() {
    //             ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
    //                 terminated = true;
    //             }
    //             _ => {}
    //         }
    //     }
    //     terminated
    // }
    pub fn clean_extra_inst(core:&mut Core, bb: BasicBlock) {
        let dfg = core.dfg();

        let layout = core.layout(); // 获取 func_data 的可变引用
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

        let insts = core.layout_mut().bb_mut(bb).insts_mut();

        while *insts.back_key().unwrap() != it.unwrap() {
            insts.pop_back();
        }
    }
    pub fn clean_all_extra_inst(core:&mut Core) {
        let layout = core.layout();
        let blocks = layout.bbs().iter();
        let mut vector = Vec::new();
        for block in blocks{
            vector.push(*block.0);
        }
        for block in vector{
            BBExaminer::clean_extra_inst(core, block);
        }
    }
}
