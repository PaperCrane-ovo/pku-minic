use koopa::ir::{builder::LocalInstBuilder, dfg::DataFlowGraph, layout, FunctionData};

/// This module is responsible for examining the basic blocks to make sure that they are all ending with a terminator instruction.
pub struct BBExaminer{}
impl BBExaminer{
    pub fn new() -> Self{
        BBExaminer{}
    }
    pub fn examine_ret(&self,func_data:&mut FunctionData){
        let ret = func_data.dfg_mut().new_value().ret(None);
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
    pub fn examine_bb_name(&self,func_data:&mut FunctionData){
        let name = func_data.name()[1..].to_string();

        let layout = func_data.layout_mut();
        let blocks = layout.bbs_mut().iter();
        let mut vector = Vec::new();
        let mut count = 0;
        for block in blocks {
            vector.push(*block.0);
        }
        let dfg = func_data.dfg_mut();
        for block in vector{
            let bb = dfg.bb_mut(block);
            if let None = bb.name(){
                bb.set_name(Some(format!("%{}_{}",name,count)));
                count += 1;
            }
            
        }
    }
}