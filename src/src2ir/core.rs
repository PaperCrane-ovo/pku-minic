use koopa::ir::{builder::{BasicBlockBuilder,  LocalInstBuilder, ValueBuilder}, dfg::DataFlowGraph, layout::Layout, BasicBlock, BinaryOp, Function, FunctionData, Type, Value};



use crate::ast::BType;

use super::context::Context;



pub struct Core<'a,'b>{
    pub func_data:&'a mut FunctionData,
    pub block:BasicBlock,
    pub context:&'b mut Context,
    pub return_type:BType,
}

impl <'a,'b> Core<'a,'b>{
    pub fn new(func_data:&'a mut FunctionData,context:&'b mut Context,return_type:BType)->Self{

        let block = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([block]);
        Core{
            func_data,
            block,
            context,
            return_type,
        }
    }

    pub fn func_data(&self)->&FunctionData{
        &self.func_data
    }
    pub fn func_data_mut(&mut self)->&mut FunctionData{
        &mut self.func_data
    }
    pub fn layout(&self) -> &Layout {
        &self.func_data.layout()
    }
    pub fn layout_mut(&mut self) -> &mut Layout {
        let layout = self.func_data.layout_mut();
        layout
    }
    pub fn dfg(&self) -> &DataFlowGraph {
        &self.func_data.dfg()
    }
    pub fn dfg_mut(&mut self) -> &mut DataFlowGraph {
        let dfg = self.func_data.dfg_mut();
        dfg
    }
    pub fn context(&self) -> &Context {
        &self.context
    }
    pub fn context_mut(&mut self) -> &mut Context {
        &mut self.context
    }
    pub fn block(&self) -> &BasicBlock {
        &self.block
    }
    pub fn block_mut(&mut self) -> &mut BasicBlock {
        &mut self.block
    }
    pub fn push_inst(&mut self,inst:Value){
        self.func_data.layout_mut().bb_mut(self.block).insts_mut().push_key_back(inst).unwrap();
    }
    pub fn push_insts(&mut self,insts:Vec<Value>){
        self.func_data.layout_mut().bb_mut(self.block).insts_mut().extend(insts);
    }
    pub fn new_value(&mut self,inst:InstType) -> Value {
        let new_value = self.dfg_mut().new_value();
        match inst {
            InstType::Ret(value) => {
                new_value.ret(value)
            }
            InstType::Alloc(_type) => {
                new_value.alloc(_type)
            }
            InstType::Store(value, dest) => {
                new_value.store(value, dest)
            }
            InstType::Load(src) => {
                new_value.load(src)
            }
            InstType::Branch(cond, true_block, false_block) => {
                new_value.branch(cond, true_block, false_block)
            }
            InstType::Call(func, args) => {
                new_value.call(func, args)
            }
            InstType::Binary(op, lhs, rhs) => {
                new_value.binary(op, lhs, rhs)
            }
            InstType::Jump(dest) => {
                new_value.jump(dest)
            }
            InstType::Integer(value) => {
                new_value.integer(value)
            }
        }
    }

    pub fn new_block(&mut self,name:Option<String>) -> BasicBlock {
        let new_block = self.dfg_mut().new_bb().basic_block(name);
        new_block
    }
    pub fn push_block(&mut self,block:BasicBlock){
        self.layout_mut().bbs_mut().extend([block]);
    }
    pub fn push_blocks(&mut self,blocks:Vec<BasicBlock>){
        self.layout_mut().bbs_mut().extend(blocks);
    }
    pub fn switch_block(&mut self,block:BasicBlock){
        self.block = block;
    }
    /// don't use it currently
    #[cold]
    pub fn temporarily_use_block(&mut self,block:BasicBlock,func:impl FnOnce(&mut Self)){
        let old_block = self.block;
        self.block = block;
        func(self);
        self.block = old_block;
    }

    pub fn new_int(&mut self,value:i32)->Value{
        self.new_value(InstType::Integer(value))
    }

    pub fn ty(&self) -> &BType {
        &self.return_type
    }

    

}

pub enum InstType{
    Ret(Option<Value>),
    Alloc(Type),
    Store(Value,Value),
    Load(Value),
    Branch(Value,BasicBlock,BasicBlock),
    Call(Function,Vec<Value>),
    Binary(BinaryOp,Value,Value),
    Jump(BasicBlock),
    Integer(i32),
}

