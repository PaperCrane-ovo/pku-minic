//! stack.rs
//! calculate stack size
//! 

use std::collections::HashMap;

use koopa::ir::{FunctionData, Value};



pub struct StackFrame{
    pub frame: HashMap<Value,i32>,
    pub cnt: i32,
    pub size: usize,
}

impl StackFrame{
    pub fn get_stack_size(&mut self,func: &FunctionData) -> usize{
        let mut size = 0;
        let dfg = func.dfg();
        for (&_bb,node) in func.layout().bbs(){
            for &inst in node.insts().keys(){
                let inst = dfg.value(inst);
                if !inst.ty().is_unit(){
                    size += 4;
                }
            }
        }
        self.size = size;
        size
    }
    pub fn new() -> Self{
        StackFrame{
            frame: HashMap::new(),
            cnt: 0,
            size: 0,
        }
    }
    pub fn insert(&mut self, value: Value){
        self.frame.insert(value, self.cnt*4);
        self.cnt += 1;
    }
    pub fn get(&self, value: Value) -> i32{
        *self.frame.get(&value).unwrap()
    }
    pub fn contains(&self, value: Value) -> bool{
        self.frame.contains_key(&value)
    }

}