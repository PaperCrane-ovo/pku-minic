//! stack.rs
//! calculate stack size
//! 

use std::collections::HashMap;

use koopa::ir::{FunctionData, Value, ValueKind};

use super::register::RegId;



pub struct StackFrame{
    pub frame: HashMap<Value,i32>,
    pub pos: i32,
    pub size: usize,
    pub call:bool,
    pub param_pos: i32,
    pub reg_pos: HashMap<RegId,i32>,
}

impl StackFrame{
    pub fn get_stack_size(&mut self,func: &FunctionData) -> usize{
        let mut size = 0;
        let mut call = false;
        let mut param_count = 0;
        let dfg = func.dfg();
        for (&_bb,node) in func.layout().bbs(){
            for &inst in node.insts().keys(){
                let inst = dfg.value(inst);
                if !inst.ty().is_unit(){
                    size += 4;
                }
                if let ValueKind::Call(func_call) = inst.kind(){
                    // Calculate R
                    call = true;
                    // Calculate A
                    let arg_count = func_call.args().len();
                    if arg_count > param_count{
                        param_count = arg_count;
                    }
                }    
            }
        }
        dbg!(size,call,param_count);
        
        self.size = size + {if call {4} else {0}} + {if param_count > 8 {4*(param_count-8)} else {0}};
        self.size = if self.size % 16 == 0 {self.size} else {self.size/16*16+16};
        self.call = call;
        self.param_pos = 0;
        self.pos = self.size as i32 -4;
        self.size
    }
    pub fn new() -> Self{
        StackFrame{
            frame: HashMap::new(),
            pos: 0,
            size: 0,
            call: false,
            param_pos: 0,
            reg_pos: HashMap::new(),
        }
    }
    pub fn insert(&mut self, value: Value){
        self.frame.insert(value, self.pos);
        self.pos -= 4;
    }
    pub fn get(&self, value: Value) -> i32{
        *self.frame.get(&value).unwrap()
    }
    pub fn contains(&self, value: Value) -> bool{
        self.frame.contains_key(&value)
    }
    pub fn insert_param(&mut self,value:Value){
        self.frame.insert(value, self.param_pos);
        self.param_pos += 4;
    }
    pub fn save_reg(&mut self,reg:RegId) -> i32{
        let pos = self.pos;
        match self.reg_pos.get(&reg){
            Some(_) => {},
            None => {
                self.reg_pos.insert(reg, self.pos);
                self.pos -= 4;
            }
        }
        pos
    }
    pub fn get_reg_pos(&self,reg:RegId) -> i32{
        *self.reg_pos.get(&reg).unwrap()
    }

}