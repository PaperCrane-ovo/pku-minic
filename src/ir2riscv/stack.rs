//! stack.rs
//! calculate stack size
//! 

use std::collections::HashMap;

use koopa::ir::{FunctionData, TypeKind, Value, ValueKind};

use super::register::RegId;

#[derive(Debug,Clone,Copy,PartialEq,Eq,Hash)]
pub enum PtrType{
    Ptr,
    Array,
}


pub struct StackFrame{
    pub frame: HashMap<Value,i32>,
    pub pos: i32,
    pub size: usize,
    pub call:bool,
    pub param_pos: i32,
    pub reg_pos: HashMap<RegId,i32>,
    pub local_type: HashMap<Value,PtrType>,
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
                
                if let ValueKind::Call(func_call) = inst.kind(){
                    // Calculate R
                    call = true;
                    // Calculate A
                    let arg_count = func_call.args().len();
                    if arg_count > param_count{
                        param_count = arg_count;
                    }
                }else if let ValueKind::Alloc(_) = inst.kind(){
                    let ty = match inst.ty().kind(){
                        TypeKind::Pointer(ty) => ty,
                        _ => unreachable!(),
                    };
                    size += ty.size();
                    
                    // dbg!(ty.size());
                }else if !inst.ty().is_unit(){
                    size += inst.ty().size();
                }
                
            }
        }
        
        self.size = size+ 960 + {if call {4} else {0}} + {if param_count > 8 {4*(param_count-8)} else {0}};
        self.size = if self.size % 16 == 0 {self.size} else {self.size/16*16+16};
        self.call = call;
        self.param_pos = 0;
        self.pos = self.size as i32 -4;
        dbg!(func.name(),self.size);
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
            local_type: HashMap::new(),
        }
    }
    pub fn insert(&mut self, value: Value){
        self.frame.insert(value, self.pos);
        self.pos -= 4;
    }
    pub fn insert_array(&mut self,value: Value, size: usize){
        self.pos = self.pos - size as i32 + 4;
        self.frame.insert(value, self.pos);
        self.pos -= 4;
    }
    pub fn insert_array_member(&mut self,ptr: Value,member: Value,offset: i32) {
        let pos = *self.frame.get(&ptr).unwrap();
        self.frame.insert(member, pos - offset);
    }
    pub fn get(&self, value: Value) -> i32{
        *self.frame.get(&value).unwrap()
    }
    pub fn get_type(&self, value: Value) -> Option<&PtrType>{
        self.local_type.get(&value)
    }
    pub fn contains(&self, value: Value) -> bool{
        self.frame.contains_key(&value)
    }
    pub fn insert_param(&mut self,value:Value){
        self.frame.insert(value, self.param_pos);
        self.param_pos += 4;
    }
    pub fn clean_param(&mut self){
        self.frame.retain(|_,&mut pos| pos >= self.param_pos);
        self.param_pos = 0;
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