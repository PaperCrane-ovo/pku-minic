#![allow(dead_code)]
use std::collections::HashMap;

use koopa::ir::Value;

pub struct GlobalValue{
    pub size: usize,
    pub init: Option<i32>,
    pub name: Option<String>,
}
pub struct GlobalValues{
    pub values: HashMap<Value,GlobalValue>,
}

impl GlobalValues{
    pub fn new()->Self{
        GlobalValues{
            values: HashMap::new(),
        }
    }
    pub fn add_global(&mut self,value:Value,size:usize,init:Option<i32>,name:Option<String>){
        self.values.insert(value,GlobalValue{
            size,
            init,
            name,
        });
    }
    pub fn get_global(&self,value:&Value)->Option<&GlobalValue>{
        self.values.get(value)
    }
}