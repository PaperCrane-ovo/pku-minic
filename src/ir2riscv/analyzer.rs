use std::collections::HashMap;

use koopa::ir::{Function, Program, Value, ValueKind};

use super::global::{GlobalValue, GlobalValues};

pub struct Analyzer{
    pub global_values:GlobalValues,
    pub functions: HashMap<Function,String>,

}
impl Analyzer{
    pub fn new()->Self{
        Analyzer{
            global_values:GlobalValues::new(),
            functions: HashMap::new(),
        }
    }
    pub fn analyze_global(&mut self,program:&Program){
        let globals = program.borrow_values();
        for (value,value_data) in globals.iter(){
            if let ValueKind::GlobalAlloc(alloc) = value_data.kind(){
                let name = value_data.name().as_ref().unwrap()[1..].to_string();
                let init = alloc.init();
                let size = value_data.ty().size();
                let data = &globals[&init];
                let init = match data.kind(){
                    ValueKind::ZeroInit(_) => None,
                    ValueKind::Integer(i) => Some(i.value()),
                    _ => unreachable!("Global init value is not integer or zero"),
                };
                self.global_values.add_global(*value, size, init, Some(name));
            }
        }

    }
    pub fn analyze_function(&mut self,program:&Program){
        for (&func,func_data) in program.funcs(){
            let name = func_data.name()[1..].to_string();
            self.functions.insert(func,name);
        }
    }
    pub fn get_global(&self,value:Value) -> Option<&GlobalValue>{
        self.global_values.values.get(&value)
    }
    pub fn has_global(&self,value:Value) -> bool {
        self.global_values.values.contains_key(&value)
    }
    pub fn global_iter(&self)->impl Iterator<Item = &GlobalValue>{
        self.global_values.values.values()
    }
}

