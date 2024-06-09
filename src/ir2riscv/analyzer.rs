use std::collections::HashMap;

use koopa::ir::{entities::ValueData, Function, Program, TypeKind, Value, ValueKind};

use super::global::{GlobalValue, GlobalValues};

pub struct Analyzer<'a>{
    pub global_values:GlobalValues,
    pub functions: HashMap<Function,String>,
    pub program:&'a Program,
}
impl<'a> Analyzer<'a>{
    pub fn new(program:&'a Program)->Self{
        Analyzer{
            global_values:GlobalValues::new(),
            functions: HashMap::new(),
            program,
        }
    }
    pub fn analyze_global(&mut self,program:&Program){
        let globals = program.borrow_values();
        for (value,value_data) in globals.iter(){
            if let ValueKind::GlobalAlloc(alloc) = value_data.kind(){
                let name = value_data.name().as_ref().unwrap()[1..].to_string();
                let init = alloc.init();
                
                let data = &globals[&init];
                let size = data.ty().size();
                // dbg!(data.ty());

                let base_size = match data.ty().kind(){
                    TypeKind::Array(ty,size) => ty.size(),
                    _ => size,
                };

                


                fn get_init(global:&HashMap<Value,ValueData>,data:&ValueData,init:&mut Vec<i32>){
                    match data.kind(){
                        ValueKind::Integer(i) => init.push(i.value()),
                        ValueKind::Aggregate(init_list) => {
                            for init_val in init_list.elems() {
                                get_init(global,&global[init_val],init);
                            }
                        }
                        _ => unreachable!("Global init value is not integer or zero"),
                    }
                }
                let init = if let ValueKind::ZeroInit(_) = data.kind(){
                    None
                }else{
                    let mut init = Vec::new();
                    get_init(&globals,data,&mut init);
                    Some(init)
                };

                self.global_values.add_global(*value, size, init, Some(name),base_size);
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

