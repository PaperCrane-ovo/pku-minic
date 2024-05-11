use std::{collections::HashMap, hash::Hash};

use koopa::ir::Value;

/// 符号表的实现

pub enum Symbol{
    Const(i32),
    Var(Value),
}
pub struct SymTable{
    table: ChainMap<String,Symbol>,
}

struct ChainMap<K:Eq + Hash,V>{
    maps:Vec<HashMap<K,V>>,
}

impl <K:Eq + Hash,V> ChainMap<K,V>{
    fn new()->Self{
        Self{
            maps:vec![],
        }
    }
    fn insert(&mut self,key:K,value:V){
        self.maps.last_mut().unwrap().insert(key,value);
    }
    fn get(&self,key:&K)->Option<&V>{
        for map in self.maps.iter().rev(){
            if let Some(value) = map.get(key){
                return Some(value);
            }
        }
        None
    }
    fn push(&mut self){
        self.maps.push(HashMap::new());
    }
    fn pop(&mut self){
        self.maps.pop();
    }
    
}

impl SymTable{
    pub fn new() -> Self{
        let mut table = ChainMap::new();
        table.push();
        Self{
            table,
        }
    }
    pub fn insert(&mut self,name:String,symbol:Symbol){
        self.table.insert(name,symbol);
    }
    pub fn get(&self,name:&String)->Option<&Symbol>{
        self.table.get(name)
    }
    pub fn contains(&self,name:&String)->bool{
        self.table.get(name).is_some()
    }
    pub fn push(&mut self){
        self.table.push();
    }
    pub fn pop(&mut self){
        self.table.pop();
    }
}
