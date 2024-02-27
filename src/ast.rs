// Path: src/ast.rs

trait Dump2Koopa {
    fn dump2koopa(&self, string:&mut String);
}

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn dump2koopa(&self, string: &mut String) {
        self.func_def.dump2koopa(string);
    }
}
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}
impl Dump2Koopa for FuncDef {
    fn dump2koopa(&self, string: &mut String) {
        let appended_str = format!("fun @{}(): ", self.ident);
        string.push_str(&appended_str);
        self.func_type.dump2koopa(string);
        self.block.dump2koopa(string);
    }
}
  
#[derive(Debug)]
pub enum FuncType {
    Int,
}
impl Dump2Koopa for FuncType {
    fn dump2koopa(&self, string: &mut String){
        match self {
            FuncType::Int => string.push_str("i32 "),
        }
    }
}

#[derive(Debug)]
pub struct Block{
    pub stmt: Stmt,
}
impl Dump2Koopa for Block {
    fn dump2koopa(&self, string: &mut String){
        string.push_str("{\n");
        string.push_str("%entry:\n");
        self.stmt.dump2koopa(string);
        string.push_str("}\n");
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}
impl Dump2Koopa for Stmt {
    fn dump2koopa(&self, string: &mut String){
        string.push_str(&(format!("ret {}\n", self.num)));
    }
}