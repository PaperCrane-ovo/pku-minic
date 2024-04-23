use koopa::ir::Value;

/// 符号表的实现

pub enum Symbol{
    Const(i32),
    Var(Value),
}
pub struct SymTable{
    
}