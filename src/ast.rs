// Path: src/ast.rs

// 定义全局变量cur_varid，用于记录当前的变量id
static mut CUR_VARID: i32 = 0;



#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}



#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}


#[derive(Debug)]
pub enum FuncType {
    Int,
}


#[derive(Debug)]
pub struct Block{
    pub stmt: Stmt,
}


#[derive(Debug)]
pub enum Stmt {
    Return(Return),
}



#[derive(Debug)]
pub struct Return {
    pub exp: Exp,
}


#[derive(Debug)]
pub enum Exp{
    UnaryExp{
        op: MyUnaryOp,
        exp: Box<Exp>,
    },
    BinaryExp{
        op: MyBinaryOp,
        exp1: Box<Exp>,
        exp2: Box<Exp>,
    },
    Number(i32),
}

#[derive(Debug)]
pub enum MyUnaryOp{
    Pos,
    Neg,
    Not,
}

#[derive(Debug)]
pub enum MyBinaryOp{
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LAnd,
    LOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}
