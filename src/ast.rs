// Path: src/ast.rs

// 定义全局变量cur_varid，用于记录当前的变量id
static mut CUR_VARID: i32 = 0;


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
        string.push_str(&(format!("fun @{}(): ", self.ident)));
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

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
pub enum Stmt {
    Return(Return),
}

impl Dump2Koopa for Stmt {
    fn dump2koopa(&self, string: &mut String){
        match self {
            Stmt::Return(r) => r.dump2koopa(string),
        }
    }
}

#[derive(Debug)]
pub struct Return {
    pub exp: Exp,
}
impl Dump2Koopa for Return {
    fn dump2koopa(&self, string: &mut String){
        
        self.exp.dump2koopa(string);
        string.push_str("   ret\n");
        string.push_str("\n");
    }
}

#[derive(Debug)]
pub struct Exp{
    pub unary_exp: UnaryExp,
}
impl Dump2Koopa for Exp {
    fn dump2koopa(&self, string: &mut String){
        self.unary_exp.dump2koopa(string);
    }
}

#[derive(Debug)]
pub enum UnaryExp{
    Primary(PrimaryExp),
    Unary(UnaryOp, Box<UnaryExp>),
}

impl Dump2Koopa for UnaryExp {
    fn dump2koopa(&self, string: &mut String){
        match self {
            Self::Primary(p) => p.dump2koopa(string),
            Self::Unary(op, exp) => {
                match exp.as_ref(){ // TODO
                    // 匹配到一元表达式的数字，作为IR的第一个操作数
                    Self::Primary(PrimaryExp::Number(num)) => {
                        string.push_str(&(format!("%{}= ",unsafe{CUR_VARID})));
                        match op{
                            UnaryOp::Neg => {
                                string.push_str("sub 0 , ");
                            },
                            UnaryOp::Not => {
                                string.push_str("eq 0 , ");
                            }
                        
                        }
                        string.push_str(&(format!("{}\n", num)));
                        unsafe{
                            CUR_VARID += 1;
                        }

                    }
                    // 表达式后面接着表达式，直接递归
                    Self::Unary(op_,exp_) => {
                        exp.dump2koopa(string);
                    }

                    _ => {}
                }
            }

        }
    }
}

#[derive(Debug)]
pub enum UnaryOp{
    Neg,
    Not,
}

impl Dump2Koopa for UnaryOp {
    fn dump2koopa(&self, string: &mut String){
        match self {
            UnaryOp::Neg => string.push_str("sub 0 ,"),
            UnaryOp::Not => string.push_str("eq 0 ,"),
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExp{
    Exp(Box<Exp>),
    Number(i32),
}

impl Dump2Koopa for PrimaryExp {
    fn dump2koopa(&self, string: &mut String){
        match self {
            PrimaryExp::Exp(exp) => exp.dump2koopa(string),
            PrimaryExp::Number(n) => string.push_str(&format!("  %{}", n)),
        }
    }
}


