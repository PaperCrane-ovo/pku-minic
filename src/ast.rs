#![allow(dead_code)]
// Path: src/ast.rs

use std::ops::Range;




#[derive(Debug,Clone)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItem>,
}

impl Spanned for CompUnit{
    fn start_pos(&self)->usize{
        self.global_items.first().unwrap().start_pos()
    }
    fn end_pos(&self)->usize{
        self.global_items.last().unwrap().end_pos()
    }
}

#[derive(Debug,Clone)]
pub enum GlobalItem{
    Decl(Decl),
    FuncDef(FuncDef),
}

impl Spanned for GlobalItem{
    fn start_pos(&self)->usize{
        match self{
            GlobalItem::Decl(decl)=>decl.start_pos(),
            GlobalItem::FuncDef(func_def)=>func_def.start_pos(),
        }
    }
    fn end_pos(&self)->usize{
        match self{
            GlobalItem::Decl(decl)=>decl.end_pos(),
            GlobalItem::FuncDef(func_def)=>func_def.end_pos(),
        }
    }
}


#[derive(Debug,Clone)]
pub struct FuncDef {
    pub func_type: Span<BType>,
    pub ident: Span<String>,
    pub params: Vec<Span<FuncParam>>,
    pub block: Span<Block>,
}

impl Spanned for FuncDef{
    fn start_pos(&self)->usize{
        self.func_type.start_pos()
    }
    fn end_pos(&self)->usize{
        self.block.end_pos()
    }
}

/// deprecated because of the existence of BType
// #[derive(Debug,Clone)]
// pub enum FuncType {
//     Int,
//     Void,
// }

// impl NonSpanned for FuncType{}

#[derive(Debug,Clone)]
pub struct FuncParam {
    pub ty: Span<BType>,
    pub ident: Span<String>,
    pub indices: Option<Vec<ConstExp>>,
}

impl NonSpanned for FuncParam{}

#[derive(Debug,Clone)]
pub struct Block{
    pub items: Vec<BlockItem>,
}

impl NonSpanned for Block{}

#[derive(Debug,Clone)]
pub enum BlockItem{
    Decl{decl:Decl},
    Stmt{stmt:Span<Stmt>},
}

impl Spanned for BlockItem{
    fn start_pos(&self)->usize{
        match self{
            BlockItem::Decl{decl}=>decl.start_pos(),
            BlockItem::Stmt{stmt}=>stmt.start_pos(),
        }
    }
    fn end_pos(&self)->usize{
        match self{
            BlockItem::Decl{decl}=>decl.end_pos(),
            BlockItem::Stmt{stmt}=>stmt.end_pos(),
        }
    }
}

#[derive(Debug,Clone)]
pub enum Decl{
    Var(Span<VarDecl>),
    Const(Span<ConstDecl>),
}

impl Spanned for Decl{
    fn start_pos(&self)->usize{
        match self{
            Decl::Var(var_decl)=>var_decl.start_pos(),
            Decl::Const(const_decl)=>const_decl.start_pos(),
        }
    }
    fn end_pos(&self)->usize{
        match self{
            Decl::Var(var_decl)=>var_decl.end_pos(),
            Decl::Const(const_decl)=>const_decl.end_pos(),
        }
    }
}

#[derive(Debug,Clone)]
pub struct VarDecl{
    pub ty: Span<BType>,
    pub defs: Vec<Span<VarDef>>,
}

impl NonSpanned for VarDecl{}

#[derive(Debug,Clone)]
pub struct VarDef{
    pub ident: Span<String>,
    pub indices: Vec<ConstExp>,
    pub init: Option<Span<InitVal>>,
}

impl NonSpanned for VarDef{}

#[derive(Debug,Clone)]
pub struct ConstDecl{
    pub ty: Span<BType>,
    pub defs: Vec<ConstDef>,
}

impl NonSpanned for ConstDecl{}

#[derive(Debug,Clone)]
pub struct ConstDef{
    pub ident: Span<String>,
    pub indices: Vec<ConstExp>,
    pub init: Span<InitVal>,
}

impl Spanned for ConstDef{
    fn start_pos(&self)->usize{
        self.ident.start_pos()
    }
    fn end_pos(&self)->usize{
        self.init.end_pos()
    }
}


#[derive(Debug,Clone)]
pub enum InitVal{
    Exp(Exp),
    InitList(Vec<Span<InitVal>>),
}

impl NonSpanned for InitVal{}


#[derive(Debug,Clone,PartialEq)]
pub enum BType{
    Int,
    Void,
}

impl NonSpanned for BType{}

#[derive(Debug,Clone)]
pub struct ConstExp{
    pub exp: Exp,
}

impl Spanned for ConstExp{
    fn start_pos(&self)->usize{
        self.exp.start_pos()
    }
    fn end_pos(&self)->usize{
        self.exp.end_pos()
    }
}

#[derive(Debug,Clone)]
pub enum Stmt {
    Return(Option<Exp>),
    Assign{
        lval: Span<LVal>,
        exp: Exp,
    },
    Block{
        block: Span<Block>,
    },
    Exp{
        exp:Option<Exp>
    },
    // 需要解决移进规约冲突
    If{
        cond: Exp,
        then: Box<Span<Stmt>>,
        els: Option<Box<Span<Stmt>>>,
    },
    While {
        cond: Exp,
        body: Box<Span<Stmt>>,
    },
    Break(Span<()>),
    Continue(Span<()>),
}

impl NonSpanned for Stmt{}





#[derive(Debug,Clone)]
pub enum Exp{
    UnaryExp{
        op: Span<MyUnaryOp>,
        exp: Box<Exp>,
    },
    BinaryExp{
        op: Span<MyBinaryOp>,
        exp1: Box<Exp>,
        exp2: Box<Exp>,
    },
    Number(Span<i32>),
    LVal(Span<LVal>),
    Call(Span<CallExp>),
}

impl Spanned for Exp{
    fn start_pos(&self)->usize{
        match self{
            Exp::UnaryExp{op,exp: _}=>op.start_pos(),
            Exp::BinaryExp{op: _,exp1,exp2:_}=>exp1.start_pos(),
            Exp::Number(num)=>num.start_pos(),
            Exp::LVal(ident)=>ident.start_pos(),
            Exp::Call(call_exp)=>call_exp.start_pos(),
        }
    }
    fn end_pos(&self)->usize{
        match self{
            Exp::UnaryExp{op: _,exp}=>exp.end_pos(),
            Exp::BinaryExp{op: _,exp1: _,exp2}=>exp2.end_pos(),
            Exp::Number(num)=>num.end_pos(),
            Exp::LVal(ident)=>ident.end_pos(),
            Exp::Call(call_exp)=>call_exp.end_pos(),
        }
    }
}

#[derive(Debug,Clone)]
pub struct LVal{
    pub ident: Span<String>,
    pub indices: Vec<Exp>,
}
impl NonSpanned for LVal{}

#[derive(Debug,Clone)]
pub struct CallExp{
    pub ident: Span<String>,
    pub args: Vec<Exp>,

}

impl NonSpanned for CallExp{}



#[derive(Debug,Clone)]
pub enum MyUnaryOp{
    Pos,
    Neg,
    Not,
}

impl NonSpanned for MyUnaryOp{}

#[derive(Debug,Clone)]
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

impl NonSpanned for MyBinaryOp{}

impl NonSpanned for i32{}

impl NonSpanned for String{}

impl NonSpanned for (){}


pub trait Spanned{
    fn start_pos(&self)->usize;
    fn end_pos(&self)->usize;
    fn span(&self) -> Range<usize>{
        self.start_pos()..self.end_pos()
    }
}
pub trait NonSpanned{
    fn into_span(self,start:usize,end:usize)->Span<Self>
    where
        Self:Sized
    {
        Span{
            node:self,
            start,
            end,
        }
    }
}

#[derive(Debug,Clone)]
pub struct Span<T>{
    pub node:T,
    pub start:usize,
    pub end:usize,
}

impl<T> Spanned for Span<T>{
    fn start_pos(&self)->usize{
        self.start
    }
    fn end_pos(&self)->usize{
        self.end
    }
}


