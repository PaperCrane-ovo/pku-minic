use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

use crate::ast::*;

impl CompUnit {
    pub fn build_ir(self) -> Program {
        let mut program = Program::new();
        self.generate_program(&mut program);
        program
    }
    fn generate_program(self, program: &mut Program) {
        self.func_def.generate_program(program);
    }
}

impl FuncDef {
    pub fn generate_program(self, program: &mut Program) {
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident),
            vec![],
            self.func_type.generate_program(),
        ));
        let func_data = program.func_mut(func);
        self.block.generate_program(func_data);
    }
}

impl FuncType {
    pub fn generate_program(self) -> Type {
        match self {
            FuncType::Int => Type::get_i32(),
        }
    }
}

impl Block {
    pub fn generate_program(self, func_data: &mut FunctionData) {
        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        self.stmt.generate_program(func_data, entry);
    }
}

impl Stmt {
    pub fn generate_program(self, func_data: &mut FunctionData,block:BasicBlock) {
        match self {
            Stmt::Return(r) => {
                let expr = r.generate_program(func_data,block);
                let ret = func_data.dfg_mut().new_value().ret(Some(expr));
                func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(ret);
            }
        }
    }
}

impl Return{
    pub fn generate_program(self, func_data: &mut FunctionData,block:BasicBlock) -> Value {
        self.exp.generate_program(func_data,block)
    }
}

impl Exp{
    pub fn generate_program(self, func_data: &mut FunctionData,block:BasicBlock) -> Value {
        match self {
            // 一元运算表达式
            Exp::UnaryExp{op,exp} => {
                let expr = exp.generate_program(func_data,block);
                match op {
                    MyUnaryOp::Neg => {
                        // Neg转为0减，即二元运算
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let neg = func_data.dfg_mut().new_value().binary(BinaryOp::Sub,zero,expr);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(neg);
                        neg
                    }
                    MyUnaryOp::Not => {
                        // 逻辑取反，和0比较相等
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let not = func_data.dfg_mut().new_value().binary(BinaryOp::Eq,zero,expr);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(not);
                        not
                    }
                    MyUnaryOp::Pos => {
                        expr
                    }
                }
            }
            Exp::BinaryExp{op,exp1,exp2} => {
                let expr1 = exp1.generate_program(func_data,block);
                let expr2 = exp2.generate_program(func_data,block);
                match op {
                    MyBinaryOp::Add => {
                        let add = func_data.dfg_mut().new_value().binary(BinaryOp::Add,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(add);
                        add
                    }
                    MyBinaryOp::Sub => {
                        let sub = func_data.dfg_mut().new_value().binary(BinaryOp::Sub,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(sub);
                        sub
                    }
                    MyBinaryOp::Mul => {
                        let mul = func_data.dfg_mut().new_value().binary(BinaryOp::Mul,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(mul);
                        mul
                    }
                    MyBinaryOp::Div => {
                        let div = func_data.dfg_mut().new_value().binary(BinaryOp::Div,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(div);
                        div
                    }
                    MyBinaryOp::Mod => {
                        let _mod = func_data.dfg_mut().new_value().binary(BinaryOp::Mod,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(_mod);
                        _mod
                    }
                    MyBinaryOp::Eq => {
                        let eq = func_data.dfg_mut().new_value().binary(BinaryOp::Eq,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(eq);
                        eq
                    }
                    MyBinaryOp::Ne => {
                        let ne = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(ne);
                        ne
                    }
                    MyBinaryOp::Lt => {
                        let lt = func_data.dfg_mut().new_value().binary(BinaryOp::Lt,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(lt);
                        lt
                    }
                    MyBinaryOp::Gt => {
                        let gt = func_data.dfg_mut().new_value().binary(BinaryOp::Gt,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(gt);
                        gt
                    }
                    MyBinaryOp::Le => {
                        let le = func_data.dfg_mut().new_value().binary(BinaryOp::Le,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(le);
                        le
                    }
                    MyBinaryOp::Ge => {
                        let ge = func_data.dfg_mut().new_value().binary(BinaryOp::Ge,expr1,expr2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(ge);
                        ge
                    }
                    // 逻辑运算需要先转为0和1然后再进行运算
                    MyBinaryOp::LAnd => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let and1 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq,zero,expr1);
                        let and2 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq,zero,expr2);
                        let and = func_data.dfg_mut().new_value().binary(BinaryOp::And,and1,and2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(and1);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(and2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(and);
                        and
                    }
                    MyBinaryOp::LOr => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let or1 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq,zero,expr1);
                        let or2 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq,zero,expr2);
                        let or = func_data.dfg_mut().new_value().binary(BinaryOp::Or,or1,or2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(or1);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(or2);
                        func_data.layout_mut().bb_mut(block).insts_mut().push_key_back(or);
                        or
                    }

                    
                }
            }
            Exp::Number(n) => {
                func_data.dfg_mut().new_value().integer(n)
            }
        }
    }
}