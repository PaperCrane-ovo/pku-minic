use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BasicBlock, BinaryOp, FunctionData, Program, Type, Value,
};

use crate::ast::*;

use super::symtable::{SymTable, Symbol};

impl CompUnit {
    pub fn build_ir(self) -> Program {
        let mut symtable = SymTable::new();
        let mut program = Program::new();
        self.generate_program(&mut program, &mut symtable);
        program
    }
    fn generate_program(self, program: &mut Program, symtable: &mut SymTable) {
        self.func_def.generate_program(program, symtable);
    }
}

impl FuncDef {
    pub fn generate_program(self, program: &mut Program, symtable: &mut SymTable) {
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.node),
            vec![],
            self.func_type.node.generate_program(),
        ));
        let func_data = program.func_mut(func);
        self.block.node.generate_program(func_data, symtable);
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
    pub fn generate_program(self, func_data: &mut FunctionData, symtable: &mut SymTable) {
        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        for item in self.items {
            item.generate_program(func_data, symtable, entry);
        }
    }
}

impl BlockItem {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            BlockItem::Decl { decl } => {
                decl.generate_program(func_data, symtable, block);
            }
            BlockItem::Stmt { stmt } => {
                stmt.node.generate_program(func_data, symtable, block);
            }
        }
    }
}

impl Decl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            Decl::Var(var_decl) => {
                var_decl.node.generate_program(func_data, symtable, block);
            }
            Decl::Const(const_decl) => {
                const_decl.node.generate_program(func_data, symtable, block);
            }
        }
    }
}

impl BType {
    pub fn build_ir(&self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
        }
    }
}

impl VarDecl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        for def in self.defs {
            def.generate_program(&self.ty.node, func_data, symtable, block);
        }
    }
}

impl VarDef {
    pub fn generate_program(
        self,
        ty: &BType,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        let var = func_data.dfg_mut().new_value().alloc(ty.build_ir());
        func_data
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .push_key_back(var);

        if let Some(init) = self.init {
            let init_val = init.generate_program(symtable, func_data, block);
            let store = func_data.dfg_mut().new_value().store(init_val, var);
            func_data
                .layout_mut()
                .bb_mut(block)
                .insts_mut()
                .push_key_back(store);
        }

        symtable.insert(self.ident.node, Symbol::Var(var))
    }
}

impl InitVal{
    pub fn generate_program(
        self,
        symtable: &mut SymTable,
        func_data: &mut FunctionData,
        block: BasicBlock,
    ) -> Value {
        self.exp.generate_program(symtable,func_data, block)
    }
}

impl ConstDecl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        for def in self.defs {
            def.generate_program(&self.ty.node, func_data, symtable, block);
        }
    }
}

impl ConstDef{
    pub fn generate_program(
        self,
        ty: &BType,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        let val = self.exp.calculate_const(func_data,symtable,block);
        symtable.insert(self.ident.node, Symbol::Const(val));
    }
}

impl ConstExp{
    pub fn calculate_const(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) -> i32 { // TODO: error handling
        self.exp.calculate_const(func_data,symtable,block)
    }
}



impl Stmt {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            Stmt::Return(r) => {
                let expr = r.generate_program(symtable,func_data, block);
                let ret = func_data.dfg_mut().new_value().ret(Some(expr));
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .push_key_back(ret);
            }
            Stmt::Assign { ident, exp } => {
                let var = symtable.get(&ident.node);
                let var = match var{
                    Some(Symbol::Var(var))=>*var,
                    Some(Symbol::Const(_))=>panic!("cannot assign to a const"),
                    None=>panic!("variable not found"),
                };

                let exp = exp.generate_program(symtable, func_data, block);
                let store = func_data.dfg_mut().new_value().store(exp,var);
            
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .push_key_back(store);
            }
        }
    }
}

impl Return {
    pub fn generate_program(self,symtable:&mut SymTable, func_data: &mut FunctionData, block: BasicBlock) -> Value {
        self.exp.generate_program(symtable,func_data, block)
    }
}

impl Exp {
    pub fn generate_program(self,symtable:&mut SymTable, func_data: &mut FunctionData, block: BasicBlock) -> Value {
        match self {
            // 一元运算表达式
            Exp::UnaryExp { op, exp } => {
                let expr = exp.generate_program(symtable,func_data, block);
                match op.node {
                    MyUnaryOp::Neg => {
                        // Neg转为0减，即二元运算
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let neg = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Sub, zero, expr);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(neg);
                        neg
                    }
                    MyUnaryOp::Not => {
                        // 逻辑取反，和0比较相等
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let not = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Eq, zero, expr);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(not);
                        not
                    }
                    MyUnaryOp::Pos => expr,
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                let expr1 = exp1.generate_program(symtable,func_data, block);
                let expr2 = exp2.generate_program(symtable,func_data, block);
                match op.node {
                    MyBinaryOp::Add => {
                        let add =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Add, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(add);
                        add
                    }
                    MyBinaryOp::Sub => {
                        let sub =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Sub, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(sub);
                        sub
                    }
                    MyBinaryOp::Mul => {
                        let mul =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Mul, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(mul);
                        mul
                    }
                    MyBinaryOp::Div => {
                        let div =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Div, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(div);
                        div
                    }
                    MyBinaryOp::Mod => {
                        let _mod =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Mod, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(_mod);
                        _mod
                    }
                    MyBinaryOp::Eq => {
                        let eq = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Eq, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(eq);
                        eq
                    }
                    MyBinaryOp::Ne => {
                        let ne =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::NotEq, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(ne);
                        ne
                    }
                    MyBinaryOp::Lt => {
                        let lt = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Lt, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(lt);
                        lt
                    }
                    MyBinaryOp::Gt => {
                        let gt = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Gt, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(gt);
                        gt
                    }
                    MyBinaryOp::Le => {
                        let le = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Le, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(le);
                        le
                    }
                    MyBinaryOp::Ge => {
                        let ge = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Ge, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(ge);
                        ge
                    }
                    // 逻辑运算需要先转为0和1然后再进行运算
                    MyBinaryOp::LAnd => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let and1 =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::NotEq, zero, expr1);
                        let and2 =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::NotEq, zero, expr2);
                        let and = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::And, and1, and2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(and1);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(and2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(and);
                        and
                    }
                    MyBinaryOp::LOr => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let or1 =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::NotEq, zero, expr1);
                        let or2 =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::NotEq, zero, expr2);
                        let or = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Or, or1, or2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(or1);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(or2);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(or);
                        or
                    }
                }
            }
            Exp::Number(n) => func_data.dfg_mut().new_value().integer(n.node),

            Exp::LVar(name) => {
                let var = symtable.get(&name.node);
                let var = match var{
                    Some(Symbol::Var(var))=> {
                        let load = func_data.dfg_mut().new_value().load(*var);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .push_key_back(load);
                        load
                    }
                    Some(Symbol::Const(n))=>func_data.dfg_mut().new_value().integer(*n),
                    None=>panic!("variable not found"),
                };
                var
            }
        }
    }
    pub fn calculate_const(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) -> i32 {
        match self{
            Exp::UnaryExp { op, exp } => {
                let exp = exp.calculate_const(func_data,symtable,block);
                match op.node {
                    MyUnaryOp::Neg => -exp,
                    MyUnaryOp::Not => if exp == 0 {1} else {0},
                    MyUnaryOp::Pos => exp,
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                let exp1 = exp1.calculate_const(func_data,symtable,block);
                let exp2 = exp2.calculate_const(func_data,symtable,block);
                match op.node {
                    MyBinaryOp::Add => exp1 + exp2,
                    MyBinaryOp::Sub => exp1 - exp2,
                    MyBinaryOp::Mul => exp1 * exp2,
                    MyBinaryOp::Div => exp1 / exp2,
                    MyBinaryOp::Mod => exp1 % exp2,
                    MyBinaryOp::Eq => if exp1 == exp2 {1} else {0},
                    MyBinaryOp::Ne => if exp1 != exp2 {1} else {0},
                    MyBinaryOp::Lt => if exp1 < exp2 {1} else {0},
                    MyBinaryOp::Gt => if exp1 > exp2 {1} else {0},
                    MyBinaryOp::Le => if exp1 <= exp2 {1} else {0},
                    MyBinaryOp::Ge => if exp1 >= exp2 {1} else {0},
                    MyBinaryOp::LAnd => if exp1 != 0 && exp2 != 0 {1} else {0},
                    MyBinaryOp::LOr => if exp1 != 0 || exp2 != 0 {1} else {0},
                }

            }
            Exp::Number(n) => n.node,
            Exp::LVar(name) => {
                let var = symtable.get(&name.node);
                let var = match var{
                    Some(Symbol::Var(_))=>panic!("cannot use a variable as a const"),
                    Some(Symbol::Const(n))=>*n,
                    None=>panic!("variable not found"),
                };
                var
            }
        }
    }
}
