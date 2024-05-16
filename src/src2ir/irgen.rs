use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BasicBlock, BinaryOp, FunctionData, Program, Type, Value, ValueKind,
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

        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        let mut block = entry;

        self.block
            .node
            .generate_program(func_data, symtable, &mut block);

        // a temporary fix to remove all insts after ret
        // let dfg = func_data.dfg();

        // let layout = func_data.layout(); // 获取 func_data 的可变引用
        // let insts = layout.bbs().node(&entry).unwrap().insts();

        // let mut it: Option<Value> = None;

        // for i in insts.iter() {
        //     match dfg.value(*i.0).kind() {
        //         ValueKind::Return(_) => {
        //             it = Some(*i.0);
        //             break;
        //         }
        //         _ => {}
        //     }
        // }

        // let insts = func_data.layout_mut().bb_mut(entry).insts_mut();

        // while *insts.back_key().unwrap() != it.unwrap() {
        //     insts.pop_back();
        // }
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
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        entry: &mut BasicBlock,
    ) {
        dbg!("enter a block");
        symtable.push();

        for item in self.items {
            item.generate_program(func_data, symtable, entry);
        }

        symtable.pop();
        dbg!("exit a block");
    }
}

impl BlockItem {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
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
        block: &mut BasicBlock,
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
        block: &mut BasicBlock,
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
        block: &mut BasicBlock,
    ) {
        let var = func_data.dfg_mut().new_value().alloc(ty.build_ir());
        func_data
            .layout_mut()
            .bb_mut(*block)
            .insts_mut()
            .push_key_back(var)
            .unwrap();

        if let Some(init) = self.init {
            let init_val = init.generate_program(symtable, func_data, block);
            let store = func_data.dfg_mut().new_value().store(init_val, var);
            func_data
                .layout_mut()
                .bb_mut(*block)
                .insts_mut()
                .push_key_back(store)
                .unwrap();
        }

        symtable.insert(self.ident.node, Symbol::Var(var))
    }
}

impl InitVal {
    pub fn generate_program(
        self,
        symtable: &mut SymTable,
        func_data: &mut FunctionData,
        block: &mut BasicBlock,
    ) -> Value {
        self.exp.generate_program(symtable, func_data, block)
    }
}

impl ConstDecl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
    ) {
        for def in self.defs {
            def.generate_program(&self.ty.node, func_data, symtable, block);
        }
    }
}

impl ConstDef {
    pub fn generate_program(
        self,
        _ty: &BType,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
    ) {
        let val = self.exp.calculate_const(func_data, symtable, block);
        symtable.insert(self.ident.node, Symbol::Const(val));
    }
}

impl ConstExp {
    pub fn calculate_const(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
    ) -> i32 {
        // TODO: error handling
        self.exp.calculate_const(func_data, symtable, block)
    }
}

impl Stmt {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
    ) {
        match self {
            Stmt::Return(r) => {
                let expr = r.generate_program(symtable, func_data, block);
                let ret = func_data.dfg_mut().new_value().ret(Some(expr));
                func_data
                    .layout_mut()
                    .bb_mut(*block)
                    .insts_mut()
                    .push_key_back(ret)
                    .unwrap();
            }
            Stmt::Assign { ident, exp } => {
                let var = symtable.get(&ident.node);
                let var = match var {
                    Some(Symbol::Var(var)) => *var,
                    Some(Symbol::Const(_)) => panic!("cannot assign to a const"),
                    None => panic!("variable not found"),
                };

                let exp = exp.generate_program(symtable, func_data, block);
                let store = func_data.dfg_mut().new_value().store(exp, var);

                func_data
                    .layout_mut()
                    .bb_mut(*block)
                    .insts_mut()
                    .push_key_back(store)
                    .unwrap();
            }
            Stmt::Block { block: _block } => {
                _block.node.generate_program(func_data, symtable, block);
            }
            Stmt::Exp { exp } => {
                if let Some(exp) = exp {
                    exp.generate_program(symtable, func_data, block);
                }
            }
            Stmt::If { cond, then, els } => {
                // 处理if的生成
                // 为then和else分别创建两个block
                let cond = cond.generate_program(symtable, func_data, block);
                let mut then_block = func_data.dfg_mut().new_bb().basic_block(None);
                let mut else_block = func_data.dfg_mut().new_bb().basic_block(None);
                let merge_block = func_data.dfg_mut().new_bb().basic_block(None);
                func_data
                    .layout_mut()
                    .bbs_mut()
                    .extend([then_block, else_block, merge_block]);
                // if cond
                let br = func_data
                    .dfg_mut()
                    .new_value()
                    .branch(cond, then_block, else_block);
                func_data
                    .layout_mut()
                    .bb_mut(*block)
                    .insts_mut()
                    .extend([br]);

                let mut then_ret = false;
                let mut else_ret = false;

                // then
                then.node
                    .generate_program(func_data, symtable, &mut then_block);

                let dfg = func_data.dfg();
                let layout = func_data.layout(); // 获取 func_data 的可变引用
                let insts = layout.bbs().node(&then_block).unwrap().insts();

                for i in insts.iter() {
                    match dfg.value(*i.0).kind() {
                        ValueKind::Return(_) => {
                            then_ret = true;
                            break;
                        }
                        _ => {}
                    }
                }
                if !then_ret {
                    let br = func_data.dfg_mut().new_value().jump(merge_block);
                    func_data
                        .layout_mut()
                        .bb_mut(then_block)
                        .insts_mut()
                        .extend([br]);
                }

                // else
                if let Some(els) = els {
                    els.node
                        .generate_program(func_data, symtable, &mut else_block);
                }

                let dfg = func_data.dfg();
                let layout = func_data.layout(); // 获取 func_data 的可变引用
                let insts = layout.bbs().node(&else_block).unwrap().insts();

                for i in insts.iter() {
                    match dfg.value(*i.0).kind() {
                        ValueKind::Return(_) => {
                            else_ret = true;
                            break;
                        }
                        _ => {}
                    }
                }
                if !else_ret {
                    let br = func_data.dfg_mut().new_value().jump(merge_block);
                    func_data
                        .layout_mut()
                        .bb_mut(else_block)
                        .insts_mut()
                        .extend([br]);
                }


                // merge
                *block = merge_block;
                
            }
        }
    }
}

impl Return {
    pub fn generate_program(
        self,
        symtable: &mut SymTable,
        func_data: &mut FunctionData,
        block: &mut BasicBlock,
    ) -> Value {
        self.exp.generate_program(symtable, func_data, block)
    }
}

impl Exp {
    pub fn generate_program(
        self,
        symtable: &mut SymTable,
        func_data: &mut FunctionData,
        block: &mut BasicBlock,
    ) -> Value {
        match self {
            // 一元运算表达式
            Exp::UnaryExp { op, exp } => {
                let expr = exp.generate_program(symtable, func_data, block);
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(neg)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(not)
                            .unwrap();
                        not
                    }
                    MyUnaryOp::Pos => expr,
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                let expr1 = exp1.generate_program(symtable, func_data, block);
                let expr2 = exp2.generate_program(symtable, func_data, block);
                match op.node {
                    MyBinaryOp::Add => {
                        let add =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Add, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(add)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(sub)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(mul)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(div)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(_mod)
                            .unwrap();
                        _mod
                    }
                    MyBinaryOp::Eq => {
                        let eq = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Eq, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(eq)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(ne)
                            .unwrap();
                        ne
                    }
                    MyBinaryOp::Lt => {
                        let lt = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Lt, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(lt)
                            .unwrap();
                        lt
                    }
                    MyBinaryOp::Gt => {
                        let gt = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Gt, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(gt)
                            .unwrap();
                        gt
                    }
                    MyBinaryOp::Le => {
                        let le = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Le, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(le)
                            .unwrap();
                        le
                    }
                    MyBinaryOp::Ge => {
                        let ge = func_data
                            .dfg_mut()
                            .new_value()
                            .binary(BinaryOp::Ge, expr1, expr2);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(ge)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(and1)
                            .unwrap();
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(and2)
                            .unwrap();
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(and)
                            .unwrap();
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
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(or1)
                            .unwrap();
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(or2)
                            .unwrap();
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(or)
                            .unwrap();
                        or
                    }
                }
            }
            Exp::Number(n) => func_data.dfg_mut().new_value().integer(n.node),

            Exp::LVar(name) => {
                let var = symtable.get(&name.node);
                let var = match var {
                    Some(Symbol::Var(var)) => {
                        let load = func_data.dfg_mut().new_value().load(*var);
                        func_data
                            .layout_mut()
                            .bb_mut(*block)
                            .insts_mut()
                            .push_key_back(load)
                            .unwrap();
                        load
                    }
                    Some(Symbol::Const(n)) => func_data.dfg_mut().new_value().integer(*n),
                    None => panic!("variable not found"),
                };
                var
            }
        }
    }
    pub fn calculate_const(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: &mut BasicBlock,
    ) -> i32 {
        match self {
            Exp::UnaryExp { op, exp } => {
                let exp = exp.calculate_const(func_data, symtable, block);
                match op.node {
                    MyUnaryOp::Neg => -exp,
                    MyUnaryOp::Not => {
                        if exp == 0 {
                            1
                        } else {
                            0
                        }
                    }
                    MyUnaryOp::Pos => exp,
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                let exp1 = exp1.calculate_const(func_data, symtable, block);
                let exp2 = exp2.calculate_const(func_data, symtable, block);
                match op.node {
                    MyBinaryOp::Add => exp1 + exp2,
                    MyBinaryOp::Sub => exp1 - exp2,
                    MyBinaryOp::Mul => exp1 * exp2,
                    MyBinaryOp::Div => exp1 / exp2,
                    MyBinaryOp::Mod => exp1 % exp2,
                    MyBinaryOp::Eq => {
                        if exp1 == exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::Ne => {
                        if exp1 != exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::Lt => {
                        if exp1 < exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::Gt => {
                        if exp1 > exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::Le => {
                        if exp1 <= exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::Ge => {
                        if exp1 >= exp2 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::LAnd => {
                        if exp1 != 0 && exp2 != 0 {
                            1
                        } else {
                            0
                        }
                    }
                    MyBinaryOp::LOr => {
                        if exp1 != 0 || exp2 != 0 {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
            Exp::Number(n) => n.node,
            Exp::LVar(name) => {
                let var = symtable.get(&name.node);
                let var = match var {
                    Some(Symbol::Var(_)) => panic!("cannot use a variable as a const"),
                    Some(Symbol::Const(n)) => *n,
                    None => panic!("variable not found"),
                };
                var
            }
        }
    }
}
