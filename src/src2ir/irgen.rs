use super::{
    context::Context,
    core::{Core, InstType},
};

use koopa::ir::{
    builder::{GlobalInstBuilder, ValueBuilder}, BinaryOp, FunctionData, Program, Type, Value
};

use crate::ast::*;

use super::{
    bbexaminer::BBExaminer,
    symtable::{SymTable, Symbol},
};

impl CompUnit {
    pub fn build_ir(self) -> Program {
        let mut symtable = SymTable::new();
        let mut program = Program::new();
        symtable.push();
        self.generate_program(&mut program, &mut symtable);
        symtable.pop();
        program
    }
    fn generate_program(self, program: &mut Program, symtable: &mut SymTable) {
        // declare sysy lib functions
        self.decl_func(program, symtable, "@getint", vec![], Type::get_i32());
        self.decl_func(program, symtable, "@getch", vec![], Type::get_i32());
        self.decl_func(program, symtable, "@getarray", vec![Type::get_pointer(Type::get_i32())], Type::get_i32());
        self.decl_func(program, symtable, "@putint", vec![Type::get_i32()], Type::get_unit());
        self.decl_func(program, symtable, "@putch", vec![Type::get_i32()], Type::get_unit());
        self.decl_func(program, symtable, "@putarray", vec![Type::get_i32(), Type::get_pointer(Type::get_i32())], Type::get_unit());
        self.decl_func(program, symtable, "@starttime", vec![], Type::get_unit());
        self.decl_func(program, symtable, "@stoptime", vec![], Type::get_unit());

        for item in self.global_items {
            match item {
                GlobalItem::Decl(decl) => decl.generate_program_global(program, symtable),
                GlobalItem::FuncDef(func_def) => func_def.generate_program(program, symtable),
            }
        }
    }
    fn decl_func(&self,program:&mut Program,symtable:&mut SymTable,name:&str,params_ty:Vec<Type>,ret_ty:Type){
        let func = program.new_func(
            FunctionData::new_decl(name.to_string(), params_ty, ret_ty)
        );
        symtable.insert(name[1..].to_string(), Symbol::Func(func));
    }
}

impl FuncDef {
    pub fn generate_program(self, program: &mut Program, symtable: &mut SymTable) {
        let params = self
            .params
            .iter()
            .map(|param| param.node.generate_program())
            .collect();

        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.node),
            params,
            self.func_type.node.build_ir(),
        ));

        // add context manager
        let mut context = Context::new();

        let mut func_data = program.func_mut(func);

        let return_type = self.func_type.node;

        let mut core = Core::new(&mut func_data, &mut context, return_type);

        // insert function into symbol table

        symtable.insert(self.ident.node.clone(), Symbol::Func(func));

        symtable.push();

        // insert parameters into symbol table and koopa ir

        for (i, param) in self.params.iter().enumerate() {
            let (_, ty) = param.node.generate_program();
            let var = core.new_value(InstType::Alloc(ty));
            let dfg = core.dfg_mut();
            let name = format!("%{}",param.node.ident.node);
            dfg.set_value_name(var, Some(name));

            symtable.insert(param.node.ident.node.clone(), Symbol::Var(var));
            let cur_param = core.func_data().params()[i];
            let store = core.new_value(InstType::Store(cur_param, var));
            core.push_insts(vec![var, store]);
        }

        self.block.node.generate_program(&mut core, symtable);
        symtable.pop();

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
        let bb_examiner = BBExaminer::new();
        bb_examiner.examine_ret(&mut core);
        bb_examiner.examine_bb_name(&mut core);
        BBExaminer::clean_all_extra_inst(&mut core);
    }
}

/// Deprecated because of the existence of BType
// impl FuncType {
//     pub fn generate_program(&self) -> Type {
//         match self {
//             FuncType::Int => Type::get_i32(),
//             FuncType::Void => Type::get_unit(),
//         }
//     }
// }

impl FuncParam {
    pub fn generate_program(&self) -> (Option<String>, Type) {
        (
            Some(format!("@{}", self.ident.node)),
            self.ty.node.build_ir(),
        )
    }
}

impl Block {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) {
        dbg!("enter a block");
        symtable.push();

        for item in self.items {
            item.generate_program(core, symtable);
        }

        symtable.pop();
        dbg!("exit a block");
    }
}

impl BlockItem {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) {
        match self {
            BlockItem::Decl { decl } => {
                decl.generate_program(core, symtable);
            }
            BlockItem::Stmt { stmt } => {
                stmt.node.generate_program(core, symtable);
            }
        }
    }
}

impl Decl {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) {
        match self {
            Decl::Var(var_decl) => {
                var_decl.node.generate_program(core, symtable);
            }
            Decl::Const(const_decl) => {
                const_decl.node.generate_program(symtable);
            }
        }
    }
    pub fn generate_program_global(self,program:&mut Program,symtable:&mut SymTable){
        match self {
            Decl::Var(var_decl) => {
                var_decl.node.generate_program_global(program, symtable);
            }
            Decl::Const(const_decl) => {
                const_decl.node.generate_program(symtable);
            }
        }
    }
}

impl BType {
    pub fn build_ir(&self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
            BType::Void => Type::get_unit(),
        }
    }
}

impl VarDecl {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) {
        for def in self.defs {
            def.generate_program(&self.ty.node, core, symtable);
        }
    }
    pub fn generate_program_global(self,program:&mut Program,symtable:&mut SymTable){
        for def in self.defs {
            def.generate_program_global(program, &self.ty.node, symtable);
        }
    }
}

impl VarDef {
    pub fn generate_program(self, ty: &BType, core: &mut Core, symtable: &mut SymTable) {
        let var = core.new_value(InstType::Alloc(ty.build_ir()));
        core.push_inst(var);

        if let Some(init) = self.init {
            let init_val = init.generate_program(core, symtable);
            let store = core.new_value(InstType::Store(init_val, var));
            core.push_inst(store);
        }

        symtable.insert(self.ident.node, Symbol::Var(var));
    }
    pub fn generate_program_global(self,program:&mut Program,ty:&BType,symtable:&mut SymTable){
        let init = match self.init{
            Some(init) => {
                let val = init.exp.calculate_const(symtable);
                program.new_value().integer(val)
            }
            None => program.new_value().zero_init(ty.build_ir()),
        };

        let global_var = program.new_value().global_alloc(init);
        program.set_value_name(global_var, Some(format!("@{}",self.ident.node)));
        
        symtable.insert(self.ident.node, Symbol::Var(global_var));
    }

}

impl InitVal {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) -> Value {
        self.exp.generate_program(core, symtable)
    }
}

impl ConstDecl {
    pub fn generate_program(self,symtable: &mut SymTable) {
        for def in self.defs {
            def.generate_program(&self.ty.node, symtable);
        }
    }
}

impl ConstDef {
    pub fn generate_program(self, _ty: &BType, symtable: &mut SymTable) {
        let val = self.exp.calculate_const(symtable);
        symtable.insert(self.ident.node, Symbol::Const(val));
    }
}

impl ConstExp {
    pub fn calculate_const(self,symtable: &mut SymTable) -> i32 {
        // TODO: error handling
        self.exp.calculate_const(symtable)
    }
}

impl Stmt {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) {
        match self {
            Stmt::Return(r) => {
                let expr = r.generate_program(core, symtable);
                let ret = core.new_value(InstType::Ret(Some(expr)));
                core.push_inst(ret);
            }
            Stmt::Assign { ident, exp } => {
                let var = symtable.get(&ident.node);
                let var = match var {
                    Some(Symbol::Var(var)) => *var,
                    Some(Symbol::Const(_)) => panic!("cannot assign to a const"),
                    Some(Symbol::Func(_)) => panic!("cannot assign to a function"),
                    None => panic!("variable not found"),
                };

                let exp = exp.generate_program(core, symtable);
                dbg!(ident.start_pos(),ident.end_pos());
                let store = core.new_value(InstType::Store(exp, var));
                core.push_inst(store);
            }
            Stmt::Block { block: _block } => {
                _block.node.generate_program(core, symtable);
            }
            Stmt::Exp { exp } => {
                if let Some(exp) = exp {
                    exp.generate_program(core, symtable);
                }
            }
            Stmt::If { cond, then, els } => {
                // 处理if的生成
                // 为then和else分别创建两个block
                let cond = cond.generate_program(core, symtable);
                let then_block = core.new_block(None);
                let else_block = core.new_block(None);
                let merge_block = core.new_block(None);
                core.push_blocks(vec![then_block, else_block, merge_block]);
                // if cond
                let br = core.new_value(InstType::Branch(cond, then_block, else_block));
                core.push_inst(br);


                // then
                core.switch_block(then_block);
                then.node.generate_program(core, symtable);

                let bb_examiner = BBExaminer::new();
                if !bb_examiner.is_terminated(core, then_block) {
                    let br = core.new_value(InstType::Jump(merge_block));
                    core.push_inst(br);
                }

                // else
                if let Some(els) = els {
                    core.switch_block(else_block);
                    els.node.generate_program(core, symtable);
                }

                if !bb_examiner.is_terminated(core, else_block) {
                    let br = core.new_value(InstType::Jump(merge_block));
                    core.switch_block(else_block);
                    core.push_inst(br);
                }
                
                // merge
                core.switch_block(merge_block);
            }
            Stmt::While { cond, body } => {
                let cond_block = core.new_block(None);
                let body_block = core.new_block(None);
                let next_block = core.new_block(None);

                core.push_blocks(vec![cond_block, body_block, next_block]);

                core.context_mut().push_loop_bound(cond_block, next_block);
                let jump_to_cond = core.new_value(InstType::Jump(cond_block));
                core.push_inst(jump_to_cond);
                core.switch_block(cond_block);
                let cond = cond.generate_program(core, symtable);
                let br = core.new_value(InstType::Branch(cond, body_block, next_block));

                core.push_inst(br);
                core.switch_block(body_block);
                body.node.generate_program(core, symtable);
                // TODO : return in while
                let jump_to_cond = core.new_value(InstType::Jump(cond_block));

                let bb_examiner = BBExaminer::new();
                if !bb_examiner.is_terminated(core, body_block) {
                    core.push_inst(jump_to_cond);
                }
                core.context_mut().pop_loop_bound();
                core.switch_block(next_block);
            }
            Stmt::Break(_span) => {
                let loop_bound = core.context().get_loop_bound();
                if !loop_bound.is_none() {
                    let jump = core.new_value(InstType::Jump(loop_bound.unwrap().exit));
                    core.push_inst(jump);
                }
            }
            Stmt::Continue(_span) => {
                let loop_bound = core.context().get_loop_bound();
                if !loop_bound.is_none() {
                    let jump = core.new_value(InstType::Jump(loop_bound.unwrap().entry));
                    core.push_inst(jump);
                }
            }
        }
    }
}

impl Return {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) -> Value {
        self.exp.generate_program(core, symtable)
    }
}

impl Exp {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) -> Value {
        match self {
            // 一元运算表达式
            Exp::UnaryExp { op, exp } => {
                let expr = exp.generate_program(core, symtable);
                match op.node {
                    MyUnaryOp::Neg => {
                        // Neg转为0减，即二元运算
                        let zero = core.new_int(0);
                        let neg = core.new_value(InstType::Binary(BinaryOp::Sub, zero, expr));
                        core.push_inst(neg);
                        neg
                    }
                    MyUnaryOp::Not => {
                        // 逻辑取反，和0比较相等
                        let zero = core.new_int(0);
                        let not = core.new_value(InstType::Binary(BinaryOp::Eq, zero, expr));
                        core.push_inst(not);
                        not
                    }
                    MyUnaryOp::Pos => expr,
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                let expr1 = exp1.generate_program(core, symtable);
                let expr2 = exp2.generate_program(core, symtable);
                match op.node {
                    MyBinaryOp::Add => {
                        let add = core.new_value(InstType::Binary(BinaryOp::Add, expr1, expr2));
                        core.push_inst(add);
                        add
                    }
                    MyBinaryOp::Sub => {
                        let sub = core.new_value(InstType::Binary(BinaryOp::Sub, expr1, expr2));
                        core.push_inst(sub);
                        sub
                    }
                    MyBinaryOp::Mul => {
                        let mul = core.new_value(InstType::Binary(BinaryOp::Mul, expr1, expr2));
                        core.push_inst(mul);
                        mul
                    }
                    MyBinaryOp::Div => {
                        let div = core.new_value(InstType::Binary(BinaryOp::Div, expr1, expr2));
                        core.push_inst(div);
                        div
                    }
                    MyBinaryOp::Mod => {
                        let _mod = core.new_value(InstType::Binary(BinaryOp::Mod, expr1, expr2));
                        core.push_inst(_mod);
                        _mod
                    }
                    MyBinaryOp::Eq => {
                        let eq = core.new_value(InstType::Binary(BinaryOp::Eq, expr1, expr2));
                        core.push_inst(eq);

                        eq
                    }
                    MyBinaryOp::Ne => {
                        let ne = core.new_value(InstType::Binary(BinaryOp::NotEq, expr1, expr2));
                        core.push_inst(ne);
                        ne
                    }
                    MyBinaryOp::Lt => {
                        let lt = core.new_value(InstType::Binary(BinaryOp::Lt, expr1, expr2));
                        core.push_inst(lt);
                        lt
                    }
                    MyBinaryOp::Gt => {
                        let gt = core.new_value(InstType::Binary(BinaryOp::Gt, expr1, expr2));
                        core.push_inst(gt);
                        gt
                    }
                    MyBinaryOp::Le => {
                        let le = core.new_value(InstType::Binary(BinaryOp::Le, expr1, expr2));
                        core.push_inst(le);
                        le
                    }
                    MyBinaryOp::Ge => {
                        let ge = core.new_value(InstType::Binary(BinaryOp::Ge, expr1, expr2));
                        core.push_inst(ge);
                        ge
                    }
                    // 逻辑运算需要先转为0和1然后再进行运算
                    MyBinaryOp::LAnd => {
                        // 实现短路求值
                        let result = core.new_value(InstType::Alloc(Type::get_i32()));
                        core.push_inst(result);
                        let zero = core.new_int(0);
                        let one = core.new_int(1);

                        let store = core.new_value(InstType::Store(zero, result));
                        core.push_inst(store);

                        let and1 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr1));
                        core.push_inst(and1);
                        let and2_block = core.new_block(None);
                        let and2_next_block = core.new_block(None);
                        let merge_block = core.new_block(None);
                        core.push_blocks(vec![and2_block, and2_next_block, merge_block]);

                        let br = core.new_value(InstType::Branch(and1, and2_block, merge_block));
                        core.push_inst(br);

                        let and2 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr2));
                        core.switch_block(and2_block);
                        core.push_inst(and2);
                        let br =
                            core.new_value(InstType::Branch(and2, and2_next_block, merge_block));
                        core.push_inst(br);

                        let store = core.new_value(InstType::Store(one, result));
                        core.switch_block(and2_next_block);
                        core.push_inst(store);

                        let jump = core.new_value(InstType::Jump(merge_block));
                        core.push_inst(jump);

                        core.switch_block(merge_block);
                        let load = core.new_value(InstType::Load(result));
                        core.push_inst(load);
                        load
                    }
                    MyBinaryOp::LOr => {
                        let result = core.new_value(InstType::Alloc(Type::get_i32()));
                        core.push_inst(result);

                        let zero = core.new_int(0);
                        let one = core.new_int(1);
                        let store = core.new_value(InstType::Store(one, result));
                        core.push_inst(store);

                        let or2_block = core.new_block(None);
                        let or2_next_block = core.new_block(None);
                        let merge_block = core.new_block(None);
                        core.push_blocks(vec![or2_block, or2_next_block, merge_block]);

                        let or1 =
                            core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr1));
                        core.push_inst(or1);

                        let br = core.new_value(InstType::Branch(or1, merge_block, or2_block));

                        core.push_inst(br);

                        let or2 =
                            core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr2));
                        core.switch_block(or2_block);
                        core.push_inst(or2);

                        let br = core.new_value(InstType::Branch(or2, merge_block, or2_next_block));
                        core.push_inst(br);

                        let store = core.new_value(InstType::Store(zero, result));
                        core.switch_block(or2_next_block);
                        core.push_inst(store);

                        let jump = core.new_value(InstType::Jump(merge_block));
                        core.push_inst(jump);
                        
                        core.switch_block(merge_block);
                        let load = core.new_value(InstType::Load(result));
                        core.push_inst(load);

                        load
                    }
                }
            }
            Exp::Number(n) => core.new_int(n.node),

            Exp::LVar(name) => {
                let var = symtable.get(&name.node);
                let var = match var {
                    Some(Symbol::Var(var)) => {
                        let load = core.new_value(InstType::Load(*var));
                        core.push_inst(load);
                        load
                    }
                    Some(Symbol::Const(n)) => core.new_int(*n),

                    Some(Symbol::Func(_)) => unreachable!("cannot use a function as a variable"),

                    None => panic!("variable not found,start:{},end:{}", name.start, name.end),
                };
                var
            }
            Exp::Call(call) => {
                // TODO: Error handling
                let func = symtable.get(&call.node.ident.node).unwrap();
                let func = match func{
                    Symbol::Func(func) => *func,
                    _ => unreachable!("cannot call a variable"),
                };
                let args = call.node.args.into_iter().map(|arg| arg.generate_program(core, symtable)).collect();
                let call = core.new_value(InstType::Call(func, args));
                core.push_inst(call);
                call
            }
        }
    }
    pub fn calculate_const(
        self,
        symtable: &mut SymTable,
    ) -> i32 {
        match self {
            Exp::UnaryExp { op, exp } => {
                let exp = exp.calculate_const(symtable);
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
                let exp1 = exp1.calculate_const(symtable);
                let exp2 = exp2.calculate_const(symtable);
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
                    Some(Symbol::Func(_)) => unreachable!("cannot use a function as a variable"),
                    None => panic!("variable not found"),
                };
                var
            }
            Exp::Call(_) => unreachable!("cannot call a function in a const expression"),
        }
    }
}
