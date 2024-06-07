use std::mem::take;

use koopa::ir::{
    builder::{GlobalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type, Value,
};

use crate::ast::{self, *};

use super::{
    bbexaminer::BBExaminer,
    context::Context,
    core::{Core, InstType},
    error::MyError,
    mytype::{MyIndexCast, MyType, MyTypeCast},
    symtable::{SymTable, Symbol},
};

type Result<T> = std::result::Result<T, MyError>;

// type Result<T> = std::result::Result<T, MyError>;

impl CompUnit {
    pub fn build_ir(self) -> Result<Program> {
        let mut symtable = SymTable::new();
        let mut program = Program::new();
        symtable.push();
        self.generate_program(&mut program, &mut symtable)?;
        symtable.pop();
        Ok(program)
    }
    fn generate_program(self, program: &mut Program, symtable: &mut SymTable) -> Result<()> {
        self.decl_func(program, symtable, "@getint", vec![], MyType::Int);
        self.decl_func(program, symtable, "@getch", vec![], MyType::Int);
        self.decl_func(
            program,
            symtable,
            "@getarray",
            vec![MyType::Int.into_ptr()],
            MyType::Int,
        );
        self.decl_func(
            program,
            symtable,
            "@putint",
            vec![MyType::Int],
            MyType::Void,
        );
        self.decl_func(program, symtable, "@putch", vec![MyType::Int], MyType::Void);
        self.decl_func(
            program,
            symtable,
            "@putarray",
            vec![MyType::Int, MyType::Int.into_ptr()],
            MyType::Void,
        );
        self.decl_func(program, symtable, "@starttime", vec![], MyType::Void);
        self.decl_func(program, symtable, "@stoptime", vec![], MyType::Void);

        for item in self.global_items {
            match item {
                GlobalItem::Decl(decl) => decl.generate_program_global(program, symtable)?,
                GlobalItem::FuncDef(func_def) => func_def.generate_program(program, symtable)?,
            }
        }
        Ok(())
    }
    fn decl_func(
        &self,
        program: &mut Program,
        symtable: &mut SymTable,
        name: &str,
        params_ty: Vec<MyType>,
        ret_ty: MyType,
    ) {
        let func = program.new_func(FunctionData::new_decl(
            name.to_string(),
            params_ty.iter().map(MyType::to_koopa).collect(),
            ret_ty.to_koopa(),
        ));

        symtable.insert(name[1..].to_string(), Symbol::Func(func, params_ty, ret_ty));
    }
}

impl FuncDef {
    pub fn generate_program(self, program: &mut Program, symtable: &mut SymTable) -> Result<()> {
        let params = self
            .params
            .iter()
            .map(|param| param.node.generate_program(&symtable))
            .collect();

        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.node),
            params,
            self.func_type.node.build_ir_primitive().to_koopa(),
        ));


        let params_ty = self
            .params
            .clone()
            .into_iter()
            .map(|p| p.node.get_type(symtable))
            .collect::<Result<Vec<_>>>()?;

        // add context manager
        let mut context = Context::new();

        let mut func_data = program.func_mut(func);

        let return_type = self.func_type.node;
        let ret_type = return_type.build_ir_primitive();

        let mut core = Core::new(&mut func_data, &mut context, return_type);

        // insert function into symbol table

        symtable.insert(
            self.ident.node.clone(),
            Symbol::Func(func, params_ty.clone(), ret_type),
        );

        symtable.push();


        // insert parameters into symbol table and koopa ir

        for (i, param) in self.params.iter().enumerate() {
            let ty = &params_ty[i];
            
            let var = core.new_value(InstType::Alloc(ty.to_koopa()));
            let dfg = core.dfg_mut();
            let name = format!("%{}", param.node.ident.node);
            dfg.set_value_name(var, Some(name));

            symtable.insert(param.node.ident.node.clone(), Symbol::Var(var, ty.clone(), false));
            let cur_param = core.func_data().params()[i];
            let store = core.new_value(InstType::Store(cur_param, var));
            core.push_insts(vec![var, store]);
        }



        self.block.node.generate_program(&mut core, symtable)?;
        symtable.pop();

        let bb_examiner = BBExaminer::new();
        bb_examiner.examine_ret(&mut core);
        bb_examiner.examine_bb_name(&mut core);
        BBExaminer::clean_all_extra_inst(&mut core);

        Ok(())
    }
}

impl FuncParam {
    pub fn get_type(self, symtable: &SymTable) -> Result<MyType> {
        Ok(if let Some(indices) = self.indices {
            self.ty.node.build_ir(indices, symtable)?.0.into_ptr()
        } else {
            self.ty.node.build_ir_primitive()
        })
    }
    pub fn generate_program(&self,symtable: &SymTable) -> (Option<String>, Type) {
        let str = Some(format!("@{}", self.ident.node));
        let ty = <ast::FuncParam as Clone>::clone(&self).get_type(symtable).unwrap().to_koopa();
        (str, ty)
    }
}

impl Block {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        dbg!("enter a block");
        symtable.push();

        for item in self.items {
            item.generate_program(core, symtable)?;
        }

        symtable.pop();
        dbg!("exit a block");
        Ok(())
    }
}

impl BlockItem {
    pub fn generate_program(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        match self {
            BlockItem::Decl { decl } => {
                decl.generate_program_local(core, symtable)?;
            }
            BlockItem::Stmt { stmt } => {
                stmt.node.generate_program_local(core, symtable)?;
            }
        }
        Ok(())
    }
}

impl Decl {
    pub fn generate_program_local(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        match self {
            Decl::Var(var_decl) => {
                var_decl.node.generate_program_local(core, symtable)?;
            }
            Decl::Const(const_decl) => {
                const_decl.node.generate_program_local(core, symtable)?;
            }
        }
        Ok(())
    }
    pub fn generate_program_global(
        self,
        program: &mut Program,
        symtable: &mut SymTable,
    ) -> Result<()> {
        match self {
            Decl::Var(var_decl) => {
                var_decl.node.generate_program_global(program, symtable)?;
            }
            Decl::Const(const_decl) => {
                const_decl.node.generate_program_global(program, symtable)?;
            }
        }
        Ok(())
    }
}

impl BType {
    pub fn build_ir_primitive(&self) -> MyType {
        match self {
            BType::Int => MyType::Int,
            BType::Void => MyType::Void,
        }
    }
    pub fn build_ir(
        &self,
        indices: Vec<ConstExp>,
        symtable: &SymTable,
    ) -> Result<(MyType, Vec<i32>)> {
        let indices = indices
            .into_iter()
            .map(|index| index.calculate_const(symtable))
            .collect::<Result<Vec<_>>>()?;
        let mut ty = self.build_ir_primitive();
        for index in indices.iter().copied().rev() {
            ty = ty.into_array(index);
        }
        Ok((ty, indices))
    }
}

impl ConstDecl {
    pub fn generate_program_local(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        for def in self.defs {
            def.generate_program_local(&self.ty.node, symtable, core)?;
        }
        Ok(())
    }

    pub fn generate_program_global(
        self,
        program: &mut Program,
        symtable: &mut SymTable,
    ) -> Result<()> {
        for def in self.defs {
            def.generate_program_global(&self.ty.node, symtable, program)?;
        }
        Ok(())
    }
}

impl ConstDef {
    pub fn generate_program_local(
        self,
        _ty: &BType,
        symtable: &mut SymTable,
        core: &mut Core,
    ) -> Result<()> {
        // let val = self.exp.calculate_const(symtable);
        // symtable.insert(self.ident.node, Symbol::Const(val));
        if self.indices.is_empty() {
            // constant variable
            self.calculate_const(symtable)
        } else {
            // array
            let (ty, indices) = _ty.build_ir(self.indices, symtable)?;
            let array = core.new_value(InstType::Alloc(ty.to_koopa()));
            core.push_inst(array);
            self.init
                .canonicalize(&indices)?
                .generate_program_local(array, symtable, core, true)?;

            symtable.insert(self.ident.node, Symbol::Var(array, ty, true));
            Ok(())
        }
    }
    pub fn generate_program_global(
        self,
        _ty: &BType,
        symtable: &mut SymTable,
        program: &mut Program,
    ) -> Result<()> {
        if self.indices.is_empty() {
            // constant variable
            self.calculate_const(symtable)
        } else {
            // array
            let (ty, indices) = _ty.build_ir(self.indices, symtable)?;
            let init_list = self
                .init
                .canonicalize(&indices)?
                .generate_program_global(symtable, program)?;

            let array = program.new_value().global_alloc(init_list);

            symtable.insert(self.ident.node, Symbol::Var(array, ty, true));
            Ok(())
        }
    }
    fn calculate_const(self, symtable: &mut SymTable) -> Result<()> {
        let exp = self.init.canonicalize(&[])?.calculate_const(symtable)?;
        symtable.insert(self.ident.node, Symbol::Const(exp));
        Ok(())
    }
}

impl ConstExp {
    pub fn calculate_const(self, symtable: &SymTable) -> Result<i32> {
        self.exp.calculate_const(symtable)
    }
}

impl VarDecl {
    pub fn generate_program_local(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        for def in self.defs {
            def.node
                .generate_program_local(&self.ty.node, symtable, core)?;
        }
        Ok(())
    }

    pub fn generate_program_global(
        self,
        program: &mut Program,
        symtable: &mut SymTable,
    ) -> Result<()> {
        for def in self.defs {
            def.node
                .generate_program_global(&self.ty.node, symtable, program)?;
        }
        Ok(())
    }
}

impl VarDef {
    pub fn generate_program_local(
        self,
        _ty: &BType,
        symtable: &mut SymTable,
        core: &mut Core,
    ) -> Result<()> {
        let (ty, indices) = _ty.build_ir(self.indices, symtable)?;
        let var = core.new_value(InstType::Alloc(ty.to_koopa()));
        core.dfg_mut()
            .set_value_name(var, Some(format!("@{}", self.ident.node)));
        core.push_inst(var);

        if let Some(init) = self.init {
            init.canonicalize(&indices)?
                .generate_program_local(var, symtable, core, false)?;
        }

        symtable.insert(self.ident.node, Symbol::Var(var, ty, false));
        Ok(())
    }
    pub fn generate_program_global(
        self,
        _ty: &BType,
        symtable: &mut SymTable,
        program: &mut Program,
    ) -> Result<()> {
        let (ty, indices) = _ty.build_ir(self.indices, symtable)?;

        let init = if let Some(init) = self.init {
            init.canonicalize(&indices)?
                .generate_program_global(symtable, program)?
        } else {
            program.new_value().zero_init(ty.to_koopa())
        };

        let global_var = program.new_value().global_alloc(init);
        program.set_value_name(global_var, Some(format!("@{}", self.ident.node)));

        symtable.insert(self.ident.node, Symbol::Var(global_var, ty, false));
        Ok(())
    }
}

impl Span<InitVal> {
    pub fn canonicalize(self, indices: &[i32]) -> Result<Span<InitVal>> {
        if indices.is_empty() {
            // not array
            return match self.node {
                InitVal::Exp(_) => Ok(self),
                InitVal::InitList(inits) => {
                    if inits.len() != 1 {
                        return Err("not array but length is 1".into());
                    }
                    let init = inits.into_iter().next().unwrap();
                    match init.node {
                        InitVal::Exp(_) => Ok(init),
                        _ => Err("compile in `canonicalize`".into()),
                    }
                }
            };
        }

        // array
        let inits = match self.node {
            InitVal::Exp(_) => return Err("array but not array".into()),
            InitVal::InitList(inits) => inits,
        };
        let mut init_vals = vec![vec![]; indices.len()];

        for init in inits {
            let (base, init) = match init.node {
                InitVal::Exp(_) => (indices.len() - 1, init),
                InitVal::InitList(_) => {
                    let base = init_vals
                        .iter()
                        .rposition(|init| !init.is_empty())
                        .unwrap_or(0);
                    let sub_indices = &indices[base + 1..];
                    let init = init.canonicalize(sub_indices)?;
                    (base, init)
                }
            };
            let mut carry = Some(init);
            for i in (0..=base).rev() {
                if let Some(carry) = carry.take() {
                    init_vals[i].push(carry);
                } else {
                    break;
                }
                if init_vals[i].len() >= indices[i] as usize {
                    carry = Some(InitVal::InitList(take(&mut init_vals[i])).into_span(0, 0));
                }
            }
            if let Some(carry) = carry {
                return Ok(carry);
            }
        }

        let mut carry = None;
        for i in (0..indices.len()).rev() {
            if let Some(carry) = carry.take() {
                init_vals[i].push(carry);
            }
            init_vals[i].resize(indices[i] as usize, Self::to_zero(&indices[i + 1..]));
            carry = Some(InitVal::InitList(take(&mut init_vals[i])).into_span(0, 0));
        }
        Ok(carry.unwrap())
    }

    fn to_zero(indices: &[i32]) -> Span<InitVal> {
        match indices {
            [] => InitVal::Exp(Exp::Number(0.into_span(0, 0))).into_span(0, 0),
            [index, indices @ ..] => InitVal::InitList(
                (0..*index)
                    .map(|_| Self::to_zero(indices))
                    .collect::<Vec<_>>(),
            )
            .into_span(0, 0),
        }
    }
    pub fn generate_program_local(
        self,
        ptr: Value,
        symtable: &SymTable,
        core: &mut Core,
        is_const: bool,
    ) -> Result<()> {
        match self.node {
            InitVal::Exp(exp) => {
                let val = if is_const {
                    let val = exp.calculate_const(symtable)?;
                    core.new_int(val)
                } else {
                    exp.generate_program_local(core, symtable, Some(MyType::Int))?
                };
                let store = core.new_value(InstType::Store(val, ptr));
                core.push_inst(store);
            }
            InitVal::InitList(init_list) => {
                for (i, init) in init_list.into_iter().enumerate() {
                    let index = core.new_int(i as i32);
                    let ptr = core.new_value(InstType::GetElementPtr(ptr, index));
                    core.push_inst(ptr);
                    init.generate_program_local(ptr, symtable, core, is_const)?;
                }
            }
        }
        Ok(())
    }
    pub fn generate_program_global(
        self,
        symtable: &SymTable,
        program: &mut Program,
    ) -> Result<Value> {
        match self.node {
            InitVal::Exp(exp) => {
                let val = exp.calculate_const(symtable)?;
                Ok(program.new_value().integer(val))
            }
            InitVal::InitList(init_list) => {
                let mut init_vals = vec![];
                for init in init_list {
                    let init = init.generate_program_global(symtable, program)?;
                    init_vals.push(init);
                }
                Ok(program.new_value().aggregate(init_vals))
            }
        }
    }
    pub fn calculate_const(self, symtable: &SymTable) -> Result<i32> {
        match self.node {
            InitVal::Exp(exp) => exp.calculate_const(symtable),
            InitVal::InitList(_) => Err("compile in `calculate_const`".into()),
        }
    }
}

impl Stmt {
    pub fn generate_program_local(self, core: &mut Core, symtable: &mut SymTable) -> Result<()> {
        match self {
            Stmt::Return(r) => {
                let mut expr = match r {
                    Some(r) => Some(r.generate_program_local(core, symtable, Some(MyType::Int))?),
                    None => None,
                };
                if expr.is_none() && core.return_type != BType::Void {
                    expr = Some(core.new_int(0));
                }
                if expr.is_some() && core.return_type == BType::Void {
                    expr = None;
                }
                let ret = core.new_value(InstType::Ret(expr));
                core.push_inst(ret);
            }
            Stmt::Assign { lval, exp } => {
                // let var = symtable.get(&ident.node);
                // let var = match var {
                //     Some(Symbol::Var(var)) => *var,
                //     Some(Symbol::Const(_)) => panic!("cannot assign to a const"),
                //     Some(Symbol::Func(_)) => panic!("cannot assign to a function"),
                //     None => panic!("variable not found"),
                // };

                let (var, ty) = lval.generate_lval(symtable, core)?;

                let exp = exp.generate_program_local(core, symtable, Some(ty))?;
                let store = core.new_value(InstType::Store(exp, var));
                core.push_inst(store);
            }
            Stmt::Block { block: _block } => {
                _block.node.generate_program(core, symtable)?;
            }
            Stmt::Exp { exp } => {
                if let Some(exp) = exp {
                    exp.generate_program_local(core, symtable, None)?;
                }
            }
            Stmt::If { cond, then, els } => {
                // 处理if的生成
                // 为then和else分别创建两个block
                let cond = cond.generate_program_local(core, symtable, Some(MyType::Int))?;
                let then_block = core.new_block(None);
                let else_block = core.new_block(None);
                let merge_block = core.new_block(None);
                core.push_blocks(vec![then_block, else_block, merge_block]);
                // if cond
                let br = core.new_value(InstType::Branch(cond, then_block, else_block));
                core.push_inst(br);

                // then
                core.switch_block(then_block);
                then.node.generate_program_local(core, symtable)?;

                let bb_examiner = BBExaminer::new();
                if !bb_examiner.is_terminated(core, core.block) {
                    let br = core.new_value(InstType::Jump(merge_block));
                    core.push_inst(br);
                }

                // else
                core.switch_block(else_block);

                if let Some(els) = els {
                    els.node.generate_program_local(core, symtable)?;
                }

                if !bb_examiner.is_terminated(core, core.block) {
                    let br = core.new_value(InstType::Jump(merge_block));
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
                let cond = cond.generate_program_local(core, symtable, Some(MyType::Int))?;
                let br = core.new_value(InstType::Branch(cond, body_block, next_block));

                core.push_inst(br);
                core.switch_block(body_block);
                body.node.generate_program_local(core, symtable)?;
                // TODO : return in while
                let jump_to_cond = core.new_value(InstType::Jump(cond_block));

                let bb_examiner = BBExaminer::new();
                if !bb_examiner.is_terminated(core, core.block) {
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
        Ok(())
    }
}

impl Exp {
    pub fn generate_program_local(
        self,
        core: &mut Core,
        symtable: &SymTable,
        ty: Option<MyType>,
    ) -> Result<Value> {
        match self {
            // 一元运算表达式
            Exp::UnaryExp { op, exp } => {
                let expr = exp.generate_program_local(core, symtable, Some(MyType::Int))?;
                match op.node {
                    MyUnaryOp::Neg => {
                        // Neg转为0减，即二元运算
                        let zero = core.new_int(0);
                        let neg = core.new_value(InstType::Binary(BinaryOp::Sub, zero, expr));
                        core.push_inst(neg);
                        Ok(neg)
                    }
                    MyUnaryOp::Not => {
                        // 逻辑取反，和0比较相等
                        let zero = core.new_int(0);
                        let not = core.new_value(InstType::Binary(BinaryOp::Eq, zero, expr));
                        core.push_inst(not);
                        Ok(not)
                    }
                    MyUnaryOp::Pos => Ok(expr),
                }
            }
            Exp::BinaryExp { op, exp1, exp2 } => {
                match op.node {
                    // 逻辑运算需要先转为0和1然后再进行运算
                    MyBinaryOp::LAnd => {
                        // 实现短路求值
                        let result = core.new_value(InstType::Alloc(Type::get_i32()));
                        core.push_inst(result);
                        let zero = core.new_int(0);
                        let one = core.new_int(1);

                        let store = core.new_value(InstType::Store(zero, result));
                        core.push_inst(store);

                        let expr1 =
                            exp1.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let and1 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr1));
                        core.push_inst(and1);
                        let and2_block = core.new_block(None);
                        let and2_next_block = core.new_block(None);
                        let merge_block = core.new_block(None);
                        core.push_blocks(vec![and2_block, and2_next_block, merge_block]);

                        let br = core.new_value(InstType::Branch(and1, and2_block, merge_block));
                        core.push_inst(br);

                        core.switch_block(and2_block);
                        let expr2 =
                            exp2.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let and2 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr2));
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
                        Ok(load)
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

                        let expr1 =
                            exp1.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let or1 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr1));
                        core.push_inst(or1);

                        let br = core.new_value(InstType::Branch(or1, merge_block, or2_block));

                        core.push_inst(br);

                        core.switch_block(or2_block);
                        let expr2 =
                            exp2.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let or2 = core.new_value(InstType::Binary(BinaryOp::NotEq, zero, expr2));
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

                        Ok(load)
                    }
                    _ => {
                        let expr1 =
                            exp1.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let expr2 =
                            exp2.generate_program_local(core, symtable, Some(MyType::Int))?;
                        let bin_op = match op.node {
                            MyBinaryOp::Add => BinaryOp::Add,
                            MyBinaryOp::Sub => BinaryOp::Sub,
                            MyBinaryOp::Mul => BinaryOp::Mul,
                            MyBinaryOp::Div => BinaryOp::Div,
                            MyBinaryOp::Mod => BinaryOp::Mod,
                            MyBinaryOp::Eq => BinaryOp::Eq,
                            MyBinaryOp::Ne => BinaryOp::NotEq,
                            MyBinaryOp::Lt => BinaryOp::Lt,
                            MyBinaryOp::Gt => BinaryOp::Gt,
                            MyBinaryOp::Le => BinaryOp::Le,
                            MyBinaryOp::Ge => BinaryOp::Ge,
                            _ => unreachable!(),
                        };
                        let bin = core.new_value(InstType::Binary(bin_op, expr1, expr2));
                        core.push_inst(bin);
                        Ok(bin)
                    }
                }
            }
            Exp::Number(n) => Ok(core.new_int(n.node)),

            Exp::LVal(lval) => lval.generate_rval(symtable, core, ty),

            Exp::Call(call_exp) => {
                let func = symtable
                    .get(&call_exp.node.ident.node)
                    .ok_or::<MyError>("function not found".into())?;
                let (&func, param_ty, _ret_ty) = match func {
                    Symbol::Func(func, param_ty, ret_ty) => {
                        (func, param_ty.clone(), ret_ty.clone())
                    }
                    _ => Err(MyError::new("not a function"))?,
                };
                let args = call_exp
                    .node
                    .args
                    .into_iter()
                    .zip(param_ty.into_iter())
                    .map(|(arg, ty)| arg.generate_program_local(core, symtable, Some(ty)))
                    .collect::<Result<Vec<_>>>()?;
                let call = core.new_value(InstType::Call(func, args));
                core.push_inst(call);

                Ok(call)
            }
        }
    }
    pub fn calculate_const(self, symtable: &SymTable) -> Result<i32> {
        Ok(match self {
            Exp::UnaryExp { op, exp } => {
                let exp = exp.calculate_const(symtable)?;
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
                let exp1 = exp1.calculate_const(symtable)?;
                let exp2 = exp2.calculate_const(symtable)?;
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
            Exp::LVal(name) => {
                let var = symtable
                    .get(&name.node.ident.node)
                    .ok_or::<MyError>("not found variable".into())?;
                let var = match var {
                    Symbol::Var(_, _, _) => panic!("cannot use a variable as a const"),
                    Symbol::Const(n) => *n,
                    Symbol::Func(_, _, _) => unreachable!("cannot use a function as a variable"),
                };
                var
            }
            Exp::Call(_) => unreachable!("cannot call a function in a const expression"),
        })
    }
}

impl Span<LVal> {
    pub fn generate_lval(self, symtable: &SymTable, core: &mut Core) -> Result<(Value, MyType)> {
        let var = symtable
            .get(&self.node.ident.node)
            .ok_or::<MyError>("variable not found".into())?;
        let (var, ty) = match var {
            Symbol::Var(var, ty, false) => (*var, ty.clone()),
            _ => Err(MyError::new("lval get error"))?,
        };
        let (var, ty) = self.index(var, ty, symtable, core)?;
        Ok((var, ty))
    }
    pub fn generate_rval(
        self,
        symtable: &SymTable,
        core: &mut Core,
        ty: Option<MyType>,
    ) -> Result<Value> {
        let var = symtable
            .get(&self.node.ident.node)
            .ok_or(MyError::new("variable not found"))?;
        match var {
            Symbol::Const(i) => Ok(core.new_int(*i)),
            Symbol::Var(var, _ty, _) => {
                let (var, _ty) = self.index(*var, _ty.clone(), symtable, core)?;
                let cast = ty.as_ref().unwrap_or(&_ty);
                match _ty.cast(cast) {
                    MyTypeCast::Array2Ptr => {
                        let index = core.new_int(0);
                        let ptr = core.new_value(InstType::GetElementPtr(var, index));
                        core.push_inst(ptr);
                        Ok(ptr)
                    }
                    MyTypeCast::Noop => {
                        let load = core.new_value(InstType::Load(var));
                        core.push_inst(load);
                        Ok(load)
                    }
                    _ => Err(MyError::new("type error"))?,
                }
            }
            Symbol::Func(_, _, _) => Err(MyError::new("function cannot be used as a variable"))?,
        }
    }
    pub fn index(
        self,
        mut var: Value,
        mut ty: MyType,
        symtable: &SymTable,
        core: &mut Core,
    ) -> Result<(Value, MyType)> {
        for index in self.node.indices {
            let index = index.generate_program_local(core, symtable, Some(MyType::Int))?;
            let (ptr, new_ty) = match ty.to_index() {
                MyIndexCast::Array(ty) => {
                    let ptr = core.new_value(InstType::GetElementPtr(var, index));
                    (ptr, ty)
                }
                MyIndexCast::Ptr(ty) => {
                    let ptr = core.new_value(InstType::Load(var));
                    core.push_inst(ptr);
                    let ptr = core.new_value(InstType::GetPtr(ptr, index));
                    (ptr, ty)
                }
                _ => Err(MyError::new("type error"))?,
            };
            var = ptr;
            ty = new_ty;
            core.push_inst(ptr);
        }
        Ok((var, ty))
    }
}
