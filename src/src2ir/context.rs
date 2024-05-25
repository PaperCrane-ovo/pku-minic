use koopa::ir::BasicBlock;


pub struct LoopBound{
    pub entry: BasicBlock,
    pub exit: BasicBlock,
}
pub struct Context{
    pub loop_bound: Vec<LoopBound>,
}

impl Context{
    pub fn new() -> Self{
        Context{
            loop_bound: Vec::new(),
        }
    }
    pub fn push_loop_bound(&mut self,entry: BasicBlock,exit: BasicBlock){
        self.loop_bound.push(LoopBound{
            entry,
            exit,
        });
    }
    pub fn pop_loop_bound(&mut self){
        self.loop_bound.pop();
    }
    pub fn get_loop_bound(&self) -> Option<&LoopBound>{
        self.loop_bound.last()
    }
}