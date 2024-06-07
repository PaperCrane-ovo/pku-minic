use koopa::ir::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MyType{
    Int,
    Void,
    Ptr(Box<MyType>),
    Array(Box<MyType>,i32),
}

impl MyType {
    pub fn into_ptr(self)->MyType{
        MyType::Ptr(Box::new(self))
    }
    pub fn into_array(self,size:i32)->MyType{
        MyType::Array(Box::new(self),size)
    }
    pub fn to_koopa(&self) -> Type {
        match self {
            MyType::Int => Type::get_i32(),
            MyType::Void => Type::get_unit(),
            MyType::Ptr(t) => Type::get_pointer(t.to_koopa()),
            MyType::Array(t, size) => Type::get_array(t.to_koopa(), *size as usize),
        }
    }
    pub fn to_index(&self)->MyIndexCast{
        match self{
            MyType::Ptr(t) => MyIndexCast::Ptr(*t.clone()),
            MyType::Array(t,_) => MyIndexCast::Array(*t.clone()),
            _ => MyIndexCast::None,
        }
    }
    pub fn cast(&self,other:&Self) -> MyTypeCast{
        if self == other {
            MyTypeCast::Noop
        }else {
            match (self,other) {
                (MyType::Array(t1, _), MyType::Ptr(t2)) => {
                    if t1 == t2 {
                        MyTypeCast::Array2Ptr
                    } else {
                        MyTypeCast::None
                    }
                },
                _ => MyTypeCast::None,
            }
        }
    }
}

pub enum MyIndexCast{
    Ptr(MyType),
    Array(MyType),
    None,
}
pub enum MyTypeCast{
    Array2Ptr,
    None,
    Noop,
}