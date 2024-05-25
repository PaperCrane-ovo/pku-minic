use std::fmt::Display;

use miette::Result;

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
pub struct i12{
    pub value: i32,
} 
impl i12{
    pub fn build(value: i32) -> Result<i12,()>{
        if value >= -2048 && value <= 2047 {
            Ok(i12{value})
        }else{
            Err(())
        }
    }
}
impl Display for i12{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

