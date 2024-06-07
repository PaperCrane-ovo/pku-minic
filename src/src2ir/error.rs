use std::{error::Error, fmt::Display};

#[derive(Debug,)]
pub struct MyError{
    msg:String,
    start_pos:usize,
    end_pos:usize,
}

impl Display for MyError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)->std::fmt::Result{
        write!(f,"{} at {}..{}",self.msg,self.start_pos,self.end_pos)
    }
}

impl Error for MyError{}

impl From<&str> for MyError{
    fn from(value: &str) -> Self {
        MyError{
            msg:value.to_string(),
            start_pos:0,
            end_pos:0,
        }
    }
}

impl From<(&str,usize,usize)> for MyError{
    fn from(value: (&str,usize,usize)) -> Self {
        MyError{
            msg:value.0.to_string(),
            start_pos:value.1,
            end_pos:value.2,
        }
    }
}

impl MyError{
    pub fn new(msg:&str) -> Self{
        MyError{
            msg:msg.to_string(),
            start_pos:0,
            end_pos:0,
        }
    }
}
