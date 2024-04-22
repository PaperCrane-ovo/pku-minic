use std::collections::HashMap;
use once_cell::sync::Lazy; // Add this import

use koopa::ir::Value;

pub static mut CUR_REG: u32 = 0;
pub const temp_reg: [&str;15] = ["t0", "t1", "t2", "t3", "t4", "t5", "t6","a0","a1","a2","a3","a4","a5","a6","a7"];
pub static mut reg_cache: Lazy<HashMap<Value, String>> = Lazy::new(|| HashMap::new()); // Replace the line with this
