use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::{Result, Write};
use crate::ir2riscv::riscvgen::GenerateAsm;

mod ast;
mod ir2riscv;
mod src2ir;
mod utils;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

enum Mode {
    KOOPA,
    RISCV,
}

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let _mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let _output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 模式
    let mode = match _mode.as_str() {
        "-koopa" => Mode::KOOPA,
        "-riscv" => Mode::RISCV,
        _ => panic!("Invalid mode"),
    };

    match mode {
        Mode::KOOPA => {
            // 调用 lalrpop 生成的 parser 解析输入文件
            let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

            // 输出解析得到的 AST
            println!("{:#?}", ast);
            // 输出 koopa 代码到output文件
            // 输出重定向
            let koopa_ir = ast.build_ir();
            let outfile = std::fs::File::create(_output)?;
            let mut generator = KoopaGenerator::new(outfile);
            generator.generate_on(&koopa_ir)?;
        
        }
        Mode::RISCV => {
            let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
            let ir_program = ast.build_ir();


            let mut outfile = std::fs::File::create(_output)?;
            let mut asm = String::new();
            ir_program.generate(&mut asm);
            outfile.write_all(asm.as_bytes())?;

        }
    }

    Ok(())
}
