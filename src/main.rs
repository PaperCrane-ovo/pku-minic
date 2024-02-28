use koopa::front::Driver;
use koopa::ir::Program;
use koopa::ir::ValueKind;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::{Result, Write};

mod ast;

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

    let mut output = String::new();
    match mode {
        Mode::KOOPA => {
            // 调用 lalrpop 生成的 parser 解析输入文件
            let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

            // 输出解析得到的 AST
            // println!("{:#?}", ast);
            // 输出 koopa 代码到output文件
            // 输出重定向

            ast.dump2koopa(&mut output);
        }
        Mode::RISCV => {
            // 调用 lalrpop 生成的 parser 解析输入文件
            let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

            // 输出解析得到的 AST
            // println!("{:#?}", ast);
            // 输出 koopa 代码到output文件
            // 输出重定向
            let mut result = String::new();

            ast.dump2koopa(&mut result);
            // 使用koopa建立内存形式的Koopa IR
            let driver = Driver::from(result);
            let program = driver.generate_program().unwrap();

            // 使用koopa建立RISCV汇编代码
            koopa2riscv(&program, &mut output);
        }
    }

    let mut outfile = std::fs::File::create(_output)?;
    outfile.write_all(output.as_bytes())?;


    Ok(())
}

fn koopa2riscv(program: &koopa::ir::Program, riscv_program: &mut String) {
    riscv_program.push_str("    .text\n");

    for &func in program.func_layout() {
        let func_data = program.func(func);
        riscv_program.push_str(&(format!("    .globl {}\n", &(func_data.name())[1..])));
        riscv_program.push_str(&(format!("{}:\n", &(func_data.name())[1..])));
        for (&_bb, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = func_data.dfg().value(inst);

                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        riscv_program.push_str(&(format!("    li a0, {:?}\n", int.value())));
                    }
                    ValueKind::Return(ret) => {

                        let ret_data = func_data.dfg().value(ret.value().unwrap());
                        match ret_data.kind() {
                            ValueKind::Integer(int) => {
                                riscv_program.push_str(&(format!("    li a0, {:?}\n", int.value())));
                            }
                            _ => {}
                        }
                        riscv_program.push_str("    ret\n");
                    }
                    _ => {}
                }
            }
        }
    }
}
