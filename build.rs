use std::process::Command;


fn main() {
    lalrpop::process_root().unwrap();
    
    let autotest_dir = Command::new("which").arg("autotest").output().unwrap();

    panic!("{:?}", autotest_dir.stdout)

}
  