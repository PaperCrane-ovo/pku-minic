set shell := ["powershell.exe", "-c"]

binary := "pku-minic" + if os_family() == "windows" { ".exe" } else { "" }
binary_path := "target/debug/" + binary

sysy := "hello.c"
ir := "output.koopa"
riscv := "output.riscv"

buildtest :build test

test: testkoopa testriscv



testkoopa:
    @echo "Testing koopa..."
    {{binary_path}} -koopa {{sysy}} -o {{ir}}

testriscv:
    @echo "Testing riscv..."
    {{binary_path}} -riscv {{sysy}} -o {{riscv}}

build:
    @echo "Building..."
    cargo build

clean:
    @echo "Cleaning..."
    cargo clean

buildkoopa: build testkoopa

buildriscv: build testriscv

docker:
    docker run -it --rm -v E:/courses/pku-minic:/root/compiler maxxing/compiler-dev


autotest-riscv:
    docker run -it --rm -v E:/courses/pku-minic:/root/compiler maxxing/compiler-dev autotest -riscv -s lv8 /root/compiler


autotest-koopa:
    docker run -it --rm -v E:/courses/pku-minic:/root/compiler maxxing/compiler-dev autotest -koopa -s lv8 /root/compiler