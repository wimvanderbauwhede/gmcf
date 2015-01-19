Assuming ghci is started in $GANNET_DIR/t/GannetVMTests

:cd ../../Compiler
:set -cpp -optP-DWORDSZ=64
:load Main.hs
:set -fbreak-on-exception
:main -p -v -Y ../t/GannetVMTests/hello.yml ../t/GannetVMTests/test_let.td
:trace
:hist



