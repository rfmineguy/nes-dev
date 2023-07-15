ca65 src/main.s -v -o out/main.o --debug-info -l out/main.lst
ld65 out/main.o -v -o out/main.nes -t nes --dbgfile out/main.dbg -m out/main.map