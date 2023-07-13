ca65 src/main.s -o out/main.o --debug-info
ld65 out/main.o -o out/main.nes -t nes --dbgfile out/main.dbg