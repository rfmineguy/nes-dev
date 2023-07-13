ca65 main.s -o main.o --debug-info
ld65 main.o -o main.nes -t nes --dbgfile main.dbg
move main.o out
move main.nes out
move main.dbg out