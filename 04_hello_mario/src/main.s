.segment "HEADER"
.byte "NES", $1a
.byte $02
.byte $01
.byte %00000000
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
.segment "STARTUP"
reset:
    sei             ;disable interrupts
    cld             ;disable decimal mode (NES 6502 doesn't support it)
    ldx #$40
    stx $4017       ;disable sound irq
    stx #$ff
    tsx             ;initialize the stack to 0xff
    inx             ;ff + 1 = 0
.zeroPPU:
    stx $2000       ;PPUCTRL = 0
    stx $2001       ;PPUMASK = 0
    stx $4010       ;...

loop:
    jmp loop

nmi:
    rti

.segment "VECTORS"
.word nmi reset

.segment "CHARS"
.incbin "hellomario.chr"
