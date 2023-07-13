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
    :
    bit $2002       ;test bit 7 (vblank ready)
    bpl -           ;branch if not negative (negative if bit 7 is 1)
    txa             ;A=X
.clearMem:
    sta $0000, X    ;$0000->$00FF   set to 0
    sta $0100, X    ;$0100->$01FF   set to 0
    ;sta $0200, X   ;$0200->$02FF
    sta $0300, X    ;$0300->$03FF   set to 0
    sta $0400, X    ;$0400->$04FF   set to 0
    sta $0500, X    ;$0500->$05FF   set to 0
    sta $0600, X    ;$0600->$06FF   set to 0
    sta $0700, X    ;$0700->$07FF   set to 0
    lda #$ff
    sta $0200, X    ;$0200->$02FF   set to FF (set aside for sprite data)
    lda #$00
    inx
    bne .clearMem   ;if x rolled over to 0 move on
loop:
    jmp loop

nmi:
    rti

.segment "VECTORS"
.word nmi reset

.segment "CHARS"
.incbin "hellomario.chr"
