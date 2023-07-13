.segment "HEADER"
.byte "NES", $1a
.byte $02
.byte $01
.byte %00000000
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
zp_world: .res 2
.segment "STARTUP"
reset:
    sei             ;disable interrupts
    cld             ;disable decimal mode (NES 6502 doesn't support it)
    ldx #$40
    stx $4017       ;disable sound irq
    ldx #$ff
    tsx             ;initialize the stack to 0xff
    inx             ;ff + 1 = 0
ZeroPPU:
    stx $2000       ;PPUCTRL = 0
    stx $2001       ;PPUMASK = 0
    stx $4010       ;...
    :
    bit $2002       ;test bit 7 (vblank ready)
    bpl :-          ;branch if not negative (negative if bit 7 is 1)
    txa             ;A=X
ClearMem:
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
    bne ClearMem   ;if x rolled over to 0 move on
:
    bit $2002       ;test bit 7 (vblank ready)
    bpl :-          ;branch if not negative (negative if bit 7 is 1)

    lda #$02        ;OAM DMA high address (setup the address where sprite data lives $0200)
    sta $4014       ;OAMDMA
    nop             ;this operation takes a moment

    lda #$3F        ;setup PPUADDR to 0x3f00 (see PPU memory map for why, its the BG Pallete)
    sta $2006       ;highbyte = 3f
    lda #$00
    sta $2006       ;lowbyte = 00f
    sta $2006
    sta $2006

    ldx #$00
LoadPalettes:           ; load pallete data into 0x03ff
    lda PalleteData, X  ; A = [PalleteData + X]
    sta $2007           ; PPUDATA (automatically increments the address we write A to)
    inx
    cpx #$20            ; there are 32 colors for the 2 palletes (BG and Sprite)
    bne LoadPalettes    ; loop as long as we haven't reached 20

    ; Initialize world to point to world data label in zeropage
    lda #<WorldData     ; get low byte of label, put into A
    sta zp_world        ; store A into the world variable first byte
    lda #>WorldData     ; get high byte of label, put into A
    sta zp_world+1      ; store A into the world variable second byte

    bit $2002           ; read from PPUSTATUS, reading this resets $2006 so its ready for its first byte again
    lda #$20             ; setup address in PPU for nametable
    sta $2006
    lda #$00
    sta $2006           ; $2000 -> PPUADDR ($2000 is nametable 1)

    ldx #$00            ; x counts the amount of times y has overflowed
    ldy #$00            ; y counts 0->255
LoadWorld:
    lda (zp_world), Y   ; load value at (world + y) address A = *(world + Y)
    sta $2007           ; PPUDATA
    iny
    cpx #$03
    bne :+              ; go to next un-named label
    cpy #$C0
    beq DoneLoadingWorld
:
    cpy #$00
    bne LoadWorld
    inx
    inc zp_world+1     ; increment high byte of world variable
    jmp LoadWorld
DoneLoadingWorld:
    ldx #$00
LoadSprites:
    lda SpriteData, X   ; get sprite byte
    sta $0200, X        ; set oam data
    inx
    cpx #$20            ; there are 32 bytes for sprite pallete
    bne LoadSprites

    cli                 ; enable interrupts
    lda #%10010000      ; enable nmi, set backround to use chr2 tiles ($1000)
    sta $2000
    lda #%00011110      ; enable sprites/background for left 8 pixels, and generally enable sprites and backgrounds
    sta $2001

    ldx #$00

loop:
    jmp loop

nmi:
    lda #$02            ; copy sprite data from $0200 -> PPU for display
    sta $4014           ; -
    rti

PalleteData:
; each pallete starts with $22 (our background color)
.byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0f ;background pallete
.byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17 ;sprite pallete

WorldData:
.incbin "world.bin"      ; the world is represented as just data

SpriteData:
.byte $10, $00, $00, $08 ; y-pos, tile-num, pallete-num, x-pos
.byte $10, $01, $00, $10
.byte $18, $02, $00, $08
.byte $18, $03, $00, $10
.byte $20, $04, $00, $08
.byte $20, $05, $00, $10
.byte $28, $06, $00, $08
.byte $28, $07, $00, $10

.segment "VECTORS"
.word nmi
.word reset

.segment "CHARS"
.incbin "hellomario.chr"