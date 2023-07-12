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

nmi:

.segment "VECTORS"
.word nmi
.word reset
.word 0
.segment "CHARS"
.incbin "hellomario.chr"
