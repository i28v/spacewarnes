playerXPos       = $000A
playerYPos       = $000B
collisionHandler = $000C
numberOfBulletsBeingFired = $000D
bulletsX = $000E
bulletsY = $000E+16
fireDelayHandler = $001F+8
isBulletBeingFired = $0020+8
collisonram = $700

maxbullets = $10

.segment "HEADER"

.byte "NES"
.byte $1A
.byte $02
.byte $01
.byte %00000000
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00

.segment "ZEROPAGE"



.segment "STARTUP"

Reset:
    sei 
    cld         
    ldx #$40
    stx $4017
    ldx #$FF
    txs 
    inx 
    stx $2000
    stx $2001 
    stx $4010
:
    bit $2002
    bpl :-
    txa 

clearmem:
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x 
    sta $0600, x
    sta $0700, x
    lda #$FF
    sta $0200, x
    lda #$00
    inx 
    bne clearmem 
:
    bit $2002
    bpl :-
    lda #$02
    sta $4014
    nop 
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx #$00

loadpalettes:
    lda PaletteData, x
    sta $2007
    inx 
    cpx #$20
    bne loadpalettes
    ldx #$00   

init:
    ldx #$00
initCollisionRam:
    lda CollisionMap, x
    sta collisonram, x
    inx 
    cpx #$78
    bne initCollisionRam
    lda #$5A
    sta playerXPos 
    lda #$AA
    sta playerYPos
    lda #$00
    sta numberOfBulletsBeingFired
    

enableNMI:
    cli 
    lda #%10010000
    sta $2000
    lda #%00011110
    sta $2001

Forever:
    jmp Forever

CheckCollide:
    txa 
    lsr 
    lsr 
    lsr 
    lsr 
    lsr 
    lsr 
    sta collisionHandler
    tya 
    lsr 
    lsr  
    lsr
    asl 
    asl 
    clc 
    adc collisionHandler
    tay 
    txa 
    lsr 
    lsr 
    lsr 
    and #%00000111
    tax 
    lda collisonram, y
    and BitMask, x
    rts 

update:
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    lda $4016
    and #%00000001 
    cmp #%00000001
    bne A_not_pressed
    lda fireDelayHandler
    cmp #$09
    bne A_not_pressed
    lda numberOfBulletsBeingFired
    cmp #maxbullets
    beq A_not_pressed
    inc numberOfBulletsBeingFired
    lda playerXPos 
    clc 
    adc #$04
    sta bulletsX, x 
    lda playerYPos 
    sec 
    sbc #$08
    sta bulletsY, x
A_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001 
    bne B_not_pressed
B_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Select_not_pressed
Select_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Start_not_pressed
Start_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Up_not_pressed
    dec playerYPos
    ldx playerXPos 
    ldy playerYPos 
    jsr CheckCollide
    beq :+
    inc playerYPos
:
Up_not_pressed:
    lda $4016
    and #%00000001 
    cmp #%00000001
    bne Down_not_pressed
    inc playerYPos
    ldx playerXPos 
    ldy playerYPos 
    jsr CheckCollide
    beq :+
    dec playerYPos
:
Down_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Left_not_pressed
    dec playerXPos 
    ldx playerXPos 
    ldy playerYPos 
    jsr CheckCollide
    beq :+
    inc playerXPos
:    
Left_not_pressed:
    lda $4016
    and #%00000001
    cmp #%00000001
    bne Right_not_pressed
    inc playerXPos
    ldx playerXPos 
    ldy playerYPos 
    jsr CheckCollide
    beq :+
    dec playerXPos
:
Right_not_pressed:
    inc fireDelayHandler
    lda fireDelayHandler
    cmp #$0A
    bne :+
    lda #$00
    sta fireDelayHandler
:
    lda numberOfBulletsBeingFired
    cmp #$00
    beq :+
    ldx #$00
move_bullets_loop:
    lda bulletsY, x
    sec 
    sbc #$02
    sta bulletsY, x
    inx
    cpx numberOfBulletsBeingFired
    bne move_bullets_loop
:
    lda numberOfBulletsBeingFired
    cmp #$00
    beq endCheckBulletPos
    ldx #$00
checkBulletPosLoop:
    lda bulletsY, x 
    cmp #$00
    bne :+
    ;dec numberOfBulletsBeingFired
:
    inx 
    cpx numberOfBulletsBeingFired
    bne checkBulletPosLoop
endCheckBulletPos:

    rts
    
draw:
    lda #$08
    clc 
    adc playerYPos
    sta $0200
    lda #$00
    sta $0201
    sta $0202
    lda #$08
    clc
    adc playerXPos
    sta $0203
    lda #$08
    clc 
    adc playerYPos 
    sta $0204
    lda #$00
    sta $0205
    lda #%01000000
    sta $0206
    lda #$10
    clc 
    adc playerXPos
    sta $0207
    lda #$10
    clc 
    adc playerYPos 
    sta $0208
    lda #$01
    sta $0209
    lda  #$00
    sta $020A
    lda #$08
    clc 
    adc playerXPos 
    sta $020B
    lda #$10
    clc 
    adc playerYPos 
    sta $020C
    lda #$01
    sta $020D
    lda #%01000000
    sta $020E
    lda #$10
    clc 
    adc playerXPos 
    sta $020F
    lda numberOfBulletsBeingFired
    cmp #$00
    beq clearBulletsFromPPURam
    ldx #$00
    ldy #$00
drawBulletsLoop:
    lda #$08
    clc 
    adc bulletsY, x
    sta $0210, y
    lda #$02
    sta $0211, y 
    lda #%00000001
    sta $0212, y 
    lda #$08
    clc 
    adc bulletsX, x 
    sta $0213, y 
    inx 
    cpx numberOfBulletsBeingFired
    beq :+
    tya 
    clc 
    adc #$04
    tay
    jmp drawBulletsLoop
:
    jmp endDraw
clearBulletsFromPPURam:
    ldx #$30
    lda #$00
:
    sta $0210, x 
    dex 
    cpx #$00
    bne :-
endDraw:
    rts 

NMI:
    lda #$00
    sta $2003
    lda #$02
    sta $4014
    jsr draw 
    jsr update
    rti 

PaletteData:
.byte $0F,$29,$1A,$0F,$22,$36,$17,$0F,$22,$30,$21,$0F,$22,$27,$17,$0F  
.byte $0F,$3D,$2D,$2C,$0F,$1C,$2B,$39,$0F,$06,$0F,$12,$22,$0F,$36,$17  

CollisionMap:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110
    .byte %00000000, %00000000, %00000000, %00000110 
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    
BitMask:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001
    
.segment "VECTORS"
    .word NMI
    .word Reset
.segment "CHARS"
    .incbin "../chrrom.chr"