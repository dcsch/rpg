;
; rpg.asm
;
; An Atari 2600 VCS videogame.
;
; Copyright (C) David Schweinsberg 1999, 2006
;------------------------------------------------------------------------------

	processor 6502

	include VCS.H

;
; Timing constants
; ----------------

;	PAL             | NTSC
;	----------------+-----------------
;	 48 VBLANK      |  40 VBLANK
;	228 KERNEL      | 192 KERNEL
;	 36 OVERSCAN    |  30 OVERSCAN
;	---             | ---
;	312 lines/frame | 262 lines/frame

; -------------------
; NTSC

VBLANK_TIME	= $2b
KERNEL_LINES	= $c0
PF_FLOOR_LINES	= $a0
PF_STATUS_LINES	= $20
OVERSCAN_TIME	= $1a

; -------------------


;
; Constants
; ---------

MOVING		= $81		; Last joystick movement

X_POS		= $8a		; X position of player
Y_POS		= $8b		; Y position of player

P0_DATA_PTR	= $8d		; player's graphics data (address; 2 bytes)
P1_1_DATA_PTR	= $8f		; player's graphics data (address; 2 bytes)
P1_2_DATA_PTR	= $92		; player's graphics data (address; 2 bytes) ERROR shift down 1 byte
P1_3_DATA_PTR	= $94		; player's graphics data (address; 2 bytes)

;PF_COLOUR	= $84
;PF_BG_COLOUR	= $8e		; playfield background colour
;PF_TB_COLOUR	= $00		; playfield top border colour
;PF_SB_COLOUR	= $04		; playfield status bar colour
;P0_COLOUR	= $00		; player 0 colour
;ENEMY_COLOUR	= $1c

DECODED_PF0	= $a0		; PF0 values decoded from landscape
DECODED_PF1	= $a9		; PF1 values decoded from landscape
DECODED_PF2	= $b2		; PF2 values decoded from landscape
DECODED_COLUPF	= $bb
DECODED_COLUBK	= $c4
JUMP_POINT	= $cd

;
; Start of the ROM code and data
;

	org	$f000

ENTER
	cld
	sei
	ldx	#$ff			; Set the stack
	txs
	inx
	txa

clear
	sta	VSYNC,x			; Initialise zero page to zeros
	inx
	bne	clear

initplay	subroutine

	lda	#$00			; Set joystick ports for input
	sta	SWCHA

	;
	; Set MSB for P0/P1 data
	;
	lda	#>PLAYER_DATA
	sta	P0_DATA_PTR+1
	lda	#>STATUS_DATA
	sta	P1_1_DATA_PTR+1
	sta	P1_2_DATA_PTR+1
	sta	P1_3_DATA_PTR+1

	;
	; Set the initial location of the player in the world
	;
;	lda	#$0B
	lda	#$00
	sta	X_POS
;	lda	#$14
	lda	#$00
	sta	Y_POS

;==============================================================================
; MAIN LOOP
; ---------
; Composed of three stages:
;	* VBLANK
;		- Sync
;		- Read joystick
;		- Load playfield RAM
;	* KERNEL
;		- Render display
;	* OVERSCAN
;
; Each KERNEL line consists of:
;	* 68 color clocks of blank (22.6 cycles)
;	* 160 color clocks of display (53.3 cycles)
;==============================================================================

;
; VBLANK
;
start_vblank	subroutine
	ldy	#$02
	lda	#$00

	sty	VBLANK			; turn on VBLANK

	sty	WSYNC			; 
	sty	VSYNC			; turn on VSYNC

	sty	WSYNC			; wait for three scan lines
	sty	WSYNC
	sty	WSYNC

	sta	VSYNC			; turn off VSYNC

	;
	; set timer for VBLANK_TIME
	;
	lda	#VBLANK_TIME
	sta	TIM64T

	;
	; Read joystick
	;  D7 - right P0
	;  D6 - left  P0
	;  D5 - down  P0
	;  D4 - up    P0
	;  D3 - right P1
	;  D2 - left  P1
	;  D1 - down  P1
	;  D0 - up    P1
	;
joystick	subroutine

	lda	SWCHA
	asl
	bcc	.right
	asl
	bcc	.left
	asl
	bcc	.down
	asl
	bcc	.up
	lda	#$00
	sta	MOVING
	jmp	end_joystick

.right
	lda	MOVING
	bne	end_joystick
	inc	X_POS
	lda	#$01
	sta	MOVING
	jmp	end_joystick

.left
	lda	MOVING
	bne	end_joystick
	dec	X_POS
	lda	#$01
	sta	MOVING
	jmp	end_joystick

.down
	lda	MOVING
	bne	end_joystick
	inc	Y_POS
	lda	#$01
	sta	MOVING
	jmp	end_joystick

.up
	lda	MOVING
	bne	end_joystick
	dec	Y_POS
	lda	#$01
	sta	MOVING

end_joystick

	;
	; Limit the position to within 32x32
	;
	lda	#$1F
	and	X_POS
	sta	X_POS
	lda	#$1F
	and	Y_POS
	sta	Y_POS

       LDA    #$00    
       STA    $87     
       CLC            
       LDA    Y_POS
       ROL            
       ROL            
       ROL            
       STA    $86     
       ROL    $86     
       ROL    $87     
       CLC            
       ROL    $86     
       ROL    $87     
       LDA    #$F8    
       ORA    $87     
       STA    $87     
       LDX    #$08    

LF0AC: STX    $82     
       LDA    X_POS
       CLC            
       ADC    #$08    
       AND    #$1F    
       TAY            
       LDA    ($86),Y 
       STA    $83     
       LDA    #$C0    
       STA    $B2,X   
       DEY            
       BPL    LF0C3   
       LDY    #$1F    
LF0C3: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF0CE   
       STA    $84     
       JMP    LF0D4   
LF0CE: LDA    #$30    
       ORA    $B2,X   
       STA    $B2,X   
LF0D4: DEY            
       BPL    LF0D9   
       LDY    #$1F    
LF0D9: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF0E4   
       STA    $84     
       JMP    LF0EA   
LF0E4: LDA    #$0C    
       ORA    $B2,X   
       STA    $B2,X   
LF0EA: DEY            
       BPL    LF0EF   
       LDY    #$1F    
LF0EF: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF0FA   
       STA    $84     
       JMP    LF100   
LF0FA: LDA    #$03    
       ORA    $B2,X   
       STA    $B2,X   
LF100: DEY            
       BPL    LF105   
       LDY    #$1F    
LF105: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF114   
       STA    $84     
       LDA    #$00    
       STA    $A9,X   
       JMP    LF118   
LF114: LDA    #$03    
       STA    $A9,X   
LF118: DEY            
       BPL    LF11D   
       LDY    #$1F    
LF11D: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF128   
       STA    $84     
       JMP    LF12E   
LF128: LDA    #$0C    
       ORA    $A9,X   
       STA    $A9,X   
LF12E: DEY            
       BPL    LF133   
       LDY    #$1F    
LF133: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF13E   
       STA    $84     
       JMP    LF144   
LF13E: LDA    #$30    
       ORA    $A9,X   
       STA    $A9,X   
LF144: DEY            
       BPL    LF149   
       LDY    #$1F    
LF149: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF154   
       STA    $84     
       JMP    LF15A   
LF154: LDA    #$C0    
       ORA    $A9,X   
       STA    $A9,X   
LF15A: DEY            
       BPL    LF15F   
       LDY    #$1F    
LF15F: LDA    ($86),Y 
       CMP    $83     
       BEQ    LF16E   
       STA    $84     
       LDA    #$00    
       STA    $A0,X   
       JMP    LF172   
LF16E: LDA    #$C0    
       STA    $A0,X   
LF172: LDX    $83     
       LDA    LFC00,X 
       LDX    $82     
       STA    $BB,X   
       LDX    $84     
       LDA    LFC00,X 
       LDX    $82     
       STA    $C4,X   
       CLC            
       LDA    $86     
       ADC    #$20    
       STA    $86     
       LDA    $87     
       ADC    #$00    
       AND    #$FB    
       STA    $87     
       DEX            
       BMI    LF199   
       JMP    LF0AC   

	;
	; Wait until timer decrements to zero
	;
LF199:	lda	INTIM
	bne	LF199

	sta	WSYNC
	sta	VBLANK		; Turn VBLANK off
	sta	WSYNC
	sta	WSYNC

	;
	; Dashed-line at top of screen
	;
	lda	#$84		; Mid-blue
	sta	COLUPF
	lda	#$8E		; Light-blue
	sta	COLUBK
	lda	#$AA		; Dashed-line (1010 1010)
	sta	PF0
	lda	#$55		; Dashed-line (0101 0101)
	sta	PF1
	lda	#$AA		; Dashed-line (1010 1010)
	sta	PF2

	;
	; Set P0 & P1 to white
	;
	lda	#$0E		; White
	sta	COLUP0
	sta	COLUP1
	lda	#$03		; 3 copies -- close
	sta	NUSIZ1

	;
	; Delay for 10 lines
	;
	ldy	#$10
LF1C6:	sta	WSYNC
	lda	#$00		; Black
	sta	COLUPF
	sta	COLUBK
	dey
	bne	LF1C6

	;
	; Reset position of P1 to the right of screen
	;
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	sta	RESP1

	;
	; Load a row of the landscape from RAM into registers, starting
	; from the top (row 9)
	;
	ldx	#$08
LF1E9:	lda	DECODED_PF0,X
	sta	PF0
	sta	WSYNC
	sta	HMOVE
	lda	DECODED_COLUPF,X
	sta	COLUPF
	lda	DECODED_COLUBK,X
	sta	COLUBK
	lda	DECODED_PF1,X
	sta	PF1
	lda	DECODED_PF2,X
	sta	PF2

	;
	; Wait until PF registers have been displayed, and then
	; set everything black so the right-hand side of the screen
	; is clear.
	;
	nop
	nop
	nop
	nop
	nop
	nop
	lda	#$00		; Black
	sta	COLUBK
	sta	COLUPF

	;
	; Load address from jump-table into RAM
	;
	lda	JUMP_TABLE_LSB,x
	sta	JUMP_POINT
	lda	JUMP_TABLE_MSB,x
	sta	JUMP_POINT+1

	jmp	(JUMP_POINT)

LF21A:
	;
	; Turn PF colours on again
	;
	sta	WSYNC
	sta	HMOVE
	lda	DECODED_COLUPF,X
	sta	COLUPF
	lda	DECODED_COLUBK,X
	sta	COLUBK

	lda	LFC04,X
	sta	RESP0		; Position P0 roughly at column 5
	asl
	asl
	asl
	asl
	sta	P0_DATA_PTR
	nop
	nop
	nop
	nop

	;
	; Set PF colours to black
	;
	lda	#$00
	sta	COLUBK
	sta	COLUPF

	;
	; Fine-tune the position of P0 at column 5 by moving
	; right by 8 clocks
	;
	lda	#$10
	sta	HMP0
	jmp	LF383

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       NOP            
       STA    RESP0   
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       STA    P0_DATA_PTR     
       NOP            
       NOP            
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$F0    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       NOP            
       NOP            
       STA    RESP0   
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       STA    P0_DATA_PTR     
       NOP            
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$D0    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       NOP            
       NOP            
       NOP            
       STA    RESP0   
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       STA    P0_DATA_PTR     
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$B0    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDY    LFC04,X 
       LDA    LFC43,Y 
       NOP            
       NOP            
       NOP            
       STA    RESP0   
       STA    P0_DATA_PTR     
       NOP            
       NOP            
       NOP            
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$F0    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       NOP            
       STA    RESP0   
       STA    P0_DATA_PTR     
       NOP            
       NOP            
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$D0    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       NOP            
       NOP            
       NOP            
       STA    RESP0   
       STA    P0_DATA_PTR     
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$10    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       ASL            
       ASL            
       ASL            
       ASL            
       NOP            
       STA    P0_DATA_PTR     
       NOP            
       NOP            
       NOP            
       STA    RESP0   
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$20    
       STA    HMP0    
       JMP    LF383   

       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    LFC04,X 
       ASL            
       ASL            
       ASL            
       ASL            
       STA    P0_DATA_PTR     
       NOP            
       NOP            
       NOP            
       NOP            
       STA    RESP0   
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    #$A0    
       STA    HMP0    
       JMP    LF383   

LF383: STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDY    LFC0D,X 
       LDA    LFC43,Y 
       STA    P1_1_DATA_PTR     
       STA    HMCLR   
       LDY    LFC16,X 
       LDA    LFC43,Y 
       STA    P1_2_DATA_PTR     
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDY    LFC1F,X 
       LDA    LFC43,Y 
       STA    P1_3_DATA_PTR     
       LDA    LFC28,X 
       STA    NUSIZ0  
       LDY    #$0C    
LF3B6: STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    (P0_DATA_PTR),Y 
       STA    GRP0    
       NOP            
       NOP            
       NOP            
       NOP            
       NOP            
       LDA    (P1_1_DATA_PTR),Y 
       STA    GRP1    
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       LDA    (P1_2_DATA_PTR),Y 
       STA    GRP1    
       LDA    (P1_3_DATA_PTR),Y 
       STA    GRP1    
       DEY            
       BNE    LF3B6   
       STA    WSYNC   
       STA    HMOVE   
       LDA    $BB,X   
       STA    COLUPF  
       LDA    $C4,X   
       STA    COLUBK  
       LDA    (P0_DATA_PTR),Y 
       STA    GRP0    
       NOP            
       NOP            
       NOP            
       NOP            
       NOP            
       NOP            
       NOP            
       LDA    #$00    
       STA    GRP1    
       LDA    #$00    
       STA    COLUBK  
       STA    COLUPF  
       DEX            
       BMI    LF407   
       JMP    LF1E9   
LF407: LDY    #$32    
LF409: STA    WSYNC   
       LDA    #$00    
       STA    COLUPF  
       STA    COLUBK  
       DEY            
       BNE    LF409   
       LDA    #$1A    
       STA    TIM64T  
       LDY    #$02    
       STY    VBLANK  
       LDA    #$00    
       STA    COLUBK  

LF421:	lda	INTIM
	bne	LF421
	jmp	start_vblank

	org	$f800

	;
	; World definition
	;
	; 0 - Sea
	; 1 - Sand
	; 2 - Grass
	; 3 - Forest
	;
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
	.byte $01,$01,$00,$00,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00
	.byte $00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
	
	.byte $02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02
	
	.byte $02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$00
	.byte $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02
	
	.byte $02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$00
	.byte $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02
	
	.byte $03,$03,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02,$02,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$02,$03,$03,$03
	
	.byte $03,$03,$03,$03,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$03,$03
	
	.byte $02,$03,$03,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$00
	.byte $00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	
	.byte $02,$02,$02,$02,$02,$02,$02,$03,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	
	.byte $03,$03,$03,$02,$02,$02,$02,$03,$03,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$03,$02,$02,$02,$03,$02,$02,$03,$03
	
	.byte $03,$03,$03,$03,$03,$02,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$03,$03,$02,$02,$03,$03,$03,$03,$03
	
	.byte $03,$03,$03,$03,$03,$02,$03,$03,$03,$03,$03,$02,$03,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$03,$02,$02,$03,$02,$03,$03,$03,$03,$03,$03
	
	.byte $03,$03,$03,$03,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$02,$02
	.byte $02,$02,$02,$03,$03,$03,$03,$02,$02,$02,$03,$03,$03,$03,$03,$03
	
	.byte $03,$03,$03,$03,$02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$02,$02
	.byte $02,$02,$03,$03,$03,$03,$03,$03,$02,$03,$03,$03,$03,$03,$03,$03
	
	.byte $03,$03,$03,$03,$03,$02,$02,$02,$03,$03,$03,$03,$03,$02,$02,$02
	.byte $02,$02,$03,$03,$03,$03,$03,$03,$02,$03,$03,$03,$03,$03,$03,$03
	
	.byte $00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$02
	.byte $02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00
	
	.byte $00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$02
	.byte $02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00
	
	.byte $00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$03,$02,$02,$02
	.byte $02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$00,$00,$00,$00,$00,$02,$02,$02,$02,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$02,$02,$02,$00,$00,$00,$00,$02,$02,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$03,$03,$03,$03,$03,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

LFC00:	.byte $84,$1C,$C6,$C2

LFC04:	.byte $00,$00,$00,$00,$01,$00,$00,$00,$00

LFC0D:	.byte $00,$01,$02

LFC10:	.byte $00,$01,$02,$00,$01,$02

LFC16:	.byte $01,$02,$00,$01,$02,$00,$01,$02,$00

LFC1F:	.byte $02,$00,$01,$02,$00,$01,$02,$00,$01

LFC28:	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00

JUMP_TABLE_LSB
	.byte $1A
	.byte $42
	.byte $6A
	.byte $92
	.byte $BA
	.byte $E3
	.byte $0B
	.byte $33
	.byte $5B

JUMP_TABLE_MSB
	.byte $F2
	.byte $F2
	.byte $F2
	.byte $F2
	.byte $F2
	.byte $F2
	.byte $F3
	.byte $F3
	.byte $F3

LFC43:	.byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$A0,$B0,$C0,$D0,$E0,$F0

	org	$fd00

PLAYER_DATA

	;
	; nothing
	;
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; man
	;
	.byte	%00000000
	.byte	%00101000
	.byte	%00101000
	.byte	%00101000
	.byte	%00101000
	.byte	%10111010
	.byte	%10111010
	.byte	%01111100

	.byte	%01111100
	.byte	%00111000
	.byte	%00010000
	.byte	%00111000
	.byte	%00111000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; castle
	;
	.byte	%00000000
	.byte	%01101100
	.byte	%01101100
	.byte	%01101100
	.byte	%01111100
	.byte	%01111100
	.byte	%01111100
	.byte	%01111100

	.byte	%01111100
	.byte	%11111110
	.byte	%10101010
	.byte	%10101010
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; ship
	;
	.byte	%00000000
	.byte	%00111000
	.byte	%01111100
	.byte	%01111100
	.byte	%11010110
	.byte	%00010110
	.byte	%00110000
	.byte	%00111000

	.byte	%00111000
	.byte	%00011000
	.byte	%00010000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	org	$fe00

STATUS_DATA

	;
	; A
	;
	.byte	%00000000
	.byte	%11000110
	.byte	%11000110
	.byte	%11000110
	.byte	%11111110
	.byte	%11000110
	.byte	%01101100
	.byte	%00111000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; B
	;
	.byte	%00000000
	.byte	%11111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11111100
	.byte	%11000110
	.byte	%11000110
	.byte	%11111100

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; C
	;
	.byte	%00000000
	.byte	%01111100
	.byte	%11000110
	.byte	%11000000
	.byte	%11000000
	.byte	%11000000
	.byte	%11000110
	.byte	%01111100

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

;
; RESTART VECTOR
;
	org	$FFFC

	.word	ENTER		; Cold start & reset vector
	.word	ENTER		; IRQ vector (and BRK vector) (not used)
