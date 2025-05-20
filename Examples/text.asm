
StartBlock0:
	; Starting new memory block at 0
.p4510   ; 65816 processor
;.i16    ; X/Y are 16 bits
;.a8     ; A is 8 bits
;.segment "CODE"
EndBlock0:
StartBlock2001:
	; Starting new memory block at $2001
.p4510   ; 65816 processor
	 .org $2001
	 .byte $09,$20 ;End of command marker (first byte after the 00 terminator)
	 .byte $0a,$00 ;10
	 .byte $fe,$02,$30,$00 ;BANK 0
	 .byte $13, $20 
	 .byte $14,$00 ;20
	 .byte $9e ;SYS
	 .byte $38,$32,$32,$34
	  .byte $00
endd_s:
	  .byte $00,$00    ;End of basic terminators
	  .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF    ;extra
	; Ending memory block at $2001
	; Resuming memory block at $2001
text:
	; LineNumber: 20
	jmp block1
	; LineNumber: 24
textio_MAXX:	.byte	$50
	; LineNumber: 25
textio_MAXY:	.byte	$19
	; LineNumber: 26
textio_SCREEN_BANK:	.byte	$00
	; LineNumber: 27
textio_SCREEN_ADR:	.word	$800
	; LineNumber: 28
textio_CHAR_BANK:	.byte	$00
	; LineNumber: 29
textio_CHAR_ADR:	.word	$00
	; LineNumber: 30
textio_s	= $02
	; LineNumber: 38
textio_COLORS:	.byte $0, $1, $2, $3, $4, $5, $6, $7
	.byte $8, $9, $a, $b, $c, $d, $e, $f
	.byte $40, $41, $42, $43, $44, $45, $46, $47
	.byte $48, $49, $4a, $4b, $4c, $4d, $4e, $4f
	; LineNumber: 14
counter1:	.word	0
	; LineNumber: 15
counter2:	.word	0
	; LineNumber: 16
font:	.byte	0
	; LineNumber: 17
i:	.byte	0
	; LineNumber: 18
hello:		.asciiz	"HELLO WORLD"

	; NodeProcedureDecl -1
	; ***********  Defining procedure : initprintstring
	;    Procedure type : Built-in function
	;    Requires initialization : no
print_text = $4c
print_number_text: .asciiz "    "
printstring:
	ldy #0
printstringloop:
	lda (print_text),y
	cmp #0 ;keep
	beq printstring_done
	cmp #64
	bcc printstring_skip
	sec
	sbc #64
printstring_skip:
	sta (screenmemory),y
	iny
	dex
	cpx #0
	beq printstring_done
	jmp printstringloop
printstring_done:
	rts
end_procedure_initprintstring:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4c
mul16x8_num1 = $4e
mul16x8_num2 = $50
mul16x8_procedure:
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd:
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop:
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop:
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
end_procedure_init16x8mul:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto2
screenmemory =  $fe
colormemory =  $fb
screen_x = $4c
screen_y = $4e
SetScreenPosition:
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop:
	clc
	adc #80
	bcc sskip
	inc screenmemory+1
sskip:
	dey
	bne syloop
sydone:
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone:
	sta screenmemory
	rts
initmoveto_moveto2:
	rts
end_procedure_initmoveto:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4c
multiplier_a = $4e
multiply_eightbit:
	cpx #$00
	beq mul_end
	dex
	stx $4e
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop:
	bcc mul_skip
mul_mod:
	adc multiplier_a
mul_skip:
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end:
	txa
	rts
initeightbitmul_multiply_eightbit3:
	rts
end_procedure_initeightbitmul:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_Fill
	;    Procedure type : User-defined procedure
	; LineNumber: 30
	; LineNumber: 29
localVariable_memory_Fill_memory_adrhi:	.word	0
	; LineNumber: 29
localVariable_memory_Fill_memory_adrlo:	.word	0
	; LineNumber: 29
localVariable_memory_Fill_memory_countx:	.word	0
	; LineNumber: 29
localVariable_memory_Fill_memory_value:	.word	0
memory_Fill_block4:
memory_Fill:
	; LineNumber: 31
		lda localVariable_memory_Fill_memory_countx
		ldy localVariable_memory_Fill_memory_countx+1
		sta @dmaFillCount
		sty @dmaFillCount+1
		
		lda localVariable_memory_Fill_memory_value
		ldy localVariable_memory_Fill_memory_value+1
		sta @dmaFillValue
		sty @dmaFillValue+1
		
		lda localVariable_memory_Fill_memory_adrlo
		ldy localVariable_memory_Fill_memory_adrlo+1
		sta @dmaFillDst
		sty @dmaFillDst+1
	
		lda localVariable_memory_Fill_memory_adrhi
		ldy localVariable_memory_Fill_memory_adrhi+1
		sta @dmaFillDstBank
		sty @dmaDstMB
		
		
		sta $d707												; execute memory
		.byte $0B												; F011B memory format
		.byte $81
		@dmaDstMB:
		.byte $00												; set MB of destination
		.byte $00												; end of job options
		.byte $03												; fill
		@dmaFillCount: 		.word $0028							; count
		@dmaFillValue: 		.word $00							; value
		.byte $00												; src bank
		@dmaFillDst: 		.word $f800							; dst
		@dmaFillDstBank: 	.byte $01							; dst bank
		.byte $00												; cmd hi
		.word $00												; modulo / ignored
	
	; LineNumber: 69
	rts
end_procedure_memory_Fill:
	;*************************************************	memory::Poke32								 												 	adrhi   - High-integer of address				 	adrlo 	- High-integer of address		     	val 	    - Value to poke						 												 *************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_Poke32
	;    Procedure type : User-defined procedure
	; LineNumber: 137
	; LineNumber: 136
localVariable_memory_Poke32_memory_adrhi:	.word	0
	; LineNumber: 136
localVariable_memory_Poke32_memory_adrlo:	.word	0
	; LineNumber: 136
localVariable_memory_Poke32_memory_val:	.byte	0
memory_Poke32_block5:
memory_Poke32:
	; LineNumber: 138
			lda localVariable_memory_Poke32_memory_adrhi+1
			sta $43
			lda localVariable_memory_Poke32_memory_adrhi
			sta $42
			lda localVariable_memory_Poke32_memory_adrlo+1
			sta $41
			lda	localVariable_memory_Poke32_memory_adrlo
			sta $40
			lda localVariable_memory_Poke32_memory_val
			ldz #0
			nop
			sta ($40),z
		
	; LineNumber: 153
	rts
end_procedure_memory_Poke32:
	;************************************************	textio::Set40x25																																************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set40x25
	;    Procedure type : User-defined procedure
	; LineNumber: 90
textio_Set40x25:
	; LineNumber: 91
	lda #$19
	; Calling storevariable on generic assign expression
	sta textio_MAXY
	; LineNumber: 92
	lda #$28
	; Calling storevariable on generic assign expression
	sta textio_MAXX
	; LineNumber: 93
		lda #%10000000
		trb $d031
		
		lda #%00001000
		trb $d031
		
		lda #25
		sta $d07b	
	
	; LineNumber: 103
	rts
end_procedure_textio_Set40x25:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_ClearScreen
	;    Procedure type : User-defined procedure
	; LineNumber: 113
	; LineNumber: 112
localVariable_textio_ClearScreen_textio_ch:	.byte	0
	; LineNumber: 112
localVariable_textio_ClearScreen_textio_c:	.byte	0
textio_ClearScreen_block7:
textio_ClearScreen:
	; LineNumber: 114
	ldy #0 ; Fake 16 bit
	lda textio_SCREEN_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Fill_memory_adrhi
	sty localVariable_memory_Fill_memory_adrhi+1
	ldy textio_SCREEN_ADR+1 ;keep
	lda textio_SCREEN_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrlo
	sty localVariable_memory_Fill_memory_adrlo+1
	; Mul 16x8 setup
	; Load16bitvariable : textio_MAXX
	ldy #0
	lda textio_MAXX
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXY
	lda textio_MAXY
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_countx
	sty localVariable_memory_Fill_memory_countx+1
	ldy #0 ; Fake 16 bit
	lda localVariable_textio_ClearScreen_textio_ch
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Fill_memory_value
	sty localVariable_memory_Fill_memory_value+1
	jsr memory_Fill
	; LineNumber: 115
	; Integer constant assigning
	; Load16bitvariable : #$ff08
	ldy #$ff
	lda #$08
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrhi
	sty localVariable_memory_Fill_memory_adrhi+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrlo
	sty localVariable_memory_Fill_memory_adrlo+1
	; Mul 16x8 setup
	; Load16bitvariable : textio_MAXX
	lda textio_MAXX
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXY
	lda textio_MAXY
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_countx
	sty localVariable_memory_Fill_memory_countx+1
	ldy #0 ; Fake 16 bit
	lda localVariable_textio_ClearScreen_textio_c
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Fill_memory_value
	sty localVariable_memory_Fill_memory_value+1
	jsr memory_Fill
	; LineNumber: 116
	rts
end_procedure_textio_ClearScreen:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_SetScreenBackground
	;    Procedure type : User-defined procedure
	; LineNumber: 127
	; LineNumber: 126
localVariable_textio_SetScreenBackground_textio_back:	.byte	0
	; LineNumber: 126
localVariable_textio_SetScreenBackground_textio_border:	.byte	0
textio_SetScreenBackground_block8:
textio_SetScreenBackground:
	; LineNumber: 128
		lda localVariable_textio_SetScreenBackground_textio_back
		sta $d020
		lda localVariable_textio_SetScreenBackground_textio_border
		sta $d021
	
	; LineNumber: 134
	rts
end_procedure_textio_SetScreenBackground:
	;************************************************	textio::SetCharLocation																			b			- Bank of charset location			adr			- address of charset location														************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_SetCharLocation
	;    Procedure type : User-defined procedure
	; LineNumber: 145
	; LineNumber: 144
localVariable_textio_SetCharLocation_textio_b:	.byte	0
	; LineNumber: 144
localVariable_textio_SetCharLocation_textio_adr:	.word	0
textio_SetCharLocation_block9:
textio_SetCharLocation:
	; LineNumber: 146
	lda localVariable_textio_SetCharLocation_textio_b
	; Calling storevariable on generic assign expression
	sta textio_CHAR_BANK
	; LineNumber: 147
	ldy localVariable_textio_SetCharLocation_textio_adr+1 ;keep
	lda localVariable_textio_SetCharLocation_textio_adr
	; Calling storevariable on generic assign expression
	sta textio_CHAR_ADR
	sty textio_CHAR_ADR+1
	; LineNumber: 148
		sta 	$D068
		lda localVariable_textio_SetCharLocation_textio_adr+1
		sta $D069
		lda localVariable_textio_SetCharLocation_textio_b
		sta $D06A	
	
	; LineNumber: 156
	rts
end_procedure_textio_SetCharLocation:
	;************************************************	textio::SetScreenLocation																		b			- Bank of screen Location			adr			- address of screen Location		************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_SetScreenLocation
	;    Procedure type : User-defined procedure
	; LineNumber: 166
	; LineNumber: 165
localVariable_textio_SetScreenLocation_textio_b:	.byte	0
	; LineNumber: 165
localVariable_textio_SetScreenLocation_textio_adr:	.word	0
textio_SetScreenLocation_block10:
textio_SetScreenLocation:
	; LineNumber: 167
	lda localVariable_textio_SetScreenLocation_textio_b
	; Calling storevariable on generic assign expression
	sta textio_SCREEN_BANK
	; LineNumber: 168
	ldy localVariable_textio_SetScreenLocation_textio_adr+1 ;keep
	lda localVariable_textio_SetScreenLocation_textio_adr
	; Calling storevariable on generic assign expression
	sta textio_SCREEN_ADR
	sty textio_SCREEN_ADR+1
	; LineNumber: 169
		lda localVariable_textio_SetScreenLocation_textio_b
		sta $d062
		lda localVariable_textio_SetScreenLocation_textio_adr
		sta $d061
		lda localVariable_textio_SetScreenLocation_textio_adr+1
		sta $d060		
	
	; LineNumber: 177
	rts
end_procedure_textio_SetScreenLocation:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_PrintChar
	;    Procedure type : User-defined procedure
	; LineNumber: 188
	; LineNumber: 187
localVariable_textio_PrintChar_textio_x:	.byte	0
	; LineNumber: 187
localVariable_textio_PrintChar_textio_y:	.byte	0
	; LineNumber: 187
localVariable_textio_PrintChar_textio_ch:	.byte	0
	; LineNumber: 187
localVariable_textio_PrintChar_textio_c:	.byte	0
textio_PrintChar_block11:
textio_PrintChar:
	; LineNumber: 189
	ldy #0 ; Fake 16 bit
	lda textio_SCREEN_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Poke32_memory_adrhi
	sty localVariable_memory_Poke32_memory_adrhi+1
	; Generic 16 bit op
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_textio_PrintChar_textio_y
	lda localVariable_textio_PrintChar_textio_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXX
	lda textio_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintChar_rightvarInteger_var14 = $54
	sta textio_PrintChar_rightvarInteger_var14
	sty textio_PrintChar_rightvarInteger_var14+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy textio_SCREEN_ADR+1 ;keep
	lda textio_SCREEN_ADR
	clc
	adc localVariable_textio_PrintChar_textio_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintChar_skip16
	iny
textio_PrintChar_skip16:
	; Low bit binop:
	clc
	adc textio_PrintChar_rightvarInteger_var14
textio_PrintChar_wordAdd12:
	sta textio_PrintChar_rightvarInteger_var14
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var14+1
	tay
	lda textio_PrintChar_rightvarInteger_var14
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintChar_textio_ch
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 190
	; Integer constant assigning
	; Load16bitvariable : #$ff8
	ldy #$0f
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrhi
	sty localVariable_memory_Poke32_memory_adrhi+1
	; Generic 16 bit op
	ldy #0
	lda localVariable_textio_PrintChar_textio_x
textio_PrintChar_rightvarInteger_var19 = $54
	sta textio_PrintChar_rightvarInteger_var19
	sty textio_PrintChar_rightvarInteger_var19+1
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_textio_PrintChar_textio_y
	ldy #0
	lda localVariable_textio_PrintChar_textio_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXX
	lda textio_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc textio_PrintChar_rightvarInteger_var19
textio_PrintChar_wordAdd17:
	sta textio_PrintChar_rightvarInteger_var19
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var19+1
	tay
	lda textio_PrintChar_rightvarInteger_var19
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintChar_textio_c
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 191
	rts
end_procedure_textio_PrintChar:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_PrintString
	;    Procedure type : User-defined procedure
	; LineNumber: 204
	; LineNumber: 203
localVariable_textio_PrintString_textio_i:	.byte	0
	; LineNumber: 201
localVariable_textio_PrintString_textio_x:	.byte	0
	; LineNumber: 201
localVariable_textio_PrintString_textio_y:	.byte	0
	; LineNumber: 201
localVariable_textio_PrintString_textio_c:	.byte	0
textio_PrintString_block20:
textio_PrintString:
	; LineNumber: 205
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_i
	; LineNumber: 206
textio_PrintString_while21:
textio_PrintString_loopstart25:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; cmp #$00 ignored
	beq textio_PrintString_localfailed41
	jmp textio_PrintString_ctb22
textio_PrintString_localfailed41:
	jmp textio_PrintString_edblock24
textio_PrintString_ctb22: ;Main true block ;keep:
	; LineNumber: 207
	; LineNumber: 208
	ldy #0 ; Fake 16 bit
	lda textio_SCREEN_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Poke32_memory_adrhi
	sty localVariable_memory_Poke32_memory_adrhi+1
	; Generic 16 bit op
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_textio_PrintString_textio_y
	lda localVariable_textio_PrintString_textio_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXX
	lda textio_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintString_rightvarInteger_var45 = $54
	sta textio_PrintString_rightvarInteger_var45
	sty textio_PrintString_rightvarInteger_var45+1
	; Generic 16 bit op
	ldy textio_SCREEN_ADR+1 ;keep
	lda textio_SCREEN_ADR
textio_PrintString_rightvarInteger_var48 =  $56
	sta textio_PrintString_rightvarInteger_var48
	sty textio_PrintString_rightvarInteger_var48+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip50
	iny
textio_PrintString_skip50:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var48
textio_PrintString_wordAdd46:
	sta textio_PrintString_rightvarInteger_var48
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var48+1
	tay
	lda textio_PrintString_rightvarInteger_var48
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var45
textio_PrintString_wordAdd43:
	sta textio_PrintString_rightvarInteger_var45
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var45+1
	tay
	lda textio_PrintString_rightvarInteger_var45
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 209
	; Integer constant assigning
	; Load16bitvariable : #$ff8
	ldy #$0f
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrhi
	sty localVariable_memory_Poke32_memory_adrhi+1
	; Generic 16 bit op
	ldy #0
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_textio_PrintString_textio_y
	lda localVariable_textio_PrintString_textio_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : textio_MAXX
	lda textio_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintString_rightvarInteger_var53 = $54
	sta textio_PrintString_rightvarInteger_var53
	sty textio_PrintString_rightvarInteger_var53+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip55
	iny
textio_PrintString_skip55:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var53
textio_PrintString_wordAdd51:
	sta textio_PrintString_rightvarInteger_var53
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var53+1
	tay
	lda textio_PrintString_rightvarInteger_var53
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintString_textio_c
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 210
	; Test Inc dec D
	inc localVariable_textio_PrintString_textio_i
	; LineNumber: 211
	jmp textio_PrintString_while21
textio_PrintString_edblock24:
textio_PrintString_loopend26:
	; LineNumber: 212
	rts
end_procedure_textio_PrintString:
block1:
main_block_begin_:
	; LineNumber: 21
	;************************************************	TRSE Mega65 StdLib								Example : text.ras																				Shows usage of textio unit																	************************************************
	jsr textio_Set40x25
	; LineNumber: 25
	
; // no need to move the screen ram
; //	textio::Set80x25();  
; // no need to move the screen ram
; //textio::Set80x50();	
; // ! move the screen ram
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenLocation_textio_b
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenLocation_textio_adr
	sty localVariable_textio_SetScreenLocation_textio_adr+1
	jsr textio_SetScreenLocation
	; LineNumber: 27
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenBackground_textio_back
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenBackground_textio_border
	jsr textio_SetScreenBackground
	; LineNumber: 28
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_ch
	lda #$6
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_c
	jsr textio_ClearScreen
	; LineNumber: 29
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	; LineNumber: 30
	lda #$0
	; Calling storevariable on generic assign expression
	sta font
	; LineNumber: 31
	; Integer constant assigning
	; Load16bitvariable : #$ffff
	ldy #$ff
	lda #$ff
	; Calling storevariable on generic assign expression
	sta counter1
	sty counter1+1
	; LineNumber: 32
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta counter2
	sty counter2+1
	; LineNumber: 38
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
MainProgram_forloop56:
	; LineNumber: 35
	; LineNumber: 36
	lda i
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintChar_textio_x
	lda #$a
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintChar_textio_y
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintChar_textio_ch
	; Load Byte array
	; CAST type NADA
	ldx i
	lda textio_COLORS,x 
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintChar_textio_c
	jsr textio_PrintChar
	; LineNumber: 37
MainProgram_loopstart57:
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$20
	cmp i ;keep
	bne MainProgram_forloop56
MainProgram_loopdone61: ;keep:
MainProgram_loopend58:
	; LineNumber: 39
	lda #<hello
	ldx #>hello
	sta textio_s
	stx textio_s+1
	lda #$14
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_x
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_y
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_c
	jsr textio_PrintString
	; LineNumber: 41
MainProgram_while62:
MainProgram_loopstart66:
	; Binary clause Simplified: NOTEQUALS
	clc
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_localfailed149
	jmp MainProgram_ctb63
MainProgram_localfailed149:
	jmp MainProgram_edblock65
MainProgram_ctb63: ;Main true block ;keep:
	; LineNumber: 42
	; LineNumber: 43
	
; // cycle endlessly through fonts
	lda counter1
	sec
	sbc #$01
	sta counter1+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs MainProgram_WordAdd151
	dec counter1+1
MainProgram_WordAdd151:
	; LineNumber: 44
	; Binary clause INTEGER: EQUALS
	lda counter1+1   ; compare high bytes
	cmp #$00 ;keep
	bne MainProgram_localfailed193
	lda counter1
	cmp #$00 ;keep
	bne MainProgram_localfailed193
	jmp MainProgram_ctb153
MainProgram_localfailed193:
	jmp MainProgram_edblock155
MainProgram_ctb153: ;Main true block ;keep:
	; LineNumber: 45
	; LineNumber: 46
	lda counter2
	sec
	sbc #$01
	sta counter2+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs MainProgram_WordAdd195
	dec counter2+1
MainProgram_WordAdd195:
	; LineNumber: 47
	; Binary clause INTEGER: EQUALS
	lda counter2+1   ; compare high bytes
	cmp #$00 ;keep
	bne MainProgram_localfailed215
	lda counter2
	cmp #$00 ;keep
	bne MainProgram_localfailed215
	jmp MainProgram_ctb197
MainProgram_localfailed215:
	jmp MainProgram_edblock199
MainProgram_ctb197: ;Main true block ;keep:
	; LineNumber: 48
	; LineNumber: 49
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta counter2
	sty counter2+1
	; LineNumber: 50
	; Test Inc dec D
	inc font
	; LineNumber: 51
	lda #$0
	cmp font ;keep
	bne MainProgram_casenext218
	; LineNumber: 51
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext218:
	lda #$1
	cmp font ;keep
	bne MainProgram_casenext220
	; LineNumber: 52
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$d800
	ldy #$d8
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext220:
	lda #$2
	cmp font ;keep
	bne MainProgram_casenext222
	; LineNumber: 53
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext222:
	lda #$3
	cmp font ;keep
	bne MainProgram_casenext224
	; LineNumber: 54
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$d800
	ldy #$d8
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext224:
	lda #$4
	cmp font ;keep
	bne MainProgram_casenext226
	; LineNumber: 55
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$9000
	ldy #$90
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext226:
	lda #$5
	cmp font ;keep
	bne MainProgram_casenext228
	; LineNumber: 56
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_b
	; Integer constant assigning
	; Load16bitvariable : #$9800
	ldy #$98
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetCharLocation_textio_adr
	sty localVariable_textio_SetCharLocation_textio_adr+1
	jsr textio_SetCharLocation
	jmp MainProgram_caseend217
MainProgram_casenext228:
	; LineNumber: 59
	; LineNumber: 60
	lda #$0
	; Calling storevariable on generic assign expression
	sta font
	; LineNumber: 61
MainProgram_caseend217:
	; LineNumber: 62
MainProgram_edblock199:
	; LineNumber: 63
	; Integer constant assigning
	; Load16bitvariable : #$ffff
	ldy #$ff
	lda #$ff
	; Calling storevariable on generic assign expression
	sta counter1
	sty counter1+1
	; LineNumber: 64
MainProgram_edblock155:
	; LineNumber: 65
	jmp MainProgram_while62
MainProgram_edblock65:
MainProgram_loopend67:
	; LineNumber: 66
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

