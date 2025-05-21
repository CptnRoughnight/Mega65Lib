
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
	; LineNumber: 22
	jmp block1
	; LineNumber: 16
system_SCREEN_BANK:	.byte	$00
	; LineNumber: 17
system_SCREEN_ADR:	.word	$800
	; LineNumber: 18
system_CHAR_BANK:	.byte	$00
	; LineNumber: 19
system_CHAR_ADR:	.word	$00
	; LineNumber: 22
system_MAXX:	.byte	$50
	; LineNumber: 23
system_MAXY:	.byte	$19
	; LineNumber: 30
textio_currentFont:	.byte	$00
	; LineNumber: 32
textio_isLowerCase:	.byte	$00
	; LineNumber: 33
textio_s	= $02
	; LineNumber: 41
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

	; LineNumber: 20
newChar:	.byte $7e, $ff, $db, $ff, $c3, $e7, $ff, $7e
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
	; ***********  Defining procedure : system_SetScreenBackground
	;    Procedure type : User-defined procedure
	; LineNumber: 87
	; LineNumber: 86
localVariable_system_SetScreenBackground_system_back:	.byte	0
	; LineNumber: 86
localVariable_system_SetScreenBackground_system_border:	.byte	0
system_SetScreenBackground_block4:
system_SetScreenBackground:
	; LineNumber: 88
		lda localVariable_system_SetScreenBackground_system_back
		sta $d020
		lda localVariable_system_SetScreenBackground_system_border
		sta $d021
	
	; LineNumber: 94
	rts
end_procedure_system_SetScreenBackground:
	;**************************************************************	system::SetCharLocation																											b			- Bank of charset location							adr			- address of charset location																					**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_SetCharLocation
	;    Procedure type : User-defined procedure
	; LineNumber: 105
	; LineNumber: 104
localVariable_system_SetCharLocation_system_b:	.byte	0
	; LineNumber: 104
localVariable_system_SetCharLocation_system_adr:	.word	0
system_SetCharLocation_block5:
system_SetCharLocation:
	; LineNumber: 106
	lda localVariable_system_SetCharLocation_system_b
	; Calling storevariable on generic assign expression
	sta system_CHAR_BANK
	; LineNumber: 107
	ldy localVariable_system_SetCharLocation_system_adr+1 ;keep
	lda localVariable_system_SetCharLocation_system_adr
	; Calling storevariable on generic assign expression
	sta system_CHAR_ADR
	sty system_CHAR_ADR+1
	; LineNumber: 108
		sta 	$D068
		lda localVariable_system_SetCharLocation_system_adr+1
		sta $D069
		lda localVariable_system_SetCharLocation_system_b
		sta $D06A	
	
	; LineNumber: 116
	rts
end_procedure_system_SetCharLocation:
	;**************************************************************	system::SetScreenLocation																										b			- Bank of screen Location							adr			- address of screen Location					**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_SetScreenLocation
	;    Procedure type : User-defined procedure
	; LineNumber: 126
	; LineNumber: 125
localVariable_system_SetScreenLocation_system_b:	.byte	0
	; LineNumber: 125
localVariable_system_SetScreenLocation_system_adr:	.word	0
system_SetScreenLocation_block6:
system_SetScreenLocation:
	; LineNumber: 127
	lda localVariable_system_SetScreenLocation_system_b
	; Calling storevariable on generic assign expression
	sta system_SCREEN_BANK
	; LineNumber: 128
	ldy localVariable_system_SetScreenLocation_system_adr+1 ;keep
	lda localVariable_system_SetScreenLocation_system_adr
	; Calling storevariable on generic assign expression
	sta system_SCREEN_ADR
	sty system_SCREEN_ADR+1
	; LineNumber: 129
		lda localVariable_system_SetScreenLocation_system_b
		sta $d062
		lda localVariable_system_SetScreenLocation_system_adr
		sta $d061
		lda localVariable_system_SetScreenLocation_system_adr+1
		sta $d060		
	
	; LineNumber: 137
	rts
end_procedure_system_SetScreenLocation:
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
memory_Fill_block7:
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_Copy
	;    Procedure type : User-defined procedure
	; LineNumber: 82
	; LineNumber: 81
localVariable_memory_Copy_memory_srcadrhi:	.word	0
	; LineNumber: 81
localVariable_memory_Copy_memory_srcadrlo:	.word	0
	; LineNumber: 81
localVariable_memory_Copy_memory_dstadrhi:	.word	0
	; LineNumber: 81
localVariable_memory_Copy_memory_dstadrlo:	.word	0
	; LineNumber: 81
localVariable_memory_Copy_memory_countx:	.word	0
memory_Copy_block8:
memory_Copy:
	; LineNumber: 83
		lda localVariable_memory_Copy_memory_countx
		ldy localVariable_memory_Copy_memory_countx+1
		sta @dmaCopyCount
		sty @dmaCopyCount+1
				
		lda localVariable_memory_Copy_memory_dstadrlo
		ldy localVariable_memory_Copy_memory_dstadrlo+1
		sta @dmaFillDst
		sty @dmaFillDst+1
	
		lda localVariable_memory_Copy_memory_dstadrhi
		ldy localVariable_memory_Copy_memory_dstadrhi+1
		sta @dmaFillDstBank
		sty @dmaDstMB
		
		lda localVariable_memory_Copy_memory_srcadrlo
		ldy localVariable_memory_Copy_memory_srcadrlo+1
		sta @dmaSrc
		sty @dmaSrc+1
		
		lda localVariable_memory_Copy_memory_srcadrhi
		ldy localVariable_memory_Copy_memory_srcadrhi+1
		sta @dmaSrcBank
		sty @dmaSrcMB
		
		sta $d707												; execute memory
		.byte $0B												; F011B memory format
		.byte $81
		@dmaDstMB:			.byte $00							; set MB of destination
		.byte $80
		@dmaSrcMB:			.byte $00
		.byte $00												; end of job options
		.byte $00												; copy
		@dmaCopyCount: 		.word $0028							; count
		@dmaSrc: 			.word $00							; source
		@dmaSrcBank:     	.byte $00							; src bank
		@dmaFillDst: 		.word $f800							; dst
		@dmaFillDstBank: 	.byte $01							; dst bank
		.byte $00												; cmd hi
		.word $00												; modulo / ignored
	
	; LineNumber: 126
	rts
end_procedure_memory_Copy:
	;**************************************************************	memory::Poke32								 																 					adrhi   - High-integer of address				 				adrlo 	- High-integer of address		     					val 	    - Value to poke						 															 				**************************************************************
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
memory_Poke32_block9:
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
	;**************************************************************	textio::Set40x25																																											**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set40x25
	;    Procedure type : User-defined procedure
	; LineNumber: 93
textio_Set40x25:
	; LineNumber: 94
	lda #$19
	; Calling storevariable on generic assign expression
	sta system_MAXY
	; LineNumber: 95
	lda #$28
	; Calling storevariable on generic assign expression
	sta system_MAXX
	; LineNumber: 96
		lda #%10000000
		trb $d031
		
		lda #%00001000
		trb $d031
		
		lda #25
		sta $d07b	
	
	; LineNumber: 106
	rts
end_procedure_textio_Set40x25:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_ClearScreen
	;    Procedure type : User-defined procedure
	; LineNumber: 116
	; LineNumber: 115
localVariable_textio_ClearScreen_textio_ch:	.byte	0
	; LineNumber: 115
localVariable_textio_ClearScreen_textio_c:	.byte	0
textio_ClearScreen_block11:
textio_ClearScreen:
	; LineNumber: 117
	ldy #0 ; Fake 16 bit
	lda system_SCREEN_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Fill_memory_adrhi
	sty localVariable_memory_Fill_memory_adrhi+1
	ldy system_SCREEN_ADR+1 ;keep
	lda system_SCREEN_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrlo
	sty localVariable_memory_Fill_memory_adrlo+1
	; Mul 16x8 setup
	; Load16bitvariable : system_MAXX
	ldy #0
	lda system_MAXX
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : system_MAXY
	lda system_MAXY
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
	; LineNumber: 118
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
	; Load16bitvariable : system_MAXX
	lda system_MAXX
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : system_MAXY
	lda system_MAXY
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
	; LineNumber: 119
	rts
end_procedure_textio_ClearScreen:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_PrintChar
	;    Procedure type : User-defined procedure
	; LineNumber: 131
	; LineNumber: 130
localVariable_textio_PrintChar_textio_x:	.byte	0
	; LineNumber: 130
localVariable_textio_PrintChar_textio_y:	.byte	0
	; LineNumber: 130
localVariable_textio_PrintChar_textio_ch:	.byte	0
	; LineNumber: 130
localVariable_textio_PrintChar_textio_c:	.byte	0
textio_PrintChar_block12:
textio_PrintChar:
	; LineNumber: 132
	ldy #0 ; Fake 16 bit
	lda system_SCREEN_BANK
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
	; Load16bitvariable : system_MAXX
	lda system_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintChar_rightvarInteger_var15 = $54
	sta textio_PrintChar_rightvarInteger_var15
	sty textio_PrintChar_rightvarInteger_var15+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy system_SCREEN_ADR+1 ;keep
	lda system_SCREEN_ADR
	clc
	adc localVariable_textio_PrintChar_textio_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintChar_skip17
	iny
textio_PrintChar_skip17:
	; Low bit binop:
	clc
	adc textio_PrintChar_rightvarInteger_var15
textio_PrintChar_wordAdd13:
	sta textio_PrintChar_rightvarInteger_var15
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var15+1
	tay
	lda textio_PrintChar_rightvarInteger_var15
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintChar_textio_ch
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 133
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
textio_PrintChar_rightvarInteger_var20 = $54
	sta textio_PrintChar_rightvarInteger_var20
	sty textio_PrintChar_rightvarInteger_var20+1
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_textio_PrintChar_textio_y
	ldy #0
	lda localVariable_textio_PrintChar_textio_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : system_MAXX
	lda system_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc textio_PrintChar_rightvarInteger_var20
textio_PrintChar_wordAdd18:
	sta textio_PrintChar_rightvarInteger_var20
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var20+1
	tay
	lda textio_PrintChar_rightvarInteger_var20
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintChar_textio_c
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 134
	rts
end_procedure_textio_PrintChar:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_PrintString
	;    Procedure type : User-defined procedure
	; LineNumber: 147
	; LineNumber: 146
localVariable_textio_PrintString_textio_i:	.byte	0
	; LineNumber: 144
localVariable_textio_PrintString_textio_x:	.byte	0
	; LineNumber: 144
localVariable_textio_PrintString_textio_y:	.byte	0
	; LineNumber: 144
localVariable_textio_PrintString_textio_c:	.byte	0
textio_PrintString_block21:
textio_PrintString:
	; LineNumber: 148
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_i
	; LineNumber: 149
textio_PrintString_while22:
textio_PrintString_loopstart26:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; cmp #$00 ignored
	beq textio_PrintString_localfailed42
	jmp textio_PrintString_ctb23
textio_PrintString_localfailed42:
	jmp textio_PrintString_edblock25
textio_PrintString_ctb23: ;Main true block ;keep:
	; LineNumber: 150
	; LineNumber: 151
	ldy #0 ; Fake 16 bit
	lda system_SCREEN_BANK
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
	; Load16bitvariable : system_MAXX
	lda system_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintString_rightvarInteger_var46 = $54
	sta textio_PrintString_rightvarInteger_var46
	sty textio_PrintString_rightvarInteger_var46+1
	; Generic 16 bit op
	ldy system_SCREEN_ADR+1 ;keep
	lda system_SCREEN_ADR
textio_PrintString_rightvarInteger_var49 = $56
	sta textio_PrintString_rightvarInteger_var49
	sty textio_PrintString_rightvarInteger_var49+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip51
	iny
textio_PrintString_skip51:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var49
textio_PrintString_wordAdd47:
	sta textio_PrintString_rightvarInteger_var49
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var49+1
	tay
	lda textio_PrintString_rightvarInteger_var49
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var46
textio_PrintString_wordAdd44:
	sta textio_PrintString_rightvarInteger_var46
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var46+1
	tay
	lda textio_PrintString_rightvarInteger_var46
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 152
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
	; Load16bitvariable : system_MAXX
	lda system_MAXX
	sta mul16x8_num2
	jsr mul16x8_procedure
textio_PrintString_rightvarInteger_var54 = $54
	sta textio_PrintString_rightvarInteger_var54
	sty textio_PrintString_rightvarInteger_var54+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip56
	iny
textio_PrintString_skip56:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var54
textio_PrintString_wordAdd52:
	sta textio_PrintString_rightvarInteger_var54
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var54+1
	tay
	lda textio_PrintString_rightvarInteger_var54
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda localVariable_textio_PrintString_textio_c
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 153
	; Test Inc dec D
	inc localVariable_textio_PrintString_textio_i
	; LineNumber: 154
	jmp textio_PrintString_while22
textio_PrintString_edblock25:
textio_PrintString_loopend27:
	; LineNumber: 155
	rts
end_procedure_textio_PrintString:
	;**************************************************************	textio::SetFont																													font		- font nr										**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_SetFont
	;    Procedure type : User-defined procedure
	; LineNumber: 163
	; LineNumber: 162
localVariable_textio_SetFont_textio_font:	.byte	0
textio_SetFont_block57:
textio_SetFont:
	; LineNumber: 164
	lda #$0
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext59
	; LineNumber: 164
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Integer constant assigning
	; Load16bitvariable : #$9000
	ldy #$90
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	jmp textio_SetFont_caseend58
textio_SetFont_casenext59:
	lda #$1
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext61
	; LineNumber: 165
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	jmp textio_SetFont_caseend58
textio_SetFont_casenext61:
	lda #$2
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext63
	; LineNumber: 166
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
textio_SetFont_casenext63:
textio_SetFont_caseend58:
	; LineNumber: 169
	lda localVariable_textio_SetFont_textio_font
	; Calling storevariable on generic assign expression
	sta textio_currentFont
	; LineNumber: 170
	rts
end_procedure_textio_SetFont:
	;**************************************************************	textio::ToggleLowerCase																										**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_ToggleLowerCase
	;    Procedure type : User-defined procedure
	; LineNumber: 180
	; LineNumber: 179
localVariable_textio_ToggleLowerCase_textio_d:	.word	0
textio_ToggleLowerCase_block65:
textio_ToggleLowerCase:
	; LineNumber: 181
	; Binary clause Simplified: NOTEQUALS
	clc
	lda textio_isLowerCase
	; cmp #$00 ignored
	beq textio_ToggleLowerCase_eblock68
textio_ToggleLowerCase_ctb67: ;Main true block ;keep:
	; LineNumber: 181
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ToggleLowerCase_textio_d
	sty localVariable_textio_ToggleLowerCase_textio_d+1
	jmp textio_ToggleLowerCase_edblock69
textio_ToggleLowerCase_eblock68:
	; LineNumber: 183
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ToggleLowerCase_textio_d
	sty localVariable_textio_ToggleLowerCase_textio_d+1
textio_ToggleLowerCase_edblock69:
	; LineNumber: 186
	lda #$0
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext75
	; LineNumber: 186
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var79 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var79
	sty textio_ToggleLowerCase_rightvarInteger_var79+1
	; Integer constant assigning
	; Load16bitvariable : #$9000
	ldy #$90
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var79
textio_ToggleLowerCase_wordAdd77:
	sta textio_ToggleLowerCase_rightvarInteger_var79
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var79+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var79
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	jmp textio_ToggleLowerCase_caseend74
textio_ToggleLowerCase_casenext75:
	lda #$1
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext80
	; LineNumber: 187
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var84 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var84
	sty textio_ToggleLowerCase_rightvarInteger_var84+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var84
textio_ToggleLowerCase_wordAdd82:
	sta textio_ToggleLowerCase_rightvarInteger_var84
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var84+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var84
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	jmp textio_ToggleLowerCase_caseend74
textio_ToggleLowerCase_casenext80:
	lda #$2
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext85
	; LineNumber: 188
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var89 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var89
	sty textio_ToggleLowerCase_rightvarInteger_var89+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var89
textio_ToggleLowerCase_wordAdd87:
	sta textio_ToggleLowerCase_rightvarInteger_var89
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var89+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var89
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$1000
	ldy #$10
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
textio_ToggleLowerCase_casenext85:
textio_ToggleLowerCase_caseend74:
	; LineNumber: 191
	rts
end_procedure_textio_ToggleLowerCase:
	;**************************************************************	textio::CharDef																													charNum			- char num to replace							data 			- pointer to array of 8 byte -> chardata																	**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_CharDef
	;    Procedure type : User-defined procedure
	; LineNumber: 201
	; LineNumber: 200
localVariable_textio_CharDef_textio_charNum:	.byte	0
	; LineNumber: 200
localVariable_textio_CharDef_textio_data	= $04
textio_CharDef_block90:
textio_CharDef:
	; LineNumber: 202
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	ldy localVariable_textio_CharDef_textio_data+1 ;keep
	lda localVariable_textio_CharDef_textio_data
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	ldy #0 ; Fake 16 bit
	lda system_CHAR_BANK
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	; Generic 16 bit op
	ldy system_CHAR_ADR+1 ;keep
	lda system_CHAR_ADR
textio_CharDef_rightvarInteger_var93 = $54
	sta textio_CharDef_rightvarInteger_var93
	sty textio_CharDef_rightvarInteger_var93+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$8
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : localVariable_textio_CharDef_textio_charNum
	lda localVariable_textio_CharDef_textio_charNum
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc textio_CharDef_rightvarInteger_var93
textio_CharDef_wordAdd91:
	sta textio_CharDef_rightvarInteger_var93
	; High-bit binop
	tya
	adc textio_CharDef_rightvarInteger_var93+1
	tay
	lda textio_CharDef_rightvarInteger_var93
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	; LineNumber: 203
	rts
end_procedure_textio_CharDef:
block1:
main_block_begin_:
	; LineNumber: 23
	;**************************************************************	TRSE Mega65 StdLib												Example : text.ras																												Shows usage of textio unit																									**************************************************************
	jsr textio_Set40x25
	; LineNumber: 27
	
; // no need to move the screen ram
; //	textio::Set80x25();  
; // no need to move the screen ram
; //	textio::Set80x50();	
; // ! move the screen ram
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetCharLocation_system_b
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetCharLocation_system_adr
	sty localVariable_system_SetCharLocation_system_adr+1
	jsr system_SetCharLocation
	; LineNumber: 28
	
; // set char location to $40000
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetScreenLocation_system_b
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetScreenLocation_system_adr
	sty localVariable_system_SetScreenLocation_system_adr+1
	jsr system_SetScreenLocation
	; LineNumber: 30
	
; // new screen location $50000
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetScreenBackground_system_back
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_system_SetScreenBackground_system_border
	jsr system_SetScreenBackground
	; LineNumber: 31
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_ch
	lda #$6
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_c
	jsr textio_ClearScreen
	; LineNumber: 34
	lda #$0
	; Calling storevariable on generic assign expression
	sta font
	; LineNumber: 35
	; Integer constant assigning
	; Load16bitvariable : #$ffff
	ldy #$ff
	lda #$ff
	; Calling storevariable on generic assign expression
	sta counter1
	sty counter1+1
	; LineNumber: 36
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta counter2
	sty counter2+1
	; LineNumber: 42
	lda #$0
	; Calling storevariable on generic assign expression
	sta i
MainProgram_forloop94:
	; LineNumber: 39
	; LineNumber: 40
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
	; LineNumber: 41
MainProgram_loopstart95:
	; Compare is onpage
	; Test Inc dec D
	inc i
	lda #$20
	cmp i ;keep
	bne MainProgram_forloop94
MainProgram_loopdone99: ;keep:
MainProgram_loopend96:
	; LineNumber: 43
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
	; LineNumber: 45
MainProgram_while100:
MainProgram_loopstart104:
	; Binary clause Simplified: NOTEQUALS
	clc
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_localfailed187
	jmp MainProgram_ctb101
MainProgram_localfailed187:
	jmp MainProgram_edblock103
MainProgram_ctb101: ;Main true block ;keep:
	; LineNumber: 46
	; LineNumber: 47
	
; // cycle endlessly through fonts
	lda counter1
	sec
	sbc #$01
	sta counter1+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs MainProgram_WordAdd189
	dec counter1+1
MainProgram_WordAdd189:
	; LineNumber: 48
	; Binary clause INTEGER: EQUALS
	lda counter1+1   ; compare high bytes
	cmp #$00 ;keep
	bne MainProgram_localfailed231
	lda counter1
	cmp #$00 ;keep
	bne MainProgram_localfailed231
	jmp MainProgram_ctb191
MainProgram_localfailed231:
	jmp MainProgram_edblock193
MainProgram_ctb191: ;Main true block ;keep:
	; LineNumber: 49
	; LineNumber: 50
	lda counter2
	sec
	sbc #$01
	sta counter2+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs MainProgram_WordAdd233
	dec counter2+1
MainProgram_WordAdd233:
	; LineNumber: 51
	
; // SetFont will overwrite our new char
	; Binary clause INTEGER: EQUALS
	lda counter2+1   ; compare high bytes
	cmp #$00 ;keep
	bne MainProgram_localfailed253
	lda counter2
	cmp #$00 ;keep
	bne MainProgram_localfailed253
	jmp MainProgram_ctb235
MainProgram_localfailed253:
	jmp MainProgram_edblock237
MainProgram_ctb235: ;Main true block ;keep:
	; LineNumber: 52
	; LineNumber: 53
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$4
	; Calling storevariable on generic assign expression
	sta counter2
	sty counter2+1
	; LineNumber: 54
	; Test Inc dec D
	inc font
	; LineNumber: 55
	lda #$0
	cmp font ;keep
	bne MainProgram_casenext256
	; LineNumber: 55
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetFont_textio_font
	jsr textio_SetFont
	jmp MainProgram_caseend255
MainProgram_casenext256:
	lda #$1
	cmp font ;keep
	bne MainProgram_casenext258
	; LineNumber: 56
	jsr textio_ToggleLowerCase
	jmp MainProgram_caseend255
MainProgram_casenext258:
	lda #$2
	cmp font ;keep
	bne MainProgram_casenext260
	; LineNumber: 57
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetFont_textio_font
	jsr textio_SetFont
	jmp MainProgram_caseend255
MainProgram_casenext260:
	lda #$3
	cmp font ;keep
	bne MainProgram_casenext262
	; LineNumber: 58
	jsr textio_ToggleLowerCase
	jmp MainProgram_caseend255
MainProgram_casenext262:
	lda #$4
	cmp font ;keep
	bne MainProgram_casenext264
	; LineNumber: 59
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetFont_textio_font
	jsr textio_SetFont
	jmp MainProgram_caseend255
MainProgram_casenext264:
	lda #$5
	cmp font ;keep
	bne MainProgram_casenext266
	; LineNumber: 60
	jsr textio_ToggleLowerCase
	jmp MainProgram_caseend255
MainProgram_casenext266:
	; LineNumber: 63
	; LineNumber: 64
	lda #$0
	; Calling storevariable on generic assign expression
	sta font
	; LineNumber: 65
MainProgram_caseend255:
	; LineNumber: 66
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_textio_CharDef_textio_charNum
	lda #<newChar
	ldx #>newChar
	sta localVariable_textio_CharDef_textio_data
	stx localVariable_textio_CharDef_textio_data+1
	jsr textio_CharDef
	; LineNumber: 67
MainProgram_edblock237:
	; LineNumber: 68
	; Integer constant assigning
	; Load16bitvariable : #$ffff
	ldy #$ff
	lda #$ff
	; Calling storevariable on generic assign expression
	sta counter1
	sty counter1+1
	; LineNumber: 69
MainProgram_edblock193:
	; LineNumber: 70
	jmp MainProgram_while100
MainProgram_edblock103:
MainProgram_loopend105:
	; LineNumber: 71
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

