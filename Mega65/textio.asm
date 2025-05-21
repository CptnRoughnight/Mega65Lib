
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
textio:
	; LineNumber: 204
	jmp block1
	; LineNumber: 16
system_SCREEN_BANK:	.byte	$00
	; LineNumber: 17
system_SCREEN_ADR:	.word	$800
	; LineNumber: 18
system_CHAR_BANK:	.byte	$00
	; LineNumber: 19
system_CHAR_ADR:	.word	$00
	; LineNumber: 20
system_LogicalRowSize:	.word	0
	; LineNumber: 21
system_RowSize:	.byte	0
	; LineNumber: 22
system_MAXX:	.byte	$50
	; LineNumber: 23
system_MAXY:	.byte	$19
	; LineNumber: 18
memory_p	= $02
	; LineNumber: 30
textio_currentFont:	.byte	$00
	; LineNumber: 32
textio_isLowerCase:	.byte	$00
	; LineNumber: 33
textio_s	= $04
	; LineNumber: 41
textio_COLORS:	.byte $0, $1, $2, $3, $4, $5, $6, $7
	.byte $8, $9, $a, $b, $c, $d, $e, $f
	.byte $40, $41, $42, $43, $44, $45, $46, $47
	.byte $48, $49, $4a, $4b, $4c, $4d, $4e, $4f
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
	;**************************************************************	TRSE Mega65 StdLib												system.TRU																														Procedures:																														Functions:															Enable40Mhz 		- Enable 40Mhz								EnableVic4  		- Enables Vic4 registers					DisableC65Rom 	- Disable C65 Rom																						****************************************************************************************************************************	system::Enable40Mhz																																											**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_Enable40Mhz
	;    Procedure type : User-defined procedure
	; LineNumber: 34
system_Enable40Mhz:
	; LineNumber: 35
			lda #$41
			sta $00
		
	; LineNumber: 39
	rts
end_procedure_system_Enable40Mhz:
	;**************************************************************	system::EnableVic4																																											**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_EnableVic4
	;    Procedure type : User-defined procedure
	; LineNumber: 46
system_EnableVic4:
	; LineNumber: 48
			lda #$00
			tax
			tay
			taz
			map
			eom
			
			lda #$47
			sta $d02f
			lda #$53
			sta $d02f		
		
	; LineNumber: 61
	rts
end_procedure_system_EnableVic4:
	;**************************************************************	system::DisableC65Rom																																										**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_DisableC65Rom
	;    Procedure type : User-defined procedure
	; LineNumber: 69
system_DisableC65Rom:
	; LineNumber: 70
			lda #$70
			sta $d640
			eom		
		
	; LineNumber: 75
	rts
end_procedure_system_DisableC65Rom:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_SetScreenBackground
	;    Procedure type : User-defined procedure
	; LineNumber: 87
	; LineNumber: 86
localVariable_system_SetScreenBackground_system_back:	.byte	0
	; LineNumber: 86
localVariable_system_SetScreenBackground_system_border:	.byte	0
system_SetScreenBackground_block7:
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
system_SetCharLocation_block8:
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
system_SetScreenLocation_block9:
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
	;**************************************************************	system::SetRowSize																												newSize			- new row size in byte																						**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : system_SetRowSize
	;    Procedure type : User-defined procedure
	; LineNumber: 147
	; LineNumber: 146
localVariable_system_SetRowSize_system_newSize:	.byte	0
system_SetRowSize_block10:
system_SetRowSize:
	; LineNumber: 148
	lda localVariable_system_SetRowSize_system_newSize
	; Calling storevariable on generic assign expression
	sta system_RowSize
	; LineNumber: 149
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : localVariable_system_SetRowSize_system_newSize
	lda localVariable_system_SetRowSize_system_newSize
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta system_LogicalRowSize
	sty system_LogicalRowSize+1
	; LineNumber: 150
	; Poke
	; Optimization: shift is zero
	lda localVariable_system_SetRowSize_system_newSize
	sta $d05e
	; LineNumber: 151
	; Poke
	; Optimization: shift is zero
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy system_LogicalRowSize+1 ;keep
	lda system_LogicalRowSize
	and #$ff
	; Testing for byte:  #$00
	; RHS is word, no optimization
	pha 
	tya 
	and #$00
	tay 
	pla 
	sta $d058
	; LineNumber: 152
	; Poke
	; Optimization: shift is zero
	; Generic 16 bit op
	ldy #0
	lda #$ff
system_SetRowSize_rightvarInteger_var14 = $54
	sta system_SetRowSize_rightvarInteger_var14
	sty system_SetRowSize_rightvarInteger_var14+1
	ldy system_LogicalRowSize+1 ;keep
	lda system_LogicalRowSize
system_SetRowSize_tempVarShift_var15 = $56
	sta system_SetRowSize_tempVarShift_var15
	sty system_SetRowSize_tempVarShift_var15+1
	; COUNT : 8
		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

		lsr system_SetRowSize_tempVarShift_var15+1 ;keep
	ror system_SetRowSize_tempVarShift_var15+0 ;keep

	lda system_SetRowSize_tempVarShift_var15
	ldy system_SetRowSize_tempVarShift_var15+1
	; Low bit binop:
	and system_SetRowSize_rightvarInteger_var14
system_SetRowSize_wordAdd12:
	sta system_SetRowSize_rightvarInteger_var14
	; High-bit binop
	tya
	and system_SetRowSize_rightvarInteger_var14+1
	tay
	lda system_SetRowSize_rightvarInteger_var14
	sta $d059
	; LineNumber: 153
	rts
end_procedure_system_SetRowSize:
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
memory_Fill_block16:
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
memory_Copy_block17:
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
memory_Poke32_block18:
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
	;**************************************************************	memory::Poke32																													adrhi   - High-integer of address								adrlo 	- High-integer of address								returns - peeked value																										**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_Peek32
	;    Procedure type : User-defined procedure
	; LineNumber: 165
	; LineNumber: 164
localVariable_memory_Peek32_memory_adrhi:	.word	0
	; LineNumber: 164
localVariable_memory_Peek32_memory_adrlo:	.word	0
memory_Peek32_block19:
memory_Peek32:
	; LineNumber: 166
			lda localVariable_memory_Poke32_memory_adrhi+1
			sta $43
			lda localVariable_memory_Poke32_memory_adrhi
			sta $42
			lda localVariable_memory_Poke32_memory_adrlo+1
			sta $41
			lda	localVariable_memory_Poke32_memory_adrlo
			sta $40
			
			ldz #0
			nop
			lda($40),z ; return value in A
		
	; LineNumber: 180
	rts
end_procedure_memory_Peek32:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_WPoke32
	;    Procedure type : User-defined procedure
	; LineNumber: 191
	; LineNumber: 190
localVariable_memory_WPoke32_memory_adrhi:	.word	0
	; LineNumber: 190
localVariable_memory_WPoke32_memory_adrlo:	.word	0
	; LineNumber: 190
localVariable_memory_WPoke32_memory_val:	.word	0
memory_WPoke32_block20:
memory_WPoke32:
	; LineNumber: 192
			lda localVariable_memory_WPoke32_memory_adrhi+1
			sta $43
			lda localVariable_memory_WPoke32_memory_adrhi
			sta $42
			lda localVariable_memory_WPoke32_memory_adrlo+1
			sta $41
			lda	localVariable_memory_WPoke32_memory_adrlo
			sta $40
			
			lda localVariable_memory_WPoke32_memory_val
			ldz #0
			nop
			sta ($40),z
			
			lda localVariable_memory_WPoke32_memory_val+1
			ldz #1
			nop
			sta ($40),z
		
	; LineNumber: 212
	rts
end_procedure_memory_WPoke32:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_WPeek32
	;    Procedure type : User-defined procedure
	; LineNumber: 223
	; LineNumber: 222
localVariable_memory_WPeek32_memory_adrhi:	.word	0
	; LineNumber: 222
localVariable_memory_WPeek32_memory_adrlo:	.word	0
memory_WPeek32_block21:
memory_WPeek32:
	; LineNumber: 224
			lda localVariable_memory_WPeek32_memory_adrhi+1
			sta $43
			lda localVariable_memory_WPeek32_memory_adrhi
			sta $42
			lda localVariable_memory_WPeek32_memory_adrlo+1
			sta $41
			lda	localVariable_memory_WPeek32_memory_adrlo
			sta $40
			
			ldz #1
			nop
			lda ($40),z		 
			tay				; high byte in Y
			
			ldz #0
			nop
			lda ($40),z		; low byte in A
		
	; LineNumber: 243
	rts
end_procedure_memory_WPeek32:
	;**************************************************************	TRSE Mega65 StdLib												textio.TRU																														Procedures:															Set80x50 	- set 80x50 textmode								Set80x25		- set 80x25 textmode							Set40x25		- set 40x25 textmode							ClearScreen	- Clear screen with char and color					SetScreenLocation	- Set memory address of screen 				SetCharLocation	- Set memory address of chardata				SetScreenBackground 	- Set Background and Border color			PrintChar	- print char at location with color 				PrintString 	- print string at location with color 			SetFont		- set font 											ToggleLowerCase	- toggles between uppercase and lowercase 			CharDef		- replace a char with new data 																				**************************************************************
; // pointer for strings**************************************************************	textio::COLOR																																												****************************************************************************************************************************	textio::Set80x50																																											**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set80x50
	;    Procedure type : User-defined procedure
	; LineNumber: 53
textio_Set80x50:
	; LineNumber: 54
	lda #$32
	; Calling storevariable on generic assign expression
	sta system_MAXY
	; LineNumber: 55
		lda #%10000000
		tsb $d031
		
		lda #%00001000
		tsb $d031
		
		lda #50
		sta $d07b	
	
	; LineNumber: 65
	rts
end_procedure_textio_Set80x50:
	;**************************************************************	textio::Set80x25																																											**************************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set80x25
	;    Procedure type : User-defined procedure
	; LineNumber: 73
textio_Set80x25:
	; LineNumber: 74
	lda #$19
	; Calling storevariable on generic assign expression
	sta system_MAXY
	; LineNumber: 75
		lda #%10000000
		tsb $d031
		
		lda #%00001000
		trb $d031
		
		lda #25
		sta $d07b	
	
	; LineNumber: 85
	rts
end_procedure_textio_Set80x25:
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
textio_ClearScreen_block25:
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
textio_PrintChar_block26:
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
textio_PrintChar_rightvarInteger_var29 = $54
	sta textio_PrintChar_rightvarInteger_var29
	sty textio_PrintChar_rightvarInteger_var29+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy system_SCREEN_ADR+1 ;keep
	lda system_SCREEN_ADR
	clc
	adc localVariable_textio_PrintChar_textio_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintChar_skip31
	iny
textio_PrintChar_skip31:
	; Low bit binop:
	clc
	adc textio_PrintChar_rightvarInteger_var29
textio_PrintChar_wordAdd27:
	sta textio_PrintChar_rightvarInteger_var29
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var29+1
	tay
	lda textio_PrintChar_rightvarInteger_var29
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
textio_PrintChar_rightvarInteger_var34 = $54
	sta textio_PrintChar_rightvarInteger_var34
	sty textio_PrintChar_rightvarInteger_var34+1
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
	adc textio_PrintChar_rightvarInteger_var34
textio_PrintChar_wordAdd32:
	sta textio_PrintChar_rightvarInteger_var34
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var34+1
	tay
	lda textio_PrintChar_rightvarInteger_var34
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
textio_PrintString_block35:
textio_PrintString:
	; LineNumber: 148
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_i
	; LineNumber: 149
textio_PrintString_while36:
textio_PrintString_loopstart40:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; cmp #$00 ignored
	beq textio_PrintString_localfailed56
	jmp textio_PrintString_ctb37
textio_PrintString_localfailed56:
	jmp textio_PrintString_edblock39
textio_PrintString_ctb37: ;Main true block ;keep:
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
textio_PrintString_rightvarInteger_var60 = $54
	sta textio_PrintString_rightvarInteger_var60
	sty textio_PrintString_rightvarInteger_var60+1
	; Generic 16 bit op
	ldy system_SCREEN_ADR+1 ;keep
	lda system_SCREEN_ADR
textio_PrintString_rightvarInteger_var63 = $56
	sta textio_PrintString_rightvarInteger_var63
	sty textio_PrintString_rightvarInteger_var63+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip65
	iny
textio_PrintString_skip65:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var63
textio_PrintString_wordAdd61:
	sta textio_PrintString_rightvarInteger_var63
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var63+1
	tay
	lda textio_PrintString_rightvarInteger_var63
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var60
textio_PrintString_wordAdd58:
	sta textio_PrintString_rightvarInteger_var60
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var60+1
	tay
	lda textio_PrintString_rightvarInteger_var60
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
textio_PrintString_rightvarInteger_var68 = $54
	sta textio_PrintString_rightvarInteger_var68
	sty textio_PrintString_rightvarInteger_var68+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda localVariable_textio_PrintString_textio_x
	clc
	adc localVariable_textio_PrintString_textio_i
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip70
	iny
textio_PrintString_skip70:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var68
textio_PrintString_wordAdd66:
	sta textio_PrintString_rightvarInteger_var68
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var68+1
	tay
	lda textio_PrintString_rightvarInteger_var68
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
	jmp textio_PrintString_while36
textio_PrintString_edblock39:
textio_PrintString_loopend41:
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
textio_SetFont_block71:
textio_SetFont:
	; LineNumber: 164
	lda #$0
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext73
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
	jmp textio_SetFont_caseend72
textio_SetFont_casenext73:
	lda #$1
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext75
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
	jmp textio_SetFont_caseend72
textio_SetFont_casenext75:
	lda #$2
	cmp localVariable_textio_SetFont_textio_font ;keep
	bne textio_SetFont_casenext77
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
textio_SetFont_casenext77:
textio_SetFont_caseend72:
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
textio_ToggleLowerCase_block79:
textio_ToggleLowerCase:
	; LineNumber: 181
	; Binary clause Simplified: NOTEQUALS
	clc
	lda textio_isLowerCase
	; cmp #$00 ignored
	beq textio_ToggleLowerCase_eblock82
textio_ToggleLowerCase_ctb81: ;Main true block ;keep:
	; LineNumber: 181
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ToggleLowerCase_textio_d
	sty localVariable_textio_ToggleLowerCase_textio_d+1
	jmp textio_ToggleLowerCase_edblock83
textio_ToggleLowerCase_eblock82:
	; LineNumber: 183
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ToggleLowerCase_textio_d
	sty localVariable_textio_ToggleLowerCase_textio_d+1
textio_ToggleLowerCase_edblock83:
	; LineNumber: 186
	lda #$0
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext89
	; LineNumber: 186
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var93 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var93
	sty textio_ToggleLowerCase_rightvarInteger_var93+1
	; Integer constant assigning
	; Load16bitvariable : #$9000
	ldy #$90
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var93
textio_ToggleLowerCase_wordAdd91:
	sta textio_ToggleLowerCase_rightvarInteger_var93
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var93+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var93
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
	jmp textio_ToggleLowerCase_caseend88
textio_ToggleLowerCase_casenext89:
	lda #$1
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext94
	; LineNumber: 187
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var98 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var98
	sty textio_ToggleLowerCase_rightvarInteger_var98+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var98
textio_ToggleLowerCase_wordAdd96:
	sta textio_ToggleLowerCase_rightvarInteger_var98
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var98+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var98
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
	jmp textio_ToggleLowerCase_caseend88
textio_ToggleLowerCase_casenext94:
	lda #$2
	cmp textio_currentFont ;keep
	bne textio_ToggleLowerCase_casenext99
	; LineNumber: 188
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Generic 16 bit op
	ldy localVariable_textio_ToggleLowerCase_textio_d+1 ;keep
	lda localVariable_textio_ToggleLowerCase_textio_d
textio_ToggleLowerCase_rightvarInteger_var103 = $54
	sta textio_ToggleLowerCase_rightvarInteger_var103
	sty textio_ToggleLowerCase_rightvarInteger_var103+1
	; Integer constant assigning
	; Load16bitvariable : #$d000
	ldy #$d0
	lda #$00
	; Low bit binop:
	clc
	adc textio_ToggleLowerCase_rightvarInteger_var103
textio_ToggleLowerCase_wordAdd101:
	sta textio_ToggleLowerCase_rightvarInteger_var103
	; High-bit binop
	tya
	adc textio_ToggleLowerCase_rightvarInteger_var103+1
	tay
	lda textio_ToggleLowerCase_rightvarInteger_var103
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
textio_ToggleLowerCase_casenext99:
textio_ToggleLowerCase_caseend88:
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
localVariable_textio_CharDef_textio_data	= $08
textio_CharDef_block104:
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
textio_CharDef_rightvarInteger_var107 = $54
	sta textio_CharDef_rightvarInteger_var107
	sty textio_CharDef_rightvarInteger_var107+1
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
	adc textio_CharDef_rightvarInteger_var107
textio_CharDef_wordAdd105:
	sta textio_CharDef_rightvarInteger_var107
	; High-bit binop
	tya
	adc textio_CharDef_rightvarInteger_var107+1
	tay
	lda textio_CharDef_rightvarInteger_var107
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
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

