
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
	; LineNumber: 213
	jmp block1
	; LineNumber: 18
memory_p	= $02
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
textio_s	=  $04
	; LineNumber: 38
textio_COLORS:	.byte $0, $1, $2, $3, $4, $5, $6, $7
	.byte $8, $9, $a, $b, $c, $d, $e, $f
	.byte $40, $41, $42, $43, $44, $45, $46, $47
	.byte $48, $49, $4a, $4b, $4c, $4d, $4e, $4f
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
memory_Copy_block5:
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
memory_Poke32_block6:
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
	;************************************************	memory::Poke32																					adrhi   - High-integer of address					adrlo 	- High-integer of address				returns - peeked value																		************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_Peek32
	;    Procedure type : User-defined procedure
	; LineNumber: 165
	; LineNumber: 164
localVariable_memory_Peek32_memory_adrhi:	.word	0
	; LineNumber: 164
localVariable_memory_Peek32_memory_adrlo:	.word	0
memory_Peek32_block7:
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
memory_WPoke32_block8:
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
memory_WPeek32_block9:
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
	;*************************************************************	TRSE Mega65 StdLib											textio.TRU																												Procedures:																												Functions:														Set80x50 	- set 80x50 textmode								Set80x25		- set 80x25 textmode								Set40x25		- set 40x25 textmode								ClearScreen	- Clear screen with char and color				SetScreenLocation	- Set memory address of screen 			SetCharLocation	- Set memory address of chardata				SetScreenBackground 	- Set Background and Border color		PrintChar	- print char at location with color 				PrintString 	- print string at location with color 																*************************************************************
; // pointer for strings************************************************	textio::COLOR																																************************************************************************************************	textio::Set80x50																																************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set80x50
	;    Procedure type : User-defined procedure
	; LineNumber: 50
textio_Set80x50:
	; LineNumber: 51
	lda #$32
	; Calling storevariable on generic assign expression
	sta textio_MAXY
	; LineNumber: 52
		lda #%10000000
		tsb $d031
		
		lda #%00001000
		tsb $d031
		
		lda #50
		sta $d07b	
	
	; LineNumber: 62
	rts
end_procedure_textio_Set80x50:
	;************************************************	textio::Set80x25																																************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set80x25
	;    Procedure type : User-defined procedure
	; LineNumber: 70
textio_Set80x25:
	; LineNumber: 71
	lda #$19
	; Calling storevariable on generic assign expression
	sta textio_MAXY
	; LineNumber: 72
		lda #%10000000
		tsb $d031
		
		lda #%00001000
		trb $d031
		
		lda #25
		sta $d07b	
	
	; LineNumber: 82
	rts
end_procedure_textio_Set80x25:
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
textio_ClearScreen_block13:
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
textio_SetScreenBackground_block14:
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
textio_SetCharLocation_block15:
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
textio_SetScreenLocation_block16:
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
textio_PrintChar_block17:
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
textio_PrintChar_rightvarInteger_var20 = $54
	sta textio_PrintChar_rightvarInteger_var20
	sty textio_PrintChar_rightvarInteger_var20+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy textio_SCREEN_ADR+1 ;keep
	lda textio_SCREEN_ADR
	clc
	adc localVariable_textio_PrintChar_textio_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintChar_skip22
	iny
textio_PrintChar_skip22:
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
textio_PrintChar_rightvarInteger_var25 = $54
	sta textio_PrintChar_rightvarInteger_var25
	sty textio_PrintChar_rightvarInteger_var25+1
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
	adc textio_PrintChar_rightvarInteger_var25
textio_PrintChar_wordAdd23:
	sta textio_PrintChar_rightvarInteger_var25
	; High-bit binop
	tya
	adc textio_PrintChar_rightvarInteger_var25+1
	tay
	lda textio_PrintChar_rightvarInteger_var25
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
textio_PrintString_block26:
textio_PrintString:
	; LineNumber: 205
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_textio_PrintString_textio_i
	; LineNumber: 206
textio_PrintString_while27:
textio_PrintString_loopstart31:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; cmp #$00 ignored
	beq textio_PrintString_edblock30
textio_PrintString_ctb28: ;Main true block ;keep:
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
textio_PrintString_rightvarInteger_var42 = $54
	sta textio_PrintString_rightvarInteger_var42
	sty textio_PrintString_rightvarInteger_var42+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy textio_SCREEN_ADR+1 ;keep
	lda textio_SCREEN_ADR
	clc
	adc localVariable_textio_PrintString_textio_x
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc textio_PrintString_skip44
	iny
textio_PrintString_skip44:
	; Low bit binop:
	clc
	adc textio_PrintString_rightvarInteger_var42
textio_PrintString_wordAdd40:
	sta textio_PrintString_rightvarInteger_var42
	; High-bit binop
	tya
	adc textio_PrintString_rightvarInteger_var42+1
	tay
	lda textio_PrintString_rightvarInteger_var42
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	; Load pointer array
	ldy localVariable_textio_PrintString_textio_i
	lda (textio_s),y
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 210
	
; //	memory::Poke32($ff8,(x+i)+y*MAXX,c);
	; Test Inc dec D
	inc localVariable_textio_PrintString_textio_i
	; LineNumber: 211
	jmp textio_PrintString_while27
textio_PrintString_edblock30:
textio_PrintString_loopend32:
	; LineNumber: 212
	rts
end_procedure_textio_PrintString:
block1:
main_block_begin_:
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

