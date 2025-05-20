
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
HelloWorld:
	; LineNumber: 6
	jmp block1
	; LineNumber: 17
textio_MAXX:	.byte	$50
	; LineNumber: 18
textio_MAXY:	.byte	$19
	; LineNumber: 19
textio_SCREEN_BANK:	.byte	$00
	; LineNumber: 20
textio_SCREEN_ADR:	.word	$800
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
initeightbitmul_multiply_eightbit2:
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
memory_Fill_block3:
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
	;************************************************	textio::Set80x25																																************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_Set80x25
	;    Procedure type : User-defined procedure
	; LineNumber: 50
textio_Set80x25:
	; LineNumber: 51
	lda #$19
	; Calling storevariable on generic assign expression
	sta textio_MAXY
	; LineNumber: 52
		lda #%10000000
		tsb $d031
		
		lda #%00001000
		trb $d031
		
		lda #25
		sta $d07b	
	
	; LineNumber: 62
	rts
end_procedure_textio_Set80x25:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_ClearScreen
	;    Procedure type : User-defined procedure
	; LineNumber: 72
	; LineNumber: 71
localVariable_textio_ClearScreen_textio_ch:	.byte	0
	; LineNumber: 71
localVariable_textio_ClearScreen_textio_c:	.byte	0
textio_ClearScreen_block5:
textio_ClearScreen:
	; LineNumber: 73
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
	; LineNumber: 74
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
	; LineNumber: 75
	rts
end_procedure_textio_ClearScreen:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : textio_SetScreenBackground
	;    Procedure type : User-defined procedure
	; LineNumber: 86
	; LineNumber: 85
localVariable_textio_SetScreenBackground_textio_back:	.byte	0
	; LineNumber: 85
localVariable_textio_SetScreenBackground_textio_border:	.byte	0
textio_SetScreenBackground_block6:
textio_SetScreenBackground:
	; LineNumber: 87
		lda localVariable_textio_SetScreenBackground_textio_back
		sta $d020
		lda localVariable_textio_SetScreenBackground_textio_border
		sta $d021
	
	; LineNumber: 93
	rts
end_procedure_textio_SetScreenBackground:
block1:
main_block_begin_:
	; LineNumber: 7
	jsr textio_Set80x25
	; LineNumber: 8
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenBackground_textio_back
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_textio_SetScreenBackground_textio_border
	jsr textio_SetScreenBackground
	; LineNumber: 9
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_ch
	lda #$6
	; Calling storevariable on generic assign expression
	sta localVariable_textio_ClearScreen_textio_c
	jsr textio_ClearScreen
	; LineNumber: 10
	jmp * ; loop like (�/%
	; LineNumber: 12
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

