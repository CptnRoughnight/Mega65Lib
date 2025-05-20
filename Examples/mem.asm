
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
mem:
	; LineNumber: 15
	jmp block1
	; LineNumber: 13
val:	.word	0
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
memory_Copy_block4:
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
	; NodeProcedureDecl -1
	; ***********  Defining procedure : memory_WPeek32
	;    Procedure type : User-defined procedure
	; LineNumber: 223
	; LineNumber: 222
localVariable_memory_WPeek32_memory_adrhi:	.word	0
	; LineNumber: 222
localVariable_memory_WPeek32_memory_adrlo:	.word	0
memory_WPeek32_block6:
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
block1:
main_block_begin_:
	; LineNumber: 16
	;************************************************	TRSE Mega65 StdLib								Example : mem.ras																				Shows the usage of the mega65 memory unit 													************************************************
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrhi
	sty localVariable_memory_Fill_memory_adrhi+1
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_adrlo
	sty localVariable_memory_Fill_memory_adrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$7d0
	ldy #$07
	lda #$d0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_countx
	sty localVariable_memory_Fill_memory_countx+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$57
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Fill_memory_value
	sty localVariable_memory_Fill_memory_value+1
	jsr memory_Fill
	; LineNumber: 17
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrhi
	sty localVariable_memory_Copy_memory_srcadrhi+1
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_srcadrlo
	sty localVariable_memory_Copy_memory_srcadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$ff08
	ldy #$ff
	lda #$08
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrhi
	sty localVariable_memory_Copy_memory_dstadrhi+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_dstadrlo
	sty localVariable_memory_Copy_memory_dstadrlo+1
	; Integer constant assigning
	; Load16bitvariable : #$7d0
	ldy #$07
	lda #$d0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Copy_memory_countx
	sty localVariable_memory_Copy_memory_countx+1
	jsr memory_Copy
	; LineNumber: 18
	; Integer constant assigning
	; Load16bitvariable : #$ff8
	ldy #$0f
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrhi
	sty localVariable_memory_Poke32_memory_adrhi+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_adrlo
	sty localVariable_memory_Poke32_memory_adrlo+1
	lda #$d
	; Calling storevariable on generic assign expression
	sta localVariable_memory_Poke32_memory_val
	jsr memory_Poke32
	; LineNumber: 19
	; Integer constant assigning
	; Load16bitvariable : #$ff8
	ldy #$0f
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_memory_WPeek32_memory_adrhi
	sty localVariable_memory_WPeek32_memory_adrhi+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_memory_WPeek32_memory_adrlo
	sty localVariable_memory_WPeek32_memory_adrlo+1
	jsr memory_WPeek32
	; Calling storevariable on generic assign expression
	sta val
	sty val+1
	; LineNumber: 19
	jmp * ; loop like (�/%
	; LineNumber: 21
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

