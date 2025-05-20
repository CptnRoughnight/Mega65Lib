
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
	; LineNumber: 20
	jmp block1
	; LineNumber: 14
misc_s	= $02
	; LineNumber: 19
fileio_s	=  $04
	; LineNumber: 18
savedFile:		.asciiz	"SAVE.BIN"

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
	;*************************************************************	TRSE Mega65 StdLib											misc.TRU																													Procedures:																												Functions:														strlen - returns length of string																					*************************************************************************************************************	misc::strlen																							s	- pointer to string 																		returns												byte : length of string					************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : misc_strlen
	;    Procedure type : User-defined procedure
	; LineNumber: 27
	; LineNumber: 26
localVariable_misc_strlen_misc_index:	.byte	0
misc_strlen_block4:
misc_strlen:
	; LineNumber: 28
misc_strlen_while5:
misc_strlen_loopstart9:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_misc_strlen_misc_index
	lda (misc_s),y
	; cmp #$00 ignored
	beq misc_strlen_edblock8
misc_strlen_ctb6: ;Main true block ;keep:
	; LineNumber: 28
	; Test Inc dec D
	inc localVariable_misc_strlen_misc_index
	jmp misc_strlen_while5
misc_strlen_edblock8:
misc_strlen_loopend10:
	; LineNumber: 30
	; LineNumber: 31
	lda localVariable_misc_strlen_misc_index
	rts
end_procedure_misc_strlen:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fileio_SaveFile
	;    Procedure type : User-defined procedure
	; LineNumber: 33
	; LineNumber: 32
localVariable_fileio_SaveFile_fileio_l:	.byte	0
	; LineNumber: 30
localVariable_fileio_SaveFile_fileio_b:	.word	0
	; LineNumber: 30
localVariable_fileio_SaveFile_fileio_addr:	.word	0
	; LineNumber: 30
localVariable_fileio_SaveFile_fileio_endadr:	.word	0
fileio_SaveFile_block13:
fileio_SaveFile:
	; LineNumber: 34
	lda fileio_s
	ldx fileio_s+1
	sta misc_s
	stx misc_s+1
	jsr misc_strlen
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_SaveFile_fileio_l
	; LineNumber: 35
		sei
		
		lda localVariable_fileio_SaveFile_fileio_b+1	; for load address
		ora #$80
		ldy localVariable_fileio_SaveFile_fileio_b
		ldx #$00	; bank for filename
		jsr $ff6b
			
		
		lda #$00
		ldx #$08
		ldy #$00
		jsr $FFBA
		
		
		lda localVariable_fileio_SaveFile_fileio_l
		ldx fileio_s
		ldy fileio_s+1
		jsr $FFBD
	
		
		lda localVariable_fileio_SaveFile_fileio_addr
		sta $63
		lda localVariable_fileio_SaveFile_fileio_addr+1
		sta $64
		
		ldx localVariable_fileio_SaveFile_fileio_endadr
		ldy localVariable_fileio_SaveFile_fileio_endadr+1
		lda #$63
		jsr $FFD8
		bcs @derrorsave
	jmp @goexitsave
    @derrorsave:
        inc $d021
        jmp @derrorsave
        
    @goexitsave:
		cli
     
	; LineNumber: 75
	rts
end_procedure_fileio_SaveFile:
	;************************************************	fileio::LoadFile																					b  		- bank of address						addr  	- address to load to						s  		- pointer to filename-string															************************************************
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fileio_LoadFile
	;    Procedure type : User-defined procedure
	; LineNumber: 88
	; LineNumber: 87
localVariable_fileio_LoadFile_fileio_l:	.byte	0
	; LineNumber: 85
localVariable_fileio_LoadFile_fileio_b:	.word	0
	; LineNumber: 85
localVariable_fileio_LoadFile_fileio_addr:	.word	0
fileio_LoadFile_block14:
fileio_LoadFile:
	; LineNumber: 89
	lda fileio_s
	ldx fileio_s+1
	sta misc_s
	stx misc_s+1
	jsr misc_strlen
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_l
	; LineNumber: 90
		sei		
	
		lda localVariable_fileio_LoadFile_fileio_b+1	; for load address
		ora #$80
		ldy localVariable_fileio_LoadFile_fileio_b
		ldx #$00	; bank for filename
		jsr $ff6b
		lda #$00
		ldx #$08
		ldy #$00
		jsr $FFBA
					
		lda localVariable_fileio_LoadFile_fileio_l
		ldx fileio_s
		ldy fileio_s+1
		jsr $FFBD
	
		lda #$00
		ldx localVariable_fileio_LoadFile_fileio_addr
		ldy localVariable_fileio_LoadFile_fileio_addr+1
		jsr $FFD5
		bcs @derror
		jmp @goexit
	@derror:
		inc $d020
		jmp @derror
		
	@goexit:
		cli
	
	; LineNumber: 122
	rts
end_procedure_fileio_LoadFile:
block1:
main_block_begin_:
	; LineNumber: 21
	;************************************************	TRSE Mega65 StdLib								Example : files.ras																				Shows the usage of the mega65 fileio unit 		to use it run buildfiles.sh command				needs vice c1541 to be installed!																************************************************
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_SaveFile_fileio_b
	sty localVariable_fileio_SaveFile_fileio_b+1
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_SaveFile_fileio_addr
	sty localVariable_fileio_SaveFile_fileio_addr+1
	; Integer constant assigning
	; Load16bitvariable : #$fd0
	ldy #$0f
	lda #$d0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_SaveFile_fileio_endadr
	sty localVariable_fileio_SaveFile_fileio_endadr+1
	lda #<savedFile
	ldx #>savedFile
	sta fileio_s
	stx fileio_s+1
	jsr fileio_SaveFile
	; LineNumber: 22
	
; // save screen to file
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
	; LineNumber: 23
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_b
	sty localVariable_fileio_LoadFile_fileio_b+1
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_addr
	sty localVariable_fileio_LoadFile_fileio_addr+1
	lda #<savedFile
	ldx #>savedFile
	sta fileio_s
	stx fileio_s+1
	jsr fileio_LoadFile
	; LineNumber: 23
	
; // load file back to screen
	jmp * ; loop like (�/%
	; LineNumber: 25
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

