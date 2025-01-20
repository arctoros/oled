.SEGMENT "OLED"

IMAGE = $F000

OLEDLD:
				LDA #<IMAGE
				STA IMGPTR+0
				LDA #>IMAGE
				STA IMGPTR+1
				LDY #0
				LDX #4
@LOOP:	LDA (IMGPTR),Y
				JSR OLED
				INY
				BNE @LOOP 						; Loop until y=0 (256 times)
				INC IMGPTR+1 					; +256
				DEX
				BNE @LOOP 						; Loop until x=0 (4 times)
				RTS

OLED:
				STA PORTB
				LDA #OLEDEN
				INC PORTA							; Assuming that D/C# is still high (from init)
				DEC PORTA
				RTS

OLEDINIT:
				LDA #$FF
				STA DDRB
				LDA #(OLEDDC|OLEDEN)
				STA DDRA
				LDA #OLEDEN
				STA PORTA

				LDX #$00
				LDY #INSTLEN
@LOOP:	LDA OLEDINST,X
				STA PORTB
				DEC PORTA
				INC PORTA
				INX
				DEY
				BNE @LOOP

@EXIT:	LDA #OLEDDC
				STA PORTA
				RTS

INSTLEN = $0F

OLEDINST:
				.byte $AE
				.byte $20, $01				; Set Memory Adressing Mode to vertical
				.byte $21, $00, $7F		; Set Column Address
				.byte $22, $00, $07		; Set Page Address
				.byte $40
				.byte $A1
				.byte $C8
				.byte $D3, $00
				.byte $AF							; Display ON
