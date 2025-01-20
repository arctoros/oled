.SETCPU "65C02"
.DEBUGINFO +

;+-- SYMBOLS ---
.ZEROPAGE
RDPTR:	.RES $1
WRPTR:	.RES $1
IMGPTR: .RES $2

.SEGMENT "INBUFFER"
INBUFFER:	.RES $100

.SEGMENT "BIOS"
; ACIA
SERDAT	= $5000								; Serial RX/TX register
SERSTAT = $5001								; ACIA status flags
SERCMD  = $5002								; Setup for parity, echo and interrupts
SERPTC  = $5003								; Serupt for serial protocol (stop bits, word length, baud)

; VIA
PORTB 	= $6000 							; I/O Register B
PORTA 	= $6001								; I/O Register A
DDRB 		= $6002								; Data direction register B (0 = Input, 1 = Output)
DDRA 		= $6003								; Data direction register A (0 = Input, 1 = Output)
T1CL 		= $6004								; Timer 1 low-order counter
T1CH 		= $6005								; Timer 1 high-order counter
T1LL 		= $6006								; Timer 1 low-order latch
T1LH 		= $6007								; Timer 1 high-order latch
T2CL 		= $6008								; Timer 2 low-order counter
T2CH 		= $6009								; Timer 2 high-order counter
SHFTR 	= $600a								; Shift register
ACR 		= $600b								; Auxiliary control register
PCR 		= $600c								; Peripheral control register
IFR 		= $600d								; Interrupt flag register
IER 		= $600e								; Interrupt enable register

DELAY 	= $7D

OLEDDC	= %10000000
OLEDEN	= %00000001
;---

;+-- RESET ---
RESET:
				JSR BUFFERINIT
				CLC
				CLI
				JSR SERIALINIT
				JSR OLEDINIT

				JSR	OLEDLD 
				JMP MON
;---

;+-- I/O ROUTINES ---
SERIALINIT:
				LDA #%00010000
				STA SERSTAT						; Soft reset by writing (value irrelevant)
				STA SERPTC						; N-8-1; 112500 baud
				LDA #%00001001				; No parity; no echo; interrupts
				STA SERCMD
				RTS

MONCOUT:
CHROUT:
				PHA
				STA SERDAT
				LDA #DELAY
TXWAIT:
				DEC
				BNE TXWAIT
				PLA
				RTS

MONRDKEY:
CHRIN:
				JSR BUFFERSIZE
				BEQ @NOKEYPRESSED
				JSR RDBUFFER
				JSR CHROUT						; echo
				SEC
				RTS
@NOKEYPRESSED:
				CLC
				RTS

WRBUFFER:
				PHX
				LDX WRPTR
				STA INBUFFER, X
				INC WRPTR
				PLX
				RTS

RDBUFFER:
				PHX
				LDX RDPTR
				LDA INBUFFER, X
				INC RDPTR
				PLX
				RTS

BUFFERSIZE:
				LDA WRPTR
				SEC
				SBC RDPTR
				RTS

BUFFERINIT:
				STA RDPTR
				STA WRPTR
				RTS
;---

;+-- INTERRUPT HANDLERS
IRQ:
NMI:
				PHA
				LDA SERSTAT						; Assume only source of IRQ is incoming data
				LDA SERDAT
				JSR WRBUFFER
				PLA
				RTI
;---

;+-- INCLUDED PROGRAMS ---
.INCLUDE "monarc.s"
.INCLUDE "oled.s"
.SEGMENT "IMAGE"
.INCBIN "Converter/image.bin"
;---

;+-- Vectors ---
.SEGMENT "RESETVEC"
				.ADDR	NMI		         	; NMI	vector 
				.ADDR RESET         	; RESET vector
				.ADDR IRQ		         	; IRQ vector
;---
