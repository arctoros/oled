.SEGMENT "MONARC"

; Original port by Winston Gayler with additional adaptations by Wendell Sander.
; Source code reverse-engineered and ported to CA65 assembler by Jeff Tranter <tranter@pobox.com>.
; Ported to Ben Eater's hardware configuration by reddit user @The8BitEnthusiast

.MACRO STR ARG
	.REPEAT .STRLEN(ARG), I
		.BYTE .STRAT(ARG, I) | $80
	.ENDREP
.ENDMACRO

; ***********************
; * APPLE-II            *
; * MINI-ASSEMBLER      *
; ***********************

FORMAT 	= $2E
LENGTH 	= $2F
A2_MODE = $31
PROMPT 	= $33
A2_YSAV = $34
A2_L 		= $35
PCL 		= $3A
PCH 		= $3B
A1H 		= $3D
A2L 		= $3E
A2H 		= $3F
A4L 		= $42
A4H 		= $43
FMT 		= $44
A2_IN 	= $200
CURSUP 	= $FC1A

REL:													; IS FMT COMPATIBLE
				SBC #$81
				LSR										; WITH RELATIVE MODE?
				BNE ERR3							; NO.
				LDY A2H
				LDX A2L								; DOUBLE DECREMENT
				BNE REL2
				DEY
REL2:
				DEX
				TXA
				CLC
				SBC PCL								; FORM ADDR-PC-2
				STA A2L
				BPL REL3
				INY
REL3:
				TYA
				SBC PCH
ERR3:													; ERROR IF >1-BYTE BRANCH

				BNE ERR
FINDOP:
				LDY LENGTH
FNDOP2:												; MOVE INST TO (PC)
				LDA A1H,Y
				STA (PCL),Y
				DEY
				BPL FNDOP2
				NOP
				NOP
				NOP
				NOP
				NOP
				NOP
				JSR INSTDSP						; TYPE FORMATTED LINE
				JSR PCADJ							; UPDATE PC
				STY PCH
				STA PCL
				JMP NXTLINE						; GET NEXT LINE
FAKEMON3:											; GO TO DELIM HANDLER
				JSR TOSUB
				LDY A2_YSAV						; RESTORE Y-INDEX
FAKEMON:											; READ PARAM
				JSR GETNUM
				STY A2_YSAV						; SAVE Y-INDEX
				LDY #$17							; INIT DELIMITER INDEX
FAKEMON2:											; CHECK NEXT DELIM
				DEY
				BMI RESETZ						; ERR IF UNRECOGNIZED DELIM
				CMP CHRTBL,Y					; COMPARE WITH DELIM TABLE
				BNE FAKEMON2					; NO MATCH
				CPY #$15							; MATCH, IS IT CR
				BNE FAKEMON3					; NO, HANDLE IT IN MONITOR
				LDA A2_MODE
				LDY #$0
				DEC A2_YSAV
				JSR BL1								; HANDLE CR OUTSIDE MONITOR
				JMP NXTLINE
TRYNEXT:											; GET TRIAL OPCODE
				LDA A1H
				JSR L6								; GET FMT+LENGTH FOR OPCODE
				TAX
				LDA MNEMR,X						; GET LOWER MNEMONIC BYTE
				CMP A4L								; MATCH?
				BNE NEXTOP						; NO, TRY NEXT OPCODE.
				LDA MNEML,X						; GET UPPER MNEMONIC BYTE
				CMP A4H								; MATCH?
				BNE NEXTOP						; NO, TRY NEXT OPCODE
				LDA FMT
				LDY FORMAT						; GET TRIAL FORMAT
				CPY #$9D							; TRIAL FORMAT RELATIVE?
				BEQ REL								; YES.
NREL:													; SAME FORMAT?
				CMP FORMAT
				BEQ FINDOP						; YES.
NEXTOP:												; NO, TRY NEXT OPCODE
				DEC A1H
				BNE TRYNEXT
				INC FMT								; NO MORE, TRY WITH LEN=2
				DEC A2_L							; WAS L=2 ALREADY?
				BEQ TRYNEXT						; NO.
ERR:													; YES, UNRECOGNIZED INST.

				LDY A2_YSAV
ERR2:
				TYA
				TAX
				JSR PRBL2							; PRINT ^ UNDER LAST READ
				LDA #$DE							; CHAR TO INDICATE ERROR
				JSR COUT							; POSITION.
RESETZ:
				JSR BELL
NXTLINE:											; '!'
				LDA #$A1
				STA PROMPT						; INITIALIZE PROMPT
				JSR GETLNZ						; GET LINE.
				JSR ZMODE							; INIT SCREEN STUFF
				LDA A2_IN							; GET CHAR
				CMP #$A0							; ASCII BLANK?
				BEQ SPACE							; YES
				INY
				CMP #$A4							; ASCII '$' IN COL 1?
				BEQ FAKEMON						; YES, SIMULATE MONITOR
				DEY										; NO, BACKUP A CHAR
				JSR GETNUM						; GET A NUMBER
				CMP #$93							; ':' TERMINATOR?
ERR4:													; NO, ERR.
				BNE ERR2
				TXA
				BEQ ERR2							; NO ADR PRECEDING COLON.
				JSR A1PCLP						; MOVE ADR TO PCL, PCH.
SPACE:												; COUNT OF CHARS IN MNEMONIC
				LDA #$3
				STA A1H
NXTMN:												; GET FIRST MNEM CHAR.
				JSR GETNSP
NXTM:
				ASL A
				SBC #$BE							; SUBTRACT OFFSET
				CMP #$C2							; LEGAL CHAR?
				BCC ERR2							; NO.
				ASL A									; COMPRESS-LEFT JUSTIFY
				ASL A
				LDX #$4
NXTM2:												; DO 5 TRIPLE WORD SHIFTS
				ASL A
				ROL A4L
				ROL A4H
				DEX
				BPL NXTM2
				DEC A1H								; DONE WITH 3 CHARS?
				BEQ NXTM2							; YES, BUT DO 1 MORE SHIFT
				BPL NXTMN							; NO
FORM1:												; 5 CHARS IN ADDR MODE
				LDX #$5
FORM2:												; GET FIRST CHAR OF ADDR
				JSR GETNSP
				STY A2_YSAV
				CMP CHAR1,X						; FIRST CHAR MATCH PATTERN?
				BNE FORM3							; NO
				JSR GETNSP						; YES, GET SECOND CHAR
				CMP CHAR2,X						; MATCHES SECOND HALF?
				BEQ FORM5							; YES.
				LDA CHAR2,X						; NO, IS SECOND HALF ZERO?
				BEQ FORM4							; YES.
				CMP #$A4							; NO,SECOND HALF OPTIONAL?
				BEQ FORM4							; YES.
				LDY A2_YSAV
FORM3:												; CLEAR BIT-NO MATCH
				CLC
FORM4:												; BACK UP 1 CHAR
				DEY
FORM5:												; FORM FORMAT BYTE
				ROL FMT
				CPX #$3								; TIME TO CHECK FOR ADDR.
				BNE FORM7							; NO
				JSR GETNUM						; YES
				LDA A2H
				BEQ FORM6							; HIGH-ORDER BYTE ZERO
				INX										; NO, INCR FOR 2-BYTE
FORM6:												; STORE LENGTH
				STX A2_L
				LDX #$3								; RELOAD FORMAT INDEX
				DEY										; BACKUP A CHAR
FORM7:												; SAVE INDEX
				STX A1H
				DEX										; DONE WITH FORMAT CHECK?
				BPL FORM2							; NO.
				LDA FMT								; YES, PUT LENGTH
				ASL A									; IN LOW BITS
				ASL A
				ORA A2_L
				CMP #$20
				BCS FORM8							; ADD "$" IF NONZERO LENGTH
				LDX A2_L							; AND DON'T ALREADY HAVE IT
				BEQ FORM8
				ORA #$80
FORM8:
				STA FMT
				STY A2_YSAV
				LDA A2_IN,Y						; GET NEXT NONBLANK
				CMP #$BB							; '' START OF COMMENT?
				BEQ FORM9							; YES
				CMP #$8D							; CARRIAGE RETURN?
				BNE ERR4							; NO, ERR.
FORM9:
				JMP TRYNEXT
GETNSP:
				LDA A2_IN,Y
				INY
				CMP #$A0							; GET NEXT NON BLANK CHAR
				BEQ GETNSP
				RTS

; ADD FILLER BYTES SO THAT THE MINI-ASSEMBLER STARTS AT THE DOCUMENTED
; ENTRY POINT AT ADDRESS $F666
				.BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
				.BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
				.BYTE $FF

				LDA ($3A,X)
				TAY
				LSR
				BCC L0+1
				LSR
				.BYTE $09							; ORA #
MINIASM:
				JMP RESETZ
L0:
				LDA ($3A,X)
L6:
				TAY
				LSR A
				BCC L1
				LSR A
				ORA #$80
				JMP INSDS2
L1:
				JMP IEVEN
L8:
				RTS
L3:														; BIT   $D012 ; CHARACTER OUT
				AND #$7F							; CLEAR HIGH BIT
				JSR CHROUT	
				RTS
L7:
				ORA #$80							; SET HIGH BIT
				CMP #$9B							; USE ESC FOR LINE KILL
				BNE L4
				LDA #$98
L4:														; USE CONTROL H FOR BACKSPACE

				CMP #$88
				BNE L5
				LDA #$DF							; UNDERSCORE OUT
L5:
				RTS

				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0

				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0

				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0

				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0,0,0,0,0,0,0,0,0,0
				.byte 0

; ***************************
; * APPLE II                *
; * SYSTEM MONITOR          *
; ***************************

LOC0 		= $00
LOC1 		= $01
WNDLFT 	= $20
WNDWDTH = $21
WNDTOP 	= $22
WNDBTM 	= $23
CH 			= $24
CV 			= $25
GBASL 	= $26
GBASH 	= $27
BASL 		= $28
BASH 		= $29
BAS2L 	= $2A
BAS2H 	= $2B
H2 			= $2C
LMNEM 	= $2C
RTNL 		= $2C
V2 			= $2D
RMNEM 	= $2D
RTNH 		= $2D
MASK 		= $2E
CHKSUM 	= $2E
LASTIN 	= $2F
A2_SIGN = $2F
COLOR 	= $30
INVFLG 	= $32
YSAV1 	= $35
CSWL 		= $36
CSWH 		= $37
KSWL 		= $38
KSWH 		= $39
XQT 		= $3C
A1L 		= $3C
A3L 		= $40
A3H 		= $41
A5L 		= $44
A5H 		= $45
ACC 		= $45
XREG 		= $46
YREG 		= $47
STATUS 	= $48
SPNT 		= $49
RNDL 		= $4E
RNDH 		= $4F
ACL 		= $50
ACH 		= $51
XTNDL 	= $52
XTNDH 	= $53
AUXL 		= $54
AUXH 		= $55
PICK 		= $95
USRADR 	= $03F8
IRQLOC 	= $03FE
IOADR 	= $C000
KBD 		= $C000
KBDSTRB = $C010
TAPEOUT = $C020
SPKR 		= $C030
TXTCLR 	= $C050
TXTSET 	= $C051
MIXCLR 	= $C052
MIXSET	= $C053
LOWSCR 	= $C054
HISCR 	= $C055
LORES 	= $C056
HIRES 	= $C057
TAPEIN 	= $C060
PADDL0 	= $C064
PTRIG 	= $C070
BASIC 	= $E000
BASIC2 	= $E2B3

PLOT:													; Y-COORD/2
				LSR
				PHP										; SAVE LSB IN CARRY
				JSR GBASCALC					; CALC BASE ADR IN GBASL,H
				PLP										; RESTORE LSB FROM CARRY
				LDA #$0F							; MASK $0F IF EVEN
				BCC RTMASK
				ADC #$E0							; MASK $F0 IF ODD
RTMASK:
				STA MASK
PLOT1:												; DATA
				LDA (GBASL),Y
				EOR COLOR							; EOR COLOR
				AND MASK							; AND MASK
				EOR (GBASL),Y					; XOR DATA
				STA (GBASL),Y					; TO DATA
				RTS
HLINE:												; PLOT SQUARE
				JSR PLOT
HLINE1:												; DONE?
				CPY H2
				BCS RTS1							; YES, RETURN
				INY										; NO, INCR INDEX (X-COORD)
				JSR PLOT1							; PLOT NEXT SQUARE
				BCC HLINE1						; ALWAYS TAKEN
VLINEZ:												; NEXT Y-COORD
				ADC #$01
VLINE:												; SAVE ON STACK
				PHA
				JSR PLOT							; PLOT SQUARE
				PLA
				CMP V2								; DONE?
				BCC VLINEZ						; NO, LOOP
RTS1:
				RTS
CLRSCR:												; MAX Y, FULL SCRN CLR
				LDY #$2F
				BNE CLRSC2						; ALWAYS TAKEN
CLRTOP:												; MAX Y, TOP SCREEN CLR
				LDY #$27
CLRSC2:												; STORE AS BOTTOM COORD
				STY V2
; FOR VLINE CALLS
				LDY #$27							; RIGHTMOST X-COORD (COLUMN)
CLRSC3:												; TOP COORD FOR VLINE CALLS
				LDA #$00
				STA COLOR							; CLEAR COLOR (BLACK)
				JSR VLINE							; DRAW VLINE
				DEY										; NEXT LEFTMOST X-COORD
				BPL CLRSC3						; LOOP UNTIL DONE
				RTS
GBASCALC:											; FOR INPUT 000DEFGH
				PHA
				LSR
				AND #$03
				ORA #$04							; GENERATE GBASH=000001FG
				STA GBASH
				PLA										; AND GBASL=HDEDE000
				AND #$18
				BCC GBCALC
				ADC #$7F
GBCALC:
				STA GBASL
				ASL A
				ASL A
				ORA GBASL
				STA GBASL
				RTS
NXTCOL:												; INCREMENT COLOR BY 3
				LDA COLOR
				CLC
				ADC #$03
SETCOL:												; SETS COLOR=17*A MOD 16
				AND #$0F
				STA COLOR
				ASL A									; BOTH HALF BYTES OF COLOR EQUAL
				ASL A
				ASL A
				ASL A
				ORA COLOR
				STA COLOR
				RTS
SCRN:													; READ SCREEN Y-COORD/2
				LSR A
				PHP										; SAVE LSB (CARRY)
				JSR GBASCALC					; CALC BASE ADDRESS
				LDA (GBASL),Y					; GET BYTE
				PLP										; RESTORE LSB FROM CARRY
SCRN2:												; IF EVEN, USE LO H
				BCC RTMSKZ
				LSR A
				LSR A
				LSR A									; SHIFT HIGH HALF BYTE DOWN
				LSR A
RTMSKZ:												; MASK 4-BITS
				AND #$0F
				RTS
INSDS1:												; PRINT PCL,H
				LDX PCL
				LDY PCH
				JSR PRYX2
				JSR PRBLNK						; FOLLOWED BY A BLANK
				JMP L0
				NOP
				NOP
				NOP
				NOP
INSDS2:
				BCS ERR1							; XXXXXX11 INVALID OP
				CMP #$A2
				BEQ ERR1							; OPCODE $89 INVALID
				AND #$87							; MASK BITS
IEVEN:												; LSB INTO CARRY FOR L/R TEST
				LSR A
				TAX
				LDA FMT1,X						; GET FORMAT INDEX BYTE
				JSR SCRN2							; R/L H-BYTE ON CARRY
				BNE GETFMT
ERR1:													; SUBSTITUTE $80 FOR INVALID OPS
				LDY #$80
				LDA #$00							; SET PRINT FORMAT INDEX TO 0
GETFMT:
				TAX
				LDA FMT2,X						; INDEX INTO PRINT FORMAT TABLE
				STA FORMAT						; SAVE FOR ADR FIELD FORMATTING
				AND #$03							; MASK FOR 2-BIT LENGTH (P=1 BYTE, 1=2 BYTE, 2=3 BYTE)
				STA LENGTH
				TYA										; OPCODE
				AND #$8F							; MASK FOR 1XXX1010 TEST
				TAX										; SAVE IT
				TYA										; OPCODE TO A AGAIN
				LDY #$03
				CPX #$8A
				BEQ MNNDX3
MNNDX1:
				LSR A
				BCC MNNDX3						; FORM INDEX INTO MNEMONIC TABLE
				LSR A
MNNDX2:												; 1) 1XXX1010->00101XXX
				LSR A
				ORA #$20							; 2) XXXYYY01->00111XXX
				DEY										; 3) XXXYYY10->00110XXX
				BNE MNNDX2						; 4) XXXYY100->00100XXX
				INY										; 5) XXXXX000->000XXXXX
MNNDX3:
				DEY
				BNE MNNDX1
				RTS
				.BYTE $FF,$FF,$FF
INSTDSP:											; GEN FMT, LEN BYTES
				JSR INSDS1
				PHA										; SAVE MNEMONIC TABLE INDEX
PRNTOP:
				LDA (PCL),Y
				JSR A2_PRBYTE
				LDX #$01							; PRINT 2 BLANKS
PRNTBL:
				JSR PRBL2
				CPY LENGTH						; PRINT INST (1-3 BYTES)
				INY										; IN A 12 CHR FIELD
				BCC PRNTOP
				LDX #$03							; CHAR COUNT FOR MNEMONIC PRINT
				CPY #$04
				BCC PRNTBL
				PLA										; RECOVER MNEMONIC INDEX
				TAY
				LDA MNEML,Y
				STA LMNEM							; FETCH 3-CHAR MNEMONIC
				LDA MNEMR,Y						; (PACKED IN 2-BYTES)
				STA RMNEM
PRMN1:
				LDA #$00
				LDY #$05
PRMN2:												; SHIFT 5 BITS OF
				ASL RMNEM
				ROL LMNEM							; CHARACTER INTO A
				ROL										; (CLEARS CARRY)
				DEY
				BNE PRMN2
				ADC #$BF							; ADD "?" OFFSET
				JSR COUT							; OUTPUT A CHAR OF MNEM
				DEX
				BNE PRMN1
				JSR PRBLNK						; OUTPUT 3 BLANKS
				LDY LENGTH
				LDX #$06							; CNT FOR 6 FORMAT BITS
PRADR1:
				CPX #$03
				BEQ PRADR5						; IF X=3 THEN ADDR.
PRADR2:
				ASL FORMAT
				BCC PRADR3
				LDA CHAR1-1,X
				JSR COUT
				LDA CHAR2-1,X
				BEQ PRADR3
				JSR COUT
PRADR3:
				DEX
				BNE PRADR1
				RTS
PRADR4:
				DEY
				BMI PRADR2
				JSR A2_PRBYTE
PRADR5:
				LDA FORMAT
				CMP #$E8							; HANDLE REL ADR MODE
				LDA (PCL),Y						; SPECIAL (PRINT TARGET,
				BCC PRADR4						; NOT OFFSET)
RELADR:
				JSR PCADJ3
				TAX										; PCL,PCH+OFFSET+1 TO A,Y
				INX
				BNE PRNTYX						; +1 TO Y,X
				INY
PRNTYX:
				TYA
PRNTAX:												; OUTPUT TARGET ADR
				JSR A2_PRBYTE
PRNTX:												; OF BRANCH AND RETURN
				TXA
				JMP A2_PRBYTE
PRBLNK:												; BLANK COUNT
				LDX #$03
PRBL2:												; LOAD A SPACE
				LDA #$A0
PRBL3:												; OUTPUT A BLANK
				JSR COUT
				DEX
				BNE PRBL2							; LOOP UNTIL COUNT=0
				RTS
PCADJ:												; 0=1-BYTE, 1=2-BYTE
				SEC
PCADJ2:												; 2=3-BYTE
				LDA LENGTH
PCADJ3:
				LDY PCH
				TAX										; TEST DISPLACEMENT SIGN
				BPL PCADJ4						; (FOR REL BRANCH)
				DEY										; EXTEND NEG BY DEC PCH
PCADJ4:
				ADC PCL
				BCC RTS2							; PCL+LENGTH(OR DISPL)+1 TO A
				INY										; CARRY INTO Y (PCH)
RTS2:
				RTS

; FMT1 BYTES: XXXXXXY0 INSTRS
; IF Y=0 THEN LEFT HALF BYTE
; IF Y=1 THEN RIGHT HALF BYTE
; (X=INDEX)
FMT1:
				.BYTE $04,$20,$54,$30,$0D
				.BYTE $80,$04,$90,$03,$22
				.BYTE $54,$33,$0D,$80,$04
				.BYTE $90,$04,$20,$54,$33
				.BYTE $0D,$80,$04,$90,$04
				.BYTE $20,$54,$3B,$0D,$80
				.BYTE $04,$90,$00,$22,$44
				.BYTE $33,$0D,$C8,$44,$00
				.BYTE $11,$22,$44,$33,$0D
				.BYTE $C8,$44,$A9,$01,$22
				.BYTE $44,$33,$0D,$80,$04
				.BYTE $90,$01,$22,$44,$33
				.BYTE $0D,$80,$04,$90
				.BYTE $26,$31,$87,$9A	; $ZZXXXY01 INSTR'S
FMT2:													; ERR
				.BYTE $00
				.BYTE $21							; IMM
				.BYTE $81							; Z-PAGE
				.BYTE $82							; ABS
				.BYTE $00							; IMPLIED
				.BYTE $00							; ACCUMULATOR
				.BYTE $59							; (ZPAG,X)
				.BYTE $4D							; (ZPAG),Y
				.BYTE $91							; ZPAG,X
				.BYTE $92							; ABS,X
				.BYTE $86							; ABS,Y
				.BYTE $4A							; (ABS)
				.BYTE $85							; ZPAG,Y
				.BYTE $9D							; RELATIVE
CHAR1:
				STR ",),#($"
CHAR2:
				.BYTE $D9,$00,$D8,$A4,$A4,$00
; CHAR2: "Y",0,"X$$",0
; MNEML IS OF FORM:
; (A) XXXXX000
; (B) XXXYY100
; (C) 1XXX1010
; (D) XXXYYY10
; (E) XXXYYY01
; (X=INDEX)
MNEML:
				.BYTE $1C,$8A,$1C,$23,$5D,$8B
				.BYTE $1B,$A1,$9D,$8A,$1D,$23
				.BYTE $9D,$8B,$1D,$A1,$00,$29
				.BYTE $19,$AE,$69,$A8,$19,$23
				.BYTE $24,$53,$1B,$23,$24,$53
				.BYTE $19,$A1					; (A) FORMAT ABOVE
				.BYTE $00,$1A,$5B,$5B,$A5,$69
				.BYTE $24,$24					; (B) FORMAT
				.BYTE $AE,$AE,$A8,$AD,$29,$00
				.BYTE $7C,$00					; (C) FORMAT
				.BYTE $15,$9C,$6D,$9C,$A5,$69
				.BYTE $29,$53					; (D) FORMAT
				.BYTE $84,$13,$34,$11,$A5,$69
				.BYTE $23,$A0					; (E) FORMAT
MNEMR:
				.BYTE $D8,$62,$5A,$48,$26,$62
				.BYTE $94,$88,$54,$44,$C8,$54
				.BYTE $68,$44,$E8,$94,$00,$B4
				.BYTE $08,$84,$74,$B4,$28,$6E
				.BYTE $74,$F4,$CC,$4A,$72,$F2
				.BYTE $A4,$8A					; (A) FORMAT
				.BYTE $00,$AA,$A2,$A2,$74,$74
				.BYTE $74,$72					; (B) FORMAT
				.BYTE $44,$68,$B2,$32,$B2,$00
				.BYTE $22,$00					; (C) FORMAT
				.BYTE $1A,$1A,$26,$26,$72,$72
				.BYTE $88,$C8					; (D) FORMAT
				.BYTE $C4,$CA,$26,$48,$44,$44
				.BYTE $A2,$C8					; (E) FORMAT
				.BYTE $FF,$FF,$FF

A2_STEP:											; DISASSEMBLE ONE INST
				JSR INSTDSP
				PLA										; AT (PCL,H)
				STA RTNL							; ADJUST TO USER
				PLA										; STACK. SAVE
				STA RTNH							; RTN ADR.
				LDX #$08
XQINIT:												; INIT XEQ AREA
				LDA INITBL-1,X
				STA XQT,X
				DEX
				BNE XQINIT
				LDA (PCL,X)						; USER OPCODE BYTE
				BEQ XBRK							; SPECIAL IF BREAK
				LDY LENGTH						; LEN FROM DISASSEMBLY
				CMP #$20
				BEQ XJSR							; HANDLE JSR, RTS, JMP,
				CMP #$60							; JMP (), RTI SPECIAL
				BEQ XRTS
				CMP #$4C
				BEQ XJMP
				CMP #$6C
				BEQ XJMPAT
				CMP #$40
				BEQ XRTI
				AND #$1F
				EOR #$14
				CMP #$04							; COPY USER INST TO XEQ AREA
				BEQ XQ2								; WITH TRAILING NOPS
XQ1:													; CHANGE REL BRANCH
				LDA (PCL),Y
XQ2:													; DISP TO 4 FOR
				STA XQT,Y
				DEY										; JMP TO BRANCH OR
				BPL XQ1								; NBRANCH FROM XEQ.
				JSR A2_RESTORE				; RESTORE USER REG CONTENTS.
				JMP XQT								; XEQ USER OP FROM RAM
IRQM:													; (RETURN TO NBRANCH)
				STA ACC
				PLA
				PHA										; **IRQ HANDLER
				ASL A
				ASL A
				ASL A
				BMI BREAK							; TEST FOR BREAK
				JMP (IRQLOC)					; USER ROUTINE VECTOR IN RAM
BREAK:
				PLP
				JSR SAV1							; SAVE REG'S ON BREAK
				PLA										; INCLUDING PC
				STA PCL
				PLA
				STA PCH
XBRK:													; PRINT USER PC.
				JSR INSDS1
				JSR RGDSP1						; AND REG'S
				JMP MON								; GO TO MONITOR
XRTI:
				CLC
				PLA										; SIMULATE RTI BY EXPECTING
				STA STATUS						; STATUS FROM STACK, THEN RTS
XRTS:													; RTS SIMULATION
				PLA
				STA PCL								; EXTRACT PC FROM STACK
				PLA										; AND UPDATE PC BY 1 (LEN=0)
PCINC2:
				STA PCH
PCINC3:												; UPDATE PC BY LEN
				LDA LENGTH
				JSR PCADJ3
				STY PCH
				CLC
				BCC NEWPCL
XJSR:
				CLC
				JSR PCADJ2						; UPDATE PC AND PUSH
				TAX										; ONTO STACH FOR
				TYA										; JSR SIMULATE
				PHA
				TXA
				PHA
				LDY #$02
XJMP:
				CLC
XJMPAT:
				LDA (PCL),Y
				TAX										; LOAD PC FOR JMP,
				DEY										; (JMP) SIMULATE.
				LDA (PCL),Y
				STX PCH
NEWPCL:
				STA PCL
				BCS XJMP
RTNJMP:
				LDA RTNH
				PHA
				LDA RTNL
				PHA
REGDSP:												; DISPLAY USER REG
				JSR CROUT
RGDSP1:												; CONTENTS WITH
				LDA #ACC
				STA A3L								; LABELS
				LDA #ACC/256
				STA A3H
				LDX #$FB
RDSP1:
				LDA #$A0
				JSR COUT
				LDA RTBL-$FB,X
				JSR COUT
				LDA #$BD
				JSR COUT
				LDA ACC+5,X
				JSR A2_PRBYTE
				INX
				BMI RDSP1
				RTS
BRANCH:												; BRANCH TAKEN,
				CLC
				LDY #$01							; ADD LEN+2 TO PC
				LDA (PCL),Y
				JSR PCADJ3
				STA PCL
				TYA
				SEC
				BCS PCINC2
NBRNCH:												; NORMAL RETURN AFTER
				JSR A2_SAVE
				SEC										; XEQ USER OF
				BCS PCINC3						; GO UPDATE PC
INITBL:
				NOP
				NOP										; DUMMY FILL FOR
				JMP NBRNCH						; XEQ AREA
				JMP BRANCH
RTBL:
				.BYTE $C1
				.BYTE $D8
				.BYTE $D9
				.BYTE $D0
				.BYTE $D3
PREAD:												; TRIGGER PADDLES
				LDA PTRIG
				LDY #$00							; INIT COUNT
				NOP										; COMPENSATE FOR 1ST COUNT
				NOP
PREAD2:												; COUNT Y-REG EVERY
				LDA PADDL0,X
				BPL RTS2D							; 12 USEC
				INY
				BNE PREAD2						; EXIT AT 255 MAX
				DEY
RTS2D:
				RTS
INIT:													; CLR STATUS FOR DEBUG
				LDA #$00
				STA STATUS						; SOFTWARE
				LDA LORES
				LDA LOWSCR						; INIT VIDEO MODE
SETTXT:												; SET FOR TEXT MODE
				LDA TXTSET
				LDA #$00							; FULL SCREEN WINDOW
				BEQ SETWND
SETGR:												; SET FOR GRAPHICS MODE
				LDA TXTCLR
				LDA MIXSET						; LOWER 4 LINES AS
				JSR CLRTOP						; TEXT WINDOW
				LDA #$14
SETWND:												; SET FOR 40 COL WINDOW
				STA WNDTOP
				LDA #$00							; TOP IN A-REG,
				STA WNDLFT						; BTTM AT LINE 24
				LDA #$28
				STA WNDWDTH
				LDA #$18
				STA WNDBTM						; VTAB TO ROW 23
				LDA #$17
TABV:													; VTABS TO ROW IN A-REG
				STA CV
				JMP VTAB
MULPM:												; ABS VAL OF AC AUX
				JSR MD1
MUL:													; INDEX FOR 16 BITS
				LDY #$10
MUL2:													; ACX * AUX + XTND
				LDA ACL
				LSR A									; TO AC, XTND
				BCC MUL4							; IF NO CARRY,
				CLC										; NO PARTIAL PROD.
				LDX #$FE
MUL3:													; ADD MPLCND (AUX)
				LDA XTNDL+2,X
				ADC AUXL+2,X					; TO PARTIAL PROD
				STA XTNDL+2,X					; (XTND)
				INX
				BNE MUL3
MUL4:
				LDX #$03
MUL5:
				.BYTE $76
				.BYTE $50
				DEX
				BPL MUL5
				DEY
				BNE MUL2
				RTS
DIVPM:												; ABS VAL OF AC, AUX.
				JSR MD1
A2_DIV:												; INDEX FOR 16 BITS
				LDY #$10
DIV2:
				ASL ACL
				ROL ACH
				ROL XTNDL							; XTND/AUX
				ROL XTNDH							; TO AC.
				SEC
				LDA XTNDL
				SBC AUXL							; MOD TO XTND.
				TAX
				LDA XTNDH
				SBC AUXH
				BCC DIV3
				STX XTNDL
				STA XTNDH
				INC ACL
DIV3:
				DEY
				BNE DIV2
				RTS
MD1:													; ABS VAL OF AC, AUX
				LDY #$00
				STY A2_SIGN						; WITH RESULT SIGN
				LDX #AUXL							; IN LSB OF SIGN.
				JSR MD3
				LDX #ACL
MD3:													; X SPECIFIES AC OR AUX
				LDA LOC1,X
				BPL MDRTS
				SEC
				TYA
				SBC LOC0,X						; COMPL SPECIFIED REG
				STA LOC0,X						; IF NEG.
				TYA
				SBC LOC1,X
				STA LOC1,X
				INC A2_SIGN
MDRTS:
				RTS
BASCALC:											; CALC BASE ADR IN BASL,H
				PHA
				LSR A									; FOR GIVEN LINE NO
				AND #$03							; 0<=LINE NO.<=$17
				ORA #$04							; ARG=000ABCDE, GENERATE
				STA BASH							; BASH=000001CD
				PLA										; AND
				AND #$18							; BASL=EABAB000
				BCC BSCLC2
				ADC #$7F
BSCLC2:
				STA BASL
				ASL
				ASL
				ORA BASL
				STA BASL
				RTS
BELL1:												; BELL CHAR? (CNTRL-G)
				CMP #$87
				BNE RTS2B							; NO, RETURN
				LDA #$40							; DELAY .01 SECONDS
				JSR A2_WAIT
				LDY #$C0
BELL2:												; TOGGLE SPEAKER AT
				LDA #$0C
				JSR A2_WAIT						; 1 KHZ FOR .1 SEC.
				LDA SPKR
				DEY
				BNE BELL2
RTS2B:
				RTS
STOADV:												; CURSOR H INDEX TO Y-REG
				LDY CH
				NOP
				NOP
ADVANCE:											; INCREMENT CURSOR H INDEX
				INC CH
				LDA CH								; (MOVE RIGHT)
				CMP WNDWDTH						; BEYOND WINDOW WIDTH?
				BCS A2_CR							; YES CR TO NEXT LINE
A2_RTS3:											; NO,RETURN
				RTS
VIDOUT:												; CONTROL CHAR?
				CMP #$A0
				BCS STOADV						; NO,OUTPUT IT.
				TAY										; INVERSE VIDEO?
				BPL STOADV						; YES, OUTPUT IT.
				CMP #$8D							; CR?
				BEQ A2_CR							; YES.
				CMP #$8A							; LINE FEED?
				BEQ A2_LF							; IF SO, DO IT.
				CMP #$88							; BACK SPACE? (CNTRL-H)
				BNE BELL1							; NO, CHECK FOR BELL.
BS1:													; DECREMENT CURSOR H INDEX
				DEC CH
				BPL A2_RTS3						; IF POS, OK. ELSE MOVE UP
				LDA WNDWDTH						; SET CH TO WNDWDTH-1
				STA CH
				DEC CH								; (RIGHTMOST SCREEN POS)
UP:														; CURSOR V INDEX
				LDA WNDTOP
				CMP CV
				BCS RTS4							; IF TOP LINE THEN RETURN
				DEC CV								; DEC CURSOR V-INDEX
VTAB:													; GET CURSOR V-INDEX
				LDA CV
VTABZ:												; GENERATE BASE ADR
				JSR BASCALC
				ADC WNDLFT						; ADD WINDOW LEFT INDEX
				STA BASL							; TO BASL
RTS4:
				RTS
ESC1:													; ESC?
				EOR #$C0
				BEQ HOME							; IF SO, DO HOME AND CLEAR
				ADC #$FD							; ESC-A OR B CHECK
				BCC ADVANCE						; A, ADVANCE
				BEQ BS1								; B, BACKSPACE
				ADC #$FD							; ESC-C OR D CHECK
				BCC A2_LF							; C, DOWN
				BEQ UP								; D, GO UP
				ADC #$FD							; ESC-E OR F CHECK
				BCC CLREOL						; E, CLEAR TO END OF LINE
				BNE RTS4							; NOT F, RETURN
CLREOP:												; CURSOR H TO Y INDEX
				LDY CH
				LDA CV								; CURSOR V TO A-REGISTER
CLEOP1:												; SAVE CURRENT LINE ON STK
				PHA
				JSR VTABZ							; CALC BASE ADDRESS
				JSR CLEOLZ						; CLEAR TO EOL, SET CARRY
				LDY #$00							; CLEAR FROM H INDEX=0 FOR REST
				PLA										; INCREMENT CURRENT LINE
				ADC #$00							; (CARRY IS SET)
				CMP WNDBTM						; DONE TO BOTTOM OF WINDOW?
				BCC CLEOP1						; NO, KEEP CLEARING LINES
				BCS VTAB							; YES, TAB TO CURRENT LINE
HOME:													; INIT CURSOR V
				LDA WNDTOP
				STA CV								; AND H-INDICES
				LDY #$00
				STY CH								; THEN CLEAR TO END OF PAGE
				BEQ CLEOP1
A2_CR:												; CURSOR TO LEFT OF INDEX
				LDA #$00
				STA CH								; (RET CURSOR H=0)
A2_LF:												; INCR CURSOR V(DOWN 1 LINE)
				INC CV
				LDA CV
				CMP WNDBTM						; OFF SCREEN?
				BCC VTABZ							; NO, SET BASE ADDR
				DEC CV								; DECR CURSOR V (BACK TO BOTTOM)
SCROLL:												; START AT TOP OF SCRL WNDW
				LDA WNDTOP
				PHA
				JSR VTABZ							; GENERATE BASE ADR
SCRL1:												; COPY BASL,H
				LDA BASL
				STA BAS2L							; TO BAS2L,H
				LDA BASH
				STA BAS2H
				LDY WNDWDTH						; INIT Y TO RIGHTMOST INDEX
				DEY										; OF SCROLLING WINDOW
				PLA
				ADC #$01							; INCR LINE NUMBER
				CMP WNDBTM						; DONE?
				BCS SCRL3							; YES, FINISH
				PHA
				JSR VTABZ							; FORM BASL,H (BASE ADDR)
SCRL2:												; MOVE A CHR UP ON LINE
				LDA (BASL),Y
				NOP
				NOP
				DEY										; NEXT CHAR OF LINE
				BPL SCRL2
				BMI SCRL1							; NEXT LINE (ALWAYS TAKEN)
SCRL3:												; CLEAR BOTTOM LINE
				LDY #$00
				JSR CLEOLZ						; GET BASE ADDR FOR BOTTOM LINE
				BCS VTAB							; CARRY IS SET
CLREOL:												; CURSOR H INDEX
				LDY CH
CLEOLZ:
				LDA #$A0
CLEOL2:
				NOP
				NOP
				INY										; TO END OF LINES (WNDWDTH)
				CPY WNDWDTH
				BCC CLEOL2
				RTS
A2_WAIT:
				SEC
WAIT2:
				PHA
WAIT3:
				SBC #$01
				BNE WAIT3							; 1.0204 USEC
				PLA										; (13+27/2*A+5/2*A*A)
				SBC #$01
				BNE WAIT2
				RTS
NXTA4:												; INCR 2-BYTE A4
				INC A4L
				BNE NXTA1							; AND A1
				INC A4H
NXTA1:												; INCR 2-BYTE A1.
				LDA A1L
				CMP A2L
				LDA A1H								; AND COMPARE TO A2
				SBC A2H
				INC A1L								; (CARRY SET IF >=)
				BNE RTS4B
				INC A1H
RTS4B:
				RTS
HEADR:												; WRITE A*256 'LONG 1'
				LDY #$48
				JSR ZERDLY						; HALF CYCLES
				BNE HEADR							; (650 USEC EACH)
				ADC #$FE
				BCS HEADR							; THEN A 'SHORT 0'
				LDY #$21							; (400 USEC)
WRBIT:												; WRITE TWO HALF CYCLES
				JSR ZERDLY
				INY										; OF 250 USEC ('0')
				INY										; OR 500 USEC ('0')
ZERDLY:
				DEY
				BNE ZERDLY
				BCC WRTAPE						; Y IS COUNT FOR
				LDY #$32							; TIMING LOOP
ONEDLY:
				DEY
				BNE ONEDLY
WRTAPE:
				LDY IOADR,X
				LDY #$2C
				DEX
				RTS
RDBYTE:												; 8 BITS TO READ
				LDX #$08
RDBYT2:												; READ TWO TRANSITIONS
				PHA
				JSR RD2BIT						; (FIND EDGE)
				PLA
				ROL										; NEXT BIT
				LDY #$3A							; COUNT FOR SAMPLES
				DEX
				BNE RDBYT2
				RTS
RD2BIT:
				JSR RDBIT
RDBIT:												; DECR Y UNTIL
				DEY
				LDA $C081							; TAPE TRANSITION
				CMP LASTIN
				BEQ RDBIT
				STA LASTIN
				CPY #$80							; SET CARRY ON Y
				RTS
				NOP
				NOP
RDKEY:
				LDY CH
				LDA (BASL),Y					; SET SCREEN TO FLASH
				PHA
				AND #$3F
				ORA #$40
				NOP
				NOP
				PLA
				NOP
				NOP
				NOP
KEYIN:
				INC RNDL
				BNE KEYIN2						; INCR RND NUMBER
				INC RNDH
KEYIN2:
				JSR CHRIN
				BCC KEYIN
				NOP
				NOP
				JMP L7
				NOP
ESC:													; GET KEYCODE
				JSR RDKEY
				JSR ESC1							; HANDLE ESC FUNC.
RDCHAR:												; READ KEY
				JSR RDKEY
				CMP #$9B							; ESC?
				BEQ ESC								; YES, DON'T RETURN
				RTS
A2_NOTCR:
				LDA INVFLG
				PHA
				LDA #$FF
				STA INVFLG						; ECHO USER LINE
				LDA A2_IN,X						; NON INVERSE
				NOP
				NOP
				NOP
				PLA
				STA INVFLG
				LDA A2_IN,X
				CMP #$DF							; CHECK FOR EDIT KEYS
				BEQ BCKSPC						; BS, CTRL-X
				CMP #$98
				BEQ CANCEL
				CPX #$F8							; MARGIN?
				BCC NOTCR1
				JSR BELL							; YES, SOUND BELL
NOTCR1:												; ADVANCE INPUT INDEX
				INX
				BNE NXTCHAR
CANCEL:												; BACKSLASH AFTER CANCELLED LINE
				LDA #$DC
				JSR COUT
GETLNZ:												; OUTPUT CR
				JSR CROUT
A2_GETLN:
				LDA PROMPT
				JSR COUT							; OUTPUT PROMPT CHAR
				LDX #$01							; INIT INPUT INDEX
BCKSPC:												; WILL BACKSPACE TO 0
				TXA
				BEQ GETLNZ
				DEX
NXTCHAR:
				JSR RDCHAR
				CMP #PICK							; USE SCREEN CHAR
				BNE CAPTST						; FOR CTRL-U
				LDA (BASL),Y
CAPTST:
				CMP #$E0
				BCC ADDINP						; CONVERT TO CAPS
				AND #$DF
ADDINP:												; ADD TO INPUT BUF
				STA A2_IN,X
				CMP #$8D
				BNE A2_NOTCR
				JSR CLREOL						; CLR TO EOL IF CR
CROUT:
				LDA #$8D
				BNE COUT
PRA1:													; PRINT CR,A1 IN HEX
				LDY A1H
				LDX A1L
PRYX2:
				JSR CROUT
				JSR PRNTYX
				LDY #$00
				LDA #$AD							; PRINT '-'
				JMP COUT
XAM8:
				LDA A1L
				ORA #$07							; SET TO FINISH AT
				STA A2L								; MOD 8=7
				LDA A1H
				STA A2H
MODSCHK:
				LDA A1L
				AND #$07
				BNE DATAOUT
XAM:
				JSR PRA1
DATAOUT:
				LDA #$A0
				JSR COUT							; OUTPUT BLANK
				LDA (A1L),Y
				JSR A2_PRBYTE					; OUTPUT BYTE IN HEX
				JSR NXTA1
				BCC MODSCHK						; CHECK IF TIME TO,
RTS4C:												; PRINT ADDR
				RTS
XAMPM:												; DETERMINE IF MON
				LSR A
				BCC XAM								; MODE IS XAM
				LSR A									; ADD, OR SUB
				LSR A
				LDA A2L
				BCC @ADD
				EOR #$FF							; SUB: FORM 2'S COMPLEMENT
@ADD:
				ADC A1L
				PHA
				LDA #$BD
				JSR COUT							; PRINT '=', THEN RESULT
				PLA
A2_PRBYTE:										; PRINT BYTE AS 2 HEX
				PHA
				LSR A									; DIGITS, DESTROYS A-REG
				LSR A
				LSR A
				LSR A
				JSR PRHEXZ
				PLA
A2_PRHEX:											; PRINT HEX DIG IN A-REG
				AND #$0F
PRHEXZ:												; LSB'S
				ORA #$B0
				CMP #$BA
				BCC COUT
				ADC #$06
COUT:
				JSR L3
COUT1:
				CMP #$A0
				BCC COUTZ							; DON'T OUTPUT CTRL'S INVERSE
				AND INVFLG						; MASK WITH INVERSE FLAG
COUTZ:												; SAV Y-REG
				STY YSAV1
				PHA										; SAV A-REG
				JSR VIDOUT						; OUTPUT A-REG AS ASCII
				PLA										; RESTORE A-REG
				LDY YSAV1							; AND Y-REG
				RTS										; THEN RETURN
BL1:
				DEC A2_YSAV
				BEQ XAM8
BLANK:												; BLANK TO MON
				DEX
				BNE SETMDZ						; AFTER BLANK
				CMP #$BA							; DATA STORE MODE?
				BNE XAMPM							; NO, XAM, ADD, OR SUB
STOR:													; KEEP IN STORE MODE
				STA A2_MODE
				LDA A2L
				STA (A3L),Y						; STORE AS LOW BYTE AS (A3)
				INC A3L
				BNE RTS5							; INCR A3, RETURN
				INC A3H
RTS5:
				RTS
A2_SETMODE:										; SAVE CONVERTED ':', '+',
				LDY A2_YSAV
				LDA A2_IN-1,Y					; '-', '.' AS MODE.
SETMDZ:
				STA A2_MODE
				RTS
LT:
				LDX #$01
LT2:													; COPY A2 (2 BYTES) TO
				LDA A2L,X
				STA A4L,X							; A4 AND A5
				STA A5L,X
				DEX
				BPL LT2
				RTS
MOVE:													; MOVE (A1 TO A2) TO
				LDA (A1L),Y
				STA (A4L),Y						; (A4)
				JSR NXTA4
				BCC MOVE
				RTS
VFY:													; VERIFY (A1 TO A2) WITH
				LDA (A1L),Y
				CMP (A4L),Y						; (A4)
				BEQ VFYOK
				JSR PRA1
				LDA (A1L),Y
				JSR A2_PRBYTE
				LDA #$A0
				JSR COUT
				LDA #$A8
				JSR COUT
				LDA (A4L),Y
				JSR A2_PRBYTE
				LDA #$A9
				JSR COUT
VFYOK:
				JSR NXTA4
				BCC VFY
				RTS
A2_LIST:											; MOVE A1 (2 BYTES) TO
				JSR A1PC
				LDA #$14							; PC IF SPEC'D AND
LIST2:												; DISEMBLE 20 INSTRS
				PHA
				JSR INSTDSP
				JSR PCADJ							; ADJUST PC EACH INSTR
				STA PCL
				STY PCH
				PLA
				SEC
				SBC #$01							; NEXT OF 20 INSTRS
				BNE LIST2
				RTS
A1PC:													; IF USER SPEC'D ADR
				TXA
				BEQ A1PCRTS						; COPY FROM A1 TO PC
A1PCLP:
				LDA A1L,X
				STA PCL,X
				DEX
				BPL A1PCLP
A1PCRTS:
				RTS
SETINV:												; SET FOR INVERSE VID
				LDY #$3F
				BNE SETIFLG						; VIA COUT1
SETNORM:											; SET FOR NORMAL VID
				LDY #$FF
SETIFLG:
				STY INVFLG
				RTS
SETKBD:												; SIMULATE PORT #0 INPUT
				LDA #$00
INPORT:												; SPECIFIED (KEYIN ROUTINE)
				STA A2L
A2_INPRT:
				LDX #KSWL
				LDY #<KEYIN
				BNE IOPRT
SETVID:												; SIMULATE PORT #0 OUTPUT
				LDA #$00
OUTPORT:											; SPECIFIED (COUT1 ROUTINE)
				STA A2L
OUTPRT:
				LDX #CSWL
				LDY #<COUT1
IOPRT:												; SET RAM IN/OUT VECTORS
				LDA A2L
				AND #$0F
				BEQ IOPRT1
				ORA #>IOADR
				LDY #$00
				BEQ IOPRT2
IOPRT1:
				LDA #$FD
IOPRT2:
				STY LOC0,X
				STA LOC1,X
				RTS
				NOP
				NOP
XBASIC:												; TO BASIC WITH SCRATCH
				JMP BASIC
BASCONT:											; CONTINUE BASIC
				JMP BASIC2
GO:														; ADR TO PC IF SPEC'D
				JSR A1PC
				JSR A2_RESTORE				; RESTORE META REGS
				JMP (PCL)							; GO TO USER SUBR
REGZ:													; TO REG DISPLAY
				JMP REGDSP
TRACE:
				DEC A2_YSAV
STEPZ:												; ADR TO PC IF SPEC'D
				JSR A1PC
				JMP A2_STEP						; TAKE ONE STEP
A2_USR:												; TO USR SUBR AT USRADR
				JMP USRADR
WRITE:
				LDA #$40
				JSR HEADR							; WRITE 10-SEC HEADER
				LDY #$27
WR1:
				LDX #$00
				EOR (A1L,X)
				PHA
				LDA (A1L,X)
				JSR WRBYTE
				JSR NXTA1
				LDY #$1D
				PLA
				BCC WR1
				LDY #$22
				JSR WRBYTE
				BEQ BELL
WRBYTE:
				LDX #$10
WRBYT2:
				ASL A
				JSR WRBIT
				BNE WRBYT2
				RTS
CRMON:												; HANDLE A CR AS BLANK
				JSR BL1
				PLA										; THEN POP STACK
				PLA										; AND RTN TO MON
				BNE MONZ
A2_READ:											; FIND TAPEIN EDGE
				JSR RD2BIT
				LDA #$16
				JSR HEADR							; DELAY 3.5 SECONDS
				STA CHKSUM						; INIT CHKSUM=$FF
				JSR RD2BIT						; FIND TAPEIN EDGE
RD2:													; LOOK FOR SYNC BIT
				LDY #$24
				JSR RDBIT							; (SHORT 0)
				BCS RD2								; LOOP UNTIL FOUND
				JSR RDBIT							; SKIP SECOND SYNC H-CYCLE
				LDY #$3B							; INDEX FOR 0/1 TEST
RD3:													; READ A BYTE
				JSR RDBYTE
				STA (A1L,X)						; STORE AT (A1)
				EOR CHKSUM
				STA CHKSUM						; UPDATE RUNNING CHKSUM
				JSR NXTA1							; INC A1, COMPARE TO A2
				LDY #$35							; COMPENSATE 0/1 INDEX
				BCC RD3								; LOOP UNTIL DONE
				JSR RDBYTE						; READ CHKSUM BYTE
				CMP CHKSUM
				BEQ BELL							; GOOD, SOUND BELL AND RETURN
PRERR:
				LDA #$C5
				JSR COUT							; PRINT "ERR", THEN BELL
				LDA #$D2
				JSR COUT
				JSR COUT
BELL:													; OUTPUT BELL AND RETURN
				LDA #$87
				JMP COUT
A2_RESTORE:										; RESTORE 6502 REG CONTENTS
				LDA STATUS
				PHA										; USED BY DEBUG SOFTWARE
				LDA ACC
RESTR1:
				LDX XREG
				LDY YREG
				PLP
				RTS
A2_SAVE:											; SAVE 6502 REG CONTENTS
				STA ACC
SAV1:
				STX XREG
				STY YREG
				PHP
				PLA
				STA STATUS
				TSX
				STX SPNT
				CLD
				RTS
A2_RESET:											; SET SCREEN MODE
				JSR L8
				JSR INIT							; AND INIT KBD/SCREEN
				JSR SETNORM						; AS I/O DEV'S
				JSR SETKBD
MON:													; MUST SET HEX MODE!
				CLD
				JSR BELL
MONZ:													; '*' PROMPT FOR MON
				LDA #$AA
				STA PROMPT
				JSR GETLNZ						; READ A LINE
				JSR ZMODE							; CLEAR MON MODE, SCAN IDX
NXTITM:												; GET ITEM, NON-HEX
				JSR GETNUM
				STY A2_YSAV						; CHAR IN A-REG
				LDY #$17							; X-REG=0 IF NO HEX INPUT
CHRSRCH:
				DEY
				BMI MON								; NOT FOUND, GO TO MON
				CMP CHRTBL,Y					; FIND CMND CHAR IN TEL
				BNE CHRSRCH
				JSR TOSUB							; FOUND, CALL CORRESPONDING
				LDY A2_YSAV						; SUBROUTINE
				JMP NXTITM
A2_DIG:
				LDX #$03
				ASL A
				ASL A									; GOT HEX DIG,
				ASL A									; SHIFT INTO A2
				ASL A
NXTBIT:
				ASL A
				ROL A2L
				ROL A2H
				DEX										; LEAVE X=$FF IF DIG
				BPL NXTBIT
NXTBAS:
				LDA A2_MODE
				BNE NXTBS2						; IF MODE IS ZERO
				LDA A2H,X							; THEN COPY A2 TO
				STA A1H,X							; A1 AND A3
				STA A3H,X
NXTBS2:
				INX
				BEQ NXTBAS
				BNE NXTCHR
GETNUM:												; CLEAR A2
				LDX #$00
				STX A2L
				STX A2H
NXTCHR:												; GET CHAR
				LDA A2_IN,Y
				INY
				EOR #$B0
				CMP #$0A
				BCC A2_DIG						; IF HEX DIG, THEN
				ADC #$88
				CMP #$FA
				BCS A2_DIG
				RTS
TOSUB:												; PUSH HIGH-ORDER
				LDA #>GO
				PHA										; SUBR ADR ON STK
				LDA SUBTBL,Y					; PUSH LOW-ORDER
				PHA										; SUBR ADR ON STK
				LDA A2_MODE
ZMODE:												; CLR MODE, OLD MODE
				LDY #$00
				STY A2_MODE						; TO A-REG
				RTS										; GO TO SUBR VIA RTS

CHRTBL:
				.BYTE $BC							; F("CTRL-C")
				.BYTE $B2							; F("CTRL-Y")
				.BYTE $BE							; F("CTRL-E")
				.BYTE $ED							; F("T")
				.BYTE $EF							; F("V")
				.BYTE $C4							; F("CTRL-K")
				.BYTE $EC							; F("S")
				.BYTE $A9							; F("CTRL-P")
				.BYTE $BB							; F("CTRL-B")
				.BYTE $A6							; F("-")
				.BYTE $A4							; F("+")
				.BYTE $06							; F("M") (F=EX-OR $B0+$89)
				.BYTE $95							; F("<")
				.BYTE $07							; F("N")
				.BYTE $02							; F("I")
				.BYTE $05							; F("L")
				.BYTE $F0							; F("W")
				.BYTE $00							; F("G")
				.BYTE $EB							; F("R")
				.BYTE $93							; F(":")
				.BYTE $A7							; F(".")
				.BYTE $C6							; F("CR")
				.BYTE $99							; F(BLANK)

SUBTBL:
				.BYTE <BASCONT-1
				.BYTE <A2_USR-1
				.BYTE <REGZ-1
				.BYTE <TRACE-1
				.BYTE <VFY-1
				.BYTE <A2_INPRT-1
				.BYTE <STEPZ-1
				.BYTE <OUTPRT-1
				.BYTE <XBASIC-1
				.BYTE <A2_SETMODE-1
				.BYTE <A2_SETMODE-1
				.BYTE <MOVE-1
				.BYTE <LT-1
				.BYTE <SETNORM-1
				.BYTE <SETINV-1
				.BYTE <A2_LIST-1
				.BYTE <WRITE-1
				.BYTE <GO-1
				.BYTE <A2_READ-1
				.BYTE <A2_SETMODE-1
				.BYTE <A2_SETMODE-1
				.BYTE <CRMON-1
				.BYTE <BLANK-1

XQTNZ = $3C
