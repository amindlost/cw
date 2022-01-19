;*********************************************************************
;*   WLSYMTOK.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          09/11/95                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Clipper symbol token length lookup table                        *
;*                                                                   *
;*********************************************************************

Clipper5SymbolTable	EQU	$
 	DB	1	; 00, NOP
 	DB	25	; 01, PUSHC
 	DB	1	; 02, ERR1
 	DB	1	; 03, ERR2
 	DB	1	; 04, ERR3
 	DB	11	; 05, PUSHN
 	DB	3	; 06, POPF
 	DB	3	; 07, POPM
 	DB	3	; 08, POPQF
 	DB	3	; 09, PUSHA
 	DB	3	; 0a, PUSHF
 	DB	3	; 0b, PUSHM
 	DB	3	; 0c, PUSHMR
 	DB	3	; 0d, PUSHP
 	DB	3	; 0e, PUSHQF
 	DB	3	; 0f, PUSHV
 	DB	3	; 10, SFRAME
 	DB	3	; 11, SINIT
 	DB	3	; 12, SYMBOL
 	DB	3	; 13, SYMF
 	DB	1	; 14, ERR4
 	DB	1	; 15, ERR5
 	DB	1	; 16, ERR6
 	DB	1	; 17, ERR7
 	DB	1	; 18, ERR8
 	DB	3	; 19, BEGIN_SEQ
 	DB	3	; 1a, JDBG
 	DB	3	; 1b, JF
 	DB	3	; 1c, JFPT
 	DB	3	; 1d, JISW
 	DB	3	; 1e, JMP
 	DB	3	; 1f, JNEI
 	DB	3	; 20, JT
 	DB	3	; 21, JTPF
 	DB	1	; 22, ERR9
 	DB	3	; 23, PUSHBL
 	DB	3	; 24, ARRAYATI
 	DB	3	; 25, ARRAYPUTI
 	DB	3	; 26, CALL
 	DB	3	; 27, DO
 	DB	3	; 28, FRAME
 	DB	3	; 29, FUNC
 	DB	3	; 2a, LINE
 	DB	3	; 2b, MAKEA
 	DB	3	; 2c, MAKELA
 	DB	3	; 2d, PARAMS
 	DB	3	; 2e, POPFL
 	DB	3	; 2f, POPL
 	DB	3	; 30, POPS
 	DB	3	; 31, PRIVATES
 	DB	1	; 32, ERR10
 	DB	3	; 33, PUBLICS
 	DB	3	; 34, PUSHFL
 	DB	3	; 35, PUSHFLR
 	DB	3	; 36, PUSHI
 	DB	3	; 37, PUSHL
 	DB	3	; 38, PUSHLR
 	DB	3	; 39, PUSHS
 	DB	3	; 3a, PUSHSR
 	DB	3	; 3b, PUSHW
 	DB	3	; 3c, SEND
 	DB	3	; 3d, XBLOCK
 	DB	1	; 3e, ERR11
 	DB	1	; 3f, ERR12
 	DB	1	; 40, ERR13
 	DB	1	; 41, ERR14
 	DB	1	; 42, ERR15
 	DB	1	; 43, ERR16
 	DB	1	; 44, ERR17
 	DB	1	; 45, ERR18
 	DB	1	; 46, ERR19
 	DB	1	; 47, ERR20
 	DB	1	; 48, ERR21
 	DB	1	; 49, ERR22
 	DB	5	; 4a, MPOPF
 	DB	5	; 4b, MPOPM
 	DB	5	; 4c, MPOPQF
 	DB	5	; 4d, MPUSHA
 	DB	5	; 4e, MPUSHF
 	DB	5	; 4f, MPUSHM
 	DB	5	; 50, MPUSHMR
 	DB	5	; 51, MPUSHP
 	DB	5	; 52, MPUSHQF
 	DB	5	; 53, MPUSHV
 	DB	5	; 54, MSYMBOL
 	DB	5	; 55, MSYMF
 	DB	1	; 56, ABS
 	DB	1	; 57, AND
 	DB	1	; 58, ARRAYAT
 	DB	1	; 59, ARRAYPUT
 	DB	1	; 5a, BREAK
 	DB	1	; 5b, DEC
 	DB	1	; 5c, DIVIDE
 	DB	1	; 5d, DOOP
 	DB	1	; 5e, EEQ
 	DB	1	; 5f, ENDBLOCK
 	DB	1	; 60, ENDPROC
 	DB	1	; 61, END_SEQ
 	DB	1	; 62, EQ
 	DB	1	; 63, EVENTS
 	DB	1	; 64, FALSE
 	DB	1	; 65, GE
 	DB	1	; 66, GT
 	DB	1	; 67, INC
 	DB	1	; 68, LE
 	DB	1	; 69, LT
 	DB	1	; 6a, MINUS
 	DB	1	; 6b, MULT
 	DB	1	; 6c, NE
 	DB	1	; 6d, ERR23
 	DB	1	; 6e, NEGATE
 	DB	1	; 6f, NOP2
 	DB	1	; 70, NOT
 	DB	1	; 71, NULL
 	DB	1	; 72, ONE1
 	DB	1	; 73, OR
 	DB	1	; 74, PCOUNT
 	DB	1	; 75, PLUS
 	DB	1	; 76, POP
 	DB	1	; 77, PUSHRV
 	DB	1	; 78, QSELF
 	DB	1	; 79, SAVE_RET
 	DB	1	; 7a, TRUE
 	DB	1	; 7b, UNDEF
 	DB	1	; 7c, ZER0
 	DB	1	; 7d, ZZBLOCK
 	DB	1	; 7e, AXPRIN
 	DB	1	; 7f, AXPROUT
 	DB	1	; 80, BOF
 	DB	1	; 81, DELETED
 	DB	1	; 82, EOF
 	DB	1	; 83, FCOUNT
 	DB	1	; 84, FIELDNAME
 	DB	1	; 85, FLOCK
 	DB	1	; 86, FOUND
 	DB	1	; 87, FSELECT0
 	DB	1	; 88, FSELECT1
 	DB	1	; 89, LASTREC
 	DB	1	; 8a, LOCK
 	DB	1	; 8b, RECNO
 	DB	1	; 8c, BNAMES
 	DB	1	; 8d, LNAMES
 	DB	1	; 8e, SNAMES
 	DB	1	; 8f, SRCNAME
 	DB	1	; 90, TYPE
 	DB	1	; 91, WAVE
 	DB	1	; 92, WAVEA
 	DB	1	; 93, WAVEF
 	DB	1	; 94, WAVEL
 	DB	1	; 95, WAVEP
 	DB	1	; 96, WAVEPOP
 	DB	1	; 97, WAVEPOPF
 	DB	1	; 98, WAVEPOPQ
 	DB	1	; 99, WAVEQ
 	DB	1	; 9a, WSYMBOL
 	DB	1	; 9b, AADD
 	DB	1	; 9c, ASC
 	DB	1	; 9d, AT
 	DB	1	; 9e, CDOW
 	DB	1	; 9f, CHR
 	DB	1	; a0, CMONTH
 	DB	1	; a1, CTOD
 	DB	1	; a2, DATE
 	DB	1	; a3, DAY
 	DB	1	; a4, DOW
 	DB	1	; a5, DTOC
 	DB	1	; a6, DTOS
 	DB	1	; a7, EMPTY
 	DB	1	; a8, QEXP
 	DB	1	; a9, EXPON
 	DB	1	; aa, INSTR
 	DB	1	; ab, INT
 	DB	1	; ac, LEFT
 	DB	1	; ad, LEN
 	DB	1	; ae, LOGQ
 	DB	1	; af, LOWER
 	DB	1	; b0, LTRIM
 	DB	1	; b1, MAX
 	DB	1	; b2, MIN
 	DB	1	; b3, MODULUS
 	DB	1	; b4, MONTH
 	DB	1	; b5, REPLICATE
 	DB	1	; b6, ROUND
 	DB	1	; b7, SECONDS
 	DB	1	; b8, SPACE
 	DB	1	; b9, QSQRT
 	DB	1	; ba, STR1
 	DB	1	; bb, STR2
 	DB	1	; bc, STR3
 	DB	1	; bd, SUB2
 	DB	1	; be, SUB3
 	DB	1	; bf, TIME
 	DB	1	; c0, TRIM
 	DB	1	; c1, UPPER
 	DB	1	; c2, VAL
 	DB	1	; c3, VALTYPE
 	DB	1	; c4, WORD
 	DB	1	; c5, YEAR
 	DB	1	; c6, TRANS
 	DB	1	; c7, COL
 	DB	1	; c8, DEVPOS
 	DB	1	; c9, INKEY0
 	DB	1	; ca, INKEY1
 	DB	1	; cb, PCOL/INKEY2 (/Clipper 5.3)
 	DB	1	; cc, PROW/PCOL
 	DB	1	; cd, ROW/PROW
 	DB	1	; ce, SETPOS/ROW
 	DB	1	; cf, SETPOSBS/SETPOS
 	DB	1	; d0, SETPOSBS (Clipper 5.3)
 	DB	0	; d1,
 	DB	0	; d2,
 	DB	0	; d3,
 	DB	0	; d4,
 	DB	0	; d5,
 	DB	0	; d6,
 	DB	0	; d7,
 	DB	0	; d8,
 	DB	0	; d9,
 	DB	0	; da,
 	DB	0	; db,
 	DB	0	; dc,
 	DB	0	; dd,
 	DB	0	; de,
 	DB	0	; df,
 	DB	0	; e0,
 	DB	0	; e1,
 	DB	0	; e2,
 	DB	0	; e3,
 	DB	0	; e4,
 	DB	0	; e5,
 	DB	0	; e6,
 	DB	0	; e7,
 	DB	0	; e8,
 	DB	0	; e9,
 	DB	0	; ea,
 	DB	0	; eb,
 	DB	0	; ec,
 	DB	0	; ed,
 	DB	0	; ee,
 	DB	0	; ef,
 	DB	0	; f0,
 	DB	0	; f1,
 	DB	0	; f2,
 	DB	0	; f3,
 	DB	0	; f4,
 	DB	0	; f5,
 	DB	0	; f6,
 	DB	0	; f7,
 	DB	0	; f8,
 	DB	0	; f9,
 	DB	0	; fa,
 	DB	0	; fb,
 	DB	0	; fc,
 	DB	0	; fd,
 	DB	0	; fe,
 	DB	0	; ff,
