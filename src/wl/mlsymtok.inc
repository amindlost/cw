;*********************************************************************
;*   MLSYMTOK.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          07/11/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Clipper symbol token length lookup table                        *
;*                                                                   *
;*********************************************************************

summer87_table	EQU	$
	DB	0	; illegal, 0
	DB	1 	; ABS, 1
	DB	1	; ACCEPT, 2
	DB	1	; ACCUM, 3
	DB	1	; ADD_FIELD, 4
	DB	1	; AND, 5
	DB	1	; APPEND, 6
	DB	1	; ARRAY, 7
	DB	1	; ARROW, 8
	DB	1	; ASC, 9
	DB	1	; AT, a
	DB	3	; BEGIN_SEQ, b
	DB	1	; BOF, c
	DB	1	; BOX, d
	DB	1	; BREAK, e
	DB	1	; CALL, f
	DB	1	; CDOW, 10
	DB	3	; CHK_PC, 11
	DB	1	; CHR, 12
	DB	1	; CLR_RELATE, 13
	DB	1	; CMONTH, 14
	DB	1	; COL, 15
	DB	1	; CTOD, 16
	DB	1	; C_ALL, 17
	DB	1	; C_DBF, 18
	DB	1	; C_EOL, 19
	DB	1	; C_EOS, 1a
	DB	1	; C_GETS, 1b
	DB	1	; C_SCREEN, 1c
	DB	1	; DATE, 1d
	DB	1	; DAY, 1e
	DB	1	; DBF_CLOSE, 1f
	DB	1	; DBF_COPY, 20
	DB	1	; DBF_CREATE, 21
	DB	3	; DBF_OPEN, 22
	DB	3	; DBF_SELECT, 23
	DB	3	; DBF_SET, 24
	DB	1	; DBF_STRUX, 25
	DB	3	; DBXBAG, 26
	DB	3	; DECLARE, 27
	DB	1	; DELETE, 28
	DB	1	; DEL_FLAG, 29
	DB	1	; DEL_MARK, 2a
	DB	1	; DIVIDE, 2b
	DB	1	; DO, 2c
	DB	3	; DOS_CALL, 2d
	DB	1	; DOW, 2e
	DB	1	; DTOC, 2f
	DB	1	; DTOS, 30
	DB	1	; EEQ, 31 A
	DB	1	; EJECT, 32
	DB	1	; EMPTY, 33
	DB	1	; ENDPROC, 34
	DB	1	; END_SEQ, 35
	DB	3	; QENVIRON, 36
	DB	1	; EOF, 37
	DB	1	; EQ, 38 S
	DB	1	; EVENTS, 39
	DB	1	; QEXP, 3a
	DB	1	; EXPON, 3b
	DB	1	; FALIAS0, 3c
	DB	1	; FALIAS1, 3d
	DB	1	; FALSE, 3e
	DB	1	; FCOUNT, 3f
	DB	1	; FIELDNAME, 40
	DB	1	; ISFILE, 41
	DB	1	; FLOCK, 42
	DB	1	; FOUND, 43
	DB	1	; FSELECT0, 44
	DB	1	; FSELECT1, 45
	DB	1	; FUNC, 46
	DB	1	; GE, 47
	DB	1	; GET, 48
	DB	1	; GET_RANGE, 49
	DB	1	; GET_SP, 4a
	DB	1	; GET_VALID, 4b
	DB	1	; GOTO, 4c
	DB	1	; GOTO_BOTT, 4d
	DB	1	; GOTO_TOP, 4e
	DB	1	; GT, 4f
	DB	0	; UNKNOWN, 50
	DB	1	; INKEY0, 51
	DB	1	; INKEY1, 52
	DB	1	; INPUT, 53
	DB	1	; INSTR, 54
	DB	1	; INT, 55
	DB	1	; ISCOLOR, 56
	DB	3	; JEOF, 57
	DB	3	; JFALSE, 58
	DB	3	; JUMP, 59
	DB	3	; JTRUE, 5a
	DB	1	; KEYBOARD, 5b
	DB	0	; UNKNOWN, 5c
	DB	1	; LASTKEY, 5d
	DB	1	; LASTREC, 5e
	DB	1	; LBL, 5f
	DB	3	; LBL_BEGIN, 60
	DB	1	; LBL_END, 61
	DB	1	; LE, 62
	DB	1	; LEN, 63
	DB	3	; LINE, 64
	DB	1	; LOCATEP, 65
	DB	1	; LOCK, 66
	DB	1	; LOGQ, 67
	DB	1	; LOWER, 68
	DB	1	; LT, 69
	DB	1	; LTRIM, 6a
	DB	1	; MAX, 6b
	DB	1	; MEMORY, 6c
	DB	1	; MENU, 6d
	DB	1	; MIN, 6e
	DB	1	; MINUS, 6f
	DB	1	; MODULUS, 70
	DB	1	; MONTH, 71
	DB	1	; MULT, 72
	DB	1	; M_RESTORE, 73
	DB	1	; M_SAVE, 74
	DB	1	; NE, 75
	DB	1	; NEGATE, 76
	DB	1	; NETNAME, 77
	DB	3	; NEXT, 78
	DB	1	; NOT, 79
	DB	1	; NTX_CLOSE, 7a
	DB	1	; NTX_CREATE, 7b
	DB	1	; NTX_KEY, 7c
	DB	1	; NTX_OPEN, 7d
	DB	1	; NTX_ORDER, 7e
	DB	1	; CNULL, 7f
	DB	1	; ONE1, 80
	DB	1	; OR, 81
	DB	1	; PACK, 82
	DB	1	; PARAM, 83
	DB	1	; PCOL, 84
	DB	1	; PCOUNT, 85
	DB	1	; PLUS, 86
	DB	1	; POP, 87
	DB	1	; POPF, 88
	DB	1	; POPM, 89
	DB	1	; POPM2, 8a
	DB	1	; POS_XY, 8b
	DB	1	; PRINT, 8c
	DB	1	; PRINTNL, 8d
	DB	1	; PRINTS, 8e
	DB	1	; PRIVATE, 8f
	DB	0	; UNKNOWN, 90
	DB	1	; PROCLINE, 91
	DB	1	; PROCNAME, 92
	DB	1	; PROMPT, 93
	DB	1	; PROW, 94
	DB	1	; PUBLIC, 95
	DB	3	; PUSHA, 96
	DB	25	; PUSHSTR, 97
	DB	0	; UNKNOWN, 98
	DB	3	; PUSHINT, 99
	DB	3	; PUSHM, 9a
	DB	11	; PUSHDBL, 9b
	DB	3	; PUSHP, 9c
	DB	1	; PUSHS, 9d
	DB	3	; PUSHV, 9e
	DB	3	; PUSHKEY, 9f
	DB	1	; PUTM0, a0
	DB	1	; QUIT, a1 S
	DB	1	; READ, a2
	DB	1	; READVAR, a3
	DB	1	; RECALL, a4
	DB	1	; RECNO, a5
	DB	3	; REEF, a6
	DB	1	; REINDEX, a7
	DB	1	; RELEASE, a8
	DB	1	; REP, a9
	DB	1	; REPLICATE, aa
	DB	3	; REP_BEGIN, ab
	DB	1	; REP_END, ac
	DB	3	; REWIND, ad
	DB	1	; ROUND, ae
	DB	1	; ROW, af
	DB	1	; SAVE_RET, b0
	DB	1	; SAY, b1
	DB	3	; SCOPE_DEC, b2
	DB	3	; SCOPE_SET, b3
	DB	1	; SCR_RESTORE, b4
	DB	1	; SCR_SAVE, b5
	DB	1	; SECONDS, b6
	DB	1	; SEEK, b7
	DB	1	; SELECT, b8
	DB	1	; SET_ALTER, b9
	DB	1	; SET_COLOR, ba
	DB	3	; SET_DATE, bb
	DB	1	; SET_DECIMALS, bc
	DB	1	; SET_DEFAULT, bd
	DB	1	; SET_DELIM, be
	DB	3	; SET_DEVICE, bf
	DB	1	; SET_FILTER, c0
	DB	1	; SET_FMT, c1
	DB	1	; SET_FUNCTION, c2
	DB	1	; SET_KEY, c3
	DB	1	; SET_LOCATE, c4
	DB	1	; SET_MARGIN, c5
	DB	3	; SET_OFF, c6
	DB	3	; SET_ON, c7
	DB	1	; SET_PATH, c8
	DB	3	; SET_PC, c9
	DB	1	; SET_PRINTER, ca
	DB	1	; SET_RELATE, cb
	DB	1	; SKIP, cc
	DB	1	; SORT, cd
	DB	3	; SORT_BEGIN, ce
	DB	1	; SORT_END, cf
	DB	1	; SPACE, d0
	DB	1	; SPUSHM, d1
	DB	1	; QSQRT, d2
	DB	1	; STR1, d3
	DB	1	; STR2, d4
	DB	1	; STR3, d5
	DB	1	; SUB2, d6
	DB	1	; SUB3, d7
	DB	3	; SYMBOL, d8
	DB	1	; TIME, d9
	DB	3	; TO_DEST, da
	DB	1	; TRANS, db
	DB	1	; TRIM, dc
	DB	1	; TRUE, dd
	DB	1	; TYPE, de
	DB	3	; UNLOCK, df
	DB	1	; UPDATED, e0
	DB	1	; UPPER, e1
	DB	1	; USED, e2
	DB	1	; VAL, e3
	DB	1	; WAIT, e4
	DB	1	; WAVE, e5
	DB	1	; WAVEA, e6
	DB	1	; WAVEL, e7
	DB	1	; WAVEP, e8
	DB	1	; WORD, e9
	DB	3	; WSET, ea
	DB	1	; WSYMBOL, eb
	DB	1	; XRELEASE, ec
	DB	1	; YEAR, ed
	DB	1	; ZAP, ee
	DB	1	; ZER0, ef
	DB	0	; ILLEGAL, f0
	DB	0	; ILLEGAL, f1
	DB	0	; ILLEGAL, f2
	DB	0	; ILLEGAL, f3
	DB	0	; ILLEGAL, f4
	DB	0	; ILLEGAL, f5
	DB	0	; ILLEGAL, f6
	DB	0	; ILLEGAL, f7
	DB	0	; ILLEGAL, f8
	DB	0	; ILLEGAL, f9
	DB	0	; ILLEGAL, fa
	DB	0	; ILLEGAL, fb
	DB	0	; ILLEGAL, fc
	DB	0	; ILLEGAL, fd
	DB	0	; ILLEGAL, fe
	DB	0	; ILLEGAL, ff

clipper5_table	EQU	$
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
 	DB	1	; cb, PCOL
 	DB	1	; cc, PROW
 	DB	1	; cd, ROW
 	DB	1	; ce, SETPOS
 	DB	1	; cf, SETPOSBS
 	DB	0	; d0,
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
