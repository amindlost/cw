;*********************************************************************
;*   WLEQUATE.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/17/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Global file equates                                             *
;*                                                                   *
;*********************************************************************

; modifiable equate, tracks number of internal error types
INTERNAL_ERR_COUNT  EQU 14

LINKER		EQU	"WarpLink"
LINKERCAPS	EQU	"WARPLINK"

IFDEF FREELINK
	LINKER		EQU	"Freelink"
	LINKERCAPS	EQU	"FREELINK"
ENDIF

; general use equates

STDIN   EQU 0               ; standard input device
STDOUT  EQU 1               ; standard output device
STDERR  EQU 2               ; standard error device

BELL    EQU     7           ; beep the speaker
CR      EQU     13          ; carriage return
LF      EQU     10          ; line feed

; object record names

BLKDEF  EQU     7ah
BLKEND  EQU     7ch
DEBSYM  EQU     7eh

THEADR  EQU     80h
LHEADR  EQU     82h
COMENT  EQU     88h
MODEND  EQU     8ah
EXTDEF  EQU     8ch
TYPDEF  EQU     8eh
PUBDEF  EQU     90h
LOCSYM  EQU     92h
LINNUM  EQU     94h
LNAMES  EQU     96h
SEGDEF  EQU     98h
GRPDEF  EQU     9ah
FIXUPP  EQU     9ch
LEDATA  EQU     0a0h
LIDATA  EQU     0a2h
COMDEF  EQU     0b0h

BAKPAT1 EQU     0b2h        ; Quick C back patch record type 1, not implemented
BAKPAT2 EQU     0b3h        ; Quick C back patch record type 1, not implemented

LEXTDEF EQU     0b4h
LPUBDEF1	EQU 0b6h
LPUBDEF2	EQU 0b7h
LCOMDEF	EQU		0b8h
CEXTDEF	EQU		0bch
SEGDEF32	EQU	99h
LEDATA32	EQU	0a1h

WKEXT   EQU     0a8h        ; weak extern, used by PDS 7.x, COMENT class

MSLIBR  EQU     0f0h        ; MS library header record (aka MSLHED)
MSLHED  EQU     0f0h        ; MS library header record (aka MSLIBR)
MSEXTDICT   EQU 0f2h        ; MS extended dictionary identifier byte
MSLEND  EQU     0f1h        ; MS library end record

PASS_TWO_MARK   EQU 0a2h    ; COMENT class, linker pass two marker
DOSSEG_MARK 	EQU 9eh		; COMENT class, DOSSEG segment ordering
LIB_SEARCH_1    EQU 81h     ; COMENT class, default library search name type 1
LIB_SEARCH_2    EQU 9fh		; COMENT class, default library search name type 2
