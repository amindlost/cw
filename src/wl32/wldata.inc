;*********************************************************************
;*   WLDATA.INC                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/18/96                                         *
;*   Model:         Small                                            *
;*   Version:       1.3h                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   Global data declarations                                        *
;*                                                                   *
;*********************************************************************

; link options
GLOBAL	IsCreateEXEOption:BYTE	; /ex
GLOBAL	IsExitBeepOption:BYTE	; /b
GLOBAL	IsFastLoadOption:BYTE	; /fl
GLOBAL	IsLIBSearchOption:BYTE	; /ls
GLOBAL	IsLinkConfigOption:BYTE	; /lc
GLOBAL	IsLinkInfoLimitOption:BYTE	; /il
GLOBAL	IsLinkInfoOption:BYTE	; /i
GLOBAL	IsMAPOption:BYTE		; /m
GLOBAL	IsNoCopyrightOption:BYTE	; /nc
GLOBAL	IsNoDefaultLIBOption:BYTE	; /nd
GLOBAL	IsNoWarnDupeOption:BYTE	; /nwd
GLOBAL	IsNoWarnLIBDupeOption:BYTE	; /nwld
GLOBAL	IsParseDisplayOption:BYTE	; /pd
GLOBAL	IsStackOption:BYTE		; /st
GLOBAL	IsSYMOption:BYTE		; /sy
GLOBAL	IsThreePOption:BYTE		; /3p
GLOBAL	IsWarnRetCode1Option:BYTE	; /w1
GLOBAL	IsWarnUnknownOption:BYTE	; /wu

IFDEF WATCOM_ASM
GLOBAL	IsCaseSensitiveOption:BYTE	; /cs
GLOBAL	IsDStoSSOption:BYTE	; /ds
GLOBAL	IsFlatOption:BYTE	; /f
GLOBAL	IsZeroUninitOption:BYTE	; /zu
ENDIF

GLOBAL	EXEFileName [85]:BYTE
GLOBAL	MAPFileName [85]:BYTE
GLOBAL	SYMFileName [85]:BYTE

IFDEF DLLSUPPORT
GLOBAL	DLLFileName	[13]:BYTE
ENDIF

GLOBAL	DosVersion:BYTE	; dos version
GLOBAL	IsArgsFlag:BYTE	; nonzero if command line arguments
GLOBAL	IsDOSSEG:BYTE	; nonzero if DOSSEG segment ordering specified
GLOBAL	Pass2Flag:BYTE	; nonzero if on pass 2+ of link
GLOBAL	ProcessingLIBFlag:BYTE	; nonzero if processing library

GLOBAL	IOBlockSeg:WORD	; segment of 64K sized i/o block
GLOBAL	PSP:WORD		; WL32 PSP value
GLOBAL	TotalLIBCount:WORD	; total library file count
GLOBAL	TotalOBJCount:WORD	; total object module count
GLOBAL	WarningsCount:WORD	; count of linker warning messages
GLOBAL	ZeroBlockSeg:WORD	; segment of 16K block of zero values

GLOBAL	PublicSymbolCount:DWORD	; count of public symbols in program
GLOBAL	ResolvedSymbolCount:DWORD	; count of resolved symbols in program
GLOBAL	StackValue:DWORD	; user set stack value
GLOBAL	TotalCommunalCount:DWORD	; count of communals in program
GLOBAL	ZeroValue:DWORD	; known zero value, for pointers to null strings

IFDEF DLLSUPPORT
GLOBAL	TotalEXPDEFCount:DWORD	; total count of EXPDEFs
GLOBAL	TotalIMPDEFCount:DWORD	; total count of IMPDEFs
ENDIF
