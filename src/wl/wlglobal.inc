;*********************************************************************
;*   WLGLOBAL.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          05/17/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Global data public declarations                                 *
;*                                                                   *
;*********************************************************************

; flags
GLOBAL CtrlBreakStatus:BYTE
GLOBAL DOSReturnCode:BYTE
GLOBAL EMSOptionSet:BYTE

; linker option flags
GLOBAL XTOptionSet:BYTE
GLOBAL XPOptionSet:BYTE
GLOBAL XP3OptionSet:BYTE

; general byte values
GLOBAL DOSVersion:BYTE
GLOBAL EMSVersion:BYTE

; general word values
GLOBAL EMSBase:WORD
GLOBAL EMSHandle:WORD
GLOBAL PSP:WORD
GLOBAL XMSHandle:WORD

; general doubleword values
GLOBAL XMSAddr:DWORD
