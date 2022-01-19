;*********************************************************************
;*   MLGETARG.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/02/90                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   get command line arguments                                      *
;*                                                                   *
;*********************************************************************

TITLE   MACHLINK
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLDATA.INC
     
;*****************************
;* Public declarations       *
;*****************************
     
PUBLIC  getargs

;*****************************
;* Code begins               *
;*****************************

.CODE

getargs     PROC
    mov ax,es               ; swap extra segment, data segment
    mov bx,ds
    mov ds,ax
    mov es,bx               ; es -> linker data, ds -> PSP
    mov si,80h              ; si -> command line offset
    mov di,OFFSET DGROUP:cmd_line   ; di -> linker data command line location
    mov cl,[si]             ; get count of bytes in command line
    xor ch,ch               ; zap high byte of count
    jcxz    ga2             ; no arguments on command line

; check for one char as space, in case of i/o redirection of summary
    cmp cl,1
    jne ga_1                ; more than one character
    cmp BYTE PTR [si+1],' ' ; see if character is whitespace
    jbe ga2                 ; yes, no arguments on command line

ga_1:
    movsb                   ; transfer byte count of command tail in PSP
    inc cl                  ; account for return character in command tail
    rep movsb               ; transfer command tail and terminating return character
    mov ds,bx               ; ds -> linker data
    mov is_args,1           ; flag that command line argument(s) exist
    ret
ga2:
    mov ds,bx               ; ds -> linker data
    ret
getargs     ENDP

END
