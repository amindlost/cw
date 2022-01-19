;*********************************************************************
;*   MLSETUP.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          01/01/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   initial linker setup                                            *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlsetup
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

PUBLIC  setup,check_ems,check_xms

;*****************************
;* Data begins               *
;*****************************

.DATA?

; uninitialized local variables

; doubleword values
old_ctrlc   DD  ?

;*****************************
;* Constant data             *
;*****************************

.CONST

emmname     DB  'EMMXXXX0',0

termtext_len    DB  termtext_stop-term_text
term_text       DB  CR,LF,'Link terminated by user.'
termtext_stop   =   $

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   free_ems:NEAR,delete_ilf_file:NEAR
EXTRN	delete_temp_file:NEAR,delete_qlk_file:NEAR

;*****************************
;* SETUP                     *
;*****************************

; initial linker setup

setup       PROC
    push    es              ; save es -> PSP
    mov cx,ds               ; get PSP value from initial DS value
    mov ax,DGROUP           ; point data segment at DGROUP
    mov ds,ax
    mov psp,cx              ; save PSP value to memory variable

    cld                     ; make all string operations increment

    mov ah,33h              ; get or set Ctrl-Break flag
    xor al,al               ; get flag
    int 21h                 ; status returned in dl register
    mov ctrlbreak,dl        ; save status

    mov ah,30h              ; get MS-DOS version number
    int 21h
    cmp al,2                ; make sure at least 2.x
    jae s2                  ; yes
    int 20h                 ; force immediate 1.x compatible termination

s2:
    mov dos_version,al      ; save major dos version

    mov ax,3523h            ; get old ctrl-c handler address
    int 21h
    mov WORD PTR old_ctrlc,bx   ; save offset
    mov WORD PTR old_ctrlc+2,es ; save segment

    mov ax,cs
    mov ds,ax
    mov dx,OFFSET control_break ; ds:dx -> control-c handler
    mov ax,2523h            ; set ctrl-c handler
    int 21h
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

    pop es                  ; restore es -> PSP
    ret
setup       ENDP

;*****************************
;* CONTROL_BREAK             *
;*****************************

; control break handler

control_break   PROC
    mov ax,DGROUP
    mov ds,ax               ; make sure ds -> warplink data

    lds dx,old_ctrlc        ; ds:dx -> old cntrl-cl handler
    mov ax,2523h            ; set ctrl-c handler
    int 21h

    mov ax,DGROUP
    mov ds,ax               ; make sure ds -> warplink data

    call    delete_ilf_file ; delete ilf file if it exists
	call	delete_temp_file	; delete temp file if it exists
	call	delete_qlk_file	; kill qlk file too, if exists and writing to it
    call    free_ems

; write terminated feedback
cb_feed:
    mov bx,OFFSET DGROUP:term_text
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    jmp DWORD PTR old_ctrlc
control_break   ENDP

;*****************************
;* CHECK_EMS                 *
;*****************************

; check if EMS and can be used

check_ems   PROC

;*** COMMENT # temporary for 1.60f
    cmp is_no_ems,0         ; see if no EMS flag set
    jne ce_ret              ; yes

    mov dx,OFFSET DGROUP:emmname    ; ds:dx -> device name
    mov ax,3d00h            ; open for reading
    int 21h
    jc  ce_noems            ; open failed

    mov bx,ax               ; bx holds handle
    mov ax,4400h            ; IOCTL get device info
    int 21h
    jc  ce_noems            ; IOCTL call failed

    and dl,80h              ; get high bit, set if char device
    je  ce_noems            ; file device

    mov ax,4407h            ; IOCTL get output status
    int 21h
    jc  ce_noems            ; IOCTL call failed

    mov ah,3eh              ; close file
    int 21h
    jc  ce_noems            ; close failed

    mov ah,40h              ; get EMS system status
    int 67h                 ; call EMM
    or  ah,ah               ; check for EMM error
    jne ce_noems            ; EMM error occurred

    mov ah,46h              ; get EMM version
    int 67h
    or  ah,ah               ; check for error
    jne ce_noems            ; error occurred
    cmp al,32h              ; must be EMS version 3.2 or greater
    jb  ce_noems            ; no

    mov ah,41h              ;  get page frame address
    int 67h
    or  ah,ah
    jne ce_noems            ; error occurred

    mov ems_base,bx         ; save EMS base (page frame)
    ret

;*** END COMMENT #

; no, or can't use, EMS
ce_noems:
    mov is_no_ems,1         ; set no EMS flag

ce_ret:
    ret
check_ems   ENDP

;*****************************
;* CHECK_XMS                 *
;*****************************

; check if XMS and can be used, get XMM entry point

check_xms   PROC
    cmp is_xms,0            ; see if XMS flag set
    je  cx_ret              ; no

    mov ax,4300h            ; check if XMM is present
    int 2fh                 ; multiplex interrupt
    cmp al,80h              ; check if driver present
    je  cx_getaddr          ; yes, get entry point address

; no, or can't use, XMS
cx_noxms:
    mov is_xms,0            ; reset XMS flag

cx_ret:
    ret

cx_getaddr:
    mov ax,4310h            ; get XMS driver entry point
    int 2fh
    mov WORD PTR xms_addr,bx    ; save it
    mov WORD PTR xms_addr+2,es
    ret
check_xms   ENDP

END
