;*********************************************************************
;*   MLCLEAN.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          01/21/91                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   clean up after linker run complete                              *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlclean
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
     
PUBLIC  cleanup

IFNDEF JUNIOR
PUBLIC  free_ems
ENDIF

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   ovl_in_xms:BYTE,xms_ovl_handle:WORD,xms_tmp_handle:WORD

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* CLEANUP                   *
;*****************************

; restore previous state of control break flag before link
; de-allocate any allocated EMS pages
; destroys ax,dl

cleanup     PROC
    mov ax,3301h            ; set Ctrl-Break flag
    mov dl,ctrlbreak        ; get previous status of Ctrl-Break flag
    int 21h
IFNDEF JUNIOR
    call    free_ems
ENDIF
    ret
cleanup     ENDP

IFNDEF JUNIOR

;*****************************
;* FREE_EMS                  *
;*****************************

; free any used EMS/XMS

free_ems    PROC
    cmp ovl_in_xms,0        ; see if XMS was used for overlay file
    je  fe_chktmp           ; no

; overlay in XMS, free it
    mov dx,xms_ovl_handle
    mov ah,0ah              ; free extended memory block
    call    DWORD PTR xms_addr  ; do it

fe_chktmp:
    cmp tmp_in_xms,0        ; see if XMS was used for temporary file
    je  fe_chkems           ; no

; temp file in XMS, free it
    mov dx,xms_tmp_handle
    mov ah,0ah              ; free extended memory block
    call    DWORD PTR xms_addr  ; do it

fe_chkems:
    cmp is_no_ems,0         ; see if ems was used
    jne fe_ret              ; no
    mov ax,ems_handle
    or  ax,ax               ; see if any ems was allocated
    je  fe_ret              ; no

    mov dx,ax               ; get handle in dx
    mov ah,45h              ; release handle and memory pages
    int 67h

fe_ret:
    ret
free_ems    ENDP

ENDIF

END
