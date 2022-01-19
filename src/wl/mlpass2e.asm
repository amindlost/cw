;*********************************************************************
;*   MLPASS2E.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/03/92                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   linker pass 2 routines part E                                   *
;*                                                                   *
;*********************************************************************

TITLE   MACHLINK mlpass2e
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLDATA.INC
INCLUDE MLERRMES.INC
     
;*****************************
;* Public declarations       *
;*****************************

; procedures
PUBLIC  proc2_modend

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   frame_method:BYTE,target_method:BYTE
EXTRN   frame_index:WORD,target_index:WORD
EXTRN   is_resolved:BYTE
EXTRN   fixup_pos:WORD,target_segment:WORD
EXTRN   target_disp:DWORD,target_prog_off:DWORD

IFNDEF JUNIOR
EXTRN   data_offset:DWORD
ENDIF

; initialized local variables

EVEN                        ; maximize speed on 8086 and better

; byte values
EVEN
is_entry_point  DB  0       ; nonzero if have an entry point from modend record

.DATA?

; uninitialized local variables

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
main_text   DB  '$$_MAIN_ENTRY',0

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,load_file:NEAR
EXTRN   check_index:NEAR,get_target_addr:NEAR,get_frame_addr:NEAR
EXTRN   fixupp_offset_err:NEAR

IFNDEF JUNIOR
EXTRN   get_symbol_offset:NEAR,write_bytes:NEAR
ENDIF

;*****************************
;* PROC2_MODEND              *
;*****************************

; pass 2 modend record processing
; upon entry cx=record length,bp=buffer_end,al=record type
; es:si -> first byte of record past record length
; destroys ax,bx,dx,di
; updates si

proc2_modend    PROC
    cmp is_entry_point,0    ; see if previous entry point (ignore this one)
    je  pm_1                ; no
    ret                     ; entry point already given

pm_1:
    mov al,es:[si]          ; get module type byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_2:
    and al,40h              ; see if start bit set
    jne pm_2a               ; yes, record contains entry point
    ret                     ; no, no start address/entry point

pm_2a:
    mov dl,es:[si]          ; get end dat byte
    mov fixup_pos,si        ; keep -> to byte in case of error
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_3:
    test    dl,4            ; see if P bit is set
    je  pm_4                ; no

; bit 2 of end dat field, P bit in corresponding fix dat field cannot be set
    mov cl,dl
    mov ax,ENDDAT_VAL_ERR   ; bad end dat field value in MODEND record
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

pm_4:
    mov dh,dl               ; dh holds end dat field value
    and dh,70h              ; get frame field in dh
    shr dh,1
    shr dh,1
    shr dh,1
    shr dh,1                ; make frame value relative zero

    mov al,dl               ; get fix dat field
    test    al,80h          ; check if thread field for frame (fbit)
    jne pm_thrdframe        ; yes

; no thread field for frame
    cmp dh,2                ; see if index specified for this frame
    ja  pm_5                ; no

; index specified for this field
    xor ah,ah               ; zap high byte
    mov al,es:[si]          ; get frame datum first byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_4a               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_4a:
    cmp al,80h              ; see if two byte field
    jb  pm_4b               ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get frame datum second byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_4b               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pm_4b:
    mov frame_index,ax      ; frame index, if any

pm_5:
    mov frame_method,dh     ; frame method is the frame field
    jmp SHORT pm_6         ; bypass frame thread field code

; thread field for frame
pm_thrdframe:
    shr al,1                ; make frame field value relative zero
    shr al,1
    shr al,1
    shr al,1
    and al,7                ; mask off F bit
    cmp al,3                ; make sure frame thread field doesn't exceed 3
    jbe pm_5a               ; okay

; frame thread field exceeds 3
    mov cl,al
    mov ax,ENDFRAME_THRD_ERR
    jmp NEAR PTR fixupp_offset_err  ; transfer control to error handler

pm_5a:
    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:frame_thrd_meth    ; bx -> frame thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get frame method
    mov frame_method,bl     ; save to memory variable

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:frame_thrd_index   ; bx -> frame thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get frame index
    mov frame_index,ax      ; save to memory variable

pm_6:
    mov al,dl               ; get fix dat field
    test    al,8            ; check if thread field for target (tbit)
    jne pm_thrdtarg         ; yes

; no thread field for target
    xor ah,ah               ; zap high byte
    mov al,es:[si]          ; get target datum first byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_6a               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_6a:
    cmp al,80h              ; see if two byte field
    jb  pm_6b               ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte

    mov al,es:[si]          ; get target datum second byte
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_6b               ; okay
    call    load_file       ; load next portion of file into buffer, at end position

pm_6b:
    mov target_index,ax     ; target index, if any

    mov al,dl               ; get fix dat field
    and al,3                ; break out targt field
    mov target_method,al    ; target method is the targt field
;***    mov target_method,dh    ; target method is the frame field

    jmp SHORT pm_7          ; bypass target thread field code

; thread field for target
pm_thrdtarg:
    and al,3                ; get targt field value in al
    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:target_thrd_meth   ; bx -> target thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get target method
    mov target_method,bl    ; save to memory variable

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:target_thrd_index  ; bx -> target thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get target index
    mov target_index,ax     ; save to memory variable

; check index validity
pm_7:
    cmp frame_method,3      ; check if index specified for frame method
    jae pm_8                ; no
    mov ax,frame_index      ; get frame index for check_index routine
    mov bl,frame_method
    call    check_index

pm_8:
    mov ax,target_index     ; get target index for check_index
    mov bl,target_method
    call    check_index

; P bit always zero with MODEND entry point, so always target displacement
    mov al,es:[si]          ; get low byte of target displacement
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_8a               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_8a:
    mov ah,es:[si]          ; get high byte of target displacement
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pm_8b               ; okay
    call    load_file       ; load next portion of file into buffer, at end position
pm_8b:
    mov WORD PTR target_disp,ax ; save target displacement value
    cwd
    mov WORD PTR target_disp+2,dx   ; save sign extension of target displacement

; compute program offset of target address and its frame
    mov al,1
    mov is_resolved,al      ; init is_resolved flag to assume fixup okay
    call    get_target_addr

    mov al,frame_method
    cmp al,4                ; check if frame determined by location segment
    jne pm_11               ; no
    mov ax,WORD PTR target_prog_off ; get target's program offset in bx:ax
    mov bx,WORD PTR target_prog_off+2   ; get high word
    shr bx,1
    rcr ax,1                ; /2
    shr bx,1
    rcr ax,1                ; /4
    shr bx,1
    rcr ax,1                ; /8
    shr bx,1
    rcr ax,1                ; /16, paragraph value (largest possible frame number)
    mov entry_segval,ax
    jmp SHORT pm_13         ; bypass other target segment computation code

pm_11:
    cmp al,5                ; check if frame determined by target's index
    jne pm_12               ; no
    mov al,1                ; flag to get_frame_addr procedure to use target data
    call    get_frame_addr
    jmp SHORT pm_12a        ; bypass other target segment computation code

; frame determined by segment, group, or external index
pm_12:
    xor al,al               ; flag to get_frame_addr procedure to use frame data
    call    get_frame_addr

pm_12a:
    mov ax,target_segment   ; get value place in target_segment by get_frame_addr procedure
    mov entry_segval,ax     ; and place it in the programs' entry segment value memory variable

pm_13:
    cmp is_resolved,0       ; check if reference to unresolved external
    jne pm_resolved         ; no, compute program's entry offset value

; reference to unresolved external
    mov entry_segval,0      ; zero program's entry segment value
    jmp SHORT pm_ret

; convert program's entry segment and offset value to segment:offset format
pm_resolved:
    mov ax,entry_segval
    xor bx,bx               ; bx:ax will hold byte value of entry segment
    shl ax,1
    rcl bx,1                ; x2
    shl ax,1
    rcl bx,1                ; x4
    shl ax,1
    rcl bx,1                ; x8
    shl ax,1
    rcl bx,1                ; x16
    mov di,WORD PTR target_prog_off ; get absolute offset low word
    sub di,ax
    mov ax,bx
    mov bx,WORD PTR target_prog_off ; get absolute offset high word
    sub bx,ax               ; get entry offset in bx:di (bx should be zero)
    mov entry_offval,di     ; save program entry's offset to memory variable

pm_ret:
    cmp is_comfile,0        ; check if COM file
    je  pm_ret2             ; no
    cmp entry_segval,0      ; see if begins at 0:100h
    je  pm_14               ; okay so far

; bad entry point address for com file
pm_com_entry:
    mov dx,OFFSET DGROUP:exe_name
    mov ax,COM_ENTRY_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

pm_14:
    cmp entry_offval,100h   ; check offset
    jne pm_com_entry        ; bad offset

pm_ret2:
    mov is_entry_point,1    ; flag that entry point now exists

; if overlays, setup $$_main_entry variable
IFNDEF JUNIOR
    cmp ovl_count,0         ; see if any overlays
    je  pm_ret3             ; no

; write entry point offset word
    mov di,OFFSET DGROUP:main_text
    call    get_symbol_offset
    mov si,OFFSET DGROUP:entry_offval   ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes

; write entry point segment word
    add WORD PTR data_offset,2  ; bump past offset value to segment value
    adc WORD PTR data_offset+2,0    ; carry to high word
    mov si,OFFSET DGROUP:entry_segval   ; es:si -> buffer
    mov cx,2                ; write one word
    call    write_bytes
ENDIF

pm_ret3:
    ret
proc2_modend    ENDP

END
