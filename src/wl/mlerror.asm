;*********************************************************************
;*   MLERROR.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/31/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   give linker error/warning message and terminate if fatal        *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlerror
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Equates                   *
;*****************************

ALLOCATE_ERROR_DATA EQU 1   ; allocate data space for error messages

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLDATA.INC
INCLUDE MLERRMES.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC  link_error,dos_error,link_warning,give_warn_count,fixup_warning
PUBLIC  check_xms_error

;*****************************
;* External declarations     *
;*****************************

EXTRN   filename:BYTE,temp_file_name:BYTE
EXTRN   frame_method:BYTE,target_method:BYTE,target_disp:DWORD
EXTRN   frext_ingroup:BYTE
EXTRN   frame_ent_ptr:WORD,targ_ent_ptr:WORD

;*****************************
;* Data begins               *
;*****************************

.DATA

; initialized local variables

; byte values
; 'cascade_' flags prevents cascading warning solutions for same problem
cascade_unresolved  DB  0
cascade_multsym DB  0
cascade_fixup   DB  0
cascade_segdecl DB  0
cascade_nonovlseg   DB  0
cascade_nonovlsym   DB  0

.DATA?

; uninitialized local variables
; byte values
error_type  DB  ?           ; type of error
warn_type   DB  ?           ; type of warning

; word values
temp_flag   LABEL   WORD    ; used by link_warning to fake out print_lib_mod, share space with link_error
error_value DW  ?           ; 8 or 16 bit value of file error, if any

; byte string values
number_hex  DB  12 DUP (?)  ; room for doubleword max value plus 'h' suffix and '0' prefix
number_buff DB  5 DUP (?)   ; temporary buffer for decimal ASCII value (reversed)

;*****************************
;* Constant data             *
;*****************************

.CONST

warn_tlen   DB  warn_tstop-warn_text
warn_text   DB  CR,LF,CR,LF,'Total number of warnings: '
warn_tstop  =   $

fw_frame_text   DB  CR,LF,'  Frame '
fw_targ_text    DB  CR,LF,'  Target '
fw_seg_text     DB  'segment '
fw_group_text   DB  'group '
fw_loc_text     DB  'LOCATION'
fw_sym_text     DB  'symbol '
fw_disp_text    DB  ', displacement '

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   exit_link:NEAR
EXTRN   restore_ems_map:NEAR,delete_ilf_file:NEAR
EXTRN	delete_temp_file:NEAR,delete_qlk_file:NEAR

;*****************************
;* LINK_ERROR                *
;*****************************

; print fatal error feedback
; ax has error code upon entry
; dx -> filename with error, if any
; cx == value of error, if any (-> symbol name for communal overflow error)
;   (-> start of bad option if bad option)

link_error  PROC
    mov bx,DGROUP
    mov ds,bx               ; point data segment at WarpLink's data
    cmp ax,HIGH_ERR_CODE    ; make sure error code isn't out of bounds
    jbe le_code_okay        ; not out of bounds

    mov ax,INTERNAL_ERR     ; force error as internal error
    mov cx,255              ; 255 internal error value

le_code_okay:
    mov error_value,cx
    mov error_type,al       ; save type of error

    mov bx,OFFSET DGROUP:error_table_start  ; point to start of error message address table
; adjust ax for offset in table by multiplying by 8 (4 word values each entry)
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    add bx,ax               ; bx -> proper entry in table
    mov di,bx               ; save offset in table

    mov ax,[bx]             ; get error type flags
    test    ax,DOS_ERRVAL   ; see if a DOS error
    je  le_nondos           ; no
    and ax,6                ; get unknown/internal/command line/file type
    cmp ax,6                ; see if DOS file error
    jne le_1                ; no
    mov bx,OFFSET DGROUP:sttyp_fatal_dosmod ; DOS file error
    jmp SHORT print_first   ; print first part of feedback

le_1:
    mov bx,OFFSET DGROUP:sttyp_fatal_dos    ; DOS nonfile error
    jmp SHORT print_first   ; print first part of feedback

le_nondos:
    mov bx,ax               ; save error type flag value
    and ax,6                ; get unknown:unspecified/internal/command line/file type
    jne le_2                ; not unknown/unspecified type
    and bx,NOSPEC_ERRVAL    ; get unspecified bit
    je  le_unknown          ; not set, unknown
    mov bx,OFFSET DGROUP:sttyp_fatal_error
    jmp SHORT print_first   ; print first part of feedback

le_unknown:
    mov bx,OFFSET DGROUP:sttyp_fatal_unknown
    jmp SHORT print_first   ; print first part of feedback

le_2:
    cmp al,INTERNAL_ERRVAL
    jne le_3                ; no internal type
    mov bx,OFFSET DGROUP:sttyp_fatal_internal
    jmp SHORT print_first   ; print first part of feedback

le_3:
    cmp al,COMMAND_ERRVAL
    jne le_4                ; no command line/user type
    mov bx,OFFSET DGROUP:sttyp_fatal_comline
    jmp SHORT print_first   ; print first part of feedback

; error in file
le_4:
    mov bx,OFFSET DGROUP:sttyp_fatal_objmod

print_first:
    push    dx              ; save dx -> filename for file errors
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    pop dx                  ; restore dx -> filename for file errors
    mov ax,[di]             ; get error type flags
    and al,6
    cmp al,INTERNAL_ERRVAL  ; see if internal error
    jne le_5                ; no
    jmp NEAR PTR pf_value   ; yes, print value of internal error

le_5:
    cmp al,FILE_ERRVAL      ; see if file type error
    je  pf_file_err         ; yes
    cmp error_type,BAD_OPTION_ERR   ; see if bad option error
    je  pf_bad_option       ; yes
;***    jmp NEAR PTR pf_colon   ; no
    jmp SHORT pf_4          ; no

; bad option error
pf_bad_option:
    mov dx,OFFSET DGROUP:sttyp_option   ; print ', option /' string
    mov cx,10
    mov bx,STDOUT           ; write to standard output
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; print the bad option
    mov dx,error_value      ; ds:dx -> bad option string

pf_file_err:
    mov bx,dx
    xor cx,cx               ; cx will contain number of chars in file name
ploop1:
    cmp BYTE PTR [bx],' '   ; see if white space/terminator in file name found
    jbe pf_2                ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT ploop1        ; loop back to test next char

; write the file name/bad option
pf_2:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    cmp error_type,BAD_OPTION_ERR   ; see if bad option error
    je  pf_colon            ; yes

    mov ax,[di]             ; get flags
    and al,NO_OFF_ERRVAL    ; get offset/value bit field
    cmp al,NO_OFF_ERRVAL    ; see if no file offset for error
    je pf_3                 ; no offset
    call    print_offset    ; print 'offset' string and file error offset in hex

pf_3:
    call    print_lib_mod   ; check if processing library, print module name/offset if so

    cmp error_type,COMM_OVRFLW_ERR  ; see if communal symbol name should be printed
    jne pf_4                ; no

; print ", symbol "
    mov dx,OFFSET DGROUP:sttyp_symbol
    mov cx,9                ; nine chars in string
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; get number of chars in symbol name
    mov bx,error_value      ; get -> symbol name
    mov bx,dx               ; ds:dx -> symbol name
    xor cx,cx               ; cx will contain number of chars in sumbol name
ploop2:
    cmp BYTE PTR [bx],0     ; see if zero terminator in symbol name found
    je  pf_3a               ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT ploop2        ; loop back to test next char

; write the symbol name
pf_3a:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; check if value
pf_4:
    mov ax,[di]             ; get flags
    and al,NO_OFF_ERRVAL    ; get offset/value bit field
    cmp al,NO_OFF_ERRVAL    ; see if no value error
    je pf_colon             ; no value
    or  al,al
    je  pf_colon            ; no value, finish up line with colon

; print 'value' string
pf_value:
    mov bx,OFFSET DGROUP:sttyp_value
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    call    print_value     ; print error value in hex

; print a colon after the file error message
pf_colon:
    mov dx,OFFSET DGROUP:sttyp_colon
    mov cx,1                ; one char to write
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

pf_print_ps:
    call    print_problem   ; print problem
    call    print_solution  ; print solution

; delete ilf file if it exists
    call    delete_ilf_file
	call	delete_temp_file	; delete temp file if exists
	call	delete_qlk_file	; kill qlk file too, if writing to it

pf_chkovl:
    cmp is_internal,0       ; check if temporary overlay file
    je  pf_out              ; no

; delete temporary overlay file if exists
    mov bx,ovl_handle       ; get overlay file handle
    or  bx,bx               ; make sure not zero
    je  pf_out              ; zero, no overlay file to delete
    mov ah,3eh              ; close file
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    mov dx,OFFSET DGROUP:ovl_filename
    mov ah,41h              ; delete file
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

pf_out:
    mov al,error_type
    add al,2
    jmp NEAR PTR exit_link  ; terminate with return code of error+2
link_error  ENDP

;*****************************
;* LINK_WARNING              *
;*****************************

; print warning message feedback
; ax has error code upon entry
; ds:dx -> primary string associated with warning message,
; es:di -> secondary string associated with warning message, if any

link_warning    PROC
    push    ds              ; save all used registers
    push    si
    push    di
    push    dx
    push    cx
    push    bx
    push    ax              ; save error code, ax is used
    mov bx,DGROUP           ; get address of dgroup
    mov ds,bx               ; point data segment at Machlink's data
    mov warn_type,al        ; save type of warning
    cmp is_nowarn,0         ; see if warnings disabled
    je  lw_1                ; no
    jmp NEAR PTR lw_ret     ; yes

lw_1:
    mov bx,OFFSET DGROUP:error_table_start  ; point to start of error message address table
; adjust ax for offset in table by multiplying by 8 (4 word values each entry)
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    add bx,ax               ; bx -> proper entry in table
    push    bx              ; save pointer to entry
    push    dx              ; save pointer to primary string

; print 'Warning in ' string
    mov bx,OFFSET DGROUP:sttyp_warning
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    pop dx                  ; restore pointer to primary warning string
    xor cx,cx               ; cx will contain number of chars in file name
    mov bx,dx
wloop1:
    cmp BYTE PTR [bx],0     ; see if zero terminator in file name found
    je  lw_2                ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT wloop1        ; loop back to test next char

; write primary warning string
lw_2:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov al,warn_type
    cmp al,NO_EXE_STACK_WARN    ; see if secondary string used
    jne lw_secondary        ; yes

; no secondary string used
    call    print_lib_mod   ; check if processing library, print module name is so
    jmp SHORT lw_3          ; bypass secondary string code

; see if file offset used
lw_secondary:
    cmp al,SEG_GRP_DECL_WARN
    je  lw_offset
    cmp al,NONOVL_SEG_WARN
    je  lw_offset
    cmp al,NONOVL_SYM_WARN
    jne lw_no_offset        ; no offset

; print offset string and hex value
lw_offset:
    call    print_offset
    xor ax,ax               ; fake out print_lib_mod to print offset
    jmp SHORT lw_shared     ; jump to code shared with no print offset routine

lw_no_offset:
    mov al,NO_OFF_ERRVAL        ; fake out print_lib_mod NOT to print offset

lw_shared:
    mov temp_flag,ax
    push    di              ; save critical register
    mov di,OFFSET DGROUP:temp_flag
    call    print_lib_mod   ; check if processing library, print module name if so
    pop di                  ; restore critical register

lw_2a:
    mov al,warn_type
    cmp al,UNRES_EXT_WARN   ; see if symbol name printed
    je  lw_symbol           ; yes
    cmp al,MULT_DEF_SYM_WARN    ; see if symbol name printed
    je  lw_symbol           ; print symbol name
    cmp al,NONOVL_SYM_WARN
    je  lw_symbol           ; yes

; print segment
lw_segment:
    mov bx,OFFSET DGROUP:sttyp_segment
    jmp SHORT lw_printss

; print symbol
lw_symbol:
    mov bx,OFFSET DGROUP:sttyp_symbol

; print 'segment' or 'symbol' string
lw_printss:
    mov cl,[bx-1]           ; get length of string
    xor ch,ch               ; zap high byte of cx
    mov dx,bx               ; ds:dx -> string
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; get length of segment/symbol name
    mov bx,di
    xor cx,cx               ; cx will contain number of chars in segment name
    mov ax,es
    mov ds,ax               ; ds:bx -> segment/symbol name

wloop2:
    cmp BYTE PTR [bx],0     ; see if zero terminator in file name found
    je  print_segsym        ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT wloop2        ; loop back to test next char

; print name of segment/symbol from es:di
print_segsym:
    mov dx,di               ; ds:dx -> string to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

lw_3:
    pop bx                  ; restore pointer to entry
    mov di,bx               ; save it in di
    call    print_problem   ; print problem

    mov al,warn_type        ; get type of warning
    cmp al,cascade_unresolved   ; see if problem/solution already printed once
    je  lw_ret              ; yes
    cmp al,UNRES_EXT_WARN   ; see if unresolved external type
    jne lw_4                ; no
    mov cascade_unresolved,al   ; flag that problem/solution was printed
    jmp SHORT lw_print_sol  ; skip other code

; check other cascade flags
lw_4:
    cmp al,cascade_multsym 
    je  lw_ret
    cmp al,MULT_DEF_SYM_WARN    ; see if multiply defined symbol
    jne lw_5                ; no
    mov cascade_multsym,al  ; flag that problem/solution was printed
    jmp SHORT lw_print_sol  ; skip other code

lw_5:
    cmp al,cascade_segdecl
    je  lw_ret
    cmp al,SEG_GRP_DECL_WARN    ; see if segment declared in more than one group
    jne lw_6                ; no
    mov cascade_segdecl,al  ; flag that problem/solution was printed
    jmp SHORT lw_print_sol  ; skip other code

lw_6:
    cmp al,cascade_nonovlseg
    je  lw_ret
    cmp al,NONOVL_SEG_WARN  ; see if nonoverlayble segment ref warning
    jne lw_7                ; no
    mov cascade_nonovlseg,al    ; flag that problem/solution was printed
    jmp SHORT lw_print_sol  ; skip other code

lw_7:
    cmp al,cascade_nonovlsym
    je  lw_ret
    cmp al,NONOVL_SYM_WARN  ; see if nonoverlayble segment ref warning
    jne lw_8                ; no
    mov cascade_nonovlsym,al    ; flag that problem/solution was printed
    jmp SHORT lw_print_sol  ; skip other code

lw_8:

; room for more warning cascade checks

lw_print_sol:
    call    print_solution  ; print solution

lw_ret:
    inc warn_count          ; bump count of warning messages
    pop ax                  ; restore registers
    pop bx
    pop cx
    pop dx
    pop di
    pop si
    pop ds
    ret
link_warning    ENDP

;*****************************
;* PRINT_LIB_MOD             *
;*****************************

; print library module string and name
; upon entry di -> error flags
; destroys ax,bx,cx,dx

print_lib_mod   PROC
    cmp is_inlib,0          ; see if processing in library
    je  plm_ret             ; no

    mov al,tmod_name        ; check that T-module name is nonzero
    or  al,al
    je  plm_ret             ; don't print module name if zero length

; print module string and name
    mov bx,OFFSET DGROUP:sttyp_module   ; write 'module' string
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov bx,OFFSET DGROUP:tmod_name
    mov dx,bx
    xor cx,cx               ; cx will contain number of chars in module name
plm_loop:
    cmp BYTE PTR [bx],0     ; see if zero terminator in file name found
    je  plm_write           ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT plm_loop      ; loop back to test next char

; write library module name
plm_write:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov ax,[di]             ; get flags
    and al,NO_OFF_ERRVAL    ; get offset/value bit field
    cmp al,NO_OFF_ERRVAL    ; see if no file offset for error
    je plm_ret              ; no offset

; print module offset
; back off file offset from library module start prior to printing module offset
    mov ax,WORD PTR file_pos_adj
    mov bx,WORD PTR file_pos_adj+2
    push    ax              ; save old offset value
    push    bx
    sub ax,WORD PTR lib_pos_adj ; adjust for library module offset
    sbb bx,WORD PTR lib_pos_adj+2
    mov WORD PTR file_pos_adj,ax    ; update new value to memory variable
    mov WORD PTR file_pos_adj+2,bx
    call    print_offset    ; print 'offset' string and file error offset in hex
    pop WORD PTR file_pos_adj+2 ; restore old value to memory variable
    pop WORD PTR file_pos_adj

plm_ret:
    ret
print_lib_mod   ENDP

;*****************************
;* PRINT_OFFSET              *
;*****************************

; print offset value in error feedback
; upon entry si holds file offset
; destroys ax,bx,cx,dx

print_offset    PROC
    push    di              ; save critical register
    push    si

; print 'offset' string
    mov bx,OFFSET DGROUP:sttyp_offset
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    xor dl,dl               ; dl will hold count of chars in print string
    xor di,di               ; init high word of offset
    add si,WORD PTR file_pos_adj    ; compute low word of offset
    adc di,WORD PTR file_pos_adj+2  ; compute high word of offset with carry

; di:si hold file offset
po_2:
    mov bx,OFFSET DGROUP:number_hex ; point to number slot
    xor ah,ah               ; flag leading zero
    mov cx,di
    or  ch,ch               ; see if high word high byte is zero
    je  po_3                ; yes
    mov cl,ch               ; get value in cl
    call    hex_to_ascii    ; convert high word, high byte to hex
    mov ah,1                ; flag no leading zero

po_3:
    mov cx,di
    or  cl,cl               ; see if high word low byte is zero
    jne po_4                ; no
    or  ah,ah               ; see if previous value printed
    je  po_5                ; no

po_4:
    call    hex_to_ascii    ; convert to hex
    mov ah,1                ; flag no leading zero

po_5:
    mov cx,si
    or  ch,ch               ; see if low word, high byte is zero
    jne po_6                ; no
    or  ah,ah               ; see if previous value printed
    je  po_7                ; no

po_6:
    mov cl,ch               ; get value in cl
    call    hex_to_ascii    ; convert to hex
    mov ah,1                ; flag no leading zero

po_7:
    mov cx,si
    call    hex_to_ascii    ; convert to hex
    mov BYTE PTR [bx],'h'   ; terminate hex number with 'h' suffix
    inc dl                  ; and bump count of chars

    xor ch,ch               ; zap high byte
    mov cl,dl               ; cx == number of chars to print
    mov dx,OFFSET DGROUP:number_hex ; ds:dx -> string to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    pop si
    pop di                  ; restore critical register
    ret
print_offset    ENDP

;*****************************
;* PRINT_VALUE               *
;*****************************

; print value in error feedback
; upon entry di -> error type flags
; destroys ax,bx,cx

print_value     PROC
    mov cx,error_value      ; get value of error to print
    xor dl,dl               ; dl will hold count of chars in print string
    mov bx,OFFSET DGROUP:number_hex ; point to number slot
    xor ah,ah               ; flag to put leading zero in front of number if necessary
    test    WORD PTR [di],BIT16_ERRVAL  ; see if 8 or 16 bit value
    je  pv_low_byte         ; 8 bit value, bypass 16 bit code

    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    call    hex_to_ascii    ; convert the high byte
    xchg    ch,cl           ; restore low byte to cl
    mov ah,1                ; flag to hex_to_ascii procedure not to use leading zero

pv_low_byte:
    call    hex_to_ascii    ; convert low byte of value
    mov BYTE PTR [bx],'h'   ; terminate hex number with 'h' suffix
    inc dl                  ; and bump count of chars

    xor ch,ch
    mov cl,dl               ; cx == number of chars to print
    mov dx,OFFSET DGROUP:number_hex ; ds:dx -> string to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    ret
print_value     ENDP

;*****************************
;* PRINT_PROBLEM             *
;*****************************

; print linker problem
; upon entry di -> entry in error table
; destroys ax,bx,cx,dx

print_problem   PROC
    mov dx,OFFSET DGROUP:string_problem ; point to Problem string
    mov cl,string_pro_len   ; get length of string
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov bx,di               ; get previous offset in table
    mov di,[bx+2]           ; get problem message address in di
    mov dx,di               ; DS:DX -> message to print
    mov cl,[di-1]           ; cx = number of bytes to write
    mov di,bx               ; save offset in table
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    ret
print_problem   ENDP

;*****************************
;* PRINT_SOLUTION            *
;*****************************

; print solution to linker error/warning
; upon entry di -> entry in error table
; destroys ax,bx,cx,dx

print_solution  PROC
    mov dx,OFFSET DGROUP:string_solution    ; point to Solution string
    mov cl,string_sol_len   ; get length of string
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov bx,di               ; get previous offset in table
    mov di,[bx+4]           ; get cause message address in di
    mov dx,di               ; DS:DX -> message to print
    mov cl,[di-1]           ; cx = number of bytes to write
    mov di,bx               ; save offset in table
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov bx,di               ; get previous offset in table
    mov di,[bx+6]           ; get solution message address in di
    mov dx,di               ; DS:DX -> message to print
    mov si,di
    dec si                  ; SI -> number of bytes to write in line
    mov bx,STDOUT           ; write to standard output device
    mov cl,[di-2]           ; get line count in cx, ch always 0
le_loop:
    push    cx              ; save cx as is used for output
    mov di,dx               ; save dx as it is used
    mov cl,string_sp_len    ; get length of spacer
    mov dx,OFFSET DGROUP:string_spacer  ; cr/lf+blanks string to print
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    mov dx,di               ; restore previous dx value
    mov cl,[si]             ; cx = number of bytes to write
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    add si,cx               ; bump si to next message byte count
    inc si                  ; adjust for byte count byte
    add dx,cx               ; bump dx to next message line
    inc dx                  ; adjust for byte count byte
    pop cx                  ; restore line count
    loop    le_loop         ; loop thru all lines in solution message

; print trailing CR/LF
    jmp NEAR PTR gwc_ret

print_solution  ENDP

;*****************************
;* HEX_TO_ASCII              *
;*****************************

; convert 8 bit hex number to it's ASCII representation
; upon entry cl contains number
; ah == 0 if leading zero on numbers a-f, nonzero if not
; bx -> place to put digits
; destroys al,cl
; updates bx,dl (number of chars in string)

hex_to_ascii    PROC
    mov al,cl
    and al,0f0h             ; get high nybble
    shr al,1                ; make value relative zero
    shr al,1
    shr al,1
    shr al,1                ; al has relative zero value
    cmp al,0ah              ; see if need to use hex numbers a-f
    jb  hta_3               ; no

    or  ah,ah               ; see if leading zero needed
    jne hta_2               ; no
    mov BYTE PTR [bx],'0'   ; put leading zero in number
    inc bx                  ; point to next char slot
    inc dl                  ; bump count of chars in number string
hta_2:
    add al,7                ; adjust for ASCII jump to alpha chars

hta_3:
    add al,30h              ; make number an ASCII representation
    mov [bx],al             ; save it to number string
    inc bx                  ; point to next char slot
    inc dl                  ; bump count of chars in number string

    and cl,0fh              ; get low nybble
    cmp cl,0ah              ; see if hex number a-f
    jb  hta_4               ; no
    add cl,7                ; adjust for ASCII jump to alpha chars
hta_4:
    add cl,30h              ; make number an ASCII representation
    mov [bx],cl             ; save it to number string
    inc bx                  ; point to next char slot
    inc dl                  ; bump count of chars in number string
    ret
hex_to_ascii    ENDP

;*****************************
;* FIXUP_WARNING             *
;*****************************

; special format warning for fixup overflow

fixup_warning   PROC
    push    es              ; save all used registers
    push    si
    push    di
    push    dx
    push    cx
    push    bx
    push    ax              ; save error code, ax is used
    cmp is_nowarn,0         ; see if warnings disabled
    je  fw_1                ; no
    jmp NEAR PTR fw_ret     ; yes

fw_1:

    mov bx,OFFSET DGROUP:error_table_start  ; point to start of error message address table
    mov ax,FIXUP_OVRFLW_WARN    ; get fixup overflow warning value
    shl ax,1                ; x2
    shl ax,1                ; x4
    shl ax,1                ; x8, ax holds offset into error table
    add bx,ax               ; bx -> proper entry in table
    mov di,bx               ; save -> entry

; print 'Warning in ' string
    mov bx,OFFSET DGROUP:sttyp_warning
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    xor cx,cx               ; cx will contain number of chars in file name
    mov dx,OFFSET DGROUP:filename
    mov bx,dx
fwloop1:
    cmp BYTE PTR [bx],0     ; see if zero terminator in file name found
    je  fw_2                ; yes
    inc cx                  ; bump count of chars in string
    inc bx                  ; move to next char slot to test
    jmp SHORT fwloop1       ; loop back to test next char

; write file name
fw_2:
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    call    print_offset

    xor ax,ax               ; fake out print_lib_mod to print offset
    mov temp_flag,ax
    push    di
    mov di,OFFSET DGROUP:temp_flag
    call    print_lib_mod   ; check if processing library, print module name/offset if so
    pop di

; print frame [group,segment,LOCATION] <name>
    mov ax,frame_ent_ptr
    or  ax,ax               ; make sure non-null entry
    jne fw_2a               ; okay
    jmp NEAR PTR fw_print_target    ; null, move to print target

; print 'Frame ' string
fw_2a:
    mov dx,OFFSET DGROUP:fw_frame_text
    mov cx,10
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; print 'group ', 'segment ', or 'LOCATION' string
    mov al,frame_method

fw_gsl:
    or  al,al
    je  fw_fr_segment       ; segment frame
    cmp al,1
    je  fw_fr_group         ; group frame
    cmp al,4
    je  fw_fr_loc           ; location frame
    cmp al,2
    je  fw_fr_ext           ; external frame

; assume use of target index (frame method 5)
    mov dx,targ_ent_ptr     ; get target entry pointer
    or  dx,dx               ; make sure non-null
    jne fw_2b               ; okay
    jmp NEAR PTR fw_print_prob  ; null, don't print any further

fw_2b:
    mov al,target_method
    and al,3                ; reduce to segment, group, or external value
    cmp al,2                ; check if external
    je  fw_gsl              ; yes, don't modify frame entry
    mov frame_ent_ptr,dx    ; make frame entry -> target entry
    jmp SHORT fw_gsl        ; route based on target method

fw_fr_ext:
    mov al,frext_ingroup
    or  al,al               ; see if external is in a group
    je  fw_fr_segment       ; no

fw_fr_group:
    mov dx,OFFSET DGROUP:fw_group_text
    mov cx,6
    jmp SHORT fw_print_gsl

fw_fr_segment:
    mov dx,OFFSET DGROUP:fw_seg_text
    mov cx,8
    jmp SHORT fw_print_gsl

fw_fr_loc:
    mov dx,OFFSET DGROUP:fw_loc_text
    mov cx,8

fw_print_gsl:
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov al,frame_method
    cmp al,4                ; see if LOCATION frame method
    je  fw_print_target     ; yes, bypass printing name
    mov bx,frame_ent_ptr

fw_3:
    or  al,al               ; see if segment
    je  fw_frseg_name       ; yes
    cmp al,1                ; see if group
    je  fw_frgrp_name       ; yes
    cmp al,2                ; see if external
    je  fw_frext_name       ; yes

; assume use of target index (frame method 5)
    mov al,target_method
    and al,3                ; reduce to segment, group, or external value
    jmp SHORT fw_3          ; route based on target method

fw_frext_name:
    mov al,frext_ingroup
    or  al,al               ; see if external is in a group
    je  fw_frseg_name       ; no

fw_frgrp_name:
    mov ds,bx               ; ds -> frame entry
    lds dx,ds:[4]           ; ds:dx -> group name
    jmp SHORT fw_fr_name

fw_frseg_name:
;***    cmp bx,-1               ; see if overlaid segment
;***    jne fw_frseg2
;***    mov dx,OFFSET DGROUP:ovlseg_text-8  ; ds:dx -> 'Overlaid segment', minus adjustment
;***    jmp SHORT fw_fr_name

fw_frseg2:
    mov ds,bx               ; ds -> frame entry
    lds dx,ds:[8]           ; ds:dx -> segment name

fw_fr_name:
    add dx,8                ; adjust past doubleword pointers
    mov bx,dx               ; ds:bx -> string

fwloop2:
    cmp BYTE PTR [bx],0     ; see  if zero terminator
    je  fw_4                ; yes, end of name reached
    inc bx
    jmp SHORT fwloop2       ; not at end of name, keep counting chars

; ds:dx -> string, bx-dx holds string char count
fw_4:
    sub bx,dx
    mov cx,bx               ; cx holds count of chars to print
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

; print target [group,segment,symbol] <name>
fw_print_target:
    mov ax,targ_ent_ptr
    or  ax,ax               ; make sure non-null entry
    jne fw_4a               ; okay
    jmp NEAR PTR fw_print_prob  ; null, move to print problem code

; print 'Target ' string
fw_4a:
    mov dx,OFFSET DGROUP:fw_targ_text
    mov cx,11
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; print 'group ','segment ', or 'symbol ' string
    mov al,target_method
    and al,3                ; reduce to segment, group, or external value
    je  fw_tar_segment      ; segment target
    cmp al,1
    je  fw_tar_group        ; group target

; assume external target (target method 3)
    mov dx,OFFSET DGROUP:fw_sym_text
    mov cx,7
    jmp SHORT fw_print_gss

fw_tar_group:
    mov dx,OFFSET DGROUP:fw_group_text
    mov cx,7
    jmp SHORT fw_print_gss

fw_tar_segment:
    mov dx,OFFSET DGROUP:fw_seg_text
    mov cx,8

fw_print_gss:
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; print target string
    mov bx,targ_ent_ptr
    mov al,target_method
    and al,3
    je  fw_tarseg_name      ; target segment
    cmp al,1                ; see if group
    je  fw_targrp_name      ; yes

; assume use of external (target method 2/6)
    mov ds,bx
    lds dx,ds:[4]           ; ds:dx -> pubdef name
    jmp SHORT fw_tar_name

fw_targrp_name:
    mov ds,bx               ; ds -> frame entry
    lds dx,ds:[4]           ; ds:dx -> group name
    add dx,8                ; adjust past double word pointers
    jmp SHORT fw_tar_name

fw_tarseg_name:
;***    cmp bx,-1               ; see if overlaid segment
;***    jne fw_tarseg2
;***    mov dx,OFFSET DGROUP:ovlseg_text    ; ds:dx -> 'Overlaid segment'
;***    jmp SHORT fw_tar_name

fw_tarseg2:
    mov ds,bx               ; ds -> frame entry
    lds dx,ds:[8]           ; ds:dx -> segment name
    add dx,8                ; adjust past double word pointers

fw_tar_name:
    mov bx,dx               ; ds:bx -> string

fwloop3:
    cmp BYTE PTR [bx],0     ; see  if zero terminator
    je  fw_5                ; yes, end of name reached
    inc bx
    jmp SHORT fwloop3       ; not at end of name, keep counting chars

; ds:dx -> string, bx-dx holds string char count
fw_5:
    sub bx,dx
    mov cx,bx               ; cx holds count of chars to print
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

    mov al,target_method
    cmp al,2                ; see if a target displacement
    ja  fw_print_prob       ; no, don't print one

; print ', displacement ' string and displacement
    mov dx,OFFSET DGROUP:fw_disp_text
    mov cx,15
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    xor dl,dl               ; dl will hold count of chars in print string
    mov bx,OFFSET DGROUP:number_hex ; point to number slot
    xor ah,ah               ; flag leading zero
    mov cl,BYTE PTR target_disp+1
    or  cl,cl               ; see if high byte is zero
    je  fw_6                ; yes
    call    hex_to_ascii    ; convert high word, high byte to hex
    mov ah,1                ; flag no leading zero

fw_6:
    mov cl,BYTE PTR target_disp
    or  cl,cl               ; see if low byte is zero
    jne fw_7                ; no

fw_7:
    call    hex_to_ascii    ; convert to hex
    mov BYTE PTR [bx],'h'   ; terminate hex number with 'h' suffix
    inc dl                  ; and bump count of chars

    xor ch,ch               ; zap high byte
    mov cl,dl               ; cx == number of chars to print
    mov dx,OFFSET DGROUP:number_hex ; ds:dx -> string to print
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

fw_print_prob:
    call    print_problem   ; print problem

    mov al,cascade_fixup
    or  al,al               ; see if solution was printed previously
    jne fw_ret              ; yes
    inc al                  ; no, store nonzero value to flag to indicate solution printed this pass
    mov cascade_fixup,al    ; flag that problem/solution was printed

fw_print_sol:
    call    print_solution  ; print solution

fw_ret:
    inc warn_count          ; bump count of warning messages
    pop ax                  ; restore registers
    pop bx
    pop cx
    pop dx
    pop di
    pop si
    pop es
    ret
fixup_warning   ENDP

;*****************************
;* DOS_ERROR                 *
;*****************************

; ax has dos error upon entry
; convert ax to warplink error code

dos_error   PROC
    mov bx,DGROUP
    mov ds,bx               ; point data segment at WarpLink's data
    mov BYTE PTR tmod_name,0    ; zero tmod_name so that if in library a bogus module name won't print
    cmp ax,2                ; file not found
    jne de_2
    mov ax,DOS_FILE_FND_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_2:
    cmp ax,3                ; path not found
    jne de_3
    mov ax,DOS_PATH_FND_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_3:
    cmp ax,4                ; no open handles
    jne de_4
    mov ax,DOS_OUT_HAND_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_4:
    cmp ax,5                ; access denied
    jne de_5
    mov ax,DOS_ACC_DEN_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_5:
    cmp ax,6                ; invalid handle
    jne de_6
    mov ax,DOS_INV_HAND_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_6:
    cmp ax,7                ; memory control blocks destroyed
    jne de_7
    mov ax,DOS_MCB_DSTRY_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_7:
    cmp ax,8                ; insufficient memory
    jne de_8
    mov ax,DOS_INSUF_MEM_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_8:
    cmp ax,9                ; invalid memory block address
    jne de_9
    mov ax,DOS_INV_MEM_BLK_ERR
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return

de_9:
    mov ax,UNKNOWN_ERR      ; not an expected dos error, change to unknown error
    jmp NEAR PTR link_error ; all DOS errors are fatal, don't return
dos_error   ENDP

IFNDEF JUNIOR

;*****************************
;* CHECK_XMS_ERROR           *
;*****************************

; check if XMS error, ax is ZERO if error upon entry
; if error, bl has xms error upon entry
; convert ax to warplink error code

check_xms_error PROC
    or  ax,ax
    je  cxe_err             ; error occurred

cxe_ret:
    ret

; XMS error occurred
cxe_err:
    cmp bl,0a0h             ; see if no more free memory (not really an error)
    je  cxe_ret             ; no more free memory
    mov cl,bl               ; get 8 bit error code in cl
    mov ax,XMS_XMM_ERR
    jmp NEAR PTR link_error ; all XMS errors are fatal, don't return

check_xms_error ENDP

ENDIF

;*****************************
;* GIVE_WARN_COUNT           *
;*****************************

; give count of warning messages, if any
; destroys ax,bx,cx,dx,si,di

give_warn_count PROC
    cmp warn_count,0        ; see if any warning messages
    je  gwc_ret             ; no
    cmp is_nowarn,0         ; see if warnings disabled
    jne gwc_ret             ; yes

gwc_2:
    mov bx,OFFSET DGROUP:warn_text
    mov cl,[bx-1]           ; get length of string
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

    mov di,OFFSET DGROUP:number_buff    ; point to temporary number buffer
    mov ax,ds
    mov es,ax               ; es -> warplink data
    xor cx,cx               ; init count of digits
    mov ax,warn_count       ; get count of warning messages

gwc_divloop:
    xor  dx,dx              ; zero high word value
    mov  bx,0AH             ; divide by 10
    div  bx
    xchg    dx,ax           ; swap quotient into dx, remainder into ax
    or   al,30H             ; make remainder into ASCII number
    stosb                   ; save char to buffer
    inc cx                  ; bump count of digits
    xchg    dx,ax           ; restore quotient to ax
    or   ax,ax              ; check if quotient is zero
    jne  gwc_divloop        ; no, continue dividing

    mov si,di
    mov di,OFFSET DGROUP:number_hex ; place to put unreversed number
    mov bx,cx               ; save count of chars in string

gwc_revloop:
    dec si                  ; si -> char in reversed number buffer
    mov al,[si]             ; get reversed char
    stosb                   ; put in unreversed buffer
    loop    gwc_revloop     ; unreverse as many chars as in number

    mov cx,bx               ; get count of chars in string
    mov dx,OFFSET DGROUP:number_hex ;  ds:dx -> buffer to write
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF

; always give a trailing CR/LF, even if no warnings
gwc_ret:
    mov cx,2
    mov dx,OFFSET DGROUP:warn_text
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h
IFNDEF JUNIOR
    call    restore_ems_map
ENDIF
    ret
give_warn_count ENDP

END
