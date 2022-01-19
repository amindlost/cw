;*********************************************************************
;*   MACHLINK.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/06/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   main driver for linker                                          *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK machlink
PAGE    50,80

IFNDEF  NODOS
    DOSSEG
ENDIF

.MODEL  SMALL
.STACK  400h

;*****************************
;* Include files             *
;*****************************

INCLUDE MLEQUATE.INC
INCLUDE MLGLOBAL.INC
INCLUDE MLERRMES.INC

;*****************************
;* Public declarations       *
;*****************************

PUBLIC  exit_link

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   mod_alloc_base:WORD
EXTRN	writing_qlk_flag:BYTE

;*****************************
;* Constant data             *
;*****************************

.CONST

beep3   DB  BELL,BELL,BELL

stpass_len      DB  stpass_stop-stpass_text
stpass_text     DB  CR,LF,'*** Start of pass '
stpass_num      DB  '1'
stpass_stop     =   $

endpass_len     DB  endpass_stop-endpass_text
endpass_text    DB  CR,LF,'*** End of pass '
endpass_num     DB  '1'
endpass_stop    =   $

IFNDEF DEMO
ddlpass_len     DB  ddlpass_stop-ddlpass_text
ddlpass_text    DB  CR,LF,'*** Start of main DDL processing pass'
ddlpass_stop    =   $
ENDIF

obj_len         DB  obj_stop-obj_text
obj_text        DB  ' on object modules'
obj_stop        =   $

lib_len         DB  lib_stop-lib_text
lib_text        DB  ' on library modules'
lib_stop        =   $

stwrite_len     DB  stwrite_stop-stwrite_text
stwrite_text    DB  CR,LF,'*** Begin writing file(s)'
stwrite_stop    =   $

endwrite_len    DB  endwrite_stop-endwrite_text
endwrite_text   DB  CR,LF,'*** End writing file(s)'
endwrite_stop   =   $

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   setup:NEAR,getargs:NEAR,credits:NEAR,summary:NEAR
EXTRN   get_memory:NEAR,free_memory:NEAR,parse:NEAR
EXTRN   pass1:NEAR,proc1_libs:NEAR
EXTRN   init_map:NEAR,setup_exe_image:NEAR,finish_map:NEAR
EXTRN   pass2:NEAR,proc2_libs:NEAR,write_program:NEAR,cleanup:NEAR
EXTRN   give_warn_count:NEAR,give_load_size:NEAR

EXTRN   resolve_communals:NEAR
;*** EXTRN   show_unreferenced:NEAR
EXTRN   ovl_entry_point:NEAR,do_incremental:NEAR
EXTRN   ilf_rewind:NEAR,ilf_write_eof:NEAR,check_ems:NEAR
EXTRN   alloc_ems_trans:NEAR,check_xms:NEAR
EXTRN	check_qlk:NEAR,write_qlk_unres:NEAR

IFNDEF DEMO
EXTRN   reinit_variables:NEAR
EXTRN	ddl_save_libmod_entry:NEAR,proc_ddl:NEAR,create_ddl:NEAR
ENDIF

;*****************************
;* MAIN                      *
;*****************************

main        PROC
start:
    call    setup           ; system changes, segment register setup, etc.
    call    getargs         ; get command line arguments from PSP
    call    credits         ; display linker credit line
    or  is_args,0           ; check for arguments to linker
    jne m2                  ; at least one argument
    call    summary         ; display summary of linker syntax/commands

to_exit_1:
    jmp NEAR PTR exit_1     ;   and exit program
m2:
    call    get_memory      ; allocate memory for file buffers and control blocks

    mov ax,allocation_base
    mov mod_alloc_base,ax   ; save base of allocations prior to any allocations

    call    parse           ; parse linker command line
    call    free_memory     ; de-allocate memory prior to new memory allocation

    call    check_ems       ; see if useable EMS
    call    check_xms       ; see if useable XMS
    cmp is_clpinc,0         ; see if clipper incremental link flag set
    je  m3                  ; no
    call    do_incremental  ; do incremental link or setup if no ILF flag
    cmp al,'N'              ; check if should exit link (success or can't incremental link)
	je	to_exit_1			; yes

; incremental link failed, continuing with full link
; reparse options in case of library module
	mov	ax,mod_alloc_base
	mov	allocation_base,ax	; reset memory allocation base
    call    get_memory		; allocate memory for file buffers and control blocks
    call    parse			; parse linker command line
    call    free_memory		; de-allocate memory prior to new memory allocation
    call    check_ems       ; see if useable EMS
    call    check_xms       ; see if useable XMS

m3:
    call    get_memory      ; allocate memory for file buffers and control blocks

    call    alloc_ems_trans ; allocate EMS transfer buffer if necessary

IFNDEF DEMO
    mov al,is_ddl
    or  al,use_ddl          ; see if creating or using DDL
    mov any_ddl,al          ; save any DDL usage flag
    je  m_nocreate          ; no
	mov	is_sympac,0			; no symbol table compaction with DDLs
    call    create_ddl      ; create the DDL
ENDIF

m_nocreate:
    mov ax,allocation_base
    mov mod_alloc_base,ax   ; save base of allocations prior to any module stuff

    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p1ostart            ; no
    mov bx,OFFSET DGROUP:stpass_text
    call    print_info
    mov bx,OFFSET DGROUP:obj_text
    call    print_info

p1ostart:
    call    pass1           ; perform first pass of linker
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p1oend              ; no
    mov bx,OFFSET DGROUP:endpass_text
    call    print_info
    mov bx,OFFSET DGROUP:obj_text
    call    print_info
    mov bx,OFFSET DGROUP:stpass_text
    call    print_info
    mov bx,OFFSET DGROUP:lib_text
    call    print_info

p1oend:
	cmp	is_quick,0			; see if quick linking
	je	p1_p1lib			; yes
	call	check_qlk		; check quick link file

p1_p1lib:
    call    proc1_libs      ; perform first pass library processing
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p1lend              ; no
    mov bx,OFFSET DGROUP:endpass_text
    call    print_info
    mov bx,OFFSET DGROUP:lib_text
    call    print_info

p1lend:
    mov al,'2'
    mov stpass_num,al       ; change pass number to 2
    mov endpass_num,al

IFNDEF DEMO
    cmp any_ddl,0           ; see if creating or using DDL
    je  m4                  ; no

    cmp udl_proc_pass,1     ; see if processing UDL w/libs
    jne no_udl              ; no

    call    free_memory     ; de-allocate memory prior to new memory allocation
    inc udl_proc_pass       ; bump flag to indicate UDL processing done
    call    reinit_variables    ; reset the variables
    mov module_count,0      ; reinit module count
    mov ax,mod_alloc_base
    mov allocation_base,ax  ; restore base of allocations prior to any module stuff
    call    ddl_save_libmod_entry   ; save the library module entries in low memory
    call    get_memory      ; allocate memory for file buffers and control blocks

    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p1ostart            ; no
    mov bx,OFFSET DGROUP:ddlpass_text
    call    print_info
    jmp NEAR PTR p1ostart      ; reloop and process as UDL

no_udl:
    call    proc_ddl        ; finish processing the DDL
    xor al,al               ; init return code to zero
    jmp NEAR PTR exit_link  ; done
ENDIF

m4:
    call    resolve_communals   ; resolve communal variables if any, adjust segments
    call    free_memory     ; de-allocate memory prior to new memory allocation

    call    ilf_rewind      ; rewind ilf file if exists

    call    init_map        ; if map file, write header info
    call    setup_exe_image ; compute segment frame values and allocate disk/memory for executable image

    call    ilf_write_eof   ; write eof mark to ilf file, if exists

    call    get_memory      ; allocate memory for file buffers and control blocks
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p2ostart            ; no
    mov bx,OFFSET DGROUP:stpass_text
    call    print_info
    mov bx,OFFSET DGROUP:obj_text
    call    print_info

p2ostart:
    call    pass2           ; perform second pass of linker
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p2oend              ; no
    mov bx,OFFSET DGROUP:endpass_text
    call    print_info
    mov bx,OFFSET DGROUP:obj_text
    call    print_info
    mov bx,OFFSET DGROUP:stpass_text
    call    print_info
    mov bx,OFFSET DGROUP:lib_text
    call    print_info

p2oend:
    call    proc2_libs      ; perform second pass library processing
	cmp	writing_qlk_flag,0	; see if writing qlk file
	je	p2_endlib			; no
	call	write_qlk_unres

p2_endlib:
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  p2lend              ; no
    mov bx,OFFSET DGROUP:endpass_text
    call    print_info
    mov bx,OFFSET DGROUP:lib_text
    call    print_info

p2lend:
    call    free_memory     ; de-allocate memory

    cmp ovl_count,0         ; see if any overlays
    je  mach_1              ; no
    call    ovl_entry_point ; make entry point go to overlay mananger

mach_1:
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  wstart              ; no
    mov bx,OFFSET DGROUP:stwrite_text
    call    print_info

wstart:
    call    write_program   ; write finished .COM or .EXE program
    call    finish_map      ; if map file, write remaining info
    cmp is_linkinfo,0       ; see if linker information to be printed
    je  wend                ; no
    mov bx,OFFSET DGROUP:endwrite_text
    call    print_info

wend:

;***    call    show_unreferenced   ; show symbols not referenced
    call    give_warn_count ; give count of warning messages, if any
    call    give_load_size  ; give EXE load image size

exit_1:
    xor al,al               ; init return code to zero
    cmp warn_count,0        ; see if any warnings were generated
    je  exit_link           ; no
    cmp is_exit0,0          ; see if warnings generate exit code of 0
    jne exit_link           ; yes
    inc al                  ; return exit code of 1 for warnings

exit_link:
    mov ah,4ch              ; terminate
    push    ax              ; save terminate and return code
    call    cleanup         ; clean up any interim system changes made

IFNDEF DEMO
    cmp is_beep,0           ; see if should beep
    je  exit_3              ; no

    mov bx,STDOUT
    mov dx,OFFSET DGROUP:beep3
    mov cx,3
    mov ah,40h              ; write to file or device
    int 21h                 ; beep the speaker three times
ENDIF

exit_3:
    pop ax                  ; restore terminate and return code
    int 21h
main        ENDP

;*****************************
;* PRINT_INFO                *
;*****************************

; print linker pass info
; upon entry bx -> text to print, with length byte preceding
; destroys ax,bx,cx,dx

print_info  PROC
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    ret
print_info  ENDP

END start
