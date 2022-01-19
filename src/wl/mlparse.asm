;*********************************************************************
;*   MLPARSE.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/17/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Parse linker command line or response file, nonoverlay options  *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlparse
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
PUBLIC  parse,check_extension,save_lib_name,in_overlay
PUBLIC	file_not_found,is_terminator,set_exe_name

; variables
PUBLIC  exe_pathspec,libtext,objtext,module_flag
PUBLIC  clip_libnum,clp5_lib,first_udlent_ptr
PUBLIC	rsp_line,parfilename,exe_ext_ptr
PUBLIC	ddl_depend_count

;*****************************
;* Data begins               *
;*****************************
     
.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   def_lib_flag:BYTE,temp_file_name:BYTE
EXTRN   dir_pages:WORD,lib_pages_count:WORD,lib_handle:WORD
EXTRN   lib_page_num:WORD,lib_page_size:WORD,dir_offset:DWORD
EXTRN	reading_qlk_flag:BYTE

parse_pass	DB	0			; parsing pass, redone on incremental linking
ddl_depend_count	DB	0	; count of dependent DDL's

; uninitialized local variables
.DATA?

; previously initialized variables
; byte values
in_overlay  DB  ?           ; 80h if parsed files are in any overlay,
                            ; 40h set if nonvectored root call overlay, 0 otherwise
arg_type    DB  ?           ; command argument type, 0==.obj,1==.exe,2==.map,3==.lib
no_extension    DB  ?       ; nonzero if file name parse has no extension
lib_nonmod_flag DB  ?       ; nonzero if library specified that is not a module (used for /udl)

; word values
exe_ext_ptr DW  ?           ; pointer to executable file's extension in exe_name, zero if none
first_udlent_ptr    DW  ?   ; pointer to first allocated use ddl entry
alloc_udlent_ptr    DW  ?   ; pointer to last allocated use ddl entrym

clip_libnum DW  ?			; library number of CLIPPER.LIB, if exists

; byte character strings
clp5_lib    DB  'CLIPPER.LIB',0,0

; previously uninitialized variables
; byte values
plus_flag   DB  ?           ; nonzero if plus sign was parsed in response file
comma_flag  DB  ?           ; nonzero if commas was parsed in response file
comment_flag    DB  ?       ; nonzero if comment line in response file
module_flag DB  ?           ; nonzero if library specified was specific module

; word values
rsp_handle  DW  ?           ; response file handle
libname_ptr DW  ?           ; pointer to current library name, including path and drivespec if necessar

; byte character strings
parfilename	DB	128 DUP (?)	; file name being parsed from command line
rsp_line    DB  128 DUP (?) ; line read from response file
modname     DB  126 DUP (?) ; name of library module specified in command line
exe_pathspec    DB  127 DUP (?) ; path and drive spec of EXE file, if any
tbuff   DB  5 DUP (?)       ; temporary buffer

;*****************************
;* Constant data             *
;*****************************

.CONST

linktext	DB	LINKERCAPS,'='
libtext		DB  'LIB='		; environment string to check for library file path
objtext		DB  'OBJ='		; e-var to check for object module path
parsetext	DB	CR,LF,'*** Locating library module: ',0

;*****************************
;* Code begins               *
;*****************************
     
.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN   check_libobj_path:NEAR,get_lib_sys_info:NEAR,remove_dup_pages:NEAR
EXTRN   make_libmod_entry:NEAR

EXTRN   parse_ovl_option:NEAR,save_ovl_name:NEAR,parse_clp_option:NEAR
EXTRN   read_to_ems:NEAR
EXTRN	is_qlk_file:NEAR

IFNDEF DEMO
EXTRN	parse_ddl_option:NEAR
ENDIF

;*****************************
;* PARSE                     *
;*****************************

; main parsing loop
; destroys ax,bx,cx,dx,si,di,es

parse       PROC
	inc	parse_pass			; bump parsing pass count
    call    get_env_options ; get any options specified by environment
    xor ax,ax
    mov current_lib,ax      ; init current library module number for module specification
    mov prev_libname,al     ; zero out previous name of library for module specification
	mov	in_overlay,al
	mov	arg_type,al
	mov	no_extension,al
	mov	lib_nonmod_flag,al
	mov	exe_ext_ptr,ax
	mov	first_udlent_ptr,ax
	mov	alloc_udlent_ptr,ax
	mov	obj_count,ax
	mov	lib_count,ax
	mov	first_libblk_ptr,ax
	mov	last_libblk_ptr,ax
	mov	last_objblk_ptr,ax
	mov	clip_libnum,-1

    mov si,OFFSET DGROUP:cmd_line+1 ; point to command line, past char count
    call    deplus          ; change plus signs to spaces
    mov si,OFFSET DGROUP:cmd_line+1 ; point to modified command line, past char count
ploop1:
    mov di,si               ; save first char of parse string
    lodsb                   ; get char from command line string
    or  al,al               ; if zero then end of command line string
    jne parse_char          ; not at end
    jmp NEAR PTR endparse   ; at end

parse_char:
    cmp al,' '              ; ignore white space chars
    jbe ploop1              ; loop for next char
    cmp al,'/'              ; linker option
    je  parse_option
    cmp al,'@'              ; response file
    je  parse_response
    cmp al,'('              ; begin overlay
    je  parse_beginovl
    cmp al,'['              ; begin nonvectored root call overlay
    je  parse_beginovl
    cmp al,')'              ; end overlay
    je  parse_endovl
    cmp al,']'              ; end overlay
    je  parse_endovl
    cmp al,','              ; next command line argument type
    je  next_argtype
    cmp al,';'              ; end of command line argument
    je  endparse            ; all done parsing, ignore anything after semi-colon

parse_name:                 ; file name with or without path
    call    parse_file_type
    jmp SHORT ploop1        ; parse next char

parse_beginovl:             ; begin overlay
    cmp arg_type,0          ; only allow overlays for object modules and libraries
    je  pb_2                ; okay, object module
    cmp arg_type,3          ; check if library
    jne parse_name          ; no, assume file name

pb_2:
    cmp in_overlay,0        ; check flag to make sure not already in an overlay
    jne bad_left_paren      ; already in an overlay
    mov in_overlay,80h      ; set flag
    cmp al,'('              ; see if nonvectored root call overlay
    jne ploop1              ; no, parse next char
    or  in_overlay,40h      ; set nonvectored root call overlay
    jmp SHORT ploop1        ; parse next char

bad_left_paren:             ; nested parenthesis, fatal error
    mov ax,NEST_PAREN_ERR   ; put warplink error code in ax
    jmp NEAR PTR link_error ; transfer control to error handler

parse_endovl:               ; end overlay
    cmp arg_type,0          ; only allow overlays for object modules and libraries
    je  pe_2                ; okay, object module
    cmp arg_type,3          ; check if library
    jne parse_name          ; no, assume file name

pe_2:
    cmp in_overlay,0        ; check flag to make sure in overlay
    je  bad_right_paren     ; not in an overlay
    mov in_overlay,0        ; reset flag
    jmp SHORT ploop1

bad_right_paren:            ; improperly matched parenthesis, fatal error
    mov ax,UNM_RIGHT_PAREN_ERR  ; put warplink error code in ax
    jmp NEAR PTR link_error ; transfer control to error handler

next_argtype:               ; next command line argument type
    mov al,arg_type         ; get previous arg_type value
    inc al                  ; bump it up one
    mov arg_type,al         ; save back to memory variable
    cmp al,5                ; check bounds
    jae endparse            ; ignore input after .lib name
    jmp NEAR PTR ploop1

parse_option:               ; linker option
    call    set_option
    jmp NEAR PTR ploop1

parse_response:             ; response file
    call    use_rsp
    or  cl,cl               ; see if parsing termination occurred in response file
    jne endparse            ; yes
    jmp NEAR PTR ploop1     ; no

endparse:
    cmp in_overlay,0        ; check flag to make sure not still in an overlay
    je  ep_chk_obj          ; not in an overlay, no overlay error
    mov ax,UNM_LEFT_PAREN_ERR   ; put warplink error code in ax
    jmp NEAR PTR link_error ; transfer control to error handler

; check that at least one object module was given in command line
ep_chk_obj:
    xor ax,ax
    cmp obj_count,ax        ; is count of object modules nonzero
    jne ep_2                ; yes
    cmp is_ddl,al           ; see if creating DDL
    jne ep_noobj_ok         ; yes
    cmp use_ddl,al          ; see if using DDL's
    je  ep_noobj            ; no, must specify object module name

; using or creating DDL
ep_noobj_ok:
    cmp lib_count,ax        ; see if count of libraries is zero
    jne ep_2                ; no, okay if no object modules

; no object modules specified, or DDL and no object or library modules
ep_noobj:
    mov ax,NO_OBJMOD_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

ep_2:
    push    ds
    pop es                  ; es -> warplink data
    mov si,OFFSET DGROUP:exe_name   ; point to executable name
    cmp [si],al             ; check if name exists, nonzero first filename char
    jne ep_3                ; yes

; no EXE name given, use first object module name
    cmp obj_count,ax        ; see if any object modules (can only be zero if DDL's)
    je  ep_noobj            ; no object module, no EXE (DDL) name, give error
	call	set_exe_name

ep_3:
    mov al,is_ddl
    or  al,use_ddl          ; see if DDLs used
    je  ep_notddl           ; no

; DDL's are used, force DDL extension
    mov si,exe_ext_ptr      ; see if need to add an extension to executable file name
    or  si,si
    je  ep_changext         ; no, must change pre-existing extension
    mov BYTE PTR [si],'.'   ; both extensions have period in name

ep_makeddl:
    mov BYTE PTR [si+1],'D' ; add DDL extension
    mov BYTE PTR [si+2],'D'
    mov BYTE PTR [si+3],'L'
    xor ax,ax
    jmp SHORT ep_nullterm

; extension exists, but it's not DDL
ep_changext:
    mov di,OFFSET DGROUP:exe_name   ; si -> start of executable name
    mov ch,1                ; name known shorter than 256 bytes
    xor al,al
    repne   scasb           ; find end of name
    sub di,2                ; di -> last char in name
    std                     ; string ops auto decrement
    mov al,'.'              ; look backwards for period, cx known to be >128 bytes max name length
    repne   scasb
    cld                     ; string ops auto increment as normal
    mov si,di               ; si -> char before '.'
    inc si                  ; si -> '.'
    jmp SHORT ep_makeddl    ; add DDL extension

ep_notddl:
    xor ax,ax
    mov si,exe_ext_ptr      ; see if need to add an extension to executable file name
    or  si,si               ; if zero, then no extension will be added
    je  ep_4                ; don't add default extension
    mov BYTE PTR [si],'.'   ; both extensions have period in name

;***IFNDEF DEMO
    cmp is_comfile,al       ; if .com file add .com extension
    je  add_exe             ; .exe file
    mov BYTE PTR [si+1],'C' ; add COM extension
    mov BYTE PTR [si+2],'O'
    mov BYTE PTR [si+3],'M'
    jmp SHORT ep_nullterm   ; bypass EXE extension code
;***ENDIF

add_exe:
    mov BYTE PTR [si+1],'E' ; add EXE extension
    mov BYTE PTR [si+2],'X'
    mov BYTE PTR [si+3],'E'

ep_nullterm:
    mov BYTE PTR [si+4],al  ; null terminate filename string with extension

ep_4:
    cmp is_mapfile,al       ; see if map file indicated
    je  par_ovl             ; no
    mov di,OFFSET DGROUP:map_name   ; point to map name
    cmp BYTE PTR [di],al    ; check if name exists, nonzero first filename char
    jne par_ovl             ; yes

; use executable file name, without extension, as map file name
    mov di,OFFSET DGROUP:parfilename	; di -> filename string
    mov si,OFFSET DGROUP:exe_name

; ds:si -> name of first object module, es:di -> filename slot
    xor bx,bx               ; bx will index into strings
    mov dx,bx               ; dx will hold position of last found period

; find end of string, not counting extension
mp_2loop:
    mov al,BYTE PTR [bx+si] ; get char from string
    or  al,al               ; check if char in string is null terminator
    je  mp_2a               ; zero char, end of string found
    cmp al,'\'              ; backslash char resets last found period position
    jne mp_2loopa           ; bypass backslash code
    xor dx,dx               ; reset last char position
    jmp SHORT mp_2loopb     ; bypass period check code
mp_2loopa:
    cmp al,'.'              ; is char a period
    jne mp_2loopb           ; no
    mov dx,bx               ; track last found period
mp_2loopb:
    inc bx                  ; bump char count
    jmp SHORT mp_2loop      ; loop back for next char

mp_2a:
    or  dx,dx               ; was a period found not in a path (extension marker)
    je  mp_2b               ; no
    mov bx,dx               ; set termination position at period

mp_2b:    
    mov BYTE PTR [bx+di],0  ; put null terminator in filename string
mp_2c:
    dec bx                  ; back up one char
    mov al,[bx+si]          ; get object module name char
    mov [bx+di],al          ; transfer it
    or  bx,bx               ; if bx is zero then transfer is done
    jne mp_2c               ; not done, transfer next char

    mov no_extension,1      ; flag that file name does not have an extension (add .MAP extension)
    call    save_map_name   ; filename contains first object module name as map file

; check if overlay file needed, executable name still in filename variable
par_ovl:
    xor ax,ax
    cmp is_comfile,al       ; see if COM file
    je  par_saveovl         ; no
    cmp is_anyovls,al       ; see if any overlays specified
    je  par_chkconf         ; no, check com file/dynamic libraries conflict

; COM file specified with overlays
    mov ax,OVLCOM_CLASH_ERR
par_to_err:
    jmp NEAR PTR link_error

par_saveovl:
    mov al,is_ddl
    or  al,use_ddl          ; see if creating/using DDL's
    jne par_chkconf         ; yes, don't save overlay file name
    call    save_ovl_name   ; yes, save overlay file name

par_chkconf:
    xor ax,ax
    cmp is_ddl,al           ; see if create dynamic library option
    je  chk_use             ; no 
    mov use_ddl,al          ; shut off use dynamic library option
    mov is_dosseg,al        ; shut off explicit DOSSEG
    mov is_smartmem,al      ; shut off smartmem use
    jmp SHORT chk_com

chk_use:
    cmp use_ddl,al          ; see if use dynamic library option
    je  par_ret             ; no
    cmp lib_nonmod_flag,0   ; see if any libraries specified with /udl
    je  chk_com             ; no
    mov udl_proc_pass,1     ; flag udl processing, pass 1

chk_com:
    mov is_no_ems,1         ; shut off use of EMS when using DDLs
    mov is_nodeflib,1       ; shut off default libraries, too
    mov is_clpinc,al        ; force clipper incremental linking shut off
    mov is_xms,al           ; shut of extended memory use
    cmp is_comfile,al       ; see if COM file
    je  par_ret             ; no

; conflict with /c and dynamic libraries
par_conflict:
    mov ax,COMCONFLICT_ERR
    jmp SHORT par_to_err

par_ret:
    ret
parse       ENDP

;*****************************
;* SET_EXE_NAME              *
;*****************************

; set the exe_name variable if not set
; destroys ax,bx,dx,di,si,es

set_exe_name	PROC
	push	ds
	pop	es					; es -> warplink data
    mov ds,first_objblk_ptr ; ds -> block containing name of first object module
    mov si,4                ; si -> first name in block
    inc si                  ; adjust past prepended flag byte
    mov di,OFFSET DGROUP:parfilename	; di -> filename string

; ds:si -> name of first object module, es:di -> filename slot
    xor bx,bx               ; bx will index into strings
    mov dx,bx               ; dx will hold position of last found period

; find end of string, not counting extension
ep_2loop:
    mov al,BYTE PTR [bx+si] ; get char from string
    or  al,al               ; check if char in string is null terminator
    je  ep_2a               ; zero char, end of string found
    cmp al,'\'              ; backslash char resets last found period position
    jne ep_2loopa           ; bypass backslash code
    xor dx,dx               ; reset last char position
    jmp SHORT ep_2loopb     ; bypass period check code
ep_2loopa:
    cmp al,'.'              ; is char a period
    jne ep_2loopb           ; no
    mov dx,bx               ; track last found period
ep_2loopb:
    inc bx                  ; bump char count
    jmp SHORT ep_2loop      ; loop back for next char

ep_2a:
    or  dx,dx               ; was a period found not in a path (extension marker)
    je  ep_2b               ; no
    mov bx,dx               ; set termination position at period

ep_2b:    
    mov BYTE PTR es:[bx+di],0   ; put null terminator in filename string
ep_2c:
    dec bx                  ; back up one char
    mov al,[bx+si]          ; get object module name char
    mov es:[bx+di],al       ; transfer it
    or  bx,bx               ; if bx is zero then transfer is done
    jne ep_2c               ; not done, transfer next char

    push    es
    pop ds                  ; ds -> warplink data
    mov no_extension,1      ; flag that file name does not have an extension (set up for .COM or .EXE addition)
    call    save_exe_name   ; filename contains first object module name as executable file
	ret
set_exe_name	ENDP

;*****************************
;* SET_OPTION                *
;*****************************

; set linker option
; ds:si points to option char upon entry
; destroys ax,bx,dx,di,es
; updates si

set_option  PROC
    push    cx              ; save critical register
    mov cx,si               ; cx -> start of option
    lodsb                   ; get option char from command line string
    cmp al,'A'              ; check for out of bounds char value, too low
    jb  so_bad              ; out of bounds
    cmp al,'z'              ; check for out of bounds char value, too high
    ja  so_bad              ; out of bounds
    and al,0dfh             ; force char to be uppercase for following compares

; determine the linker option.
; set the option with a nonzero value, for best efficiency use AL as the
; value for Boolean flags since all allowed options are nonzero chars.

    cmp al,'A'              ; maximum Allocated Space option
    jne so_cla
    mov al,[si]             ; peek ahead at next char
    cmp al,'s'              ; see if allocated space option
    je  so_as               ; yes
    cmp al,'S'              ; check for uppercase option
    jne so_bad              ; no
so_as:
    mov is_maxparval,al     ; set maximum allocation space flag
    call    get_maxpar_size ; get size of allocated space
    jmp SHORT to_cont_opt_chk   ; continue checking options

so_cla:
    cmp al,'C'              ; create com file option
    jne so_2

    mov al,[si]             ; peek ahead at next char
    cmp al,'l'              ; see if clarion or clipper option
    je  so_1a               ; possibly
    cmp al,'L'              ; check uppercase clarion or clipper option
;***IFNDEF DEMO
    jne so_com              ; no
;***ELSE
    jne so_bad              ; no
;***ENDIF

so_1a:
    lodsb                   ; gobble 2nd char of clarion option
    mov al,[si]             ; peek ahead at next char
IFNDEF DEMO
    cmp al,'a'              ; see if clarion option
    je  so_1b               ; yes
    cmp al,'A'              ; check uppercase clarion option
    je  so_1b               ; yes
ENDIF
    cmp al,'p'              ; see if clipper option
    je  so_clp              ; yes
    cmp al,'P'              ; check uppercase clipper option
    jne so_bad              ; no, bad option
    jmp SHORT so_clp

IFNDEF DEMO
so_1b:
    lodsb                   ; gobble 3rd char of clarion option
    mov is_clarion,al       ; set clarion flag
    jmp SHORT to_cont_opt_chk   ; continue checking options
ENDIF

so_clp:
    call    parse_clp_option    ; parse the clipper options
    jmp NEAR PTR cont_opt_chk

so_com:
    mov is_comfile,al       ; set com file flag, AL is known nonzero value

to_cont_opt_chk:
    jmp NEAR PTR cont_opt_chk

so_2:
    cmp al,'D'              ; specify DOSSEG segment ordering/DDL option
    jne so_3
IFNDEF DEMO
    call    parse_ddl_option    ; either DDL option or DOSSEG option
    jmp NEAR PTR cont_opt_chk
ENDIF

; intermediate jump to bad_option code
so_bad:
    jmp NEAR PTR bad_option

so_3:
IFNDEF DEMO
    cmp al,'B'              ; see if beep on exit
    jne so_4                ; no
    mov is_beep,al          ; set beep flag
    jmp NEAR PTR cont_opt_chk
ENDIF

so_4:
    cmp al,'M'              ; create map file option
    jne so_5
    mov is_mapfile,al       ; set map file flag

    mov al,[si]             ; peek ahead at next char
    cmp al,'x'              ; see if expanded map file option
    je  so_4a               ; yes
    cmp al,'X'              ; check uppercase expanded map file option
    jne to_cont_opt         ; no

so_4a:
    lodsb                   ; gobble expanded map option char
    mov is_mapexpand,al     ; set expanded map file flag

to_cont_opt:
    jmp NEAR PTR cont_opt_chk

so_5:
    cmp al,'T'              ; see if possible temporary file option
    jne so_6                ; no
    mov al,[si]             ; peek ahead at next char
    cmp al,'f'              ; see if temporary file name option
    je  so_tfile            ; yes
    cmp al,'F'              ; check for uppercase option
    jne so_bad              ; no

so_tfile:
    lodsb                   ; gobble second char
    lodsb                   ; get colon
    cmp al,':'              ; must be colon
    jne so_bad              ; not a colon
    call    get_tempfile_name   ; get the temporary file name
    jmp NEAR PTR cont_opt_chk

so_6:
    cmp al,'S'              ; make symbols case sensitive, set stack, or sympac option
    jne so_info
    mov ah,[si]             ; peek ahead at next char
    cmp ah,'T'              ; see if set stack option
    je  so_stack
    cmp ah,'t'              ; check if lowercase set stack option
    jne so_checksp          ; no

so_stack:
    mov is_stackval,ah      ; set stack value set flag
    call    get_stack_size  ; get size of stack
    jmp NEAR PTR cont_opt_chk

so_checksp:
    cmp ah,'P'              ; see if symbol table compaction option
    je  so_sympac
    cmp ah,'p'              ; check if lowercase
    jne so_case             ; no, case sensitive flag

so_sympac:
    mov is_sympac,al		; set symbol pack flag
	mov	is_clpinc,0			; kill incremental linking
    inc si                  ; point si past 'P'
    jmp NEAR PTR cont_opt_chk

so_case:
IFNDEF DEMO
    mov is_casesense,al     ; set case sensitive flag
    jmp NEAR PTR cont_opt_chk
ENDIF

so_info:
    cmp al,'I'              ; see if print linker info
    jne so_7                ; no
    mov is_linkinfo,al      ; set info flag
    jmp NEAR PTR cont_opt_chk

so_7:
    cmp al,'O'              ; Overlay option
    jne so_udl
    call    parse_ovl_option
    jmp NEAR PTR cont_opt_chk

so_udl:
IFNDEF DEMO
    cmp al,'U'              ; possible Use Dynamic Libraries option
    jne so_8
    call    parse_udl_option
    jmp NEAR PTR cont_opt_chk
ENDIF

so_8:
    cmp al,'N'              ; No default libraries option
    jne so_9
    mov al,[si]             ; peek ahead at next char
    cmp al,'d'              ; see if no default libraries option
    je  so_nodef            ; yes
    cmp al,'D'              ; check for uppercase option
    jne bad_option          ; no

so_nodef:
    lodsb                   ; gobble second char
    mov is_nodeflib,al      ; set no default libraries flag
    jmp SHORT cont_opt_chk

so_9:
IFNDEF DEMO
    cmp al,'E'              ; extended libraries option, maybe, needs second char
    jne so_chkquick
    mov al,[si]             ; peek ahead at next char
    cmp al,'m'              ; see if Microsoft extended library option
    je  so_9a               ; yes
    cmp al,'M'              ; check for uppercase option
    jne bad_option          ; no
so_9a:
    lodsb                   ; gobble second char
    mov is_msextlib,al      ; set Microsoft extended library flag
    jmp SHORT cont_opt_chk
ENDIF

so_chkquick:
    cmp al,'Q'				; maybe quick linker option
    jne so_10
    mov al,[si]				; peek ahead at next char
    cmp al,'l'
    je  so_quicka			; yes
    cmp al,'L'				; check for uppercase option
    jne bad_option			; no
so_quicka:
    lodsb
    mov is_quick,'X'		; set quick linker flag, unknown reading or writing
    jmp SHORT cont_opt_chk

so_10:
    cmp al,'R'              ; reload active swapped out overlays option
    jne so_expand
    mov is_reload,al        ; set reload overlays flag
    jmp SHORT cont_opt_chk

so_expand:
    cmp al,'X'              ; expanded/extended memory use, maybe
    jne so_warn
    lodsb                   ; get next char
    cmp al,'p'              ; see if expanded memory use
    je  so_ems              ; yes
    cmp al,'P'              ; check for uppercase option
    jne so_chkxms           ; no
so_ems:
    mov is_no_ems,0         ; reset no use of EMS flag
    jmp SHORT cont_opt_chk

so_chkxms:
    cmp al,'t'              ; see if extended memory use
    je  so_xms              ; yes
    cmp al,'T'              ; check for uppercase option
    jne bad_option          ; no
so_xms:
    mov is_xms,al           ; set XMS flag
    jmp SHORT cont_opt_chk

so_warn:
    cmp al,'W'              ; disable warnings or generate 0 exit code for warnings
    jne bad_option
    lodsb                   ; get second char
IFNDEF DEMO
    cmp al,'N'              ; see if disable warnings option
    je  so_diswarn          ; yes
    cmp al,'n'              ; check if lowercase disable warnings option
    je  so_diswarn          ; yes
ENDIF
    cmp al,'0'              ; see if warnings exit code of 0 option
    jne bad_option          ; no, bad option

; set warnings generate exit code of 0 option
so_warnzero:
    mov is_exit0,al
    jmp SHORT cont_opt_chk

; set disable warnings flag
so_diswarn:
    mov is_nowarn,al
    jmp SHORT cont_opt_chk

; no valid options left
bad_option:                 ; bad option specified
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

cont_opt_chk:               ; check for non-whitespace chars after option
    mov al,[si]             ; peek ahead at following char
    cmp al,'/'              ; if another option then okay
    je  so_ret
    cmp al,' '              ; if zero or whitespace then okay
    ja  bad_option          ; non-whitespace, non-option char follows, bad option

so_ret:
    pop cx                  ; restore critical register
    ret
set_option  ENDP

;*****************************
;* GET_STACK_SIZE            *
;*****************************

; get size of stack specified on command line
; ds:si -> 'T' ('t') upon entry, cx -> start of option
; destroys ax
; updates si

get_stack_size  PROC
    push    bx              ; save critical register
    push    dx
    push    cx
    inc si                  ; point si past 'T'
    lodsb                   ; get next char
    cmp al,':'              ; should always be a colon
    jne gss_bad_option      ; error, not a colon

    call    get_int_value   ; get value of stack size
    jc  gss_bad_option      ; carry flag set, bad value

    add ax,1
    and ax,0fffeh           ; make value even, round up
    je  gss_bad_option      ; value was either 0 or 65535, bad value
    mov stack_value,ax      ; save value

gss_ret:
    pop cx                  ; restore critical register
    pop dx
    pop bx
    ret

gss_bad_option:
    pop cx                  ; restore cx -> start of option
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

get_stack_size  ENDP

;*****************************
;* GET_MAXPAR_SIZE           *
;*****************************

; get size of maximum paragraph allocation specified on command line
; ds:si -> 'S' ('s') upon entry, cx -> start of option
; destroys ax
; updates si

get_maxpar_size  PROC
    push    bx              ; save critical register
    push    dx
    push    cx
    inc si                  ; point si past 'S'
    lodsb                   ; get next char
    cmp al,':'              ; should always be a colon
    jne gms_bad_option      ; error, not a colon

    call    get_int_value   ; get value of stack size
    jc  gms_bad_option      ; carry flag set, bad value

    mov maxpar_value,ax     ; save value

    pop cx                  ; restore critical register
    pop dx
    pop bx
    ret

gms_bad_option:
    pop cx                  ; restore cx -> start of option
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

get_maxpar_size  ENDP

;*****************************
;* GET_INT_VALUE             *
;*****************************

; get int (0-65535) value from command line, return in ax
; return carry flag set if invalid value, reset if valid
; upon entry ds:si-> first digit
; destroys ax,bx,cx,dx
; updates si

get_int_value   PROC
    xor cx,cx               ; cx will hold count of digits
    mov bx,OFFSET DGROUP:tbuff  ; bx -> buffer to hold digits

giv_scanloop:
    lodsb                   ; get number
    cmp al,'0'              ; see if nondigit
    jb  giv_done            ; yes, done
    cmp al,'9'              ; see if nondigit
    ja  giv_done            ; yes
    inc cx                  ; bump count of digits
    cmp cx,5                ; check bounds
    ja  giv_badval          ; more than five digits, bad value
    and al,0fh              ; strip ASCII value
    mov [bx],al             ; save digit
    inc bx                  ; bump to next save char slot
    jmp SHORT giv_scanloop  ; loop for next

giv_done:
    dec si                  ; drop si back to -> nondigit char
    mov bx,OFFSET DGROUP:tbuff  ; bx -> buffer holding digits
    xor ax,ax               ; ax holds value

giv_mulloop:
    mov dl,[bx]             ; get char value
    xor dh,dh               ; zero high byte
    add ax,dx               ; add to previous stack value
    jc  giv_badval          ; overflow, value too large
    dec cx                  ; drop count of digits
    jcxz    giv_goodval     ; all done
    mov dx,10
    mul dx                  ; shift value by 1 digit (*10)
    or  dx,dx               ; check if overflow
    jne giv_badval          ; overflow occurred
    inc bx                  ; move to next digit
    jmp SHORT giv_mulloop   ; add in value of next digit

giv_goodval:
    clc                     ; clear carry flag to indicate good number
    ret

giv_badval:
    stc                     ; set carry flag ot indicate bad number
    ret
get_int_value   ENDP

;*****************************
;* DEPLUS                    *
;*****************************

; remove plus signs from passed command line string
; ds:si point to string upon entry
; destroys ax,si

deplus      PROC
dp2:
    lodsb                   ; get char from command line string
    or  al,al               ; if zero then end of command line string
    je  dp4
    cmp al,CR               ; if carriage return then end of command line string
    jne dp3                 ; not a carriage return
    mov BYTE PTR [si-1],0   ; replace carriage return with null terminator
    jmp SHORT dp4           ; flag from CR compare, end of command line string
dp3:
    cmp al,'+'              ; replace plus signs with spaces
    jne dp2
    mov BYTE PTR [si-1],' ' ; replace with space
    jmp SHORT dp2
dp4:
    ret
deplus      ENDP

;*****************************
;* USE_RSP                   *
;*****************************

; use response file
; ds:si points to first filename char upon entry
; destroys ax,bx,cx,dx,di,es
; return cl == nonzero if parsing termination occurred (';' or
;   all phases encountered), cl ==0 if not
; updates si past file name

use_rsp     PROC
    lodsb                   ; point at first char of file name
    call    get_file_name   ; setup filename string
    push    si              ; save si -> first char after file name
    mov dx,OFFSET DGROUP:parfilename	; ds:dx -> ASCIIZ file specification
    mov ax,3d00h            ; open file for read access
    int 21h
    jnc ur_1                ; no error opening file
    jmp NEAR PTR dos_error  ; error opening file

ur_1:
    mov rsp_handle,ax       ; save response file handle

ur_readloop:
    xor al,al
    mov plus_flag,al        ; reset plus flag
    mov comma_flag,al       ; reset comma flag
    mov comment_flag,al     ; reset comment flag
    mov bx,rsp_handle
    mov cx,128              ; number of bytes to read
    mov dx,OFFSET DGROUP:rsp_line   ; buffer to dump bytes read from response file
    mov ah,3fh              ; read file
    int 21h
    jnc ur_2                ; no error reading file
    mov dx,OFFSET DGROUP:parfilename
    jmp NEAR PTR dos_error  ; error reading file

ur_2:
    or  ax,ax               ; check if at end of file
    jne ur_2a               ; no
to_ur_no_term:
    jmp NEAR PTR ur_no_term ; yes

ur_2a:
    mov cx,ax               ; keep count of read chars
    mov si,OFFSET DGROUP:rsp_line   ; point to base of response file line

    cmp BYTE PTR [si],'#'   ; see if comment line
    jne ur_notcomment       ; no
    mov comment_flag,1      ; flag comment line

ur_notcomment:
    cmp ax,128              ; see if all chars were read in line
    je  ur_eol_chk          ; yes, bypass next char slot code
    mov dx,si               ; save -> base of response file line
    add si,ax               ; si -> first char slot past last read-in char
    mov BYTE PTR [si],0     ; zero terminate response file line
    mov si,dx               ; restore si -> base of response file line
    jmp SHORT ur_eol_chk    ; bypass next char slot code

ur_next_char:
    inc si                  ; point to next char

ur_eol_chk:
    cmp si,OFFSET DGROUP:rsp_line+127
    jb  ur_3                ; no

; response line longer than 127 characters (not counting CR/LF)
    mov ax,RSP_LINE_ERR     ; response line too long
    jmp NEAR PTR link_error ; transfer control to error handler

ur_3:
    mov al,[si]
    or  al,al               ; zero char means end of file reached
    je  to_ur_no_term       ; route to end of parse routine jump

    cmp al,CR               ; check for carriage return, end of line
    jne ur_4

    inc si                  ; bump to account for line feed char
    xor al,al
    cmp comment_flag,al     ; see if comment line
    jne ur_rewind_chk       ; yes, don't modify argument type
    cmp plus_flag,al        ; see if line ended with '+' continuation char
    jne ur_rewind_chk       ; yes, argument type remains the same
    cmp comma_flag,al       ; see if line ended with comma (next argument type already set)
    jne ur_rewind_chk       ; yes, argument type already updated

; no line continuation character or previous comma update, move to next argument type
    mov al,arg_type         ; get previous arg_type value
    inc al                  ; bump it up one
    mov arg_type,al         ; save back to memory variable
    cmp al,5                ; check bounds
    jb  ur_rewind_chk       ; within bounds, continue parsing
    jmp NEAR PTR ur_terminate   ; out of bounds (beyond .lib), flag termination of parsing

ur_rewind_chk:
    mov ax,OFFSET DGROUP:rsp_line-1 ; get base of buffer less 1
    add ax,cx               ; compute end of last read
    cmp ax,si               ; see if CR/LF was at end of last read
    ja  ur_endlogic         ; no, rewind file
    jmp NEAR PTR ur_readloop    ; yes, get next line, if any

; end of logical line reached before end of physical line, rewind file to end of logical line
ur_endlogic:
    sub ax,si               ; see how many bytes to rewind file
    mov dx,ax               ; dx holds file offset low word
    neg dx                  ; make it a negative number
    mov cx,0ffffh           ; sign extend thru cx
    mov bx,rsp_handle       ; get response file handle
    mov ax,4201h            ; move file pointer from current location
    int 21h                 ; rewind file
    jmp NEAR PTR ur_readloop

ur_4:
    cmp comment_flag,0      ; see if comment line
    jne ur_next_char        ; yes, don't parse line, just gobble chars
    cmp al,'+'              ; see if plus sign concatenator
    jne ur_5                ; no
    mov plus_flag,1         ; flag a plus sign in case of line continuation
    jmp SHORT ur_next_char  ; get next char in response line

ur_5:
    cmp al,' '              ; see if whitespace char
    jbe ur_next_char        ; yes

    cmp al,','              ; see if comma, next argument type indicator
    jne ur_6                ; no

    mov comma_flag,1        ; flag a comma in case of no further arguments on line
    mov al,arg_type         ; get previous arg_type value
    inc al                  ; bump it up one
    mov arg_type,al         ; save back to memory variable
    cmp al,5                ; check bounds
    jae ur_terminate        ; out of bounds (beyond .lib), flag termination of parsing
    jmp NEAR PTR ur_next_char   ; within bounds, continue parsing

ur_6:
    cmp al,'/'              ; see if option char
    jne ur_7                ; no
    inc si                  ; set si up for set_option procedure
    call    set_option      ; set the option
    jmp NEAR PTR ur_eol_chk

ur_7:
    cmp al,'('              ; see if left paren, begin nonvectored root call overlay
    je  ur_ovlchk           ; yes
    cmp al,'['              ; see if left bracket, begin overlay
    jne ur_8                ; no

ur_ovlchk:
    cmp in_overlay,0        ; check flag to make sure not already in an overlay
    je  ur_7a               ; no

; nested parens
    mov ax,NEST_PAREN_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

ur_7a:
    mov in_overlay,80h      ; set flag
    cmp al,'('              ; see if nonvectored root call overlay
    jne ur_7b               ; no
    or  in_overlay,40h      ; set nonvectored root call flag

ur_7b:
    jmp NEAR PTR ur_next_char

ur_8:
    cmp al,']'              ; see if right bracket, end overlay
    je  ur_endchk           ; yes
    cmp al,')'              ; see if right paren, end overlay
    jne ur_at_chk           ; no

ur_endchk:
    cmp in_overlay,0        ; check flag to make sure in overlay
    jne ur_8a               ; yes

; improperly matched parenthesis, fatal error
    mov ax,UNM_RIGHT_PAREN_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

ur_8a:
    mov in_overlay,0        ; reset flag
    jmp NEAR PTR ur_next_char

ur_at_chk:
    cmp al,'@'              ; see if @ (nested response file)
    jne ur_semi_chk         ; no

; attempt to nest response files
    mov ax,RSP_NEST_ERR     ; cannot nest response files, fatal error
    jmp NEAR PTR link_error ; transfer control to error handler

ur_semi_chk:
    cmp al,';'              ; see if semicolon terminator
    jne ur_file_chk         ; no

ur_terminate:
    mov cl,1                ; flag that parsing termination occurred
    jmp SHORT ur_out        ; and exit

; check if valid file name
ur_file_chk:
    inc si                  ; set si up for parse_file_type procedure
    call    parse_file_type
    mov plus_flag,0         ; reset plus flag
    mov comma_flag,0        ; reset comma flag
    jmp NEAR PTR ur_eol_chk

ur_no_term:
    xor cl,cl               ; flag that termination of parsing did not occur

ur_out:
    mov bx,rsp_handle       ; get response file handle
    mov ah,3eh              ; close file
    int 21h
    pop si                  ; restore si -> first char after file name
    ret
use_rsp     ENDP

;*****************************
;* GET_FILE_NAME             *
;*****************************

; get file name in filename string, force to all caps
; ds:si points to second char of file name upon entry, al holds first char
; destroys ax,di
; updates si
; returns di -> filename zero terminator,
; si -> char following last filename char

get_file_name   PROC
    mov di,OFFSET DGROUP:parfilename	; point to buffer for file name

; alternate entry point allows nonroutine-set name buffer
getfile_alt_entry:

; force file name to all caps
gfn_loop:
    cmp al,'a'              ; check lower bounds
    jb  gfn_2               ; not a lowercase letter
    cmp al,'z'              ; check upper bounds
    ja  gfn_2               ; not a lowercase letter
    sub al,32               ; force lowercase to uppercase

gfn_2:
    mov [di],al             ; put file name char in buffer
    inc di                  ; bump to next file name buffer slot
    mov al,[si]             ; peek ahead at following char
	call	is_terminator	; see if file name parse terminator
    je  end_filename_string

    lodsb                   ; get char from command line string
    jmp SHORT gfn_loop      ; loop back to process

end_filename_string:
    mov BYTE PTR [di],0     ; zero terminate file name buffer
    ret
get_file_name   ENDP

;*****************************
;* PARSE_FILE_TYPE           *
;*****************************

; parse file name, save as appropriate type (.obj, .exe, .com, .map, .lib)
; ds:si points to second char of file name upon entry
; destroys ax,dx,di
; updates si

parse_file_type   PROC
    push    cx              ; save critical register
    call    get_file_name   ; setup filename string
    mov dx,OFFSET DGROUP:parfilename	; point to first char of filename
    call    check_extension ; set no_extension flag status

save_name:
    mov al,arg_type         ; save filename as .obj, .exe, .map, or .lib
    or  al,al               ; check for .obj type, type 0
    jne sn_2
    call    save_obj_name   ; save the object module name
    pop cx                  ; restore critical register
    ret
sn_2:
    cmp al,1                ; check for .exe (.com) type
    jne sn_3
    call    save_exe_name   ; save the executable name
    pop cx                  ; restore critical register
    ret
sn_3:
    cmp al,2                ; check for .map type
    jne libtype
    call    save_map_name   ; save the map name
    pop cx                  ; restore critical register
    ret

libtype:                    ; assume module is library if arg_type matches nothin else
    mov di,OFFSET DGROUP:parfilename
    call    save_lib_name   ; save the library name
    pop cx                  ; restore critical register
    ret
parse_file_type ENDP

;*****************************
;* CHECK_EXTENSION           *
;*****************************

; get the file's extension, if none set no_extension flag
; upon entry di -> zero terminator of filename
; dx -> filename
; destroys dx,di

check_extension PROC
ce_loop:
    dec di                  ; back up to previous char
    cmp dx,di               ; are we at the first char of filename
    jae set_ext_off         ; yes, this file has no extension
    cmp BYTE PTR [di],'\'   ; backslash indicates end of filename (start of path)
    je  set_ext_off         ; no extension in filename before path was indicated
    cmp BYTE PTR [di],'.'   ; period indicates file extension exists
    jne ce_loop             ; not a period, keep looking
    mov no_extension,0      ; flag that file name does have an extension
    ret

set_ext_off:
    mov no_extension,1      ; flag that file name has no extension

ce_ret:
    ret
check_extension ENDP

;*****************************
;* SAVE_EXE_NAME             *
;*****************************

; upon entry, filename buffer contains name of executable file
; destroys ax,di,es (to ds, if changed)

save_exe_name   PROC
    push    si              ; save critical register
    mov di,OFFSET DGROUP:exe_name   ; point to exe_name global string
    cmp BYTE PTR [di],0     ; check if not zero (previously set)
    jne sen_exit           ; previous executable name specified, ignore this one

sen_2:
    mov si,OFFSET DGROUP:parfilename	; point to filename to transfer to global string
    mov ax,ds
    mov es,ax               ; es -> WarpLink data
sen_3:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne sen_3               ; not zero, keep tranferring

    cmp no_extension,0      ; check to see if default extension should be added
    je  sen_savepath        ; nope, extension exists

; since the default extension of .com or .exe is indeterminate,
; save a pointer to where it goes and add it in at end of parse
    dec di                  ; back up one char slot
    mov exe_ext_ptr,di      ; save pointer to where extension goes

; save the path and drive spec of the EXE file, if any
sen_savepath:
    mov BYTE PTR exe_pathspec,0 ; init path specification
    sub si,2                ; si -> last char of name
    std                     ; auto-decrement string moves

pathloop:
    lodsb                   ; get name char
    cmp al,'\'              ; see if directory indicator
    je  sen_pathspec        ; yes
    cmp al,':'              ; see if drive specifier
    je  sen_pathspec        ; yes
    cmp si,OFFSET DGROUP:parfilename	; see if at beginning or before
    jbe sen_exit            ; yes, no pathspec
    jmp SHORT pathloop      ; not at end, keep checking

sen_pathspec:
    cld                     ; auto-increment string moves
    inc si                  ; si -> directory/drive indicator
    mov ax,si
    mov si,OFFSET DGROUP:parfilename	; si-> start of pathspec
    mov di,OFFSET DGROUP:exe_pathspec   ; di-> destination

sen_transloop:
    movsb                   ; transfer pathspec char
    cmp si,ax               ; see if at end
    jbe sen_transloop       ; no
    mov BYTE PTR [di],0     ; null terminate pathspec

sen_exit:
    cld
    pop si                  ; restore critical register
    ret
save_exe_name   ENDP

;*****************************
;* SAVE_MAP_NAME             *
;*****************************

; upon entry, filename buffer contains name of map file
; destroys ax,di,es

save_map_name   PROC
    push    si              ; save critical register
    mov di,OFFSET DGROUP:map_name   ; point to map_name global string
    cmp BYTE PTR [di],0     ; check if not zero (previously set)
    jne smn_exit            ; previous map name specified, ignore this one

smn_2:
    mov is_mapfile,1        ; flag that map file exists
    mov si,OFFSET DGROUP:parfilename	; point to filename to transfer to global string
    mov ax,ds
    mov es,ax               ; ES -> DGROUP data
smn_3:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne smn_3               ; not zero, keep tranferring

    cmp no_extension,0      ; check to see if default extension should be added
    je  smn_exit            ; nope, extension exists
    mov BYTE PTR [di-1],'.' ; add period
    mov BYTE PTR [di],'M'   ; add MAP extension
    mov BYTE PTR [di+1],'A'
    mov BYTE PTR [di+2],'P'
    mov BYTE PTR [di+3],0   ; null terminate string

smn_exit:
    pop si                  ; restore critical register
    ret
save_map_name   ENDP

;*****************************
;* SAVE_OBJ_NAME             *
;*****************************

; upon entry, filename string buffer contains name of object module
; destroys ax,bx,cx,dx,di,es

save_obj_name   PROC
    push    si              ; save critical register
    mov di,OFFSET DGROUP:parfilename

son_getlen:
    push    di              ; save di -> file name
    push    ds              ; save critical segment register
    xor cx,cx               ; cx will hold length of filename string, including zero terminator
    mov bx,di               ; bx -> file name

son_loop:
    inc cx                  ; bump string char count
    cmp BYTE PTR [di],0     ; check for string terminator
    je  son_ext             ; all chars in string counted
    cmp BYTE PTR [di],':'   ; check if colon
    je  son_colon           ; yes

son_nextchar:
    inc di                  ; bump offset in string
    jmp SHORT son_loop      ; loop back for more

; check if colon at nondrive+ position, if so, then this is a library module,
son_colon:
    mov ax,di
    sub ax,bx               ; get position of colon in file name
    cmp ax,2                ; see if library module name
    jb  son_nextchar        ; no, colon at first or second position

; library module, restore stack and transfer control to save_lib_name
    pop ax                  ; trash
    pop di                  ; restore di -> file name
    pop ax                  ; trash
    jmp NEAR PTR save_lib_name  ; save_lib_name will save library module appropriately

son_ext:
    cmp no_extension,0      ; check to see if default extension should be added
    je  son_2               ; nope, extension exists
    add cx,4                ; add four chars to string
    mov BYTE PTR [di],'.'   ; add period
    mov BYTE PTR [di+1],'O' ; add OBJ extension
    mov BYTE PTR [di+2],'B'
    mov BYTE PTR [di+3],'J'
    mov BYTE PTR [di+4],0   ; null terminate string

son_2:
    mov di,bx               ; restore di -> filename
    inc cx                  ; adjust for leading flag byte

    mov ax,last_objblk_ptr  ; get segment of last object module name block allocation
    or  ax,ax               ; zero if none made yet
    jne son_3               ; at least one name block allocation made
    mov bx,OBJ_NAMBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov first_objblk_ptr,ax ; set first object module name block pointer
    mov last_objblk_ptr,ax  ; set last pointer to only block
    mov ds,ax               ; point data segment at block
    xor ax,ax
    mov WORD PTR ds:[2],ax  ; set next block pointer to null value
    mov ax,OBJ_NAMELIST_SIZE    ; free namelist space in block, all free
    mov WORD PTR ds:[0],ax  ; save to block
    mov ax,ds

son_3:
    push    ax              ; save -> object module block
    push    cx              ; save object module name char count

; check if object module exists, if not check OBJ e-var directories for it
; and set di appropriately
    mov ax,DGROUP
    mov ds,ax
    mov dx,di               ; ds:dx -> .OBJ file spec
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jnc son_obj_found       ; file found okay
	call	file_not_found	; see if file not found error

; .OBJ file not found in current directory, search OBJ environment string
son_find_obj:
    mov bx,OFFSET DGROUP:objtext    ; bx holds target string address for compares
    call    check_libobj_path   ; check e-var, error if not found, return di-> path+name
    mov no_extension,0      ; turn off default extension flag, already added if needed
    add sp,8                ; gobble values on stack
    jmp NEAR PTR son_getlen ; get length of new name

son_obj_found:
    pop cx                  ; restore library name char count
    pop ds                  ; ds -> library block

    mov ax,ds:[0]           ; get free space in block
    cmp ax,cx               ; check free space against object module name length
    jb  son_6               ; not enough room, allocate another block
    mov di,OBJ_NAMELIST_SIZE
    sub di,ax               ; di contains offset to free space in block's name list
    jmp SHORT son_7         ; bypass allocation code

son_6:
    mov ax,ds
    mov es,ax               ; keep es pointing at old block when new is allocated
    mov bx,OBJ_NAMBLK_SIZE  ; number of paragraphs to allocate
    pop ds                  ; restore data segment -> WarpLink data prior to memory allocation routine
    push    ds              ; save data segment again, will be destroyed
    call    alloc_memory    ; get memory for block allocation
    mov last_objblk_ptr,ax  ; set last pointer to new block
    mov ds,ax               ; point data segment at new block
    xor di,di               ; di is offset to free space in block, init to zero
    mov WORD PTR ds:[di],OBJ_NAMELIST_SIZE  ;save free namelist space to block, all free
    mov ds:[2],di           ; zero pointer to next block
    mov es:[2],ax           ; save pointer in old block to new block

son_7:
    sub ds:[0],cx           ; reduce space free in block the length of object module name

; transfer filename string into object module name block
    add di,4                ; adjust free space offset past the two leading words in block
    mov ax,ds
    mov es,ax               ; es:di -> destination (offset in block)
    pop ds                  ; restore data segment pointing to WarpLink's data
    pop si                  ; ds:si -> source string

; place flag byte in front of name, 80h overlaid, 40h overlaid nonvectored root call
    mov al,in_overlay
    or  al,al               ; see in in overlay
    je  son_8               ; no, use zero value for flag byte
    mov is_anyovls,al       ; set any module overlay flag

son_8:
    stosb                   ; store flag value, di -> destination for string
    dec cx                  ; adjust char count for flag byte

    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop si                  ; restore critical register
    inc obj_count           ; bump count of object modules
    ret
save_obj_name   ENDP

;*****************************
;* SAVE_LIB_NAME             *
;*****************************

; upon entry, di -> file name
; destroys ax,bx,cx,dx,di,es

save_lib_name   PROC
    push    si              ; save critical register
    mov module_flag,0       ; init library module flag

sln_getlen:
    push    di              ; save di -> file name
    push    ds              ; save critical segment register
    xor cx,cx               ; cx will hold length of filename string, including zero terminator
    mov si,cx               ; si will offset into library name block
    mov bx,di               ; bx -> file name

sln_loop:
    inc cx                  ; bump string char count
    cmp BYTE PTR [di],0     ; check for string terminator
    je  sln_ext             ; all chars in string counted
    cmp BYTE PTR [di],':'   ; check if colon
    je  sln_colon           ; yes

sln_nextchar:
    inc di                  ; bump offset in string
    jmp SHORT sln_loop      ; loop back for more

; check if colon at nondrive+ position, if so, then this is a library module,
sln_colon:
    mov ax,di
    sub ax,bx               ; get position of colon in file name
    cmp ax,2                ; see if library module name
    jb  sln_nextchar        ; no, colon at first or second position
    mov BYTE PTR [di],0     ; zero terminate the library name

; keep the module name and set no_extension flag appropriately
    call    save_module_name

sln_ext:
    cmp no_extension,0      ; check to see if default extension should be added
    je  sln_2               ; nope, extension exists
    add cx,4                ; add four chars to string
    mov BYTE PTR [di],'.'   ; add period
    mov BYTE PTR [di+1],'L' ; add LIB extension
    mov BYTE PTR [di+2],'I'
    mov BYTE PTR [di+3],'B'
    mov BYTE PTR [di+4],0   ; null terminate string

sln_2:
    mov di,bx               ; restore di -> filename
    mov libname_ptr,di		; save -> filename
    inc cx                  ; adjust for leading flag byte

    cmp module_flag,0       ; see if library module
    jne sln_3               ; yes, bypass check for library module

    mov lib_nonmod_flag,1   ; flag at least one library not specified as module
    call    check_lib_dupe  ; check for duplicate library name
	jnc	sln_3				; no duplicate found

; duplicate library name, don't save it, ax -> name entry
    add sp,4                ; gobble pushed ds,di (ds unchanged, di doesn't matter)
    pop si                  ; restore si
    ret                     ; and return without saving name

sln_3:
    mov ax,last_libblk_ptr  ; get segment of last library name block allocation
    or  ax,ax               ; zero if none made yet
    jne sln_4               ; at least one name block allocation made
    mov bx,LIB_NAMBLK_SIZE  ; number of paragraphs to allocate
    call    alloc_memory    ; get memory for block allocation
    mov first_libblk_ptr,ax ; set first library name block pointer
    mov last_libblk_ptr,ax  ; set last pointer to only block
    mov ds,ax               ; point data segment at block
    xor ax,ax
    mov WORD PTR ds:[2],ax  ; set next block pointer to null value, si preset to 0
    mov ax,LIB_NAMELIST_SIZE    ; free namelist space in block, all free
    mov WORD PTR [si],ax    ; save to block, si preset to zero
    mov ax,ds

sln_4:
    push    ax              ; save -> library block
    push    cx              ; save library name char count

; check if library file exists, if not check LIB e-var directories for it
; and set di appropriately
    mov ax,DGROUP
    mov ds,ax
    mov dx,di               ; ds:dx -> .LIB file spec
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jnc sln_lib_found       ; file found okay
	call	file_not_found	; check for file not found

; .LIB file not found in current directory, search LIB environment string
sln_find_lib:
    mov bx,OFFSET DGROUP:libtext ; bx holds target string address for compares
    call    check_libobj_path   ; check for libraries, error if not found, return di-> path+name
    mov no_extension,0      ; turn off default extension flag, already added if needed
    add sp,8                ; gobble values on stack
    jmp NEAR PTR sln_getlen ; get length of new name

sln_lib_found:
    mov libname_ptr,di		; save -> filename
    pop cx                  ; restore library name char count
    pop ds                  ; ds -> library block

    mov ax,[si]             ; get free space in block
    cmp ax,cx               ; check free space against library name length
    jb  sln_6               ; not enough room, allocate another block
    mov di,LIB_NAMELIST_SIZE
    sub di,ax               ; di contains offset to free space in block's name list
    jmp SHORT sln_7         ; bypass allocation code

sln_6:
    mov ax,ds
    mov es,ax               ; keep es pointing at old block when new is allocated
    mov bx,LIB_NAMBLK_SIZE  ; number of paragraphs to allocate
    pop ds                  ; restore data segment -> WarpLink data prior to memory allocation routine
    push    ds              ; save data segment again, will be destroyed
    call    alloc_memory    ; get memory for block allocation
    mov last_libblk_ptr,ax  ; set last pointer to new block
    mov ds,ax               ; point data segment at new block
    mov WORD PTR [si],LIB_NAMELIST_SIZE ;save free namelist space to block, all free
    xor di,di               ; di is offset to free space in block, init to zero
    mov ds:[2],di           ; zero pointer to next block
    mov es:[2],ax           ; save pointer in old block to new block

sln_7:
    sub [si],cx             ; reduce space free in block the length of library name

; transfer filename string into library name block
    add di,4                ; adjust free space offset past the two leading words in block
    mov ax,ds
    mov es,ax               ; es:di -> destination (offset in block)
    pop ds                  ; restore data segment pointing to WarpLink's data
    pop si                  ; ds:si -> source string

    cmp module_flag,0       ; see if library module
    je  sln_clp5            ; no

    call    proc_lib_module ; process library module
    jmp SHORT sln_7a        ; bypass /clp5 code

; check if /clp5 flag set, if so, check if CLIPPER.LIB, if so, place in overlay
; and keep library number
sln_clp5:
    cmp is_clip5,0          ; see if clipper 5 flag set
    je  sln_7a              ; no
    call    set_clipper 
    or  al,al               ; see if CLIPPER.LIB
    jne sln_7b              ; yes

; place flag byte in front of name, 80h overlaid, 40h overlaid nonvectored root call
sln_7a:
    mov al,in_overlay
    or  al,al               ; see if in overlay
    je  sln_8               ; no

sln_7b:
    mov is_anyovls,al       ; set any module overlay flag

sln_8:
    or  al,module_flag      ; set bit 0 if library module

; need to know if library module was specified in object module list (for DDL's)
    cmp arg_type,0          ; see if library module was specified in object module list
    jne sln_9               ; no
    or  al,2                ; yes, set bit 1 of flag byte

sln_9:
    stosb                   ; store flag value, di -> destination for string
    dec cx                  ; adjust char count for flag byte

    shr cx,1                ; convert to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop si                  ; restore critical register
    inc current_lib         ; bump current library (for module specification)
    inc lib_count           ; bump count of libraries
    ret
save_lib_name   ENDP

;*****************************
;* SAVE_MODULE_NAME          *
;*****************************

; keep the module name and set no_extension flag appropriately
; upon entry di -> lib name null terminator before module name
; destroys ax,dx

save_module_name    PROC
    mov module_flag,1       ; flag that this is a library module
    push    si              ; save critical register

    push    di              ; save -> null terminator of library name
    inc di                  ; bump past lib null terminator
    mov dx,di               ; dx -> start of module name
    mov al,[di]             ; get first char of filename
    inc di                  ; di -> second char of filename

; transfer module name to storage
    mov si,di               ; si -> second char of filename, used by get_file_name procedure
    mov di,OFFSET DGROUP:modname    ; point to buffer for file name
    call    getfile_alt_entry   ; get the module name
    pop di                  ; di -> library name null terminator

    mov ax,di               ; keep -> null terminator of library name
    call    check_extension ; re/set no_extension flag
    mov di,ax               ; restore di -> library name null terminator

    pop si                  ; restore critical register
    ret
save_module_name    ENDP

;*****************************
;* PROC_LIB_MODULE           *
;*****************************

; process library module
; ds:si -> library name upon entry
; destroys ax,bx,dx

proc_lib_module PROC
	cmp	is_quick,0			; see if qlk specified
	je	plm_checkon			; no
	cmp	is_quick,'R'		; see if reading from qlk file (no lookup required)
	je	plm_ret				; yes
	cmp	is_quick,'W'		; see if writing to qlk file
	je	plm_checkon			; yes
	call	is_qlk_file		; unknown, check if will be reading or writing
	jc	plm_writeqlk		; writing
	mov	is_quick,'R'		; flag reading
	ret

plm_writeqlk:
	mov	is_quick,'W'		; flag writing

plm_checkon:
	cmp	is_clpinc,0			; see if incremental linking
	je	plm_notinc			; no
	cmp	parse_pass,2		; see if second parse pass
	je	plm_notinc			; yes

plm_ret:
	ret						; don't process until incremental linking is over

plm_notinc:
    push    es              ; save critical register
    push    si
    push    di
    push    cx

    cmp is_linkinfo,0       ; see if linker information to be printed
    je  plm_1               ; no
    mov dx,OFFSET DGROUP:parsetext  ; print processing library module text
    mov cx,31
    mov bx,STDOUT
    mov ah,40h              ; write to device
    int 21h

    mov di,OFFSET DGROUP:modname    ; point to buffer for module name
plm_nameloop:
    cmp BYTE PTR [di],0     ; see if at end of module name
    je  plm_1               ; yes
    mov cx,1
    mov dx,di
    inc di                  ; bump to next char of module name
    mov ah,40h              ; write to device
    int 21h
    jmp SHORT plm_nameloop

plm_1:
    mov dx,libname_ptr
    mov ax,3d00h            ; open file with read access
    int 21h
    jnc plm_lib_open        ; no errors
    jmp NEAR PTR dos_error  ; error occurred

to_plm_search:
    jmp NEAR PTR plm_search

plm_lib_open:
    mov lib_handle,ax       ; save library file handle

    mov di,OFFSET DGROUP:prev_libname
    mov dx,si               ; save -> to start of current library name
    mov ax,ds
    mov es,ax               ; es -> warplink data

; See if current library for module matches previous.
plm_libloop:
    mov al,[si]
    or  al,es:[di]          ; see if both values are zero (matched to null terminator)
    je  to_plm_search       ; strings matched
    cmpsb                   ; compare a nonzero byte in the two names
    je  plm_libloop         ; bytes match, check next byte

; libraries did not match, transfer new name to previous
    xor ax,ax
    mov lib_pages_count,ax  ; init count of library pages
    mov di,OFFSET DGROUP:prev_libname
    mov si,dx

plm_transloop:
    movsb                   ; update prev_libname variable with current library
    or  BYTE PTR [si-1],0   ; see if null terminator reached
    jne plm_transloop       ; no

    mov ax,buffer_base      ; get i/o buffer segment
    mov es,ax               ; es -> i/o buffer
    xor si,si               ; si offsets into i/o buffer

    push    WORD PTR is_no_ems  ; save current no ems value (byte following doesn't get changed)
    mov is_no_ems,1         ; flag no EMS usage for library modules
    call    get_lib_sys_info    ; get library system information
    pop WORD PTR is_no_ems  ; restore no ems value

; position to library directory
    mov dx,WORD PTR dir_offset  ; dx has LSW of file offset
    mov cx,WORD PTR dir_offset+2    ; cx has MSW of file offset

p1_dir_loop:
    mov bx,lib_handle       ; get handle of library file in bx
    mov ax,4200h            ; move file pointer, from beginning of file
    int 21h

    mov ax,dir_pages        ; get total number of pages to read

; convert 512-byte pages to bytes in dx:ax
    mov dl,ah
    mov ah,al
    xor al,al
    mov dh,al               ; dx:ax holds pages*256
    shl ax,1
    rcl dx,1                ; dx:ax holds pages*512

plm_readloop:
    push    dx
    push    ax              ; save directory size left to process on stack
    mov cx,buffer_end       ; get maximum read amount
    or  dx,dx               ; see if more than 64K to read
    jne plm_readdir         ; yes
    cmp ax,cx               ; see if amount left to read is >= maximum able to read
    jae plm_readdir         ; yes
    mov cx,ax               ; only read amount necessary

; read library directory, partial or full
plm_readdir:
    xor dx,dx
    mov bl,is_no_ems
    push    bx
    mov is_no_ems,1         ; force EMS not to be used
    mov bx,lib_handle       ; get handle of library file in bx
    push    ds              ; save ds -> warplink data
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area
    call    read_to_ems
    pop ds                  ; restore ds -> warplink data
    pop bx
    mov is_no_ems,bl
    mov di,cx               ; save count of bytes read
    xor bx,bx               ; es:bx -> buffer

; store library pages, eliminating duplicates as necessary
; if unique pages >1024 then try immediately try search and fail if not found
plm_pageloop:
    mov al,es:[bx]          ; get public pointer array element value
    or  al,al               ; check if nonzero entry
    jne plm_goodpage        ; nonzero

plm_next_entry:
    inc bx                  ; point to next element of array
    cmp bl,37               ; check entry count of page (lower byte of bx never exceeds 37)
    jb  plm_pageloop        ; not done yet, keep looking at public entries

    and bx,0fe00h           ; bx -> first entry of next page (zero lower 511 bytes)
    add bx,512              ; move to next directory page
    jc  plm_pagedone        ; overflowed, no more directory pages in buffer
    cmp bx,di               ; see if past end of directory
    jb  plm_pageloop        ; no
    jmp SHORT plm_pagedone  ; yes, past end

plm_goodpage:
    xor ah,ah               ; zap high byte
    shl ax,1                ; convert to offset into directory page
    mov dx,bx
    and dx,0fe00h           ; get page value of bx
    or  ah,dh               ; merge into bx offset value for true offset
    mov si,ax               ; ds:si -> public name

    mov cl,es:[si]          ; get 1-byte length of name
    xor ch,ch               ; zap high byte
    jcxz    plm_next_entry  ; zero length name, ignore it
    inc si                  ; point to first char of name
    add si,cx               ; si -> page number of public

    mov ax,es:[si]          ; get page number value of public entry
    mov lib_page_num,ax     ; save to memory variable

; check if store another library page or process stored pages
    cmp lib_pages_count,1024    ; see if storage is full
    jb  plm_store_page      ; no

plm_destore:
    call    remove_dup_pages    ; remove duplicate pages in storage
    cmp lib_pages_count,1024    ; see if storage is still full
    jae plm_pageproc        ; yes, process the pages

; store the library page
plm_store_page:
    mov ax,lib_pages_count
    shl ax,1                ; convert to word offset
    mov si,OFFSET DGROUP:lib_page_storage   ; si -> base of stored pages
    add si,ax               ; si -> proper array to store library page number
    mov ax,lib_page_num
    mov [si],ax             ; store page
    inc lib_pages_count     ; bump count of library pages stored
    jmp NEAR PTR plm_next_entry ; loop until complete

plm_pagedone:
    call    remove_dup_pages    ; remove duplicate pages in storage
    pop ax
    pop dx                  ; get directory size to read in dx:ax
    sub ax,di               ; subtract off bytes processed
    sbb dx,0                ; borrow to high word
    mov bx,ax
    or  bx,dx               ; see if any bytes left to read
    je  plm_search          ; no
    jmp NEAR PTR plm_readloop   ; yes, get them

plm_pageproc:
    add sp,4                ; gobble directory size on stack

; search for library module in library, using pages stored in lib_page_storage array
plm_search:
    mov ax,buffer_base      ; get i/o buffer segment
    mov es,ax               ; es -> i/o buffer
    mov si,OFFSET DGROUP:lib_page_storage   ; si -> base of stored pages
    push    lib_pages_count ; save page count

plm_searchloop:
    lodsw                   ; get page to process

; get file offset of module in file
    mul lib_page_size       ; dx:ax has file offset of module
    mov cx,dx               ; cx == MSW of offset
    mov dx,ax               ; dx == LSW of offset
    mov bx,lib_handle       ; get handle of library file in bx
    mov ax,4200h            ; move file pointer, offset from beginning
    int 21h

    mov cx,1024             ; THEADR and COMENT should not exceed 1K
    mov dl,is_no_ems
    push    dx
    mov is_no_ems,1         ; force EMS not to be used
    push    ds              ; save critical register
    xor dx,dx
    mov ds,buffer_base      ; DS:DX -> segment:offset of buffer area
    call    read_to_ems     ; read file, transfer to EMS is necessary
    pop ds                  ; restore critical register
    pop cx
    mov is_no_ems,cl

; check THEADR and COMENT class A3h names for match, caseless compare
    push    si              ; save -> page in array (+2)
    mov di,3                ; es:di -> length byte of name
    mov cl,es:[di]          ; get length byte
    inc di                  ; es:di -> name
    mov si,OFFSET DGROUP:modname    ; ds:si -> module name to match

plm_theadr:
    lodsb
    mov ah,es:[di]
    inc di
    cmp ah,'a'              ; check lowercase lower boundary
    jb  plm_capped          ; not a lowercase letter
    cmp ah,'z'              ; check lowercase upper boundary
    ja  plm_capped          ; not a lowercase letter
    sub ah,20h              ; convert to uppercase

; module name and THEADR byte uppercase if alpha
plm_capped:
    cmp al,ah
    jne plm_chkcoment       ; no, check for COMENT field, if any
    dec cl                  ; see if at null terminator for both strings
    mov al,cl
    or  al,[si]
    jne plm_theadr          ; no
    jmp SHORT plm_modfound  ; yes, this is the correct module

; THEADR did not match, check for COMENT class A3h
plm_chkcoment:
    mov di,es:[1]           ; get length of THEADR record
    cmp di,512              ; can't be more than 512 bytes
    ja  plm_nomatch 
    add di,3                ; adjust for record type byte and word length

; es:di -> next record
    cmp BYTE PTR es:[di],COMENT ; see if COMENT record
    jne plm_nomatch         ; no
    cmp BYTE PTR es:[di+4],0a3h ; see if comment class A3h
    jne plm_nomatch         ; no
    add di,5                ; es:di -> length byte of name
    mov cl,es:[di]          ; get length byte
    inc di                  ; es:di -> name
    mov si,OFFSET DGROUP:modname    ; ds:si -> module name to match

plm_coment:
    lodsb
    mov ah,es:[di]
    inc di
    cmp ah,'a'              ; check lowercase lower boundary
    jb  plm_cap2            ; not a lowercase letter
    cmp ah,'z'              ; check lowercase upper boundary
    ja  plm_cap2            ; not a lowercase letter
    sub ah,20h              ; convert to uppercase

; module name and THEADR byte uppercase if alpha
plm_cap2:
    cmp al,ah
    jne plm_nomatch         ; no, no match for this module
    dec cl                  ; see if at null terminator for both strings
    mov al,cl
    or  al,[si]
    jne plm_coment          ; no
    jmp SHORT plm_modfound  ; yes, this is the correct module

plm_nomatch:
    pop si                  ; restore si -> page in array
    dec lib_pages_count     ; drop count of pages to process
    je  plm_notfound        ; no more pages
    jmp NEAR PTR plm_searchloop ; more pages left

; specified library module was not found
plm_notfound:
    mov ax,2                ; force file not found error
    mov dx,OFFSET DGROUP:modname    ; use module name
    jmp NEAR PTR dos_error

; found the module in the library, save library module entry
plm_modfound:
    pop si                  ; si -> page in array (+2)
    pop lib_pages_count     ; restore original count of pages
    mov ax,[si-2]
    mov lib_page_num,ax     ; save page number
    call    make_libmod_entry   

    mov bx,lib_handle       ; get file handle of open library
    mov ah,3eh              ; close file
    int 21h
    pop cx                  ; restore critical register
    pop di
    pop si
    pop es
    ret
proc_lib_module ENDP

;*****************************
;* SET_CLIPPER               *
;*****************************

; upon entry si-> file name including path
; if /clp5 library, set clip_libnum and flag in overlay, return al == 0c0h
; otherwise return al==0
; destroys ax

set_clipper PROC
    push    si              ; save critical register
    push    di
    push    es

set_searchloop:
    lodsb                   ; search for null terminator of file name
    or  al,al               ; at null terminator?
    jne set_searchloop      ; no

    dec si                  ; back up to null terminator
    mov ax,ds
    mov es,ax               ; es -> warplink data
    mov di,OFFSET DGROUP:clp5_lib

set_search2:
    mov al,es:[di]          ; search for null terminator of /clp5 library
    or  al,al
    je  set_2               ; at null terminator
    inc di                  ; bump to next char
    jmp SHORT set_search2

; ds:si -> null terminator of source string
; es:di -> null terminator of match string
set_2:
    std                     ; decrement string operations

set_comploop:
    cmpsb                   ; match string bytes
    jne set_noclip          ; no match, not the /clp5 library
    cmp di,OFFSET DGROUP:clp5_lib   ; see if matched to beginning of string
    jae set_comploop        ; not yet

; library names matched
    mov ax,lib_count        ; get current library number
    mov clip_libnum,ax      ; save it
    mov al,0c0h             ; flag in overlay, nonvectored root call
    jmp SHORT set_exit

set_noclip:
    xor al,al               ; show not CLIPPER.LIB

set_exit:
    cld                     ; restore to default increment string operations
    pop es                  ; restore critical register
    pop di
    pop si
    ret
set_clipper ENDP

;*****************************
;* CHECK_LIB_DUPE            *
;*****************************

; check for duplicate library name
; di -> new file name upon entry
; returns carry flag set if duplicate, reset if not duplicate,
; destroys ax,bx

check_lib_dupe  PROC
    push    es              ; save critical register
    push    si
    push    di              ; save -> new file name on stack

    mov ax,first_libblk_ptr ; get first object name block
    or  ax,ax               ; check if nonull
    je  cld_nodupe          ; null, no previous libraries, no chance of duplicate
    mov di,4                ; di -> first name in block
    mov es,ax               ; point extra segment at object name block

cld_loop:
    inc di                  ; adjust past prepended flag byte
    mov ax,LIB_NAMBLK_BYSIZE    ; size of block in bytes
    sub ax,es:[0]           ; minus free space, ax == end of used namelist
    cmp ax,di               ; check that position in list is below end
    ja  cld_2               ; not at end yet, pull name from this block's namelist

    mov ax,es:[2]           ; get pointer to next block
    mov di,4                ; di -> first name in block
    mov es,ax               ; point extra segment at object name block
    or  ax,ax               ; check that is not null
    jne cld_loop            ; non-null, next block exists, loop and check it

cld_nodupe:
	clc						; show no dupe found
    jmp SHORT cld_ret

; es:di -> library name in block, ds:si -> new library name
cld_2:
    cmp def_lib_flag,0      ; see if default library flag set
    je  cld_2a              ; no

; find zero terminator and then back up until hit beginning or '\' or ':' char
    xor bx,bx

cld_endloop:
    mov al,es:[di]          ; get char
    or  al,al
    je  cld_at_end          ; zero, found terminator
    inc di                  ; bump to next char slot
    inc bx                  ; bump count of chars in name
    jmp SHORT cld_endloop   ; loop for next char

cld_at_end:
    dec di                  ; back up one chars in name
    dec bx                  ; drop count of chars in name
    je  cld_2a              ; no more chars in name, at beginning
    mov al,es:[di]          ; get char in name
    cmp al,'\'              ; see if backslash, directory indicator
    je  cld_gopast          ; yes
    cmp al,':'              ; see if colon, drivespec indicator
    jne cld_at_end          ; no, keep looking

cld_gopast:
    inc di                  ; point di past backslash or colon in name

cld_2a:
    pop si                  ; si -> new library name
    push    si              ; restore -> new library name to stack

; es:di -> library name in block, ds:si -> new library name
    mov al,es:[di-1]        ; get flag byte preceding name
    and al,1                ; see if library module
    jne cld_gobble          ; yes, don't check as a dupe

cld_cmploop:
    mov al,es:[di]
    or  al,[si]             ; see if both at terminating zero char
    je  cld_dupe            ; yes, both names match
    cmpsb                   ; compare a char
    je  cld_cmploop         ; char matched

; didn't match, gobble remaining bytes in block's library name
cld_gobble:
    mov al,es:[di]          ; check if at end of library name in block
    or  al,al
    je  cld_3               ; yes
    inc di                  ; bump to next char
    jmp cld_gobble

cld_3:
    inc di                  ; bump to first char of next name in block
    jmp SHORT cld_loop      ; and loop back to try next name

; names match, pointer to name entry on stack
cld_dupe:
	stc						; flag that dupe was found

cld_ret:
    pop di                  ; restore critical register
    pop si
    pop es
    ret
check_lib_dupe  ENDP

;*****************************
;* GET_TEMPFILE_NAME         *
;*****************************

; get specified temporary file name
; upon entry ds:si -> first char of temporary file
; destroys ax,dx,es
; updates si

get_tempfile_name   PROC
    mov ax,ds
    mov es,ax               ; es -> warplink data segment
    mov is_tempfile,1       ; flag that temporary file name specified
    mov di,OFFSET DGROUP:temp_file_name ; es:di -> place for overlay class name

gtn_2:
    lodsb                   ; get name char
	call	is_terminator	; see if file name parse terminator
    je  gtn_done            ; yes, done

    stosb                   ; name char, transfer it
    jmp SHORT gtn_2         ; get next char

gtn_done:
    dec si                  ; drop si back to -> terminating char
    xor al,al               ; null terminate string
    stosb                   ; transfer it
    ret                     ; done
get_tempfile_name   ENDP

;*****************************
;* GET_ENV_OPTIONS           *
;*****************************

; get any options from the WARPLINK= environment variable
; destroys ax,bx,cx,dx,di,si,es

get_env_options PROC

; check for WARPLINK= in the environment
    mov bx,OFFSET DGROUP:linktext   ; bx holds target string address for compares
    mov ax,psp
    mov es,ax               ; es -> warplink's PSP
    xor si,si               ; starting location for target string check
    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment

geo_find_link:
    xor di,di               ; offset into target string

geo_loop:
    mov al,es:[si]          ; get byte from environment string
    inc si                  ; point to next char in environment
    cmp al,[bx+di]          ; does environment char match WARPLINK string char
    je  geo_byte_match      ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne geo_find_link       ; not the end of the environment
    jmp SHORT geo_ret       ; at end of environment, no WARPLINK environment char

; check that WARPLINK is not part of another environment string
geo_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne geo_2               ; not first char, test already done
    cmp si,1                ; si equals one if WARPLINK is first string in environment block
    je  geo_2               ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before WARPLINK was nonzero
    jne geo_find_link       ; yes, WARPLINK is a subset of another string, keep looking

geo_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,9                ; check if all bytes matched
    jb  geo_loop            ; not yet, keep comparing

; transfer options to env_opt_storage bytes
geo_found:
	push	es
	push	ds
	pop	es
	pop	ds					; es -> warplink data, ds -> environment options
    mov di,OFFSET DGROUP:env_opt_storage

geo_3:
    movsb                   ; transfer an option char
    cmp BYTE PTR [si],0     ; see if terminator zero char transferred
    jne geo_3               ; no, keep transferring

	push	es
	pop	ds					; restore ds -> warplink data
    mov si,OFFSET DGROUP:env_opt_storage    ; ds:si -> options

geo_4:
    lodsb                   ; get char from command line string
    or  al,al               ; if zero then done
    je  geo_ret
    cmp al,' '              ; see if whitespace char
    jbe geo_4               ; yes, ignore it
    cmp al,'/'              ; see if option indicator
    je  geo_5               ; yes, call set_option routine

; assume an option without leading '/', backup si one space
    dec si

geo_5:
    call    set_option      ; set the appropriate option
    jmp SHORT geo_4         ; loop for next option

geo_ret:
    ret
get_env_options ENDP

;*****************************
;* FILE_NOT_FOUND            *
;*****************************

; upon entry ax holds error code from file open or check for existence
; if not a file not found variation, abort to dos with error
; no registers modified

file_not_found	PROC
    cmp ax,2                ; check for file not found error
    je  fnf_ok              ; yes
    cmp ax,3                ; check if path not found or file doesn't exist error
    je  fnf_ok              ; yes
    cmp ax,5                ; check for access denied (not in current directory)
    je  fnf_ok
    jmp NEAR PTR dos_error  ; no, dos error

fnf_ok:
	ret
file_not_found	ENDP

IFNDEF DEMO

;*****************************
;* PARSE_UDL_OPTION          *
;*****************************

; parse /udl option, get DDL library name
; ds:si -> second  char of option (past 'U')
; cx -> start of option
; destroys ax,bx,dx,di,es
; updates si

parse_udl_option    PROC
    mov ax,ds
    mov es,ax               ; es -> warplink data
    lodsb                   ; get second option char
    and al,0dfh             ; force char to uppercase
    cmp al,'D'              ; check if correct
    jne puo_bad             ; no
    lodsb                   ; get third option char
    and al,0dfh             ; force char to uppercase
    cmp al,'L'              ; check if correct
    jne puo_bad             ; no
    lodsb                   ; get colon
    cmp al,':'
    jne puo_bad

    mov al,ddl_depend_count	; bump count of ddl's used
    inc al
    cmp al,15
    ja  puo_bad             ; too many ddl's specified (15 max)
;***    je  puo_bad             ; too many ddl's specified (255 max)
    mov use_ddl,al
	mov	ddl_depend_count,al

    mov bx,5                ; allocate 5 paragraphs to hold ddl name
    call    alloc_memory    ; get memory for block allocation
    mov es,ax               ; es -> ddl name block (1 entry/block)
    xor ax,ax
    mov di,ax
    stosw                   ; zero first word pointer of block

    mov ax,first_udlent_ptr ; get first pointer to ddl name used
    or  ax,ax               ; see if exists
    mov ax,es               ; get current pointer before conditional jump
    jne puo_2               ; first pointer exists

    mov first_udlent_ptr,ax ; save first pointer
    jmp SHORT puo_3

puo_2:
    mov ds,alloc_udlent_ptr ; ds -> previously last allocated block
    mov ds:[0],ax           ; save pointer to next (new) block
    mov dx,DGROUP
    mov ds,dx               ; restore ds -> warplink data

puo_3:
    mov alloc_udlent_ptr,ax ; save segment of last allocated block

; ds:si -> dynamic library name
    xor dx,dx               ; dx holds count of chars in name
    mov bx,dx               ; nonzero bx flags that '.' was encountered

puo_loop:
    lodsb                   ; get name char
	call	is_terminator	; see if at end of file name parse
    je 	puo_done            ; yes, at end of name
    cmp al,':'              ; see if colon
    je  puo_bad             ; yes, bad option, no drivespecs allowed
    cmp al,'\'              ; see if backslash
    je  puo_bad             ; yes, bad option, no pathspecs allowed
    cmp al,'.'              ; see if period encountered
    jne puo_trans           ; no
    inc bx                  ; flag period in name

puo_trans:
    stosb                   ; transfer name char
    inc dx                  ; bump count of chars in name
    cmp dl,12               ; make sure no more than 12 chars
    jbe puo_loop            ; loop back for next char

puo_bad:                    ; bad option specified
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

puo_done:
    or  dl,dl               ; make sure at least one character
    je  puo_bad             ; no, bad option
    dec si                  ; drop si back to -> terminating char
    or  bx,bx               ; see if period was in name
    jne puo_term            ; yes

; add default DDL extension
    cmp dl,8                ; can't have more than 8 chars in main filename
    ja  puo_bad             ; too many
    mov al,'.'              ; add .DDL extension
    stosb
    mov al,'D'
    stosb
    mov al,'D'
    stosb
    mov al,'L'
    stosb

puo_term:
    xor al,al               ; null terminate string
    stosb                   ; transfer it
;***    mov al,use_ddl
;***    mov es:[79],al			; put DDL number at end of list
    ret

parse_udl_option    ENDP

ENDIF

;*****************************
;* IS_TERMINATOR             *
;*****************************

; check if char in al is parsing terminator '+)(][@/,;' or whitespace
; return zero flag set if terminator found, reset if not
; destroys no registers

is_terminator	PROC
	cmp	al,' '				; check if whitespace
	ja	it_2				; no

; all whitespace matches, set zero flag
	push	ax				; save al value
	xor	al,al				; set zero flag
	pop	ax					; restore al value
	ret

it_2:
	cmp	al,'+'
	je	it_ret
	cmp	al,')'
	je	it_ret
	cmp	al,'('
	je	it_ret
	cmp	al,']'
	je	it_ret
	cmp	al,'['
	je	it_ret
	cmp	al,'@'
	je	it_ret
	cmp	al,'/'
	je	it_ret
	cmp	al,','
	je	it_ret

; fall through to return on final compare
	cmp	al,';'
	
it_ret:
	ret
is_terminator	ENDP

END
