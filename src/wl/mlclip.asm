;*********************************************************************
;*   MLCLIP.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          02/12/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Parse linker Clipper options, Clipper specific routines         *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlclip
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
PUBLIC  parse_clp_option,do_incremental,decrypt
PUBLIC  setup_incinfo,update_incinfo,check_cliplib,res_ilfseg
PUBLIC  ilf_rewind,ilf_write_eof,delete_ilf_file
PUBLIC	get_xxx_name
PUBLIC	create_clipmod_entry,parse_clipper_symbols
PUBLIC	pass2_clipcheck,fixup_clipper_tokens,process_symbol_table
PUBLIC	compsym_fixup,clip_sym_fixup,clip_final_compress

; variables
PUBLIC  smartmem_count,compress_this,new_ledata_offset
PUBLIC	clip_fix_compress,must_parse_flag

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   libtext:BYTE,objtext:BYTE,filename:BYTE
EXTRN   name_field:BYTE,obj_block_ptr:WORD,lib_block_ptr:WORD
EXTRN   libobj_flag:BYTE,mod_ovl_count:WORD,clp5_lib:BYTE
EXTRN	clipper_segdef_ptr:WORD,first_clipmod_ptr:WORD,current_clipmod_ptr:WORD
EXTRN	lxdata:BYTE,known_clipper:BYTE,symbol_overflow:BYTE
EXTRN	clipper_segindex:WORD,unique_symbol_count:WORD,prev_symbol_count:WORD
EXTRN	module_symbol_count:WORD
EXTRN	current_clipmod_ptr:WORD,first_clipmod_ptr:WORD,compression_flag:BYTE
EXTRN	is_summer87:BYTE,is_clipper5:BYTE
EXTRN	data_offset:DWORD,rec_offset:WORD,data_rec_offset:WORD
EXTRN	no_match_okay:BYTE

; initialized local variables

; bytes values
full_linkflag   DB  0       ; nonzero if automatic full link after incremental fails
EVEN
symseg_flag DB  1           ; nonzero if current LEDATA in SYMBOLS segment
                            ; during address resolution, 0 if SYMBOLS segment being resolved,
                            ; 1 if code, init to code (1) to catch later switchover to SYMBOLS
left_to_parse	DB	0		; length of symbol token left to parse
parse_length	DB	0		; nonzero if parsing asciiz token length (length of string)
old_parse_proc	DB	0		; nonzero if procedure already being compression processed
clip_fix_compress	DB	0	; nonzero if fixups following LEDATA contribute to compressed symbols
must_parse_flag	DB	0		; nonzero if must do symbol compression code at module end

; word values
EVEN
ilf_handle  DW  0           ; handle of open ilf file
change_flag     DW  0       ; nonzero if change occurred due to incremental link
prev_obj_number DW  -1      ; number of previous object module in ilf entry list
ilf_ovl_count   DW  1       ; running count of overlays, start counting relative 1
ilf_lastobj     DW  -1      ; last object module
ilf_lastlib     DW  -1      ; last library
lastused_lib    DW  -1      ; count of library name last received from get_curr_lib procedure
smartmem_count  DW  -1      ; current library count of SmartMem library
first_syment_ptr	DW	0	; pointer to first stored symbol table entry

; byte strings
tbuff   DB  '1002'          ; init'ed to ilf signature
        DW  0,0,0,0         ; space for pad value, bit flags, object file count, library count

; structures
ILF_ENTRY_STRUC STRUC
    ie_bitflags DW  ?       ; bit flags of ilf entry
    ie_date     DW  ?       ; date of file
    ie_time     DW  ?       ; timestamp of file
    ie_objnum   DW  ?       ; object module/library number
    ie_objname  DB  13 DUP (?)  ; object file/library name
    ie_filler   DB  ?
    ie_segind   DW  ?       ; segdef index of segment
    ie_segsize  DW  ?       ; size of segment
    ie_fptrlow  DW  ?       ; pointer to location in exe/ovl file
    ie_ovlnum   DW  ?       ; overlay number if in overlay/high word of file pointer
    ie_offset   DW  ?       ; unused
ILF_ENTRY_STRUC ENDS

ilf_entry   ILF_ENTRY_STRUC <>

.DATA?

; uninitialized local variables

; byte values
EVEN
ilf_internal    DB  ?       ; nonzero if internal overlays are flagged
EVEN
ilf_inovl   DB  ?           ; nonzero if ilf segment is in an overlay
clp_libflag DB  ?           ; nonzero if module is Clipper object module
compress_this	DB	?		; nonzero if compressing this particular module

; word values
EVEN
ilf_fileseg DW  ?           ; segment of memory holding ilf file image
obj_fileseg DW  ?           ; segment of memory holding obj file image
symbols_buff    DW  ?       ; segment of memory holding symbols
ilf_filesize    DW  ?       ; size of ilf file
ilf_padval  DW  ?           ; pad value specified by ilf file
entry_start DW  ?           ; first ilf entry of object module
entry_end   DW  ?           ; entry after last ilf entry of object module
ilf_lnames  DW  ?           ; current lnames index (relative zero)
sym_lnames  DW  ?           ; Clipper SYMBOLS segment lnames index (relative zero)
sym_segdef  DW  ?           ; Clipper SYMBOLS segment index number, relative zero
ilf_segdef  DW  ?           ; current segdef index (relative zero)
ilf_segpart_seg DW  ?       ; current ilf segment partition entry segment (for object module)
ilf_segpart_ent DW  ?       ; current ilf segment partition entry number (for object module)
start_lib_count DW  ?       ; library count at start sans defaults (amount specified in response file)
parse_symoff	DW	?		; offset of parsed symbol from Clipper code fixup routine
new_ledata_offset	DW	?	; data offset of LEDATA after compression, running total
reloc_lowwater	DW	?		; low water mark for shift relocation entry locations on symbol compression
prev_modsym_count	DW	?	; clipper symbols in module count prior to latest parse pass

ovl_tbuff   EQU $           ; temporary buffer to read overlay file position from ovl header
ovl_file_header  EQU $      ; first 5 words (10 bytes) of overlay file header go here
    ofh_ovl_number  DW  ?   ; number of overlay
    ofh_ovl_offset  DW  ?   ; Offset to overlay code
    ofh_ovl_size    DW  ?   ; Overlay code size in paragraphs
    ofh_near_vector DW  ?   ; Offset to near vector routine in segment
    ofh_reloc_count DW  ?   ; Relocation entry count

; doublewords
prev_fileoff    DD  ?       ; previous file offset for read in object files

; byte strings
symbol_buffer	LABEL	BYTE
exehdr_buff DB  10 DUP (?)  ; buffer to hold EXE header bytes
ilf_name    DB  13 DUP (?)

;*****************************
;* Constant data             *
;*****************************

.CONST

fail_len        DB          fail_stop-fail_text
fail_text       DB          CR,LF,'Clipper incremental link failed.'
fail_stop   =   $

proceed_len     DB          proceed_stop-proceed_text
proceed_text    DB          CR,LF,'Proceed with full link (Y/N)? '
proceed_stop    =   $

full_len        DB          full_stop-full_text
full_text       DB          CR,LF,'Proceeding with full link.'
full_stop   =   $

nochange_len    DB          nochange_stop-nochange_text
nochange_text   DB          CR,LF,'No changes made in program.'
nochange_stop   =   $

ilf_text	DB	"ILF"

summer87_token_list	DB	96h,9ah,9ch,9eh,0d8h,96h,96h,96h,96h,96h,96h,96h,96h,96h,96h,96h,96h
clipper5_token_list	DB	6h,7h,8h,9h,0ah,0bh,0ch,0dh,0eh,0fh,10h,11h,12h,13h,14h,15h,16h

; merge Clipper symbol tables into CONST data
INCLUDE MLSYMTOK.INC

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,check_libobj_path:NEAR
EXTRN   get_curr_obj:NEAR,get_curr_lib:NEAR,restore_ems_map:NEAR
EXTRN	file_not_found:NEAR,is_terminator:NEAR
EXTRN	alloc_memory:NEAR,load_file:NEAR
EXTRN	find_pubdecl_entry:NEAR,get_hash:NEAR
EXTRN	get_pubdecl_entry:NEAR,add_pubdef_name:NEAR

;*****************************
;* PARSE_CLP_OPTION          *
;*****************************

; parse Clipper option
; ds:si -> third char of option (at 'P')
; cx -> start of option
; destroys ax,dx,es
; updates si

parse_clp_option    PROC
    lodsb                   ; eat 'P'
    lodsb                   ; get Clipper option
    cmp al,'5'              ; see if Clipper 5 option
    je  par_clp5            ; yes
    cmp al,'A'              ; check bounds
	jae	par_lower			; okay

; option out of bounds
to_par_bad:
	jmp	NEAR PTR par_bad

par_lower:
    cmp al,'z'              ; check bounds
    ja  to_par_bad          ; too high

    and al,0dfh             ; force char to uppercase

    cmp al,'I'              ; see if incremental link
    jne par_smartmem        ; no
    mov is_clpinc,al        ; set clipper incremental link flag
	mov	is_sympac,0			; reset symbol table compaction option
    ret

par_clp5:
    mov is_clip5,al         ; set Clipper 5.0 option
    mov is_anyovls,al       ; overlays specified
    mov al,[si]             ; peek ahead at next char
    cmp al,':'              ; see if colon (supplied library name)
    je  par_c5_lib          ; yes
    ret                     ; no, done parsing option

par_c5_lib:
    mov dx,bx               ; save bx value
    mov bx,OFFSET DGROUP:clp5_lib
    lodsb                   ; gobble colon

par_c5_loop:
    lodsb                   ; get lib name char
	call	is_terminator	; see if file name parse terminator
    je 	par_c5_done         ; yes, done
    cmp al,'a'              ; check lower bounds
    jb  par_c5_save         ; not a lowercase letter
    cmp al,'z'              ; check upper bounds
    ja  par_c5_save         ; not a lowercase letter
    sub al,32               ; force lowercase to uppercase

par_c5_save:
    mov [bx],al             ; save library name char
    inc bx
    cmp bx,OFFSET DGROUP:clp5_lib+13    ; make sure not past 12 chars in name
    jae par_bad             ; too many chars in name
    jmp SHORT par_c5_loop

par_c5_done:
    dec si                  ; drop si back to -> terminating char
    cmp bx,OFFSET DGROUP:clp5_lib   ; make sure non-null name
    je  par_bad             ; no library name supplied
    xor al,al               ; null terminate string
    mov [bx],al             ; transfer it
    mov bx,dx               ; restore bx value
    ret                     ; done

par_smartmem:
IFNDEF DEMO
    cmp al,'S'              ; see if SMARTMEM.XXX SmartMem routines used
    jne par_full            ; no
    mov is_smartmem,al      ; set smartmem flag
    ret
ENDIF

par_full:
    cmp al,'F'              ; see if automatic full link after incremental failed
    jne eat_colon           ; no
    mov full_linkflag,al    ; set full link flag
    ret

eat_colon:
    mov ah,al               ; save option in ah
    lodsb                   ; get colon following overlay option char
    cmp al,':'              ; must be colon
    jne par_bad             ; not a colon

; check for valid option char in ah
par_check_valid:
    cmp ah,'P'              ; check if segment pad value
    jne par_bad             ; no, no more valid options
    jmp SHORT par_get_pad   ; get segment pad value

; bad option or value
par_bad:
    mov ax,BAD_OPTION_ERR
    jmp NEAR PTR link_error ; transfer control to error handler

; segment pad value
par_get_pad:
    xor dx,dx               ; dx == multiplier

pgp_2:
    lodsb                   ; get number
    cmp al,'0'              ; see nondigit
    jb  pgp_done            ; yes, done
    cmp al,'9'              ; see nondigit
    ja  pgp_done            ; yes

    and al,0fh              ; change from ASCII to actual value
    or  dh,dh               ; see if value too large for shift
    jne par_bad             ; yes
    mov dh,al               ; save value
    mov al,dl               ; get pre-existing value
    mov ah,10
    mul ah                  ; multiply pre-existing value by 10 (shift placeholder)
    xchg    dx,ax           ; save shifted value back to dx, get new value in ah
    mov al,ah               ; get value in low byte
    xor ah,ah               ; zap high byte
    add dx,ax               ; add in new value
    jmp SHORT pgp_2         ; get next digit

pgp_done:
    or  dh,dh               ; check if absolute maximum exceeded
    jne par_bad             ; yes
    mov inc_padval,dl       ; save segment pad value
    dec si                  ; drop si back to -> nondigit char
    ret                     ; done
parse_clp_option    ENDP

;*********************************************************************
;
; Structure of files with ILF (incremental link file) extension
;
; version -- 4 bytes of ASCII, current '1002'
; object module count -- 1 word
; library count -- 1 word
; pad value -- 1 word, default 16 (10h)
; bit flags -- 1 word, bit 0 set if internal overlays
; executable name without path or drive spec -- 13 bytes
;       currently always ILF name with EXE extension
; filler -- 7 bytes
; Total control information byte size == 32 bytes, 2 paragraphs
; One or more entries for each Clipper object module segment or library
; structured as follows:
;   bit flags -- 1 word, bit 0 set if changeable segment
;                        bit 1 set if Clipper SYMBOLS segment
;                        bit 2 set if library file (never changeable)
;                        bit 3 set if in overlay file
;                        bit 4 set if non-Clipper object module, nonchangeable
;                        bit 15 set if end of file (ignore following bytes)
;   date -- 1 word
;   time -- 1 word
;   object number -- 1 word
;   object/library name, no path/drive spec -- 13 bytes
;   filler (word aligner) -- 1 byte
;   segment index -- 1 word
;   segment size -- 1 word
;   EXE file pointer to segment location in EXE file -- 1 doubleword
;       OR overlay number
;   change offset, offset to start writing bytes past fixups -- 1 word
; Total entry size, each -- 32 bytes
;
;*********************************************************************

;*****************************
;* DO_INCREMENTAL            *
;*****************************

; if pre-existing ILF file, perform incremental link
; otherwise set up ILF file
; destroys ax,bx,cx,dx,si,di,es

do_incremental  PROC
	mov	bx,OFFSET DGROUP:ilf_text	; bx -> proper extension
    mov di,OFFSET DGROUP:ilf_name   ; di -> place to put ILF name
	mov	dx,di				; ds:dx -> filespec
    call    get_xxx_name    ; get ILF name
    mov ax,3d00h            ; open file for reading
    int 21h
    call    restore_ems_map
    jnc di_ilf_found        ; file found okay
	call	file_not_found	; check for file not found errors

; no (or invalid) ILF file was found, create one
di_new_ilf:
    mov ax,lib_count
    mov start_lib_count,ax  ; save starting library count (don't track default libs)
    mov dx,OFFSET DGROUP:ilf_name   ; ds:dx -> filespec
    xor cx,cx               ; normal file
    mov ah,3ch              ; create or truncate file
    int 21h
    call    restore_ems_map
	jnc	di_savehand			; no errors
    jmp NEAR PTR dos_error  ; dos error

di_savehand:
    mov ilf_handle,ax       ; save handle of ILF file
    mov bx,ax
    mov al,inc_padval
    mov BYTE PTR tbuff+4,al ; save pad value (one byte for now)
    mov ax,obj_count
    mov WORD PTR tbuff+6,ax ; save object module count
    mov ax,lib_count
    mov WORD PTR tbuff+8,ax ; save library file count
    xor ah,ah
    mov al,is_internal      ; get internal overlays flag
    or  al,al               ; see if set
    je  di_ilfbitflag       ; no
    mov al,1                ; set bit 0 to indicate internal overlays

di_ilfbitflag:
    mov WORD PTR tbuff+10,ax    ; save bit flags
    mov dx,OFFSET DGROUP:tbuff
    mov cx,12
    mov ah,40h              ; write file
    int 21h
    call    restore_ems_map
    jc  to_doserr3

    mov cx,20
    mov dx,OFFSET DGROUP:zero_table ; write zero bytes for remainder of control info
    mov ah,40h              ; write file
    int 21h
    call    restore_ems_map
    jc  to_doserr3

di_yexit:
    mov al,'Y'              ; indicate not to exit link
    ret

di_ilf_found:
    mov ilf_handle,ax       ; save file handle

; check EXE file existence
    mov dx,OFFSET DGROUP:exe_name
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    call    restore_ems_map
    jc  to_inc_bail

; if not internal overlays, and overlays specified, check OVL file existence
    cmp is_internal,0       ; see if internal overlays
    jne di_findend          ; yes
    cmp is_anyovls,0        ; see if any overlays
    je  di_findend          ; no

    mov dx,OFFSET DGROUP:ovl_filename
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    call    restore_ems_map
    jc  to_inc_bail

di_findend:
    mov bx,ilf_handle
    xor cx,cx
    mov dx,cx               ; zero hi/low file offset words
    mov ax,4202h            ; move file pointer, relative to end of file
    int 21h
    call    restore_ems_map
    jc  to_doserr3          ; error occurred

; convert bytes in dx:ax to paragraphs
    mov cx,ax               ; cx holds low word file char count
    mov ilf_filesize,ax     ; save file size assuming less than 64K

    add ax,15               ; round up
    adc dx,0
    or  dx,dx               ; fail if file >64K
    je  filesize_ok         ; file <64K

; file is larger than 64K, don't use it
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

to_inc_bail:
    jmp NEAR PTR inc_bailout    ; fail incremental link

filesize_ok:
    shr ax,1                ; /2
    shr ax,1                ; /4
    shr ax,1                ; /8
    shr ax,1                ; /16
    mov bx,ax               ; get number of paragraphs to allocate to read in ILF file
    mov ah,48h              ; allocate memory
    int 21h
    jnc mem_allocated       ; successful

to_doserr3:
    jmp NEAR PTR dos_error  ; dos error

mem_allocated:
    mov ilf_fileseg,ax      ; save segment of memory holding ILF file

; dx still zero
    mov bx,ilf_handle
    xor cx,cx
    mov ax,4200h            ; move file pointer from start of file
    int 21h                 ; rewind file
    call    restore_ems_map

    mov ax,ilf_fileseg      ; get segment of memory holding ILF file image
    mov cx,ilf_filesize
    xor dx,dx
    mov di,ds               ; save data segment
    mov ds,ax               ; ds:dx -> read buffer area
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    mov ds,di               ; restore ds -> WarpLink data
    jc  to_doserr3

    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map
    jc  to_doserr3

    mov es,ilf_fileseg
    xor si,si               ; es:si -> ilf file buffer
    lods    WORD PTR es:0   ; get first word of signature
    cmp ax,'01'             ; see if match
    jne to_remem_bailout    ; no
    lods    WORD PTR es:0   ; get second word of signature
    cmp ax,'20'             ; see if match
    jne to_remem_bailout    ; no
    je  di_getpad           ; yes, continue

to_remem_bailout:
    jmp NEAR PTR remem_bailout  ; no, invalid ILF

di_getpad:
    mov ax,first_objblk_ptr
    mov obj_block_ptr,ax    ; init current block pointer to first block

    mov ax,first_libblk_ptr
    mov lib_block_ptr,ax     ; init current block pointer to first block

    lods    WORD PTR es:0   ; get pad value
    mov ilf_padval,ax       ; save it
    lods    WORD PTR es:0   ; get number of object modules
    cmp ax,obj_count        ; check that matches current count
    jne to_remem_bailout    ; no
    lods    WORD PTR es:0   ; get number of library files
    cmp ax,lib_count        ; check that matches current count
    jne to_remem_bailout    ; no

    lods    WORD PTR es:0   ; get control info bit flags
    and al,1                ; save internal overlays flag
    mov ilf_internal,al
    mov ax,es
    add ax,2
    mov es,ax               ; es -> first entry in ilf file
    xor si,si               ; zero offset into file

di_entryloop:
    lods    WORD PTR es:0   ; get bit flags
    mov dx,ax               ; dx holds bit flags
    test    ah,80h          ; see if end of file
    je  di_noteof           ; no
    jmp NEAR PTR inc_done   ; yes, done

di_noteof:
    mov ax,si
    add ax,30
    cmp ax,ilf_filesize     ; see if entry past end of file (garbaged file)
    ja  to_remem_bailout    ; yes, fail the link

    mov ax,es:[si+4]        ; get object module/library number
    test    dl,4            ; see if library file
    jne di_getfilename      ; yes
    cmp ax,prev_obj_number  ; see if obj/lib matches previous entry's
    mov prev_obj_number,ax  ; update previous entry
    jne di_getfilename      ; no, continue
    test    dl,16           ; see if non-Clipper object module
    jne di_getfilename      ; yes
    jmp NEAR PTR next_entry ; yes, ignore this entry (same file, same date/timestamp)

di_getfilename:

; ax holds object module/library number
    push    si              ; save critical registers
    push    es
    test    dl,4            ; see if library file
    jne di_inlib

; in object module
    mov current_obj,ax
    call    get_curr_obj    ; get object module name in filename
    jmp SHORT di_havename

; in library
di_inlib:
    mov cx,ax               ; save current library count in cx

di_libloop:
    mov ax,lastused_lib
    inc ax
    mov lastused_lib,ax     ; bump count of library name last used
    mov current_lib,ax      ; use as current library
    call    get_curr_lib    ; get library name in filename
    cmp cx,lastused_lib     ; see if last name matches current library
    jne di_libloop          ; no, get another name

di_havename:
    pop es                  ; restore critical registers
    pop si

; object module or library entry in ilf file
    mov bx,dx               ; save bit flags in bx
    mov dx,OFFSET DGROUP:filename
    mov di,dx
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    call    restore_ems_map
    mov dx,bx               ; restore bit flags in dx
    jnc di_foundfile        ; file found okay
	call	file_not_found	; check for file not found errors

; object module or library not in current directory
di_findfile:
    test    dl,4            ; see if library file
    je  di_searchobj        ; no

    mov bx,OFFSET DGROUP:libtext    ; bx -> LIB string for environment searches
    jmp SHORT di_searchenv  ; search environment strings for file

di_searchobj:
    mov bx,OFFSET DGROUP:objtext    ; bx -> OBJ string for environment searches

di_searchenv:
    call    check_libobj_path   ; try to find file

; di -> file name, including any pathspec
; dx holds bit flags
di_foundfile:
    push    dx              ; save bit flags
    mov dx,di
    mov ax,3d00h            ; open with read access
    int 21h
    call    restore_ems_map
	jnc	di_savehand2
    jmp NEAR PTR dos_error

di_savehand2:
    mov ilf_handle,ax
    mov bx,ax
    mov ax,5700h            ; get file date and time
    int 21h
    call    restore_ems_map
    jc  to_doserr2

    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    pop bx                  ; get bit flags in bx
    test    bl,4            ; see if library file
    je  di_2                ; no

; library or non-Clipper object module, date/timestamp cannot change
di_cantchange:
    cmp dx,es:[si]          ; see if date has changed
    jne remem_bailout       ; yes
    cmp cx,es:[si+2]        ; see if time has changed
    jne remem_bailout       ; yes
    jmp NEAR PTR next_entry ; no change, try next entry

di_2:
    test    bl,16           ; see if non-Clipper object module
    jne di_cantchange       ; yes

; Clipper object module, see if it has changed
    cmp dx,es:[si]          ; see if date has changed
    jne modified_obj        ; yes
    cmp cx,es:[si+2]        ; see if time has changed
    jne modified_obj        ; yes

; move to next entry in ILF file
next_entry:
    add si,30               ; si-> next entry
    jmp NEAR PTR di_entryloop   ; loop back for next entry

remem_bailout:
    mov bx,ilf_fileseg      ; get memory segment allocated to hold ILF file
    mov ah,49h              ; release memory
    int 21h
    jmp NEAR PTR inc_bailout

; Clipper object module has had the date and/or timestamp changed
; check if should put in new info
modified_obj:
    mov es:[si],dx          ; update file date entry
    mov es:[si+2],cx        ; update file time entry
    call    check_modified
    or  al,al               ; see if should bailout
    jne remem_bailout       ; yes
    mov si,entry_end        ; si-> first entry after last entry of previous object module
    sub si,2                ; adjust back to start of entry
    jmp NEAR PTR di_entryloop

inc_done:
    cmp change_flag,0       ; see if any changes occurred
    jne yes_change          ; yes

; no changes occurred in EXE file, give feedback
    mov bx,OFFSET DGROUP:nochange_text
    mov cl,[bx-1]           ; get length of string to print
    xor ch,ch
    mov dx,bx               ; ds:dx -> string
    mov bx,STDOUT           ; standard output
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    jmp SHORT inc_exit

yes_change:
    mov dx,OFFSET DGROUP:ilf_name   ; ds:dx -> filespec
    mov ax,3d01h            ; open file for writing
    int 21h
    call    restore_ems_map
    jnc okay_open

to_doserr2:
    jmp NEAR PTR dos_error

okay_open:
    mov ax,ilf_fileseg      ; get segment of memory holding ILF file image
    mov bx,ilf_handle
    mov cx,ilf_filesize
    xor dx,dx
    mov di,ds               ; save data segment
    mov ds,ax               ; ds:dx -> write buffer area
    mov ah,40h              ; write file
    int 21h
    call    restore_ems_map
    mov ds,di               ; restore ds -> WarpLink data
    jc  to_doserr2

    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map
    jc  to_doserr2

inc_exit:
    mov bx,ilf_fileseg      ; get memory segment allocated to hold ILF file
    mov ah,49h              ; release memory
    int 21h
    mov al,'N'              ; indicate to exit link
    ret

do_incremental  ENDP

;*****************************
;* CHECK_MODIFIED            *
;*****************************

; assume Clipper object module with new date/timestamp has been
; modified, and update EXE file if possible.
; if not possible (non-code segment change) transfer to bailout routine
; upon entry es:si-> 2nd word of first entry of modified object module
; bx holds the entry's bit flags
; destroys ax,bx,cx,dx,di
; returns al==0 if okay to continue, nonzero if should bailout
; updates si to first entry after those of object module

check_modified  PROC
    mov entry_start,si      ; save first entry of object module
    mov dx,si               ; save first entry pointer

cm_loop:
    mov ax,es:[si-2]        ; get bit flags of this entry
    and ah,80h              ; see if end of file
    jne cm_segdone          ; yes, all object module entries found
    mov ax,es:[si+4]        ; get object module number of entry
    cmp ax,prev_obj_number  ; see if it matches previous entry
    jne cm_segdone          ; no, all object module entries found
    add si,32               ; move to next entry of object module
    jmp SHORT cm_loop       ; loop until found final object module entry

cm_segdone:
    mov entry_end,si        ; save entry after last entry of object module

    xor ax,ax
    mov WORD PTR prev_fileoff,ax    ; init previous file offset
    mov WORD PTR prev_fileoff+2,ax

    mov ilf_lnames,ax       ; init current lnames index
    mov ilf_segdef,ax       ; init current segdef index

    mov ovl_handle,ax       ; init handles used for ovl/exe/ilf files
    mov map_handle,ax

    mov di,OFFSET DGROUP:inc_seg_clcode
    mov bx,es               ; save -> object module image
    mov cx,ds
    mov es,cx               ; es -> WarpLink data
    mov cx,256              ; zero init inc_seg_clcode array of 512 bytes
    rep stosw
    mov es,bx               ; restore es -> object module image

    mov dx,OFFSET DGROUP:filename   ; ds:dx -> obj filespec
    mov ax,3d00h            ; open file for reading
    int 21h
    call    restore_ems_map
    jnc cm_3                ; file found okay

cm_doserr:
    jmp NEAR PTR dos_error

cm_3:
    mov image_handle,ax     ; save file handle

; allocate enough memory to read some object file records into memory (32K)
    mov bx,2048
    mov ah,48h              ; allocate memory
    int 21h
    jc  cm_doserr

    mov obj_fileseg,ax      ; save segment of object file image

; allocate enough memory to hold symbols (3F0H)
    mov bx,3fh
    mov ah,48h              ; allocate memory
    int 21h
    jc  cm_doserr
    mov symbols_buff,ax     ; save segment of symbols buffer

; read all or part of object file into memory
cm_readloop:
    mov ax,obj_fileseg      ; get segment of memory holding object file image
    mov bx,image_handle
    mov cx,8000h
    xor dx,dx
    mov di,ds               ; save data segment
    mov ds,ax               ; ds:dx -> read buffer area
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    mov ds,di               ; restore ds -> WarpLink data
    jc  cm_doserr

; parse through object file image, looking for SEGDEF and LEDATA records,
; stop when hit MODEND record
    mov es,obj_fileseg
    xor si,si

cm_parseloop:
    lods    BYTE PTR es:0   ; get object record
    mov dl,al
    lods    WORD PTR es:0   ; get length of record
    mov cx,ax
    cmp cx,4096             ; check if record length is >=4K
    jae cm_remem            ; yes, restore 32K memory, then bailout
    cmp dl,SEGDEF
    jne cm_chkledata
    call    inc_segdef
    or  al,al               ; see if should bailout
    je  cm_nextrec          ; no
    jmp SHORT cm_remem      ; yes

cm_chkledata:
    cmp dl,LEDATA
    jne cm_chklnames
    call    inc_ledata
    or  al,al               ; see if should bailout
    je  cm_nextrec          ; no

cm_remem:
    mov bx,symbols_buff     ; get segment of memory allocated to hold symbols
    mov ah,49h              ; release memory
    int 21h

    mov bx,obj_fileseg      ; get memory segment allocated to hold object file
    mov ah,49h              ; release memory
    int 21h
    mov al,1                ; flag should bailout of incremental link
    ret

cm_chklnames:
    cmp dl,LNAMES
    jne cm_chkmodend
    call    inc_lnames
    jmp SHORT cm_nextrec

cm_chkmodend:
    cmp dl,MODEND
    jne cm_nextrec

; MODEND, close the open exe and ovl file, if exists.  Close object file.
    mov bx,map_handle       ; get exe file handle (yes, in map_handle)
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    cmp ilf_internal,0      ; see if internal overlays
    jne cm_closeobj         ; yes
    cmp is_anyovls,0        ; see if any overlays
    je  cm_closeobj         ; no

    mov bx,ovl_handle       ; get overlay file handle
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

cm_closeobj:
    mov bx,image_handle     ; get object module handle (yes, in image_handle)
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    mov bx,obj_fileseg      ; get memory segment allocated to hold object file
    mov ah,49h              ; release memory
    int 21h

    mov bx,symbols_buff     ; get segment of memory allocated to hold symbols
    mov ah,49h              ; release memory
    int 21h

    mov es,ilf_fileseg      ; restore es->ilf file image
    mov ax,es
    add ax,2
    mov es,ax               ; es -> first entry in ilf file
    xor al,al               ; flag to continue incremental link
    ret

; move to next record
cm_nextrec:
    add si,cx               ; position to next record
    cmp si,7000h            ; see if at 28K position
    ja  cm_at28k            ; yes
    jmp NEAR PTR cm_parseloop   ; no

; at or past 28K position, seek file to si+previous offset, zero si, read in more of file
cm_at28k:
    add WORD PTR prev_fileoff,si    ; update file offset
    adc WORD PTR prev_fileoff+2,0
    mov dx,WORD PTR prev_fileoff
    mov cx,WORD PTR prev_fileoff+2  ; cx:dx hold file offset to seek to
    mov bx,image_handle
    mov ax,4200h            ; move file pointer from start of file
    int 21h
    call    restore_ems_map
    jmp NEAR PTR cm_readloop

check_modified  ENDP

;*****************************
;* INC_LNAMES                *
;*****************************

; track all lname indices of class 'CODE'
; use nonzero value in inc_seg_clcode array
; es:si -> first entry, cx holds record length
; destroys ax,bx,dx
; updates si,cx

inc_lnames  PROC

il_loop:
    cmp cx,1                ; see if at end of record
    jbe il_ret              ; yes

; check for class 'CODE', prefixed with length of 4
    mov al,es:[si]
    cmp al,4
    jne il_notcode
    cmp BYTE PTR es:[si+1],'C'
    jne il_notcode
    cmp BYTE PTR es:[si+2],'O'
    jne il_notcode
    cmp BYTE PTR es:[si+3],'D'
    jne il_notcode
    cmp BYTE PTR es:[si+4],'E'
    jne il_notcode

; a match, save it
    mov bx,ilf_lnames       ; get current lnames index
    add bx,OFFSET DGROUP:inc_seg_clcode ; add in array base
    mov [bx],al             ; make array element nonzero to flag class 'CODE'
    jmp SHORT il_next

; not code class, see if 'SYMBOLS'
il_notcode:
    cmp al,7
    jne il_next
    cmp BYTE PTR es:[si+1],'S'
    jne il_next
    cmp BYTE PTR es:[si+2],'Y'
    jne il_next
    cmp BYTE PTR es:[si+3],'M'
    jne il_next
    cmp BYTE PTR es:[si+4],'B'
    jne il_next
    cmp BYTE PTR es:[si+5],'O'
    jne il_next
    cmp BYTE PTR es:[si+6],'L'
    jne il_next
    cmp BYTE PTR es:[si+7],'S'
    jne il_next
    mov bx,ilf_lnames       ; get current lnames index
    mov sym_lnames,bx       ; save lnames index of symbols class segment

; al holds entry length
il_next:
    inc al                  ; adjust for length byte
    xor ah,ah               ; zap high byte
    add si,ax               ; compute start of next lnames
    sub cx,ax               ; adjust record length left
    inc ilf_lnames          ; bump lnames index
    jmp SHORT il_loop       ; loop back for next lnames entry

il_ret:
    ret
inc_lnames  ENDP

;*****************************
;* INC_SEGDEF                *
;*****************************

; SEGDEF record, check if changed and changeable, if so find corresponding
; entry in ilf file image.  Check if new size <= old size (old size has padval added)
; if not, bailout
; returns al==0 if okay to continue, nonzero if should bailout
; destroys ax,bx,dx,di
; updates si,cx

inc_segdef  PROC
    cmp cx,1                ; see if null segdef record
    ja iseg_2               ; no

    xor al,al               ; indicate okay to continue
    ret

iseg_2:
    lods    BYTE PTR es:0   ; get ACBP byte
    dec cx                  ; drop count of bytes remaining in record
    and al,0e0h             ; mask off all but align field
    jne iseg_noabs          ; not absolute segment, no frame number field
    sub cx,3                ; adjust 3 bytes for frame number word, offset byte
    add si,3

; es:si -> segment length
iseg_noabs:
    lods    WORD PTR es:0   ; get segment length
    sub cx,2                ; drop record byte count
    or  ax,ax               ; see if zero length
    jne iseg_notzero        ; no

; zero length segments are ignored
iseg_ok:
    inc ilf_segdef          ; bump segdef index
    xor al,al               ; indicate okay to continue
    ret

; get segment name index
iseg_notzero:
    mov dl,es:[si]          ; get low byte of segment name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp dl,80h              ; check if more than one byte index value
    jb  iseg_3              ; no, continue
    inc si                  ; go past index second byte
    dec cx                  ; decrement record length to parse

; get class name index
iseg_3:
    xor dh,dh               ; zap high byte of index
    mov dl,es:[si]          ; get low byte of class name index
    inc si
    dec cx                  ; decrement record length to parse
    cmp dl,80h              ; check if more than one byte index value
    jb  iseg_4              ; no, continue
    and dl,7fh              ; mask off high bit
    mov dh,dl               ; transfer to high word
    mov dl,es:[si]          ; get second byte, actual value
    inc si
    dec cx                  ; decrement record length to parse

; dx holds class name index, ax holds segment length
iseg_4:
    dec dx                  ; make class name index relative zero
    push    si              ; save si -> segdef record overlay name index
    mov bx,ilf_fileseg
    add bx,2
    mov es,bx               ; es -> first ilf entry
    mov si,entry_start      ; es:si -> ilf file image first entry for object file

iseg_searchloop:
    mov bx,es:[si+20]       ; bx holds segment index (remember si starts at offset two from base)
    cmp bx,ilf_segdef       ; see if segment indices match
    je  iseg_segmatch       ; yes
    add si,32               ; move to next entry
    cmp si,entry_end        ; make sure haven't shot past last entry
    jb  iseg_searchloop     ; no

; segment index not found, bailout
    pop si                  ; restore stack

iseg_bailout:
    mov al,1                ; indicate bailout of incremental link
    ret

; found proper entry, get segment length
iseg_segmatch:
    mov bx,es:[si+22]       ; bx holds segment length
    and WORD PTR es:[si-2],0fffch   ; clear changeable/SYMBOLS bit

    mov di,OFFSET DGROUP:inc_seg_clcode ; di-> array base
    add di,dx               ; add in class name index

    cmp dx,sym_lnames       ; see if SYMBOLS segment, class is SYMBOLS
    jne iseg_notsymseg      ; no
    mov dx,ilf_segdef       ; get current segdef index
    mov sym_segdef,dx       ; save as SYMBOLS segment index
    or  WORD PTR es:[si-2],2    ; set SYMBOLS bit

; start 04/14/91 code
    pop si                  ; restore si -> segdef record class name index
    mov es,obj_fileseg      ; restore es-> object file image
    jmp SHORT iseg_lenchk   ; bypass other segment checks
; end 04/14/91 code

iseg_notsymseg:
    cmp BYTE PTR [di],0     ; see if class 'CODE' or not
    je  iseg_5              ; not

    or  WORD PTR es:[si-2],1    ; set changeable bit

iseg_5:
    pop si                  ; restore si -> segdef record class name index
    mov es,obj_fileseg      ; restore es-> object file image

    cmp BYTE PTR [di],0     ; see if class 'CODE' or not
    je  len_exact           ; not class 'CODE', segment lengths must match exactly

; class 'CODE' segment, new segment length in ax must be <= old segment length
; OR class 'SYMBOLS' segment, same rule applies
iseg_lenchk:
    cmp ax,bx
    ja  iseg_bailout        ; new segment is bigger than old segment length plus pad value
    jmp NEAR PTR iseg_ok

; not class 'CODE' segment, new segment length in ax must match old in bx
len_exact:
    cmp ax,bx               ; see if segment lengths match
    jne iseg_bailout        ; no
    jmp NEAR PTR iseg_ok    ; yes, this segdef is okay

inc_segdef  ENDP

;*****************************
;* INC_LEDATA                *
;*****************************

; LEDATA record, check if CODE/SYMBOLS class, if so either open exe or ovl file
; (if not open), find ilf_file image entry of owning segdef, seek to proper
; location in exe/ovl file and write in new info.  If CODE class, adjust past
; fixup offset if offset zero (first LEDATA of segment).
; returns al==0 if okay to continue, nonzero if should bailout
; destroys ax,bx,dx,di
; updates si,cx

inc_ledata  PROC

; get segment index in dx
    xor dh,dh               ; zap high byte of index
    mov dl,es:[si]          ; get low byte of segment index
    inc si
    dec cx                  ; decrement record length to parse
    cmp dl,80h              ; check if more than one byte index value
    jb  iled_2              ; no, continue
    and dl,7fh              ; mask off high bit
    mov dh,dl               ; transfer to high word
    mov dl,es:[si]          ; get second byte, actual value
    inc si
    dec cx                  ; decrement record length to parse

; find segment entry
iled_2:
    dec dx                  ; make segment index relative zero
    push    si              ; save si -> data
    mov ax,ilf_fileseg
    add ax,2
    mov es,ax               ; es -> first ilf entry
    mov si,entry_start      ; es:si -> ilf file image first entry for object file

iled_searchloop:
    mov bx,es:[si+20]       ; bx holds segment index (remember si starts at offset two from base)
    cmp bx,dx               ; see if segment indices match
    je  iled_segmatch       ; yes
    add si,32               ; move to next entry
    cmp si,entry_end        ; make sure haven't shot past last entry
    jb  iled_searchloop     ; no

; segment index not found, bailout
iled_bailout:
    pop si                  ; restore stack
    mov al,1                ; indicate bailout of incremental link
    ret

; found proper entry, check if changeable, if not, don't write any bytes
iled_segmatch:
    xor al,al
    mov symseg_flag,al      ; init SYMBOLS segment flag
    mov ax,es:[si-2]        ; get segment bit flags
    test    al,1            ; see if segment is changeable (CODE)
    jne iled_canchange      ; yes

; check if SYMBOLS segment, perform symbol table update if possible
    test    al,2            ; see if SYMBOLS segment
    je  iled_nochange       ; no, can't modify segment bytes
    mov symseg_flag,al      ; yes, set SYMBOLS segment flag
    jmp SHORT iled_canchange

; LEDATA segment not changeable, don't write any bytes
iled_nochange:
    pop si                  ; restore stack
    mov es,obj_fileseg      ; restore es-> object file image
    xor al,al               ; indicate to continue with incremental link
    ret

; read the file pointer for start of data bytes
iled_canchange:
    mov dx,es:[si+24]       ; get low word of file offset pointer
    mov di,es:[si+26]       ; get high word/overlay number

    pop si                  ; restore si -> data
    mov es,obj_fileseg      ; restore es-> object file image

    push    cx              ; save count of bytes left in record

; check if need to open exe/ovl file
    push    dx              ; save low word of code offset
    and al,8                ; mask off all but in overlay flag
    mov ilf_inovl,al        ; save overlay status
    cmp map_handle,0        ; see if EXE file open
    jne startseek           ; yes, already opened
    mov change_flag,1       ; flag that EXE was modified

; open EXE file
    mov dx,OFFSET DGROUP:exe_name
    mov ax,3d02h            ; open for read/write access
    int 21h
    call    restore_ems_map
    jc  iled_doserr

; read first 10 EXE header bytes
iled_readhdr:
    mov map_handle,ax       ; save exe handle in map_handle variable
    mov bx,ax
    mov cx,10
    mov dx,OFFSET DGROUP:exehdr_buff
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    jc  iled_doserr         ; error occurred

; if not internal overlays, and overlays specified, open OVL file
    cmp ilf_internal,0      ; see if internal overlays
    jne startseek           ; yes
    cmp is_anyovls,0        ; see if any overlays
    je  startseek           ; no

    mov dx,OFFSET DGROUP:ovl_filename
    mov ax,3d02h            ; open for read/write access
    int 21h
    call    restore_ems_map
    jnc iled_ovlopen

iled_doserr:
    jmp NEAR PTR dos_error

iled_ovlopen:
    mov ovl_handle,ax       ; save overlay handle

startseek:
    cmp ilf_inovl,0         ; see if segment is in overlay
    jne iled_inovl          ; yes
    jmp NEAR PTR iled_exeseek   ; no

iled_inovl:
    mov bx,ovl_handle       ; default bx to overlay file handle
    cmp ilf_internal,0      ; see if internal overlays
    je  iled_3              ; no

; internal overlay, move to end of EXE file and then perform as external
    mov bx,map_handle       ; get exe file handle
    mov cx,WORD PTR exehdr_buff+4   ; get size of file in 512-byte pages
    mov ax,WORD PTR exehdr_buff+2   ; get size of file modulo 512
    or  ax,ax               ; see if any remainder
    je  iled_noremain       ; no
    dec cx                  ; drop count of pages

iled_noremain:
    mov dh,cl
    mov cl,ch
    xor dl,dl
    mov ch,dl               ; effectively multiply page value by 256 in cx:dx
    shl dx,1
    rcl cx,1                ; get page value*512 in cx:dx
    add dx,ax               ; add in page remainder
    adc cx,0                ; carry to high word
    mov ax,4200h            ; move file pointer, absolute byte offset
    int 21h
    call    restore_ems_map

; look up file position in ovl file, seek to it, read overlay control bytes.
; di holds overlay number, stack holds garbage value.
; change di to zero, stort offset to start of code on stack for use at
; iled_offseek section of code
iled_3:
    dec di                  ; make overlay number relative zero
    shl di,1
    shl di,1
    add di,2                ; adjust past count of overlays word

    mov dx,di               ; get low word of file position in ovl file
    xor cx,cx               ; zero high word of file position

    mov ax,4200h            ; move file pointer, relative beginning of file
    cmp ilf_internal,0      ; see if internal overlays
    je  iled_findfp         ; no
    inc al                  ; make move relative to current file position

iled_findfp:
    int 21h
    call    restore_ems_map

; read overlay file position
    mov cx,4                ; read doubleword position
    mov dx,OFFSET DGROUP:ovl_tbuff  ; read overlay file position
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    jc  iled_doserr

    mov dx,WORD PTR ovl_tbuff   ; get low word of position
    mov cx,WORD PTR ovl_tbuff+2 ; get high word
    sub dx,di               ; adjust for previously positioned to and read in bytes
    sbb cx,0
    sub dx,4
    sbb cx,0

    mov ax,4201h            ; move file pointer, relative current file position
    int 21h
    call    restore_ems_map

; read system information for overlay file (first 5 words)
    mov cx,10               ; read 10 bytes
    mov dx,OFFSET DGROUP:ovl_file_header    ; read overlay file header bytes
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    jnc adjust_stackval     ; no errors
    jmp NEAR PTR dos_error

adjust_stackval:
    pop ax                  ; get garbage value (low word file offset) off stack
    mov ax,ofh_ovl_offset   ; get offset to overlay code
    sub ax,10               ; adjust for system info bytes read in
    push    ax              ; put offset to overlay code on stack for low word file offset
    xor di,di               ; zero high word offset in di
    jmp SHORT iled_offseek  ; bypass root EXE file setup

; in root EXE file, seek past header
iled_exeseek:
    mov dx,WORD PTR exehdr_buff+8   ; get size of header in paragraphs
    xor cx,cx
    shl dx,1
    rcl cx,1                ; x2
    shl dx,1
    rcl cx,1                ; x4
    shl dx,1
    rcl cx,1                ; x8
    shl dx,1
    rcl cx,1                ; x16, cx:dx holds byte count of header
    mov bx,map_handle       ; get exe file handle
    mov ax,4200h            ; move file pointer from start of file
    int 21h
    call    restore_ems_map

; at start base in exe or ovl file, bx holds appropriate file handle,
; seek to code offset
iled_offseek:
    pop dx                  ; restore low word of file offset
    mov cx,di               ; get high word of file offset in dx
    mov ax,4201h            ; move file pointer from current location
    int 21h
    call    restore_ems_map

    pop cx                  ; restore count of bytes left in record
    lods    WORD PTR es:0   ; get enumerated data offset
    sub cx,2                ; drop record byte count

    cmp symseg_flag,0       ; see if code or SYMBOLS segment
    je  iled_codeseg        ; code
    jmp NEAR PTR iled_symseg    ; SYMBOLS segment

iled_codeseg:
    or  ax,ax               ; see if zero, if first LEDATA for segment
    jne iled_writedata      ; no, start writing bytes

; data offset is zero, first LEDATA, compute offset to adjust past fixup
; bytes by looking for 9a 00 00 00 00 hex bytes as final fixup and start
; at first byte following sequence.
    xor dx,dx               ; dx holds offset

iled_byteloop:
    lods    BYTE PTR es:0   ; get data byte
    dec cx
    inc dx                  ; bump offset
    cmp al,9ah              ; see if start of sequence
    jne iled_byteloop       ; no
    mov al,es:[si]
    or  al,es:[si+1]
    or  al,es:[si+2]
    or  al,es:[si+3]        ; al will be zero IFF following four bytes were zero
    or  al,al
    jne iled_byteloop       ; sequence not found

    add si,4                ; adjust past data bytes sequence
    sub cx,4
    add dx,4
    mov ax,dx               ; offset into ax

; es:si -> data bytes, cx-1 is number of bytes to write, ax is offset
; bx holds proper file handle
iled_writedata:
    dec cx                  ; drop number of bytes to write (don't write checksum byte)
    mov di,cx               ; save number of bytes to write in di
    xor cx,cx               ; zero high word of file offset
    mov dx,ax               ; get low word file offset
    mov ax,4201h            ; move file pointer from current location
    int 21h
    call    restore_ems_map
    mov cx,di               ; get bytes to write back in cx

    mov dx,si
    mov di,ds               ; save -> WarpLink data
	push	es
	pop	ds					; ds:dx -> object file image containing data bytes
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    mov ds,di               ; restore ds -> WarpLink data
    jc  iled_todoserr       ; error occurred
    jmp NEAR PTR iled_done  ; successful write

iled_todoserr:
    jmp NEAR PTR dos_error  ; error occurred

; update SYMBOLS segment
; ax holds data offset, cx holds number of bytes, bx holds file handle
; es -> object file image
iled_symseg:
    dec cx                  ; drop number of bytes to read/write (don't write checksum byte)
    mov di,cx               ; save number of bytes to write in di
    xor cx,cx               ; zero high word of file offset
    mov dx,ax               ; get low word file offset
    mov ax,4201h            ; move file pointer from current location
    int 21h
    call    restore_ems_map
    mov cx,di               ; get bytes to read/write back in cx

    mov dx,symbols_buff     ; get segment of symbols buffer
    mov ds,dx
    xor dx,dx               ; ds:dx -> read buffer
    mov ah,3fh              ; read from file
    int 21h
    call    restore_ems_map

to_iled_todoserr:
    jc  iled_todoserr       ; error occurred

; rewind back to start of segment (-cx bytes)
    mov di,cx               ; save number of bytes to write in di
    mov dx,cx               ; get low word file offset
    neg dx                  ; make negative
    mov cx,0ffffh           ; negative sign high word of file offset
    mov ax,4201h            ; move file pointer from current location
    int 21h
    call    restore_ems_map
    mov cx,di               ; get bytes to read/write back in cx

    xor di,di               ; init offset into symbols buffer
	push	ds
	push	es
	pop	ds
	pop	es

; ds:si -> object file buffer
; es:di -> EXE file symbols buffer
iled_chkfix:
    mov ax,es:[di+12]
    or  ax,es:[di+14]       ; see if nonzero fixup on symbol (can't change name)
    je  iled_notproc        ; no fixup, not a procedure reference

; procedure, names must match or fail incremental link
    cmpsw                   ; see if first two bytes match
    jne iled_badproc        ; no, fail
    cmpsw                   ; check bytes 3,4
    jne iled_badproc
    cmpsw                   ; check bytes 5,6
    jne iled_badproc
    cmpsw                   ; check bytes 7,8
    jne iled_badproc
    cmpsw                   ; check bytes 9,10
    jne iled_badproc
    sub si,10
    sub di,10               ; restore buffer pointer to start of entry

; move name from object file buffer to EXE file symbols buffer
iled_notproc:
    push    cx              ; save count of bytes to write
    mov cx,6                ; 10 chars max/variable name, 5 words + 2 for static flags in 5.0
    rep movsw               ; move them
    pop cx                  ; restore count of bytes to write
    add si,4
    add di,4                ; bump offset into buffers to next entry
    cmp di,cx               ; see if all entries updated
    jb  iled_chkfix         ; no, move to next entry
    sub si,cx               ; adjust buffer point back to beginning

	push	ds
	push	es
	pop	ds					; ds -> EXE file symbols buffer
	pop	es					; es -> object file

    xor dx,dx               ; ds:dx -> write buffer
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jc  to_iled_todoserr    ; error occurred

    mov ax,DGROUP
    mov ds,ax               ; ds -> WarpLink data

iled_done:
    add si,cx               ; adjust past bytes written
    mov cx,1                ; adjust cx for checksum byte
    xor al,al               ; indicate to continue with incremental link
    ret

; a procedure no longer matches its symbol table position
; restore ds and return al nonzero to fail incremental link
iled_badproc:
    mov ax,DGROUP
    mov ds,ax               ; ds -> WarpLink data
    mov al,1                ; indicate bailout of incremental link
    ret
inc_ledata  ENDP

;*****************************
;* GET_XXX_NAME              *
;*****************************

; get the ILF or QLK name in memory variable ilf_name or qlk_name
; upon entry di-> proper memory variable, bx -> proper extension, without '.' or null terminator
; destroys ax,cx,di,si,es

get_xxx_name    PROC
    mov si,OFFSET DGROUP:exe_name
    xor cx,cx               ; init count of chars in executable file name

gin_endloop:
    lodsb                   ; char of exe_name file
    inc cx                  ; bump count of chars in name
    or  al,al
    jne gin_endloop         ; not at end yet, continue

    dec si                  ; si -> null terminator of name

gin_startloop:
    mov al,[si]             ; get char
    cmp al,'\'              ; see if directory indicator
    je  gin_gotname         ; yes, at start of name
    cmp al,':'              ; see if drive indicator
    je  gin_gotname         ; yes
    dec si                  ; backup to previous char
    loop    gin_startloop   ; loop thru as many chars as in name

gin_gotname:
    inc si                  ; si -> first char of name
    mov ax,ds
    mov es,ax               ; es -> WarpLink data

gin_transloop:
    lodsb                   ; get char of exe name
    cmp al,'.'              ; see if extension specifier
    je  add_xxxext          ; yes, add supplied extension instead
    or  al,al               ; see if at end of name
    je  add_xxxext          ; yes
    stosb                   ; transfer char of name
    jmp SHORT gin_transloop

; add extension to exe name, bx -> to it
add_xxxext:
    mov BYTE PTR [di],'.'
	mov	al,[bx]
    mov BYTE PTR [di+1],al
	mov	al,[bx+1]
    mov BYTE PTR [di+2],al
	mov	al,[bx+2]
    mov BYTE PTR [di+3],al
    mov BYTE PTR [di+4],0
    ret
get_xxx_name    ENDP

;*****************************
;* INC_BAILOUT               *
;*****************************

; incremental link has failed, delete exe/ovl/ilf file if modified
; ask if want to continue with full link
; destroys ax,bx,cx,dx,di,si

inc_bailout PROC
    cmp change_flag,0       ; see if EXE file modified
    je  ib_2                ; no
    mov bx,map_handle       ; check if exe handle in map_handle var is nonzero
    or  bx,bx               ; see if need to delete exe file
    je  ib_chkovl           ; no
    mov ah,3eh              ; close file before deleting
    int 21h
    call    restore_ems_map
    mov dx,OFFSET DGROUP:exe_name
    mov ah,41h              ; delete file
    int 21h
    call    restore_ems_map

ib_chkovl:
    mov bx,ovl_handle       ; check if ovl handle is nonzero
    or  bx,bx               ; see if need to delete ovl file
    je  ib_delilf           ; no
    mov ah,3eh              ; close file before deleting
    int 21h
    call    restore_ems_map
    mov dx,OFFSET DGROUP:ovl_filename
    mov ah,41h              ; delete file
    int 21h
    call    restore_ems_map

ib_delilf:
    call    delete_ilf_file

ib_2:
    mov bx,OFFSET DGROUP:fail_text
    mov cl,[bx-1]           ; get length of string to print
    mov dx,bx               ; ds:dx -> string
    xor ch,ch               ; zap high byte of cx
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

    cmp full_linkflag,0     ; see if automatic full link specified
    je  char_loop           ; no
    mov si,OFFSET DGROUP:full_text
    mov cl,[si-1]           ; get length of string to print
    mov dx,si               ; ds:dx -> string
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    jmp NEAR PTR di_new_ilf ; trash old ILF file and create new one

char_loop:
    mov si,OFFSET DGROUP:proceed_text
    mov cl,[si-1]           ; get length of string to print
    mov dx,si               ; ds:dx -> string
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map

    mov ah,1                ; char input with echo
    int 21h
    cmp al,'y'              ; check if valid char
    je  ib_ret
    cmp al,'Y'              ; check if valid char
    je  ib_ret
    cmp al,'n'              ; check if valid char
    je  ib_ret
    cmp al,'N'              ; check if valid char
    jne char_loop           ; nope, loop and try again

ib_ret:
    mov si,ax               ; save keypress
    mov dx,OFFSET DGROUP:fail_text
    mov cx,2                ; print CR/LF duo
    mov bx,STDOUT           ; write to standard output device
    mov ah,40h              ; write to device
    int 21h
    call    restore_ems_map
    mov ax,si               ; get keypress back in al
    and al,0dfh             ; uppercase char
    cmp al,'Y'              ; see if should perform full link
    jne ib_exit             ; no
    jmp NEAR PTR di_new_ilf ; yes, trash old ILF file and create new one

ib_exit:
    mov is_clpinc,0         ; zero clipper incremental link flag
    ret
inc_bailout ENDP

;*****************************
;* SETUP_INCINFO             *
;*****************************

; save date/time object number, object name, of object module/library
; in ilf_entry structure, save pointer to next segpart entry
; upon entry bx holds file handle
; destroys ax,cx,dx,di,si,es

setup_incinfo   PROC
    mov clp_libflag,0       ; init clipper library flag
    mov ax,alloc_segpartblk_ptr ; get current segment partition block pointer
    mov ilf_segpart_seg,ax
    mov es,ax               ; even if no block allocated yet (es==0), it doesn't matter
    mov ax,es:[0]           ; get entry number (invalid if no blocks allocated)
    mov ilf_segpart_ent,ax  ; save it

    mov ax,5700h            ; get date and time of open file
    int 21h
    call    restore_ems_map
    mov ilf_entry.ie_date,dx
    mov ilf_entry.ie_time,cx
    cmp is_inlib,0          ; see if processing library
    je  si_procobj          ; no
    mov cx,14h              ; set library, non-Clipper module bits
    mov ax,current_lib
    jmp SHORT si_setobjnum

si_procobj:
    mov cx,10h              ; set non-Clipper module bits
    mov ax,current_obj

si_setobjnum:
    mov ilf_entry.ie_objnum,ax
    mov ilf_entry.ie_bitflags,cx    ; set bit flags to defaults

    mov si,OFFSET DGROUP:filename

si_null_loop:
    lodsb                   ; position to null terminator of filename
    or  al,al
    jne si_null_loop        ; not there yet, keep looking

si_backloop:
    mov al,[si]
    cmp al,'\'              ; back up until hit start or directory/drivespec indicator
    je  si_atname
    cmp al,':'
    je  si_atname
    cmp si,OFFSET DGROUP:filename   ; see if past start
    jb  si_atname
    dec si                  ; back up to previous char
    jmp SHORT si_backloop

si_atname:
    inc si                  ; si -> first char of name
    mov di,OFFSET DGROUP:ilf_entry.ie_objname
    mov ax,ds
    mov es,ax               ; es:di -> place to put nonpathed file name in ilf_entry structure

si_transloop:
    movsb
    cmp BYTE PTR [si-1],0   ; see if null terminator transferred
    jne si_transloop        ; no
    ret
setup_incinfo   ENDP

;*****************************
;* UPDATE_INCINFO            *
;*****************************

; if not Clipper object module, write object module/library ilf_entry
; structure to file.
; if Clipper object module step through each new segpart entry, updating
; segment length with pad value, setting bitflags, writing ilf_entry
; destroys ax,bx,dx,di,es

update_incinfo  PROC
    push    cx              ; save critical register
    cmp clp_libflag,0       ; see if Clipper object module
    jne ui_clip             ; yes

; not Clipper object module
    mov ax,mod_ovl_count    ; get count of overlays in this non-Clipper module
    add ilf_ovl_count,ax    ; update ILF overlay count to adjust for non-Clipper overlays

; see if matched previous file entry (lib or obj), if so, don't write anything
; otherwise write ilf entry and exit
    cmp is_inlib,0          ; see if in library
    jne ui_inlib            ; yes
    mov ax,current_obj
    cmp ax,ilf_lastobj      ; see if previous file entry matches this
    mov ilf_lastobj,ax
    jne ui_writenon         ; no
    jmp SHORT ui_nonret     ; yes

ui_inlib:
    mov ax,ilf_lastlib      ; get previous file entry
    cmp ax,-1               ; see if existed
    mov bx,ax               ; setup without affecting flags
    mov ax,current_lib      ; get current library in ax
    je  ui_savelib          ; delayed flag, save entry now in ax
    cmp ax,bx               ; compare current to previous entry number
    jbe ui_nonret           ; library entry equal or below previous, already saved

ui_savelib:
    cmp ax,start_lib_count  ; check if a library specified only by default
    jae ui_nonret           ; yes, don't track it
    mov ilf_lastlib,ax      ; save new previous entry

ui_writenon:
    mov cx,32               ; write length of entry
    mov dx,OFFSET DGROUP:ilf_entry
    mov bx,ilf_handle
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc ui_nonret
    jmp NEAR PTR dos_error  ; error occurred

ui_nonret:
    pop cx                  ; restore critical register
    ret

ui_clip:
    xor ax,ax
    mov ilf_segdef,ax       ; init ilf segdef index
    mov ax,ilf_segpart_seg  ; get segment of previously current segment partition block
    or  ax,ax               ; see if existed
    jne ui_2                ; yes

    mov ax,first_segpartblk_ptr ; get segment of first allocated block
    mov es,ax
    xor ax,ax               ; ax holds new entry-1
    jmp SHORT ui_firstent

ui_2:
    mov es,ax               ; es-> previously current segment partition block
    mov ax,ilf_segpart_ent  ; ax holds previously current entry value
    cmp ax,SEG_PARTENT_COUNT    ; see if last possible entry
    jb  ui_firstent         ; no
    mov es,es:[2]           ; es-> next block
    xor ax,ax               ; ax holds new entry-1

; es-> block holding first new entry
ui_firstent:
    inc ax                  ; ax holds first new entry value
    mov di,es
    add di,ax               ; di -> first entry in block

ui_scanloop:
    push    es              ; save -> block
    mov es,di               ; es-> segpart entry
    or  BYTE PTR es:[15],8  ; set Clipper object module flag
    mov ax,es:[12]          ; get segment size
    or  ax,ax               ; see if zero length segment
    jne ui_notzero          ; no
    jmp NEAR PTR ui_nextsegpart ; yes, ignore it

ui_notzero:
    mov ilf_entry.ie_segsize,ax
    mov ax,ilf_segdef       ; save segment index
    mov ilf_entry.ie_segind,ax
    mov al,es:[15]          ; get overlay flags
    and al,80h              ; see if segment is overlaid
    je  ui_notovl           ; no

; overlaid segment, save overlay number, setup bitflags
    mov ax,ilf_ovl_count
    mov ilf_entry.ie_ovlnum,ax
    inc ax
    mov ilf_ovl_count,ax    ; bump count of overlays

    mov al,inc_padval
    xor ah,ah
    add es:[12],ax          ; adjust segment size in segment partition
    add ilf_entry.ie_segsize,ax

    mov ax,9                ; show in overlay file and changeable (overlaid always changeable)
    jmp NEAR PTR ui_writebits

; nonoverlaid segment, setup bitflags
ui_notovl:
    mov cx,es               ; save -> segpart entry
    mov es,es:[4]           ; es -> owning segdef entry

    mov dx,es               ; save -> segdef entry
    les bx,es:[12]          ; es:bx -> class name entry in lnames
    add bx,8                ; adjust past prepended doubleword pointers
    xor ax,ax               ; set bit flags to default unchangeable
    cmp BYTE PTR es:[bx],'C'
    jne ui_chksym
    cmp BYTE PTR es:[bx+1],'O'
    jne ui_chksym
    cmp BYTE PTR es:[bx+2],'D'
    jne ui_chksym
    cmp BYTE PTR es:[bx+3],'E'
    jne ui_chksym

    mov es,dx               ; es -> segdef entry
    mov al,inc_padval
    xor ah,ah
    add es:[6],ax           ; add pad value to segdef segment length

    mov es,cx               ; es -> segpart entry
    add es:[12],ax          ; adjust segment size in segment partition
    add ilf_entry.ie_segsize,ax

    mov ax,1                ; set bit 0 to indicate changeable
    jmp SHORT ui_writebits  ; bypass SYMBOLS setup

; see if Clipper SYMBOLS class segment
ui_chksym:
    cmp BYTE PTR es:[bx],'S'
    jne ui_writebits
    cmp BYTE PTR es:[bx+1],'Y'
    jne ui_writebits
    cmp BYTE PTR es:[bx+2],'M'
    jne ui_writebits
    cmp BYTE PTR es:[bx+3],'B'
    jne ui_writebits
    cmp BYTE PTR es:[bx+4],'O'
    jne ui_writebits
    cmp BYTE PTR es:[bx+5],'L'
    jne ui_writebits
    cmp BYTE PTR es:[bx+6],'S'
    jne ui_writebits

; start 04/14/91 change
    mov es,dx               ; es -> segdef entry
    mov ax,32
    add es:[6],ax           ; add 2 symbol entry pad value to segdef segment length

    mov es,cx               ; es -> segpart entry
    add es:[12],ax          ; adjust segment size in segment partition
    add ilf_entry.ie_segsize,ax
; end 04/14/91 change

    mov ax,2                ; set bit 1 to indicate SYMBOLS class segment

ui_writebits:
    mov ilf_entry.ie_bitflags,ax    ; save bit flag state

; write ilf_entry
    mov cx,32               ; write length of entry
    mov dx,OFFSET DGROUP:ilf_entry
    mov bx,ilf_handle
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc ui_nextsegpart

ui_todoserr:
    jmp NEAR PTR dos_error  ; error occurred

ui_nextsegpart:
    inc ilf_segdef          ; bump segdef index
    inc di                  ; move to next entry
    pop es                  ; es-> segpart block
    mov bx,es
    mov ax,di
    sub ax,bx               ; ax holds current entry
    cmp ax,es:[0]           ; see if any remaining entries in block
    jbe to_ui_scanloop      ; yes, scan them
    mov ax,es:[2]           ; get pointer to next block
    or  ax,ax               ; see if exists
    je  ui_ret              ; no, done
    mov es,ax               ; es-> new block
    inc ax
    mov di,ax               ; di holds first entry

to_ui_scanloop:
    jmp NEAR PTR ui_scanloop

ui_ret:
    pop cx                  ; restore critical register
    ret
update_incinfo  ENDP

;*****************************
;* RES_ILFSEG                *
;*****************************

; fill exe/ovl file locations of each segment into ilf file
; upon entry es-> segdef entry, al == 1 if code segment, ==0 if SYMBOLS
; ax is destroyed

res_ilfseg  PROC
    push    bx              ; save critical registers
    push    cx
    push    dx
    cmp al,symseg_flag      ; see if switchover to SYMBOLS yet (must rewind file)
    je  ri_1                ; no
    mov symseg_flag,al      ; save new code/SYMBOLS segment flag status
    call    ilf_rewind      ; rewind file so start anew, looking for SYMBOLS segments

ri_1:
    mov dx,es               ; save -> segdef entry
    mov ax,es:[22]          ; get -> first segment partition entry
    or  ax,ax               ; see if valid
    je  to_ri_exit          ; no
    mov es,ax               ; es -> first segment partition entry
    mov al,es:[15]          ; get flags
    mov cx,es:[12]          ; get segment length in cx
    mov es,dx               ; restore es->segdef entry
    and al,8                ; check status of Clipper module flag
to_ri_exit:
    je  ri_exit             ; not a Clipper module
    or  cx,cx               ; see if zero segment length
    je  ri_exit             ; yes, ignore it

ri_readloop:
    mov cx,32               ; read one entry
    mov dx,OFFSET DGROUP:ilf_entry
    mov bx,ilf_handle
    mov ah,3fh              ; read file
    int 21h
    call    restore_ems_map
    jnc ri_2

ri_todoserr:
    jmp NEAR PTR dos_error  ; error occurred

ri_2:
    cmp ax,32               ; see if eof reached
    jne ri_internal         ; yes, internal error occurred
    mov bx,dx
    mov ax,[bx]             ; get bit flags
    and al,3                ; see if changeable/SYMBOLS bits set
    je  ri_readloop         ; no, try next entry

    and al,1                ; get changeable status
    cmp al,symseg_flag      ; see if matches code/SYMBOLS flag
    je  ri_3                ; yes

; either code segment and searching for SYMBOLS, or SYMBOLS segment and
; searching for code
    jmp SHORT ri_readloop   ; move to next entry

; at proper entry, save segment offset
ri_3:
    mov dx,es               ; save -> segdef entry
    mov es,es:[22]          ; es -> first segment partition entry
    mov al,es:[15]          ; get flags
    mov es,dx               ; restore es->segdef entry
    and al,80h              ; check overlay status
    jne ri_write            ; overlaid, no high word of segment length
    mov ax,es:[2]
    mov ilf_entry.ie_fptrlow,ax ; save low word of segment offset
    mov ax,es:[4]           ; get high word of segment offset
    mov ilf_entry.ie_ovlnum,ax  ; save it

ri_write:
    mov bx,ilf_handle
    mov cx,-1
    mov dx,-32              ; wind back 32 bytes
    mov ax,4201h            ; move file pointer from current position
    int 21h
    call    restore_ems_map

    mov cx,32               ; write one entry, 32 bytes
    mov dx,OFFSET DGROUP:ilf_entry
    mov ah,40h              ; write file
    int 21h
    call    restore_ems_map
    jc  ri_todoserr

ri_exit:
    pop dx                  ; restore critical registers
    pop cx
    pop bx
    ret

; EOF reached prior to resolving all segments
ri_internal:
    jmp SHORT ri_exit       ; ignore for now, due to NULLCODE addition
;***    mov ax,INTERNAL_ERR     ; put WarpLink error code in ax
;***    mov cx,13               ; internal error value
;***    jmp NEAR PTR link_error ; transfer control to error handler


res_ilfseg  ENDP

;*****************************
;* ILF_REWIND                *
;*****************************

; rewinds ilf file to beginning, just past control info
; destroys ax,bx,cx,dx

ilf_rewind  PROC
    cmp is_clpinc,0         ; see if Clipper incremental link in process
    je  ir_ret              ; no
    mov bx,ilf_handle
    xor cx,cx
    mov dx,32               ; position past control info
    mov ax,4200h            ; move file pointer from start of file
    int 21h
    call    restore_ems_map

ir_ret:
    ret
ilf_rewind  ENDP

;*****************************
;* ILF_WRITE_EOF             *
;*****************************

; write EOF byte to ILF file and close it
; destroys ax,bx,cx,dx

ilf_write_eof   PROC
    cmp is_clpinc,0         ; see if Clipper incremental link in process
    je  iwe_ret             ; no

    xor cx,cx
    mov dx,cx               ; seek to end of file
    mov bx,ilf_handle
    mov ax,4202h            ; move file pointer relative to end of file
    int 21h
    call    restore_ems_map

    mov ax,8000h
    mov WORD PTR tbuff,ax
    mov cx,2                ; write two bytes, eof indicator
    mov dx,OFFSET DGROUP:tbuff
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map

    cmp ax,2                ; see if two bytes were written
    jne iwe_delilf          ; no, close and delete ilf file

    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

iwe_ret:
    ret

; close and delete ilf file
iwe_delilf:
    call    delete_ilf_file
    ret

ilf_write_eof   ENDP

;*****************************
;* DELETE_ILF_FILE           *
;*****************************

; close and delete the ilf file if it exists
; destroys ax,bx,cx,dx

delete_ilf_file PROC
    mov bx,ilf_handle
    or  bx,bx               ; see if file exists
    je  dif_ret             ; no
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    mov dx,OFFSET DGROUP:ilf_name
    mov ah,41h              ; delete file
    int 21h
    xor ax,ax               ; zero out file handle so no further deletion attempts
    mov ilf_handle,ax

dif_ret:
    ret
delete_ilf_file ENDP

;*****************************
;* CHECK_CLIPLIB             *
;*****************************

; upon entry di-> COMENT default library name null terminator
; destroys ax

check_cliplib   PROC
    push    bx
    cmp is_inlib,0          ; ignore library COMENT fields
    jne cc_ret
    cmp libobj_flag,0       ; ignore library as object module
    jne cc_ret
;***    mov ax,di
;***    sub ax,OFFSET DGROUP:name_field ; compute length of library name
;***    cmp ax,7
;***    jne cc_ret              ; not proper length for CLIPPER

; check for CLIP
    mov bx,OFFSET DGROUP:name_field
    cmp BYTE PTR [bx],'C'
    jne cc_ret              ; not a match
    cmp BYTE PTR [bx+1],'L'
    jne cc_ret              ; not a match
    cmp BYTE PTR [bx+2],'I'
    jne cc_ret              ; not a match
    cmp BYTE PTR [bx+3],'P'
    jne cc_ret              ; not a match
;***    cmp BYTE PTR [di-3],'P'
;***    jne cc_ret              ; not a match
;***    cmp BYTE PTR [di-2],'E'
;***    jne cc_ret              ; not a match
;***    cmp BYTE PTR [di-1],'R'
;***    jne cc_ret              ; not a match
    mov clp_libflag,1       ; set clipper library flag (Clipper object module)

cc_ret:
    pop bx
    ret
check_cliplib   ENDP

;*****************************
;* DECRYPT                   *
;*****************************

; decrypt encrypted SmartMem library if necessary
; upon entry ax holds count of bytes to decrypt, buffer_base:dx -> buffer area
; destroys NO registers

decrypt PROC
IFNDEF DEMO
    push    ax              ; save critical register
    mov al,is_inlib
    or  al,al               ; see if in library
    je  no_decrypt          ; no
    mov al,is_smartmem
    or  al,al               ; see if smartmem switch set
    je  no_decrypt          ; no
    mov ax,smartmem_count
    cmp ax,current_lib      ; see if current library is smartmem library
    je  do_decrypt          ; yes

no_decrypt:
    pop ax                  ; no, return
    ret

do_decrypt:
    pop ax                  ; get number of bytes to decrypt
    push    ax              ; save all used registers
    push    bx
    push    cx
    push    ds

    mov cx,ax               ; get number of bytes in cx
    mov ds,buffer_base
    mov bx,dx               ; ds:dx -> portion to decrypt

dec_loop:
    mov al,[bx]             ; get byte to decrypt
    xor al,'.'              ; decrypt it
    mov [bx],al             ; save back decrypted value
    inc bx                  ; move to next byte
    loop    dec_loop        ; loop until complete

    pop ds                  ; restore critical registers
    pop cx
    pop bx
    pop ax
ENDIF
    ret

decrypt ENDP

;***** BEGIN WORK

;*****************************
;* CREATE_CLIPMOD_ENTRY      *
;*****************************

; create clipper module entry for symbol table compression use
; format as follows (12 system bytes):
;   module ID, 4 bytes
;   next module pointer, 2 bytes
;   symbol count, 2 bytes
;   prior unique symbol count 2 bytes
;   SYMBOLS segdef index, relative 1, 2 bytes
;   symbol name counts (offsets with compressed table), 2 bytes per symbol
;   symbol flags, 1 byte per symbol
; destroys ax,bx,cx,si,di,es

create_clipmod_entry	PROC
	mov	es,clipper_segdef_ptr	; es -> symbols segdef pointer
	mov	ax,es:[6]			; get length (symbol count*16)
	mov	di,ax
	mov	cx,3
	shr	ax,cl				; 2 bytes/symbol
	mov	bx,ax
	shr	bx,1				; symbol count
	add	ax,bx				; 3 bytes/symbol
	push	ax				; save total symbol byte count
	add	ax,12+15			; clipper module system bytes + round up to next para
	inc	cx					; cx==4
	shr	di,cl				; di==symbol count
	shr	ax,cl				; ax==total paras need for clipper module entry

	mov	bx,ax
	call	alloc_memory	; allocate memory for clipper module entry
	cmp	first_clipmod_ptr,0	; see if pre-existing clipper module entry
	je	cce_first			; no

	mov	es,current_clipmod_ptr	; es -> previous clipper module entry
	mov	es:[4],ax			; save -> to next (now current) entry
	jmp	SHORT cce_2

cce_first:
	mov	first_clipmod_ptr,ax	; save -> first clipper module entry

cce_2:
	mov	current_clipmod_ptr,ax	; save -> current module
	mov	es,ax				; es -> current clipper module entry
	mov	es:[6],di			; save symbol count
	mov	ax,clipper_segindex
	mov	es:[10],ax			; save symbols segment index
	xor	ax,ax
	mov	es:[4],ax			; zero next ptr
    mov es:[2],ax			; zero high word of id number

    cmp is_inlib,0			; see if processing library module
    je  cce_3				; no
    mov ax,WORD PTR lib_id	; get library id low word
    mov es:[0],ax			; save to file_mod_id number low word
    mov ax,WORD PTR lib_id+2	; get library id high word
    mov es:[2],ax			; save to file_mod_id number high word
    jmp SHORT cce_iddone

cce_3:
    mov ax,current_obj
    mov es:[0],ax           ; keep id number for pass 2 lookup

cce_iddone:
	pop	cx					; get symbol entry byte count
	mov	di,12				; point past 12 system info bytes
	xor	ax,ax
    shr cx,1                ; convert byte count to zero out to words
    rep stosw               ; zero the entries
    rcl cx,1                ; pick up carry
    rep stosb               ; zero leftover byte, if any
	ret
create_clipmod_entry	ENDP

;*****************************
;* PARSE_CLIPPER_SYMBOLS     *
;*****************************

; parse through clipper SYMBOLS data if appropriate, compress segment,
; set up counts in clipper module entry symbol entries
; upon entry cx=record length,bp=buffer_end, es:si -> enumerated data offset
; ax==segment index, relative zero
; destroys bx,dx,di,es

parse_clipper_symbols	PROC
	cmp	lxdata,LEDATA		; see if LEDATA record
	je	pcs_2

; fail
pcs_ret:
	mov	clip_fix_compress,0	; flag no compression on any fixups for this L?DATA
	ret

pcs_2:
	cmp	known_clipper,0		; see if known clipper module
	je	pcs_ret				; no
	cmp	symbol_overflow,0	; see if symbol table overflow
	jne	pcs_ret				; yes, don't process table
	cmp	ax,clipper_segindex	; see if SYMBOLS segment index
	jne	pcs_ret				; no

; LEDATA and SYMBOLS segdef and known_clipper and !symbol_overflow,
	mov	clip_fix_compress,1	; flag compression on fixups for this LEDATA
	push	ax				; save critical register
	push	cx
	push	si
	mov	ax,module_symbol_count
	mov	prev_modsym_count,ax	; save module symbol count up to now

; scan past enumerated data offset
    mov al,es:[si]          ; get low byte of data offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pcs_3               ; okay
    call    load_file
pcs_3:
    mov ah,es:[si]          ; get high byte of data offset
    inc si
    dec cx                  ; decrement record length to parse
    cmp si,bp               ; check boundary conditions
    jb  pcs_4               ; okay
    call    load_file

; es:si -> symbol table entries, cx holds total length+1
pcs_4:
    mov rec_offset,ax       ; save data record offset
	shr	cx,1
	shr	cx,1
	shr	cx,1
	shr	cx,1				; cx holds count of symbol table entries in LEDATA
	mov	bx,module_symbol_count
	add	bx,bx				; convert to word offset
	add	bx,module_symbol_count	; convert to 3-byte offset
	add	bx,12				; bx -> current symbol entry

; parse through each symbol table entry
pcs_symloop:
	push	cx				; save count of symbol table entries to parse
	push	es				; save -> i/o buffer

; copy symbol table name to symbol name buffer, including bytes after name in 16-byte entry
	push	es
	push	ds
	pop	es
	pop	ds					; ds:si -> source
	mov	di,OFFSET DGROUP:symbol_buffer	; es:di -> destination
	mov	cx,16				; 16 bytes

pcs_transloop:
	movsb
    cmp si,bp               ; check boundary conditions
    jb  pcs_nextchar        ; okay
    call    load_file
pcs_nextchar:
	loop	pcs_transloop

	push	si				; save -> next symbol name in LEDATA
	push	es
	pop	ds					; restore ds -> warplink data

	cmp	WORD PTR ds:[OFFSET DGROUP:symbol_buffer+10],0	; see if static symbol
	jne	pcs_nextsym			; yes, leave a zero pointer as indicator

; not a static/special symbol
	call	find_symbol_entry
	mov	ax,es				; get pointer to symbol table name entry, KEEP FLAGS
	mov	es,current_clipmod_ptr	; KEEP FLAGS
	mov	es:[bx],ax			; save pointer for module's symbol count/offset entry, KEEP FLAGS
	jnc	pcs_nextsym			; not a new entry
	or	BYTE PTR es:[bx+2],10h	; flag a new entry created

pcs_nextsym:
	inc	module_symbol_count	; bump current symbol
	add	bx,3				; point to next entry
	pop	si					; get -> last symbol name in LEDATA
	pop	es
	pop	cx					; get count of entries to parse
	loop	pcs_symloop		; more symbols

	mov	ax,module_symbol_count
	add	ax,unique_symbol_count
	cmp	ax,4096				; check if overflow occurred on symbols in program
	jbe	pcs_chklast

; overflow occurred
pcs_overflow:
	mov	ax,-1
	mov	es,current_clipmod_ptr
	mov	es:[2],ax			; -1 high word module ID flags end of clipper symbol table entries
	inc	ax					; ax==0
	mov	clip_fix_compress,al	; flag no compression on any fixups for this L?DATA
	inc	ax					; ax==1
	mov	symbol_overflow,al	; flag overflow
	mov	ax,prev_symbol_count
	mov	unique_symbol_count,ax	; restore unique symbol count to previous
	add	sp,6				; get si,es,cx values off of stack
	jmp	SHORT pcs_done

pcs_chklast:
	mov	es,clipper_segdef_ptr
	mov	ax,module_symbol_count
	mov	cl,4
	shl	ax,cl				; *16
	cmp	ax,es:[6]
	jb	pcs_done			; not last LEDATA

	mov	must_parse_flag,1	; flag further compression parsing needed for MODEND check

pcs_done:
	pop	si					; restore critical register
	pop	cx
	pop	ax
	ret
parse_clipper_symbols	ENDP

;*****************************
;* FIND_SYMBOL_ENTRY         *
;*****************************

; search through symbol table entries saved for symbol name in symbol_buffer
; returns carry flag set if fail (new symbol created), reset on success
; returns es -> symbol table name entry
; destroys ax,cx,dx,si,di,es

find_symbol_entry	PROC
	push	bx				; save critical register
	mov	ax,first_syment_ptr
	xor	dl,dl				; dl==0 if no previous, 12 if previous higher, 14 if lower

fse_comploop:
	or	ax,ax				; see if valid segment
	je	fse_nomatch			; no

	mov	si,OFFSET DGROUP:symbol_buffer	; ds:si -> new symbol name
	mov	es,ax
	xor	di,di				; es:di -> saved symbol name

	mov	cx,5				; must match to 10 chars
	repe	cmpsw
	je	fse_match			; match

; names didn't match, try next from high/low pointer
fse_trynext:
	jc	fse_low

fse_high:
	mov	ax,es:[12]			; get high pointer
	mov	dl,12				; flag previous lower
	jmp	SHORT fse_comploop

fse_low:
	mov	ax,es:[14]			; get low pointer
	mov	dl,14				; flag previous higher
	jmp	SHORT fse_comploop

; didn't match any symbol names on record, create symbol table entry and update pointers
; es:di -> final nomatching entry
fse_nomatch:
	mov	bx,2
	call	alloc_memory	; allocate 2 paragraphs for symbol table entry
	push	es				; save -> nonmatching entry needs pointer updated
	mov	es,ax
	xor	di,di
	mov	si,OFFSET DGROUP:symbol_buffer	; ds:si -> new symbol name
	mov	cx,5				; copy 10 name chars
	rep	movsw
	pop	es					; es-> nonmatching entry
	or	dl,dl				; see if previous exists
	je	fse_first			; no, first ever symbol entry saved

; previous pointer exists, dl holds high or low offset
	xor	dh,dh				; zap high byte of offset
	mov	di,dx
	mov	es:[di],ax			; update previous pointer

; no match found, must create new entry
fse_fail:
	mov	es,ax				; es -> new entry
	xor	ax,ax
	mov	di,12
	stosw					; zero out high/low pointers
	stosw
	stosw					; zero init procedure value, nonproc value, flag byte
	stosw
	stosb
	stc						; show failure (new entry creation)

fse_out:
	pop	bx					; restore critical register
	ret

fse_first:
	mov	first_syment_ptr,ax	; first, keep pointer to it
	jmp	SHORT fse_fail		; show failure

; name matched saved symbol table entry, es -> entry
fse_match:
	clc						; show success
	jmp	SHORT fse_out

find_symbol_entry	ENDP

;*****************************
;* CLIP_SYM_FIXUP            *
;*****************************

; process fixup on compressed symbol segment LEDATA
; destroys ax,bx,di

clip_sym_fixup	PROC
	cmp	clip_fix_compress,0	; see if compressed segment
	jne	csf_dofix			; yes
	ret

csf_dofix:
	push	cx				; save critical registers
	push	si
	push	es

	mov	cl,3
	mov	ax,data_rec_offset	; offset of fixup within this LEDATA
	shr	ax,cl				; convert to word offset
	and	al,0feh				; mask off leftover bit
	mov	bx,ax
	shr	bx,1
	add	ax,bx				; 3 bytes offset

	mov	bx,prev_modsym_count
	add	bx,bx				; convert to word offset (offset prior to this LEDATA)
	add	bx,prev_modsym_count	; convert to 3-byte offset
	add	bx,ax
	add	bx,12				; adjust past system bytes
	mov	es,current_clipmod_ptr	; es:bx -> current entry for fixup
	cmp	WORD PTR es:[bx],0	; see if static
	je	csf_ret				; yes, no new entry needed

	mov	al,80h
	or	BYTE PTR es:[bx+2],al	; flag that this entry was procedure
	mov	cx,es				; save -> current entry
	mov	es,es:[bx]			; es -> symbol table name entry

	test	BYTE PTR es:[20],al	; see if already flagged as procedure
	jne	csf_ret				; yes
	or	BYTE PTR es:[20],al	; flag as procedure
	mov	es,cx				; es -> current entry
	or	BYTE PTR es:[bx+2],10h	; flag as new (not previously flagged as procedure)

csf_ret:
	pop	es					; restore critical registers
	pop	si
	pop	cx
	ret

clip_sym_fixup	ENDP

;*****************************
;* CLIP_FINAL_COMPRESS       *
;*****************************

; final process of compressed symbols, update pointers to numbers and segment length
; destroys ax,bx,dx,di

clip_final_compress	PROC
	cmp	module_symbol_count,0	; see if any symbols in module
	jne	cfc_dofinal			; yes
	ret

cfc_dofinal:
	push	cx				; save critical registers
	push	si
	push	es
	mov	cx,module_symbol_count
	mov	bx,12				; adjust past system bytes

cfc_chkloop:
	mov	es,current_clipmod_ptr
	mov	dx,es				; save -> current clipper module pointer
	cmp	WORD PTR es:[bx],0	; see if static
	je	cfc_static			; yes, automatic new symbol
	mov	al,es:[bx+2]		; get flags on current entry
	mov	es,es:[bx]			; es -> symbol table name entry

;***** START NEW WORK

	test	al,80h			; see if procedure (always flagged in symbol table entry too)
	je	cfc_nonproc			; no
	test	al,10h			; see if new procedure
	jne	cfc_newproc			; yes
	jmp	SHORT cfc_oldproc	; flagged as procedure previously

; symbol was a variable
; symbol is new even if not marked new IFF symbol table entry nonprocedure
;  flag is not set
cfc_nonproc:
	test	al,10h			; see if new variable
	jne	cfc_newvar			; yes
	test	BYTE PTR es:[20],40h	; see if flagged as variable/nonprocedure
	jne	cfc_oldvar			; yes

cfc_newvar:
	or	BYTE PTR es:[20],40h	; flag used as variable, possibly redundant
	mov	ax,unique_symbol_count	; get unique symbol offset
	mov	es:[18],ax				; update variable unique symbol for this entry
	jmp	SHORT cfc_bumpcount

cfc_oldvar:
	mov	ax,es:[18]			; get symbol number of variable
	jmp	SHORT cfc_flagcomp	; flag compression of old symbol

cfc_oldproc:
	mov	ax,es:[16]			; get symbol number of procedure

cfc_flagcomp:
	mov	es,dx				; es -> current clipper module
	or	BYTE PTR es:[bx+2],20h	; set compress this symbol flag
	jmp	SHORT cfc_change

; es -> symbol table name entry
cfc_newproc:
	mov	ax,unique_symbol_count	; get unique symbol offset
	mov	es:[16],ax				; update procedure unique symbol for this entry
; fall through to redundant load, faster than jumping around

cfc_static:
	mov	ax,unique_symbol_count	; get unique symbol offset

cfc_bumpcount:
	inc	unique_symbol_count	; bump count of uniques
	mov	es,current_clipmod_ptr

cfc_change:
	mov	es:[bx],ax			; change pointer to offset

;***** STOP NEW WORK

; move to next entry, if any
cfc_next:
	add	bx,3
	loop	cfc_chkloop		; loop thru all entries

; update segment length, clipper module unique symbol count
	mov	dx,prev_symbol_count
	mov	ax,unique_symbol_count
	sub	ax,dx				; compute nonduplicate symbol count for module
	mov	cl,4
	shl	ax,cl
	mov	es,clipper_segdef_ptr
	mov	es:[6],ax			; update segment length
	mov	es,current_clipmod_ptr
	mov	es:[8],dx			; save unique symbols previous to this module

cfc_ret:
	pop	es					; restore critical registers
	pop	si
	pop	cx
	ret

clip_final_compress	ENDP

;*****************************
;* PASS2_CLIPCHECK           *
;*****************************

; see if Clipper module that is compressed, set appropriate flags if so
; destroys ax,bx,cx,dx,di,es

pass2_clipcheck	PROC
	xor	ax,ax
	mov	di,ax				; di is constant zero in this routine
	mov	compress_this,al	; init compress this module flag
	cmp	compression_flag,al	; see if Clipper symbol table compression in force
	je	pop_ret				; no

    cmp is_inlib,al			; see if processing library module
    jne	pop_islib			; yes
	
	cmp	current_clipmod_ptr,ax	; see if any clipper modules previous
	je	pop_clipfirst		; no

	mov	es,current_clipmod_ptr
	mov	ax,es:[4]			; get next module pointer
	or	ax,ax				; see if next module exists
	je	pop_endcomp			; no
	mov	es,ax				; es -> next clipper module
	cmp	WORD PTR es:[2],-1	; see if next module overflowed
	jne	pop_clipchk			; no, check if this module has clipper entry

pop_endcomp:
	xor	ax,ax
;***    cmp is_inlib,al			; see if processing library module
;***    jne	pop_ret				; yes, don't auto-shut off compression
	mov	compression_flag,al	; shut off further symbol compression
	ret

pop_clipfirst:
	mov	es,first_clipmod_ptr

; compare module id in es:[0] against current module id
; if match set compress_this variable, update current_clipmod_ptr variable
pop_clipchk:
	mov	ax,current_obj
	cmp	es:[di],ax			; see if module id matches
	jne	pop_ret				; no
	cmp	es:[2],di
	jne	pop_ret				; no match

; successful match
pop_match:
	mov	compress_this,1		; flag compression on this module
	mov	current_clipmod_ptr,es	; update current clipper module pointer
	mov	ax,es:[10]			; get SYMBOLS segment index, relative 0
	mov	clipper_segindex,ax	; save symbols segment index for LEDATA lookup

pop_ret:
	ret

; the library module entries can be mixed up so have to check all entries
; for ID from the beginning
pop_islib:
	mov	es,first_clipmod_ptr
	mov	bx,WORD PTR lib_id
	mov	dx,WORD PTR lib_id+2

pop_libid:
	cmp	es:[di],bx			; see if library id matches
	jne	pop_endchk			; no
	cmp	es:[2],dx			; check high word id
	je	pop_match			; succesful

; check if at end of module entries for library ID scan
pop_endchk:
	mov	ax,es:[4]			; get next module pointer
	or	ax,ax				; see if next module exists
	je	pop_ret				; no
	mov	es,ax				; es -> next clipper module
	cmp	WORD PTR es:[2],-1	; see if next module overflowed
	jne	pop_libid			; no, check its ID
	ret						; done, no match in all entries

pass2_clipcheck	ENDP

;*****************************
;* FIXUP_CLIPPER_TOKENS      *
;*****************************

; fixup tokens in Clipper code
; upon entry cx == count of data bytes, es:[si] -> first data byte
; destroys ax,bx,dx,di

fixup_clipper_tokens	PROC
	push	cx				; save critical register
	push	si
	cmp	is_clipper5,0		; see if clipper 5 code
	je	fct_summer87		; no, summer 87 code

; clipper 5 code
	mov	bx,OFFSET DGROUP:clipper5_table
	mov	dl,1				; Clipper 5 ASCIIZ opcode
	mov	dh,60h				; Clipper 5 end of code token
	mov	di,OFFSET DGROUP:clipper5_token_list	; di -> list of tokens that require fixup
	xor	al,al
	cmp	old_parse_proc,al	; see if parsing new procedure
	jne	fct_scanloop		; no
	inc	al
	mov	old_parse_proc,al	; set flag to bypass header next time
	add	si,16h				; adjust for header at start of p-code
	cmp	si,buffer_end		; see if past buffer end
	jb	fct_c52				; no
	sub	si,buffer_end		; wrap buffer

fct_c52:
	sub	cx,16h
	jmp	NEAR PTR  fct_endchk

; summer 87 code
fct_summer87:
	mov	bx,OFFSET DGROUP:summer87_table
	mov	dl,97h				; Summer 87 ASCIIZ opcode
	mov	dh,34h				; Summer 87 end of code token
	mov	di,OFFSET DGROUP:summer87_token_list	; di -> list of tokens that require fixup
	xor	al,al
	cmp	old_parse_proc,al	; see if parsing new procedure
	jne	fct_scanloop		; no
	inc	al
	mov	old_parse_proc,al	; set flag to bypass header next time
	add	si,1dh				; adjust for header at start of p-code
	cmp	si,buffer_end		; see if past buffer end
	jb	fct_sum2			; no
	sub	si,buffer_end		; wrap buffer

fct_sum2:
	sub	cx,1dh
	jmp	NEAR PTR  fct_endchk

; main p-code scanning loop
fct_scanloop:
	mov	al,es:[si]			; get data byte

; left_to_parse can hold the following values
; 	0==parsing a new token value
;	<10h==parsing an old token value, number of bytes left to parse
;	10h==parsing a length byte
;	21h==parsing symbol offset high byte
;	22h==parsing symbol offset low byte

	cmp	left_to_parse,0		; see if parsing new token
	jne	fct_notnew			; no

; parsing new token value
	cmp	al,dh				; see if end of code token
	je	fct_ended			; yes
	mov	ah,al				; save token value
	xlat					; look up length byte
	or	al,al				; zero if illegal token
	jne	fct_validtoken		; valid token

; illegal token encountered
    mov dx,OFFSET DGROUP:filename
	mov	cl,ah				; bad value in cl
    mov ax,SYMBOLTOKEN_ERR	; bad Clipper symbol token
    jmp NEAR PTR link_error	; transfer control to error handler

fct_ended:
	xor	al,al
	mov	left_to_parse,al	; reset parsing flag for next module
	mov	parse_length,al		; zero parse length byte
	mov	old_parse_proc,al	; reset parsing old procedure flag
	jmp	NEAR PTR fct_done

fct_validtoken:
	cmp	ah,dl				; see if ASCIIZ string opcode (ignore length lookup)
	jne	fct_notasciiz		; no
	mov	left_to_parse,11h	; flag parsing a length byte next, adjust 1 for autodecrement
	jmp	SHORT fct_nextbyte

fct_notasciiz:
	mov	left_to_parse,al	; save length of token to parse

; check if token has symbol that requires fixing up
	push	cx
	push	di
	mov	cx,17				; seventeen possibilities (for Clipper 5, padded from 6 in S'87)

fct_comploop:
	cmp	ah,ds:[di]			; see if a match
	jne	fct_chknextsym		; no
	mov	left_to_parse,23h	; flag parsing a symbol offset next, adjust 1 for autodecrement
	mov	cx,1				; force loop to end

fct_chknextsym:
	inc	di					; try next
	loop	fct_comploop
	pop	di
	pop	cx
	jmp	SHORT fct_nextbyte

; check if parsing asciiz string content (parse_length!=0, parse_length && left_to_parse==content length)
; if so, decrement parse_length
fct_notnew:
	cmp	parse_length,0
	je	fct_chklen
	dec	parse_length
	jmp	SHORT fct_nextbyte

; check if parsing asciiz string length byte (parse_length==0, left_to_parse==10h)
; if so, set parse_length, left_to_parse bytes
fct_chklen:
	cmp	left_to_parse,10h
	jne	fct_chkofflow
	inc	ax					; adjust for null terminator
	mov	parse_length,al
	inc	ax					; adjust for length byte
	mov	left_to_parse,al
	jmp	SHORT fct_nextbyte

; check if parsing symbol offset low byte (parse_length==0, left_to_parse==22h)
; if so, save value
fct_chkofflow:
	cmp	left_to_parse,22h
	jne	fct_chkoffhigh
	mov	BYTE PTR parse_symoff,al
	jmp	SHORT fct_nextbyte

; check if parsing symbol offset high byte (parse_length==0, left_to_parse==21h)
; if so, save value, else scanning past token arguments, ignore
fct_chkoffhigh:
	cmp	left_to_parse,21h
	jne	fct_nextbyte
	mov	BYTE PTR parse_symoff+1,al

; get next byte in token scanning
fct_nextbyte:
	inc	si					; bump -> into data record
	cmp	si,buffer_end		; see if past buffer end
	jb	fct_next2			; no
	xor	si,si				; yes, wrap buffer

fct_next2:
	dec	cx					; drop count of bytes in data record
	dec	left_to_parse		; drop count of bytes left to parse
	cmp	parse_length,0		; see if parsing asciiz
	jne	fct_endchk			; yes
	mov	al,left_to_parse
	test	al,1fh			; get value with symbol offset flag masked
	jne	fct_endchk			; more bytes associated with token, see if at end of data record

; end of bytes associate with token
	mov	parse_length,0		; zero parse length byte
	mov	left_to_parse,0		; zero out left_to_parse
	and	al,20h				; see if scanned out symbol offset
	jne	fct_symfix			; yes

fct_endchk:
	jcxz	fct_done		; no more bytes in data record
	jmp	NEAR PTR fct_scanloop

; need to fix up this symbol
fct_symfix:
	push	di				; save critical register
	push	es
	mov	es,current_clipmod_ptr
	mov	di,parse_symoff		; get symbol offst
	add	di,di				; make word offset
	add	di,parse_symoff		; make 3-byte offset
	add	di,12				; adjust past system bytes

; es:[di]-> symbol table count for this symbol
	mov	ax,es:[di]			; get new symbol number

fct_symupd:
	pop	es
	push	es				; es -> i/o buffer and back on stack
	cmp	si,2				; see if buffer wrap
	jae	fct_nowrap			; no
	mov	di,buffer_end		; point to end of buffer for wrap compensation
	dec	di					; di-> last valid byte
	cmp	si,1				; see if buffer straddle
	je	fct_straddle		; yes

; update at old buffer end
	dec	di					; di -> second to last valid byte
	stosw
	jmp	SHORT fct_upddone

; straddle buffer
fct_straddle:
	stosb
	mov	es:[si-1],ah
	jmp	SHORT fct_upddone

fct_nowrap:
	mov	es:[si-2],ax		; overwrite old symbol number

fct_upddone:
	pop	es					; restore critical register
	pop	di
	jmp	SHORT fct_endchk

fct_done:
	pop	si					; restore critical register
	pop	cx
	ret
fixup_clipper_tokens	ENDP

;*****************************
;* PROCESS_SYMBOL_TABLE      *
;*****************************

; process the symbol table data
; upon entry cx == count of data bytes, es:[si] -> first data byte
; rec_offset holds LEDATA data offset
; modifies cx to == new count of data bytes after compression
; destroys ax,bx,dx,di

process_symbol_table	PROC
	push	si				; save critical register
	push	es

	mov	reloc_lowwater,0	; init relocation low water mark
	mov	bx,rec_offset
	shr	bx,1
	shr	bx,1
	shr	bx,1				; bx holds symbol number within module (word offset)
	mov	dx,bx
	shr	dx,1
	add	bx,dx				; make 3-byte offset
	add	bx,12				; adjust past system bytes
	mov	dx,cx				; dx holds count of bytes after compression removal

pct_comploop:
	mov	es,current_clipmod_ptr	; es:[bx] -> symbol table entry
	test	BYTE PTR es:[bx+2],20h	; see if compressed symbol
	jne	pct_comp			; yes
	jmp	NEAR PTR pct_nocomp	; no

; this symbol was compressed, remove its entry from LEDATA data
pct_comp:
	pop	es					; es -> destination (i/o buffer)
	push	es				; save back on stack
	push	si				; save critical registers
	push	cx

	push	ds
	mov	di,si				; di -> destination (overwrite this symbol in LEDATA)
	add	si,16				; si -> source
	jc	pcs_of2				; overflow
	cmp	si,buffer_end		; see if past buffer
	jb	pct_transfer		; no

; buffer wrap/overflow
pcs_of2:
	sub	si,buffer_end
	jmp	SHORT pct_transfer

; stepping stone to pct_done
to_pct_done:
	jmp	NEAR PTR pct_done

pct_dioverf:
	xor	di,di				; wrap di
	jmp	SHORT pct_trans1

pct_sioverf:
	xor	si,si				; wrap si
	jmp	SHORT pct_trans2

pct_transfer:
	sub	cx,16				; move all remaining bytes between symbol entry and end
	je	pct_transdone		; no bytes to transfer
	push	es

	mov	ax,cx				; check for buffer overflow (slow transfer code)
	add	ax,di
	jc	pct_buffover		; buffer overflow will occur
	cmp	ax,buffer_end
	jae	pct_buffover		; buffer overflow will occur
	mov	ax,cx
	add	ax,si
	jc	pct_buffover		; buffer overflow will occur
	cmp	ax,buffer_end
	jae	pct_buffover		; buffer overflow will occur

; buffer will not overflow, do quick transfer code
	pop	ds					; ds -> source (i/o buffer)
	shr	cx,1				; convert byte to word count
	rep	movsw
	jmp	SHORT pct_transdone

pct_buffover:
	mov	ax,buffer_end		; ax holds constant, buffer end
	pop	ds					; ds -> source (i/o buffer)

pct_transloop:
	cmp	di,ax				; see if destination overflow
	jae	pct_dioverf			; yes

pct_trans1:
	cmp	si,ax				; see if source overflow
	jae	pct_sioverf			; yes

pct_trans2:
	movsb					; transfer a char
	loop	pct_transloop	; loop through all chars to transfer

pct_transdone:
	pop	ds					; restore critical registers

; adjust relocation table as well
	mov	cx,data_fixup_count
	jcxz	pct_compdone	; no relocation entries
	mov	si,OFFSET DGROUP:data_fixup_flag
	mov	ax,reloc_lowwater	; get low water mark for shifting relocation entries

pct_relloop:
	cmp	ax,[si]				; see if relocation entry location was shifted
	ja	pct_nextrel			; no
	sub	WORD PTR [si],10h	; shift relocation entry down one para

pct_nextrel:
	add	si,2				; move to next entry, if any
	loop	pct_relloop

pct_compdone:
	pop	cx
	pop	si
	mov	ax,16				; use as constant
	sub	dx,ax				; drop count of bytes in LEDATA data
	sub	reloc_lowwater,ax	; adjust relocation low water mark for transfer
	sub	si,ax				; adjust si for transfer
	jnc	pct_nocomp			; no carry (no buffer wrap)
	add	si,buffer_end		; adjust for buffer wrap

pct_nocomp:
	add	bx,3				; move to next symbol table entry, if any
	mov	ax,16				; use as constant
	add	reloc_lowwater,ax	; adjust low water mark
	sub	cx,ax				; drop count of data bytes
	add	si,ax				; point to next entry
	jc	pcs_of1				; overflow
	cmp	si,buffer_end		; see if past buffer
	jb	pct_donechk			; no

; buffer wrap/overflow
pcs_of1:
	sub	si,buffer_end

pct_donechk:
	jcxz	pct_done		; no more
	jmp	NEAR PTR pct_comploop

pct_done:
	mov	ax,rec_offset		; adjust data_offset with new LEDATA length
	sub	WORD PTR data_offset,ax
	sbb	WORD PTR data_offset+2,0
	mov	ax,new_ledata_offset	; get new LEDATA offset after previous compression
	add	WORD PTR data_offset,ax
	adc	WORD PTR data_offset+2,0
	add	new_ledata_offset,dx	; save new next LEDATA offset after compression

	mov	cx,dx				; get count of data bytes after compression
	pop	es					; restore critical register
	pop	si
	ret
process_symbol_table	ENDP

;*****************************
;* COMPSYM_FIXUP             *
;*****************************

; fixup of compressed symbols
; upon entry data_rec_offset holds position of fixup within LEDATA image,
;  rec_offset holds offset of LEDATA data record
; returns carry flag set if discard fixup, reset otherwise
; destroys ax,bx,dx

compsym_fixup	PROC
	push	cx				; save critical register
	push	es
	mov	bx,data_rec_offset
	add	bx,rec_offset
	and	bl,0f0h				; mask off low bits
	mov	cl,3
	shr	bx,cl				; bx contains symbol number of fixup, word offset
	mov	cx,bx
	shr	cx,1
	add	bx,cx				; make 3-byte offset
	add	bx,12				; adjust past system bytes

	mov	es,current_clipmod_ptr	; es:[bx] -> symbol table count/offset
	test	BYTE PTR es:[bx+2],20h	; see if compressed symbol
	je	cf_nocomp			; no

; compressed symbol, trash fixup
cf_comp:
	stc						; set carry flag to indicate discard fixup
	jmp	SHORT cf_ret

cf_nocomp:
	clc

cf_ret:
	pop	es					; restore critical register
	pop	cx
	ret
compsym_fixup	ENDP

;***** END WORK

END
