;*********************************************************************
;*   MLDDL2.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          02/12/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.51                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   DDL specific code, part 2                                       *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlddl2
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Equates                   *
;*****************************

DDL_HEADER_SIZE EQU 128     ; size of DDL header (on 16-byte boundary)
MOD_HEADER_SIZE EQU 64      ; size of module header in DDL file (rounded to 16-byte boundary)

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
PUBLIC  write_ddl_dict,ddl_save_libmod_entry,ddl_lib_pass

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   ddl_header:BYTE,mod_header:BYTE
EXTRN   filename:BYTE,obj_handle:WORD,lib_page_size:WORD
EXTRN   lib_page_num:WORD,lib_handle:WORD

; word
curr_buffpos    DW  0
mod_fpos_ptr    DW  ?       ; segment pointer to module file position table
ind_fpos_ptr    DW  ?       ; segment pointer to module dictionary index
curr_modpub     DW  0       ; count of current module for public processing, relative 0
index_size  DW  ?           ; size of directory index
pubchar_count   DW  ?       ; count of saved public chars in module
pubpara_count   DW  0       ; running count of paragraphs of publics in dictionary
symbuff     DW  ?           ; segment of symbols buffer
pubbuff     DW  ?           ; segment of publics buffer
current_public  DW  ?       ; current public in module, relative zero

; doublewords
pos_adjust  DD  ?           ; adjustment for file positions within a module 

; structures
DDL_HEADER_STRUC    STRUC
    dh_sig1 DB  ?           ; DDL file signature bytes
    dh_sig2 DB  ?
    dh_sig3 DB  ?
    dh_sig4 DB  ?
    dh_majorver DB  ?       ; major version number
    dh_minor1   DB  ?       ; minor version number 1
    dh_minor2   DB  ?       ; minor version number 2
    dh_minor3   DB  ?       ; minor version alpha
    dh_hdrsize  DW  ?       ; size of module header
    dh_loadsize DW  ?       ; size of DDL loader
    dh_loadstart    DD  ?   ; file position of start of loader
    dh_flags    DD  ?       ; DDL flags
                            ; bit 0==main module flag
                            ; bit 1==required root modules flag
                            ; bit 2==required overlay modules flag
                            ; bit 3==elective root modules flag
                            ; bit 4==elective overlay modules flag
                            ; bit 5==DOSSEG flag
                            ; bit 6==contains pre-loading routine
    dh_op       DD  ?       ; /op option value
    dh_st       DW  ?       ; /st option value
    dh_as       DW  ?       ; /as option value
    dh_os       DW  ?       ; /os option value
    dh_ol       DW  ?       ; /ol option value
    dh_mem      DB  ?       ; 0==free mem op, nonzero==alloc mem op
    dh_minop    DB  ?       ; 0==normal /op, 1==/op:m
    dh_ox       DB  ?       ; 0==regular memory for op, nonzero==EMS page frame for op
    dh_r        DB  ?       ; /r option setting
    dh_cla      DB  ?       ; /cla option setting
    dh_ou       DB  ?       ; /ou option setting
    dh_ort      DB  ?       ; /ort setting
    dh_orp      DB  ?       ; /orp setting
    dh_modcount DW  ?       ; module count in DDL
    dh_ddlcount DW  ?       ; count of DDL's in dependency list (for main module)
    dh_reqroot  DW  ?       ; count of required root modules in DDL
    dh_reqovl   DW  ?       ; count of required overlay modules in DDL
    dh_elecroot DW  ?       ; count of elective root modules in DDL
    dh_elecovl  DW  ?       ; count of elective overlay modules in DDL
    dh_ddlstart DD  ?       ; file position of start of DDL dependency list
    dh_preload  DD  ?       ; file position of pre-load module
    dh_modstart DD  ?       ; file position of start of DDL module file position dword entries
    dh_dictstart    DD  ?   ; file position of start of DDL dictionary

	dh_ohp3		DB	?		; flag ohp3 use
	dh_ohp_flag	DB	?		; nonzero if /ohp allocate to amount flag set
	dh_oht_flag	DB	?		; nonzero if /oht allocate to amount flag set
	dh_pad		DB	?		; pad value
	dh_ohp		DW	?		; /ohp size in K
	dh_oht		DW	?		; /oht size in K

    dh_reser3   DD  ?       ; reserved for future
    dh_reser4   DD  ?       ; reserved for future
    dh_oxevar   DB  32 DUP (?)  ; specified /ox environment variable
DDL_HEADER_STRUC    ENDS

MOD_HEADER_STRUC    STRUC
    mh_flags    DD  ?       ; module flags
                            ; bit 0==main module flag
                            ; bit 1==required root module flag
                            ; bit 2==required overlay module flag
                            ; bit 3==elective root module flag
                            ; bit 4==elective overlay module flag
                            ; bit 6==pre-load module
    mh_id       DW  ?       ; module identifier
    mh_segcount DW  ?       ; count of segments in module
    mh_grpcount DW  ?       ; count of groups in module
    mh_pubcount DW  ?       ; count of publics
    mh_comcount DW  ?       ; count of communals
    mh_extcount DW  ?       ; count of externals
    mh_lnames   DD  ?       ; file position of start of lnames name block
    mh_segdef   DD  ?       ; file position of start of segment entries
    mh_grpdef   DD  ?       ; file position of start of group entries
    mh_symbols  DD  ?       ; file position of start of symbols (pub/ext/comdef) name block
    mh_pubdef   DD  ?       ; file position of start of pubdef entries
    mh_comdef   DD  ?       ; file position of start of comdef entries
    mh_extdef   DD  ?       ; file position of start of extdef entries
    mh_binary   DD  ?       ; file position of start of binary data
    mh_startup  DB  7 DUP (?)   ; start address if main module in fixup notation
    mh_pad      DB  ?       ; pad to keep at dword boundary
    mh_binfpos  DD  ?       ; file position of each binary entry file position table
    mh_reser2   DD  ?       ; reserved for future
MOD_HEADER_STRUC    ENDS

.DATA?

; uninitialized local variables

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   dos_error:NEAR,ddl_pad:NEAR
EXTRN   pass1_obj_proc:NEAR
EXTRN   print_link_info:NEAR
EXTRN   restore_ems_map:NEAR
EXTRN   load_libmod:NEAR

;*****************************
;* WRITE_DDL_DICT            *
;*****************************

; upon entry bx==file handle
; destroys ax,bx,cx,dx,si,di,es

write_ddl_dict  PROC
    mov ax,ddl_header.dh_elecroot
    add ax,ddl_header.dh_elecovl    ; get elective module count
    jne wdd_init
    ret                     ; no elective modules, return

; allocate buffer for module file position pointer 4*module count
wdd_init:
    mov ax,ddl_header.dh_modcount
    mov di,ax               ; save module count value
    add ax,15               ; round up to next paragraph
    shr ax,1
    shr ax,1                ; convert from dword entries to paras
    mov si,ax               ; save para count
    mov dx,buffer_base
    sub dx,ax               ; compute buffer segment value (just below i/o buffer)
    mov mod_fpos_ptr,dx     ; save it

; allocate buffer for module dictionary index table 2*module count
    shr si,1                ; convert to word entries
    sub dx,si
    mov ind_fpos_ptr,dx     ; save buffer segment value, just below module file position buffer

; seek to module file position table
    mov dx,WORD PTR ddl_header.dh_modstart
    mov cx,WORD PTR ddl_header.dh_modstart+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; read module file position table into buffer
    mov cx,di
    shl cx,1
    shl cx,1                ; 4 bytes/module
    mov si,ds
    mov ds,mod_fpos_ptr
    xor dx,dx
    mov ah,3fh              ; read from file
    int 21h
    jnc wdd_2

wdd_doserr:
    jmp NEAR PTR dos_error  ; error reading from file

wdd_2:
    mov ds,si               ; ds -> warplink data

    xor cx,cx
    mov dx,cx
    mov ax,4202h            ; move file pointer from end of file
    int 21h
    mov WORD PTR ddl_header.dh_dictstart,ax ; save dictionary file position
    mov WORD PTR ddl_header.dh_dictstart+2,dx

; write dummy dictionary module index to disk (pre-alloc space)
    add di,di               ; di == 2*module count
    mov index_size,di       ; save size of index
    mov cx,di
    xor dx,dx               ; ds:dx -> garbage, doesn't matter
    mov ah,40h              ; write to file
    int 21h
    jc  wdd_doserr

; seek to module
wdd_seekmod:
    mov es,mod_fpos_ptr
    mov si,curr_modpub
    shl si,1
    shl si,1                ; dword/module
    lods    WORD PTR es:0   ; get file position low
    mov dx,ax
    mov WORD PTR pos_adjust,ax
    lods    WORD PTR es:0   ; get file position high
    mov WORD PTR pos_adjust+2,ax
    mov cx,ax
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; read module header
    mov dx,OFFSET DGROUP:mod_header
    mov cx,MOD_HEADER_SIZE
    mov ah,3fh              ; read from file
    int 21h
    jc  wdd_doserr

    xor ax,ax
    mov pubchar_count,ax    ; init public name char count
    mov current_public,ax   ; init current public id

; if required module then go to next module
    mov al,BYTE PTR mod_header.mh_flags
    and al,6                ; get required module flags
    je  wdd_compsym         ; elective, process this module

to_wdd_nextmod:
    jmp NEAR PTR wdd_nextmod    ; required, try next module

; compute symbol block size (mh_pubdef-mh_symbols)
; since known <64K quantity only deal with low words
wdd_compsym:
    mov cx,WORD PTR mod_header.mh_pubdef
    sub cx,WORD PTR mod_header.mh_symbols

; if symbol block size > unused buffer space then flush buffer
    mov ax,0ffe0h           ; get buffer size (also allow 16 bytes for padding)
    sub ax,curr_buffpos     ; subtract buffer used
    cmp ax,cx               ; see if enough unused buffer space
    jae wdd_seeksym         ; yes

    call    flush_dict_buffer   ; flush buffer to disk

; seek to symbols
wdd_seeksym:
    mov di,cx               ; save size of symbols block
    mov dx,WORD PTR mod_header.mh_symbols
    mov cx,WORD PTR mod_header.mh_symbols+2
    add dx,WORD PTR pos_adjust
    adc cx,WORD PTR pos_adjust+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; setup buffer to read symbols
    mov dx,di
    add dx,15               ; round up symbols block size to next para
    mov cl,4
    shr dx,cl
    mov ax,ind_fpos_ptr
    sub ax,dx
    mov symbuff,ax          ; save pointer to symbol buffer, below index buffer

; read symbols block
wdd_readsym:
    mov cx,di               ; get size of symbols block
    xor dx,dx
    mov di,ds
    mov ds,ax               ; ds:dx -> start of symbol buffer
    mov ah,3fh              ; read from file
    int 21h
    jc  wbb_doserr2

; seek to public entries
    mov ds,di               ; ds -> warplink data
    mov dx,WORD PTR mod_header.mh_pubdef
    mov cx,WORD PTR mod_header.mh_pubdef+2
    add dx,WORD PTR pos_adjust
    adc cx,WORD PTR pos_adjust+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; setup buffer to read public entries, 1 para/public
    mov pubchar_count,0     ; init save public name char count
    mov ax,symbuff
    mov cx,mod_header.mh_pubcount
    jcxz    to_wdd_nextmod  ; no publics exist
    sub ax,cx
    mov pubbuff,ax

; read public entries
    shl cx,1                ; convert count of publics to byte count
    shl cx,1
    shl cx,1
    shl cx,1
    xor dx,dx
    mov di,ds
    mov ds,ax               ; ds:dx -> start of public buffer
    mov ah,3fh              ; read from file
    int 21h
    jnc wbb_procpub

wbb_doserr2:
    jmp NEAR PTR dos_error  ; error reading from file

; for each public that is nonlocal
;   look up public symbol name
;   store in buffer
wbb_procpub:
    mov ds,di
    mov cx,mod_header.mh_pubcount

wbb_publoop:
    xor dx,dx               ; init count of chars transferred this pass
    mov si,current_public
    shl si,1                ; convert to paras
    shl si,1
    shl si,1
    shl si,1
    mov es,symbuff
    mov ds,pubbuff          ; ds:si -> current public entry
    mov al,ds:[si+14]       ; get definition flag
    and al,3                ; mask off unwanted bits
    cmp al,2                ; see if public/absolute
    jb  wbb_nextpub         ; no
    mov al,ds:[si+15]       ; get general flags
;***    and al,24h              ; check local bits
;***    jne wbb_nextpub         ; local, don't use

; nonlocal public, transfer to buffer
    mov si,ds:[si+4]        ; get symbol name offset in symbol block
    mov ax,DGROUP
    mov ds,ax
    mov ax,es
    mov es,buffer_base
    mov di,curr_buffpos     ; es:di -> symbol storage buffer position
    mov ds,ax               ; ds:si -> symbol name

wbb_transloop:
    movsb
    inc dx                  ; bump count of chars transferred
    mov al,[si-1]
    or  al,al               ; see if null terminator transferred
    jne wbb_transloop

wbb_nextpub:
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    add pubchar_count,dx    ; add in count of chars transferred last public, if any
    add curr_buffpos,dx     ; update buffer position
    inc current_public      ; bump count of current public
    loop    wbb_publoop     ; loop until complete
    xor ax,ax
    mov es,buffer_base

wbb_zeroloop:
    mov di,curr_buffpos     ; get current buffer position
    test    di,0fh          ; see if on paragraph boundary
    je  wdd_nextmod         ; yes
    stosb                   ; zero out pad byte
    inc curr_buffpos        ; bump current buffer position
    jmp SHORT wbb_zeroloop  ; pad to paragraph boundary

; setup for next elective module and loop back
wdd_nextmod:
    mov es,ind_fpos_ptr
    mov di,curr_modpub
    shl di,1                ; es:di -> proper dictionary module index
    mov ax,pubchar_count    ; get count of public chars
    add ax,15               ; round up to next paragraph
    mov cl,4
    shr ax,cl               ; convert to paras
    add pubpara_count,ax    ; add to running total
    or  ax,ax               ; see if any publics
    je  wdd_stashcount      ; no, put a zero in the index
    mov ax,pubpara_count    ; get para offset to end

wdd_stashcount:
    stosw                   ; save public offset to end, or zero if none

    inc curr_modpub         ; bump current module parsing for publics
    mov ax,curr_modpub
    cmp ax,ddl_header.dh_modcount
    jae wbb_done            ; all modules parsed
    jmp NEAR PTR wdd_seekmod    ; more modules to parse

wbb_done:
    call    flush_dict_buffer   ; flush pending publics to DDL

; seek to dictionary module index
    mov dx,WORD PTR ddl_header.dh_dictstart
    mov cx,WORD PTR ddl_header.dh_dictstart+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; write dictionary module index to disk
    mov cx,index_size       ; get size of index
    mov di,ds
    mov ds,ind_fpos_ptr
    xor dx,dx
    mov ah,40h              ; write to file
    int 21h
    jc  fdb_doserr
    mov ds,di               ; restore ds -> warplink data
    ret
write_ddl_dict  ENDP

;*****************************
;* FLUSH_DICT_BUFFER         *
;*****************************

; bx == file handle upon entry
; destroys ax,cx,dx,si

flush_dict_buffer   PROC

; seek to end of file for buffer write
    xor cx,cx
    mov dx,cx
    mov ax,4202h            ; move file pointer from end of file
    int 21h

    mov si,ds
    mov cx,curr_buffpos
    xor dx,dx
    mov ds,buffer_base
    mov ah,40h              ; write to file
    int 21h
    jnc fdb_2

fdb_doserr:
    jmp NEAR PTR dos_error  ; error writing to file

fdb_2:
    mov ds,si
    mov curr_buffpos,0      ; re-init buffer position
    ret
flush_dict_buffer   ENDP

;*****************************
;* DDL_SAVE_LIBMOD_ENTRY     *
;*****************************

; move old library entries to new, low memory, location for UDL use
; destroys ax,cx,es

ddl_save_libmod_entry   PROC
    mov alloc_libent_ptr,0  ; init last allocated library entry pointer
    mov ax,first_libent_ptr

dsl_loop:
    or  ax,ax               ; see if next module
    je  dsl_done            ; no

    mov cx,allocation_base
    cmp alloc_libent_ptr,0  ; see if any previously allocated library entries
    je  dsl_first           ; no
    mov es,alloc_libent_ptr ; es -> old entry
    mov es:[30],cx          ; update pointer
    jmp SHORT dsl_2

; first module entry
dsl_first:
    mov first_libent_ptr,cx ; udpate first module pointer

dsl_2:
    mov alloc_libent_ptr,cx ; update last allocated library entry to new save location
    mov es,cx               ; es -> allocation base, new module entry save location
    xor di,di               ; es:di -> new location for module entry
    mov ds,ax
    mov si,di               ; ds:si -> old module entry to save
    mov cx,LIB_MODENT_SIZE*8    ; number of words to transfer (paras*8)
    rep movsw               ; save module entry to new memory location

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    mov ax,es
    add ax,LIB_MODENT_SIZE
    mov allocation_base,ax  ; update allocation base past saved module entry
    mov ax,es:[30]          ; get pointer to next module entry to be saved
    jmp SHORT dsl_loop      ; process it

dsl_done:
    ret

ddl_save_libmod_entry   ENDP

;*****************************
;* DDL_LIB_PASS              *
;*****************************

; DDL pass library processing
; destroys ax,bx,cx,dx,di,si,es

ddl_lib_pass    PROC
    mov current_lib,-1      ; force library to load on first compare
    mov ax,first_libent_ptr ; get pointer to first library module entry

dlp_loop:
    or  ax,ax               ; check that segment is nonzero
    jne dlp_1               ; yes
    ret                     ; segment is zero, all done processing library modules

dlp_1:
    mov es,ax               ; es -> library module entry
    push    ax              ; save entry segment

    mov ax,es:[0]           ; get library
    cmp ax,current_lib      ; see if matches current library
    jne dlp_newlib          ; no
    jmp NEAR PTR  dlp_2     ; yes

dlp_newlib:
    cmp current_lib,-1      ; see if first time through loop
    je  dlp_1a              ; yes
    mov cx,ax               ; save library in cx
    mov bx,obj_handle       ; get file handle of previously opened library
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    mov ax,cx               ; get library back in ax

dlp_1a:
    mov current_lib,ax      ; update current library to reflect library being processed

; get and open library indicated by library module entry
    push    es              ; save critical register
    xor cx,cx               ; cx == library entry counter
    mov ax,first_libblk_ptr ; get first object name block

dlp_1b:
    mov si,4                ; si -> first name in block
    mov es,ax               ; point extra segment at object name block

dlp_lib_loop:
    mov al,es:[si]          ; get flag byte
    and al,0c0h             ; only get overlay bits
    mov obj_ovl_flag,al     ; save overlay status for module
    and al,40h              ; get nonvector root call status
    mov nonovl_rvect,al     ; save it to global variable
    inc si                  ; adjust past prepended flag byte in entry
    mov ax,LIB_NAMBLK_BYSIZE    ; size of block in bytes
    sub ax,es:[0]           ; minus free space, ax == end of used namelist
    cmp ax,si               ; check that position in list is below end
    ja  dlp_lib2            ; not at end yet, check namelist position

    mov ax,es:[2]           ; get pointer to next block
    jmp SHORT dlp_1b

dlp_lib2:
    cmp cx,current_lib      ; see if at proper position in library name block
    je  dlp_lib4            ; yes

; not at proper position, gobble bytes until next library name
dlp_lib_gobble:
    mov al,es:[si]          ; check if at end of library name in block
    or  al,al
    je  dlp_lib3            ; yes
    inc si                  ; bump to next char
    jmp dlp_lib_gobble

dlp_lib3:
    inc si                  ; bump to first char of next name in block
    inc cx                  ; bump count of entry scanned
    jmp SHORT dlp_lib_loop  ; and loop back to try next namelist position

dlp_lib4:
    push    ds              ; save critical register
    push    ds
    push    es
    pop ds                  ; ds:si -> library name in block
    pop es
    mov di,OFFSET DGROUP:filename   ; es:di -> destination of library name

dlp_transfer:
    movsb                   ; transfer a char from the block to filename
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne dlp_transfer        ; nonzero, keep looping

    pop ds                  ; restore critical register
    pop es                  ; restore critical register
    call    print_link_info ; print library name, if applicable
    mov dx,OFFSET DGROUP:filename   ; DS:DX -> ASCIIZ file specification
    mov ax,3d00h            ; open file with read access
    int 21h
    call    restore_ems_map
    jc  dlp_doserr          ; error occurred

    mov obj_handle,ax       ; save library file handle

dlp_2:
    mov ax,es:[4]           ; get library page size
    mov lib_page_size,ax    ; save to memory variable
    mov ax,es:[2]           ; get page number (location in lib file)
    mov lib_page_num,ax     ; save to memory variable

    mov ax,obj_handle       ; get file handle into lib_handle
    mov lib_handle,ax

    mov bx,es:[6]           ; get module length
    call    load_libmod     ; load the module
    call    pass1_obj_proc  ; pass 1 process loaded library module

    pop es                  ; es -> library module entry
    mov ax,es:[30]          ; get pointer to next entry
    jmp NEAR PTR dlp_loop   ; loop back to process it

dlp_doserr:
    jmp NEAR PTR dos_error  ; error occurred

ddl_lib_pass    ENDP

END
