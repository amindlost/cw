;*********************************************************************
;*   MLDDL1.ASM                                                      *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          04/04/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   DDL specific code, part 1                                       *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlddl1
PAGE    50,80

.MODEL  SMALL

;*****************************
;* Equates                   *
;*****************************

DDL_HEADER_SIZE EQU 128     ; size of DDL header (on 16-byte boundary)
MOD_HEADER_SIZE EQU 64      ; size of module header in DDL file (rounded to 16-byte boundary)
BIN_HEADER_SIZE EQU 20      ; size of segment binary header

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
PUBLIC  proc_ddl,create_ddl,write_ddl_module,save_module_type
PUBLIC  parse_ddl_option,reinit_variables
PUBLIC  ddl_fixupp,ddl_datarec,ddl_pad

; variables
PUBLIC  ddl_header,mod_header

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

EXTRN   exe_handle:WORD,libmod_obj_flag:BYTE
EXTRN   first_udlent_ptr:WORD
EXTRN   first_lnamesblk_ptr:WORD,first_pdnameblk_ptr:WORD
EXTRN   objtext:BYTE,mod_alloc_base:WORD
EXTRN	ddl_depend_count:BYTE

; initialized variables
EVEN
module_id   DW  0           ; current module identifier
first_ddl_fpos  DW  0       ; segment of first allocated ddl module file position block
alloc_ddl_fpos  DW  0       ; segment of last allocated ddl module file position block
pubcount    DW  0           ; count of publics in module
comcount    DW  0           ; count of communals in module
extcount    DW  0           ; count of externals in module
modend_ptr  DW  0           ; pointer to block that holds MODEND info

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

ddl_header  DDL_HEADER_STRUC    <'M','D','D','L','2','0','0','a',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>

MOD_HEADER_STRUC    STRUC
    mh_flags    DD  ?       ; module flags
                            ; bit 0==main module flag
                            ; bit 1==required root module flag
                            ; bit 2==required overlay module flag
                            ; bit 3==elective root module flag
                            ; bit 4==elective overlay module flag
                            ; bit 6==pre-load module
                            ; bit 7==contains communal variables (COMDEFs)
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

mod_header  MOD_HEADER_STRUC    <0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>

BIN_HEADER_STRUC    STRUC
    bhs_flags   DD  ?       ; flags
                            ; bit 0==overlay class flag
    bhs_segind  DW  ?       ; segment index
    bhs_length  DW  ?       ; length of segment binary data (not necessarily segment length)
    bhs_fixptr  DD  ?       ; file position of start of fixups for segment (0 if none)
    bhs_nextptr DD  ?       ; file position of next binary block (0 if last)
    bhs_offset  DW  ?       ; start offset within segment
    bhs_truelen DW  ?       ; true length of segment
BIN_HEADER_STRUC    ENDS

bin_header  BIN_HEADER_STRUC    <0,0,0,0,0,0>

FIXUP_HEADER_STRUC  STRUC
    fhs_lowcount    DW  ?   ; count of low-order byte fixups
    fhs_lowinfo     DD  ?   ; file position of low-order byte fixup info entries
    fhs_lowloc      DD  ?   ; file position of low-order byte fixup locations
    fhs_nearcount   DW  ?   ; count of near fixups
    fhs_nearinfo    DD  ?   ; file position of near fixup information entries
    fhs_nearloc     DD  ?   ; file position of near fixup locations
    fhs_farcount    DW  ?   ; count of far fixups
    fhs_farinfo     DD  ?   ; file position of far fixup information entries
    fhs_farloc      DD  ?   ; file position of far fixup locations
FIXUP_HEADER_STRUC  ENDS

.DATA?

; uninitialized local variables

; byte values
EVEN
pubcom_flag DB  ?           ; nonzero if parsing communal entries
EVEN
;***block_flag  DB  ?           ; ==2 if parsing public block, ==1 if parsing communal block
EVEN
save_fixup  DB  ?           ; nonzero if save fixup to disk
EVEN
frame_method    DB  ?       ; fixup frame method
EVEN
target_method   DB  ?       ; fixup target method
EVEN
fixdat  DB  ?               ; fixdat byte
EVEN
is_loc  DB  ?               ; nonzero if parsing LIDATA fixups for location, zero if for info
EVEN
pass_count  DB  ?           ; count of fixup parsing pass

; word values
EVEN
frame_datum DW  ?           ; fixup frame datum/index
target_datum    DW  ?       ; fixup target datum/index
data_rec_off    DW  ?       ; fixup data record offset
iter_data_off   DW  ?       ; iterated data offset
sort_buff_bound DW  ?       ; sort buffer upper boundary
proc_segment    DW  ?       ; number of segment with binary data being processed
lobyte_count    DW  ?       ; count of low-order fixups
near_count      DW  ?       ; count of near (offset) fixups
far_count       DW  ?       ; count of far (segment) fixups
binary_fpos DW  ?           ; segment of binary file position table
curr_segdefent_ptr  DW  ?   ; pointer to current segdef entry being processed for binary/fixups
seg_truelen     DW  ?       ; true length of segment saved to binary header

;***repeat_count    DW  ?       ; LIDATA iterated data block repeat count field
;***block_count     DW  ?       ; LIDATA iterated data block block count field

; doubleword values
ddl_filesize    DD  ?       ; current DDL size
module_start    DD  ?       ; file position of start of module (all file positions within relative to it)
currbin_start   DD  ?       ; file position of current binary block
binfpos_table   DD  ?       ; file position of binary file position table

; byte strings
bare_exe_name   DB  13 DUP (?)  ; EXE name without path

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
dat_default DB  'DDLMGR.DAT',0  ; default DDL manager data file name
loader_hdr  DB  4Dh,5Ah,8Bh,1,2,0,0,0,20h,00,00,00,0FFh,0FFh
loader_code DB  0EBh,16h
lc_ddlname  DB  'NONAME.DDL',0,0,0
lc_ddlpath  DB  'DDLPATH=',0
            DB  1Eh,0Eh,1Fh,0BAh,2,0,0B8h,0
            DB  3Dh,0CDh,21h,72h,3,0E9h,0Bh,1,3Dh,2,0,74h,0Ah,3Dh,3,0
            DB  74h,5,3Dh,5,0,75h,69h,26h,0A1h,2,0,2Dh,8,0,8Eh,0D8h
            DB  06h,26h,0A1h,2Ch,0,8Eh,0C0h,0B4h,30h,0CDh,21h,3Ch,2,76h,56h,6
            DB  1Eh,7,1Fh,33h,0F6h,0ACh,0Ah,4,75h,0FBh,83h,0C6h,3,8Bh,0FEh,0ACh
            DB  0Ah,0C0h,74h,8,3Ch,5Ch,75h,0F7h,8Bh,0DEh,0EBh,0F3h,8Bh,0F7h,33h,0FFh
            DB  0A4h,3Bh,0F3h,75h,0FBh,8Ch,0DBh,0Eh,1Fh,0BEh,2,0,0A4h,80h,7Ch,0FFh
            DB  0,75h,0F9h,6,1Fh,33h,0D2h,0B8h,0,43h,0CDh,21h,72h,3,0E9h,9Ah
            DB  0,3Dh,2,0,74h,0Dh,3Dh,3,0,74h,8,3Dh,5,0,74h,3
            DB  0E9h,0E4h,0,8Eh,0C3h,0BBh,0Fh,0,33h,0F6h,33h,0FFh,26h,8Ah,4,46h
            DB  2Eh,3Ah,1,74h,0Ah,26h,0Ah,4,75h,0F0h,0B8h,2,0,0EBh,0E1h,0Bh
            DB  0FFh,75h,0Ch,83h,0FEh,1,74h,7,26h,80h,7Ch,0FEh,0,75h,0DBh,47h
            DB  83h,0FFh,8,72h,0D7h,33h,0FFh,26h,8Ah,4,3Ch,3Bh,74h,0Ah,3Ch,20h
            DB  72h,6,88h,5,47h,46h,0EBh,0EFh,56h,0BEh,2,0,80h,7Dh,0FFh,5Ch
            DB  74h,4,0C6h,5,5Ch,47h,2Eh,8Ah,4,88h,5,0Ah,0C0h,74h,4,46h
            DB  47h,0EBh,0F3h,0B8h,0,43h,0CDh,21h,73h,20h,3Dh,2,0,74h,0Ah,3Dh
            DB  03h,0,74h,5,3Dh,5,0,75h,87h,5Eh,32h,0C0h,26h,38h,4,74h
            DB  99h,26h,38h,44h,1,74h,93h,46h,0EBh,0ABh,58h,0B8h,0,3Dh,0CDh,21h
            DB  72h,55h,7,8Bh,0D8h,26h,0A1h,2,0,2Dh,1,0,8Eh,0D8h,33h,0D2h
            DB  0B9h,10h,0,0B4h,3Fh,0CDh,21h,72h,3Eh,8Bh,16h,0Ch,0,8Bh,0Eh,0Eh
            DB  0,0B8h,0,42h,0CDh,21h,72h,2Fh,8Bh,0Eh,0Ah,0,8Bh,0D1h,83h,0C2h
            DB  0Fh,0D1h,0EAh,0D1h,0EAh,0D1h,0EAh,0D1h,0EAh,26h,0A1h,2,0,2Bh,0C2h,8Eh
            DB  0D8h,33h,0D2h,0B4h,3Fh,0CDh,21h,72h,0Eh,90h,90h,90h,90h,90h,90h,8Ch
            DB  0D8h,1Fh,50h,33h,0C0h,50h,0CBh,0B4h,4Ch,0CDh,21h

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

; procedures
EXTRN   dos_error:NEAR,restore_ems_map:NEAR,alloc_memory:NEAR
EXTRN   link_error:NEAR,load_file:NEAR,save_thread:NEAR
EXTRN   check_libobj_path:NEAR,write_ddl_dict:NEAR
EXTRN	file_not_found:NEAR

;*****************************
;* CREATE_DDL                *
;*****************************

; create the DDL file, write header info, write dependency list
; destroys ax,bx,cx,dx,di

create_ddl  PROC
    mov dx,OFFSET DGROUP:exe_name   ; DS:DX -> ASCIIZ file specification
    mov ah,3ch              ; create/truncate file
    xor cx,cx               ; normal file attribute
    int 21h
    jc  cd_error            ; error occurred

; write the DDL header info
    mov exe_handle,ax       ; keep file handle of DDL file
    mov bx,ax
    mov ax,DDL_HEADER_SIZE  ; get size of DDL header
    mov ddl_header.dh_hdrsize,ax    ; save it
    mov cx,ax               ; bytes to write
    mov dx,OFFSET DGROUP:ddl_header ; ds:dx -> write buffer area
    mov ah,40h              ; write to file
    int 21h
    jc  cd_error            ; error writing to file

    call    write_ddl_mgr       ; read in DDL manager data file, write it
    call    update_ddl_size ; update the DDL file size

; save start of DDL dependency list
    mov WORD PTR ddl_header.dh_ddlstart,ax
    mov WORD PTR ddl_header.dh_ddlstart+2,dx

    call    write_ddl_depend    ; write dependency list
    call    update_ddl_size ; update the DDL file size

    ret

cd_error:
    jmp NEAR PTR dos_error  ; error opening file

create_ddl  ENDP

;*****************************
;* WRITE_DDL_DEPEND          *
;*****************************

; write DDL dependency list
; upon entry bx==DDL file handle
; destroys ax,cx,dx,es

write_ddl_depend    PROC

; write as many names as are on dependency list, every 80 bytes
    mov ax,first_udlent_ptr ; init to first name on list, if any
    mov dx,2                ; point past pointer first two bytes
    mov cx,80              	; write 80 bytes

wdd_loop:
    or  ax,ax               ; see if any more names to write
    je  wdd_listdone        ; no
    mov ds,ax
    mov ah,40h              ; write to file
    int 21h
    jc  wdd_error           ; error writing to file

    mov ax,ds:[0]           ; get pointer to next name
    jmp SHORT wdd_loop      ; write it

wdd_listdone:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

wdd_ret:
    ret

wdd_error:
    jmp NEAR PTR dos_error  ; error opening file

write_ddl_depend    ENDP

;*****************************
;* WRITE_DDL_MGR             *
;*****************************

; read DDL loader from manager data file, write to DDL file
; upon entry bx==DDL file handle
; destroys ax,cx,dx

write_ddl_mgr   PROC
    call    update_ddl_size ; update the DDL file size

; update manager file position pointer (place at end of file)
    mov WORD PTR ddl_header.dh_loadstart,ax
    mov WORD PTR ddl_header.dh_loadstart+2,dx

    push    bx              ; save DDL file handle
    mov di,OFFSET DGROUP:ddldat_filename
    xor al,al
    cmp [di],al             ; see if DDL manager data file exists
    jne wdg_2               ; yes
    mov di,OFFSET DGROUP:dat_default    ; no, use default manager file name

wdg_2:
    mov dx,di
    mov ax,3d00h            ; open file with read access
    int 21h
    jnc wdg_3               ; no errors
	call	file_not_found	; check for file not found error

; DDLMGR file not found in current directory, search OBJ environment string
wdg_find_mgr:
    mov bx,OFFSET DGROUP:objtext    ; bx holds target string address for compares
    call    check_libobj_path   ; check e-var, error if not found, return di-> path+name
    jmp SHORT wdg_2

wdg_3:
    mov bx,ax               ; save file handle
    xor dx,dx
    mov cx,32768            ; DDL manager size always less than 32K
    push    ds
    mov ds,buffer_base      ; ds:dx -> buffer area to load into
    mov ah,3fh              ; read from file
    int 21h
    jc  wdd_error           ; error occurred
    pop ds
    mov ddl_header.dh_loadsize,ax   ; save count of bytes read
    mov cx,ax

    mov ah,3eh              ; close DDL manager file
    int 21h
    jc  wdd_error           ; error occurred

    pop bx                  ; restore DDL file handle
    push    ds
    mov ds,buffer_base      ; ds:dx -> buffer area to read from
    mov ah,40h              ; write to file
    int 21h
    jc  wdd_error           ; error occurred
    pop ds
    call    ddl_pad         ; pad out to even paragraph boundary

    ret
write_ddl_mgr   ENDP

;*****************************
;* UPDATE_DDL_SIZE           *
;*****************************

; seek to end of file, update DDL size variable
; destroys ax,bx,cx,dx
; returns bx==DDL file handle, filesize in dx:ax

update_ddl_size PROC
    mov bx,exe_handle
    xor cx,cx
    mov dx,cx
    mov ax,4202h            ; move file pointer from end of file
    int 21h
    mov WORD PTR ddl_filesize,ax    ; save low word of DDL file size
    mov WORD PTR ddl_filesize+2,dx  ; save high word
    ret
update_ddl_size ENDP

;*****************************
;* PROC_DDL                  *
;*****************************

; main DDL driver to process raw DDL file into final DDL file
; destroys ax,bx,cx,dx,si,di,es

proc_ddl    PROC
    call    write_ddl_modpos    ; write DDL module file position entries at end of DDL file

    mov ax,module_count
    cmp ax,1024             ; no more than 1024 modules/DDL file
    ja  pd_moderr           ; too many modules
    mov ddl_header.dh_modcount,ax   ; save count of modules in DDL file

    call    write_ddl_dict  ; write DDL dictionary
    mov bx,exe_handle
    xor cx,cx
    mov dx,cx
    mov ax,4200h            ; move file pointer from start of file
    int 21h                 ; rewind file
    call    restore_ems_map
    jnc pd_2                ; no error
    jmp NEAR PTR dos_error

pd_moderr:
    mov ax,DDLMOD_COUNT_ERR ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

pd_2:
    mov al,ddl_depend_count	; get ddl dependency count
    xor ah,ah               ; one byte value for now
    mov ddl_header.dh_ddlcount,ax

; merge in flag bits
    xor ah,ah
    mov al,is_ddl           ; see if creating DDL or using one
    or  al,al
    jne pd_useddl           ; creating DDL
    or  ah,1                ; set main module flag

pd_useddl:

    mov al,is_dosseg        ; see if DOSSEG flag set
    or  al,al
    je  pd_nodosseg         ; no
    or  ah,20h              ; set DOSSEG flag

pd_nodosseg:
    mov al,ah
    mov BYTE PTR ddl_header.dh_flags,al ; save flags

; save pertinent linker settings
    mov ax,WORD PTR ovl_pool    ; /op setting
    mov WORD PTR ddl_header.dh_op,ax
    mov ax,WORD PTR ovl_pool+2
    mov WORD PTR ddl_header.dh_op+2,ax
    mov ax,stack_value      ; /st setting
    mov ddl_header.dh_st,ax
    mov ax,maxpar_value     ; /as setting
    mov ddl_header.dh_as,ax
    mov ax,ovl_stack        ; /os setting
    mov ddl_header.dh_os,ax
    mov ax,ovl_max_load     ; /ol setting
    mov ddl_header.dh_ol,ax
    mov al,ovl_mem_alloc    ; /op +- setting
    mov ddl_header.dh_mem,al
    mov al,is_ems_ovlpool   ; /ox setting
    mov ddl_header.dh_ox,al
    mov al,is_reload        ; /r setting
    mov ddl_header.dh_r,al
    mov al,is_clarion       ; /cla setting
    mov ddl_header.dh_cla,al
    mov al,is_min_pool      ; /op:m setting
    mov ddl_header.dh_minop,al

	mov	al,is_umb
	mov	ddl_header.dh_ou,al	; /ou setting
	mov	al,ems3_flag
	mov	ddl_header.dh_ohp3,al	; /ohp3 setting
	mov	ax,ovl_ohp_size
	mov	ddl_header.dh_ohp,ax	; /ohp size
	mov	ax,ovl_oht_size
	mov	ddl_header.dh_oht,ax	; /oht size
	mov	al,ovl_ohp_alloc
	mov	ddl_header.dh_ohp_flag,al	; /ohp alloc flag
	mov	al,ovl_oht_alloc
	mov	ddl_header.dh_oht_flag,al	; /oht alloc flag
	xor	ax,ax
	cmp	is_ohp,al			; see if ohp set
	jne	pd_3				; yes
	mov	ddl_header.dh_ohp,ax	; zero it out

pd_3:
	cmp	is_oht,al			; see if oht set
	jne	pd_4				; yes
	mov	ddl_header.dh_oht,ax	; zero it out

pd_4:
    mov si,OFFSET DGROUP:ovl_ox_evar    ; /ox environment variable
    mov di,OFFSET DGROUP:ddl_header.dh_oxevar
	push	ds
	pop	ds
    mov cx,16               ; e-var storage is 32 bytes
    rep movsw

    mov dx,OFFSET DGROUP:ddl_header ; ds:dx -> write buffer area
    mov cx,DDL_HEADER_SIZE  ; size of DDL header
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jc  pd_error            ; error writing to file

    mov ah,3eh              ; close DDL file
    int 21h

    call    write_ddl_loader    ; create loader exe file
    ret

pd_error:
    jmp NEAR PTR dos_error  ; error opening file

proc_ddl    ENDP

;*****************************
;* WRITE_DDL_LOADER          *
;*****************************

; create the loader exe file
; destroys ax,bx,cx,dx,di,si

write_ddl_loader    PROC
    mov ax,ddl_header.dh_reqroot    ; make EXE loader only if required modules
    or  ax,ddl_header.dh_reqovl
    jne wdl_2
    ret                     ; no required modules, make no EXE loader

; find '.' of extension in exe_name, add EXE after
wdl_2:
    mov si,OFFSET DGROUP:exe_name
    mov dx,si               ; save dx -> name for later file open

wdl_loop:
    lodsb                   ; get char of exe_name
    or  al,al               ; see if at null terminator
    jne wdl_loop            ; no

    dec si                  ; si -> null terminator
    std                     ; make searches go backwards

wdl_loop2:
    lodsb
    cmp al,'.'              ; see if at extension mark
    jne wdl_loop2           ; no

    mov bx,si               ; save -> search position
    inc si                  ; adjust si so no unchecked chars

wdl_loop3:
    lodsb                   ; backup char
    cmp al,'\'              ; see if at pathspec
    je  wdl_savename        ; yes, save name
    cmp al,':'              ; see if drivespec
    je  wdl_savename        ; yes, save name
    cmp si,OFFSET DGROUP:exe_name   ; see if backed up to start of name
    jae wdl_loop3           ; no
    dec si                  ; yes, adjust si -> 2 chars before name

; si -> 2 chars before start of name
wdl_savename:
    add si,2                ; si -> start of name
    cld                     ; reset string operations to auto-increment
    mov cx,6                ; transfer 12 bytes of name (even if not 12 chars long)
    mov di,OFFSET DGROUP:lc_ddlname ; di -> destination of name chars

wdl_transloop:
    lodsw                   ; get 2 chars of name to transfer
    mov [di],ax             ; transfer it
    inc di
    inc di
    loop    wdl_transloop   ; loop until done

    mov al,'E'              ; add EXE extension, bx -> char before '.'
    mov [bx+2],al
    mov BYTE PTR [bx+3],'X'
    mov [bx+4],al

    mov ah,3ch              ; create/truncate file
    xor cx,cx               ; normal file attribute
    int 21h
    jc  wdl_error           ; error occurred

    mov bx,ax               ; save file handle in bx
    mov cx,14               ; write nonzero EXE header bytes
    mov dx,OFFSET DGROUP:loader_hdr
    mov ah,40h              ; write to file
    int 21h
    jc  wdl_error           ; error occurred

    mov cx,498              ; write zero EXE header bytes
    mov dx,OFFSET DGROUP:zero_table
    mov ah,40h              ; write to file
    int 21h
    jc  wdl_error           ; error occurred

    mov cx,18bh             ; write loader code
    mov dx,OFFSET DGROUP:loader_code
    mov ah,40h              ; write to file
    int 21h
    jc  wdl_error           ; error occurred

    mov ah,3eh              ; close file
    int 21h
    jc  wdl_error           ; error occurred
    ret

wdl_error:
    jmp NEAR PTR dos_error  ; error opening file

write_ddl_loader    ENDP

;*****************************
;* WRITE_DDL_MODPOS          *
;*****************************

; write DDL module file position entries at end of DDL file
; destroys ax,bx,cx,dx,es

write_ddl_modpos    PROC
    call    update_ddl_size ; update the DDL file size

; update module file position pointer (place at end of file)
    mov WORD PTR ddl_header.dh_modstart,ax
    mov WORD PTR ddl_header.dh_modstart+2,dx

    mov ax,first_ddl_fpos   ; init ax to first DLL module position

wmp_loop:
    or  ax,ax               ; see if valid block
    je  wmp_ret             ; no

    mov ds,ax               ; ds -> block
    mov dx,2                ; ds:dx -> start of file position entries in block
    mov cx,256              ; write 64 dword entries (may not all be used)
    mov ah,40h              ; write to file (bx already == handle)
    int 21h
    jc  wdl_error           ; error writing to file
    mov ax,ds:[0]           ; get pointer to next file block, if any
    jmp SHORT wmp_loop      ; write it if it exists

wmp_ret:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    ret
write_ddl_modpos    ENDP

;*****************************
;* SAVE_MODULE_TYPE          *
;*****************************

; set appropriate main DDL and module flags
; increment appropriate DDL module type counter
; destroys ax

save_module_type    PROC
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne smt_okay            ; no
    ret                     ; yes, no processing to complete

; module is elective if in library list and libmod_obj_flag variable not set
; AND not processing udl
smt_okay:
    xor ax,ax
    cmp udl_proc_pass,2     ; see if second pass of udl
    je  smt_required        ; yes, always required module
    cmp is_inlib,al         ; see if in library
    je  smt_required        ; no
    cmp libmod_obj_flag,al  ; see if library module as object module flag set
    jne smt_required        ; yes
    cmp obj_ovl_flag,al     ; see if in an overlay
    jne smt_elecovl         ; yes

; elective, in root
    inc ddl_header.dh_elecroot
    mov al,8
    jmp SHORT smt_setflags

; elective, in overlay
smt_elecovl:
    inc ddl_header.dh_elecovl
    mov al,10h
    jmp SHORT smt_setflags

; module is required if in object module list
; OR in library list and libmod_obj_flag is set
smt_required:
    cmp obj_ovl_flag,al     ; see if in an overlay
    jne smt_reqovl          ; yes

; required, in root
    inc ddl_header.dh_reqroot
    mov al,2
    jmp SHORT smt_setflags

; required, in overlay
smt_reqovl:
    inc ddl_header.dh_reqovl
    mov al,4

smt_setflags:
    or  BYTE PTR ddl_header.dh_flags,al
    or  BYTE PTR mod_header.mh_flags,al
    ret
save_module_type    ENDP

;*****************************
;* WRITE_DDL_MODULE          *
;*****************************

; write DDL module info
; destroys ax,bx,dx,di

write_ddl_module    PROC
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne wdm_okay            ; no
    ret                     ; yes, no processing to complete

wdm_okay:
    call    ddl_modend      ; save the MODEND info
    push    cx              ; save critical register
    push    si
    push    es
    call    update_ddl_size ; update the DDL file size
    push    dx              ; save high word of file size
    push    ax              ; save low word
    mov WORD PTR module_start,ax    ; save low word module start
    mov WORD PTR module_start+2,dx  ; save high word module start

; write dummy module header
    call    write_ddl_modhead

; write dummy dword entries for each segment (segment count*4 bytes)
    call    write_binary_fpos

; pad to paragraph length with zeros
    call    ddl_pad

; write consolidated lnames block
    call    write_lnames_block

; write segment entries
    call    write_segdef_block

; write group entries
    call    write_grpdef_block

; write consolidated symbol names block
    call    write_symbols_block

; write pubdef entries
    call    write_pubdef_block

; write comdef entries
    call    write_comdef_block

; write extdef entries
    call    write_extdef_block

; translate and write binary/fixup data
    call    write_binary_block

; translate modend and keep start address, if any
    call    write_modend_info

; pad to paragraph length with zeros
    call    ddl_pad

; seek back to binary file position table
    mov dx,WORD PTR binfpos_table
    mov cx,WORD PTR binfpos_table+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

    mov cx,current_segdef   ; get count of segments
    shl cx,1                ; x2
    shl cx,1                ; x4, count of bytes to write
    jcxz    wdm_1           ; no bytes to write
    xor dx,dx
    mov ds,binary_fpos      ; ds:dx -> write buffer
    mov ah,40h              ; write to device
    int 21h
    jnc wdm_1
    jmp NEAR PTR dos_error  ; error writing to file

wdm_1:
    mov ax,DGROUP
    mov ds,ax
    mov ax,mod_alloc_base   ; get memory allocation base prior to module use
    mov allocation_base,ax  ; restore pre-module memory allocation base

; save module file position
    mov ax,alloc_ddl_fpos   ; get segment of DDL module file position table
    mov es,ax
    or  ax,ax               ; see if exists
    jne wdm_2               ; yes

; file position table does not exist, build it
    mov bx,11h              ; each table is 272 bytes (64 dword entries, 1 system paragraph
    call    alloc_memory    ; get memory for block allocation
    mov first_ddl_fpos,ax   ; set first allocation pointer

wdm_init:
    mov es,ax               ; es -> table
    mov alloc_ddl_fpos,ax   ; update last allocation pointer
    xor ax,ax
    mov es:[0],ax           ; zero out pointer
    jmp SHORT wdm_3         ; ax holds entry within new block

wdm_2:
    mov ax,module_id
    and ax,3fh              ; see if need to start a new block
    jne wdm_3               ; no

; need to allocate new block, update old block to point to it
    mov bx,11h
    call    alloc_memory
    mov es,alloc_ddl_fpos   ; es -> old block
    mov es:[0],ax           ; save pointer to new block
    jmp SHORT wdm_init      ; transfer to initialization code

wdm_3:
    shl ax,1                ; x2
    shl ax,1                ; x4, ax holds dword offset
    add ax,2                ; bump past system pointer word
    mov bx,ax
    pop dx                  ; get low word of file position off of stack
    pop cx                  ; get high word
    mov es:[bx],dx          ; save it to file position table
    mov es:[bx+2],cx
    mov ax,allocation_base
    mov mod_alloc_base,ax   ; update allocation base to restore to past file position table entries

    mov ax,module_id
    inc ax                  ; update module identifier
    mov module_id,ax
    mov mod_header.mh_id,ax ; save to module header info
    mov ax,current_segdef   ; get count of segments
    mov mod_header.mh_segcount,ax   ; save it
    mov ax,current_grpdef   ; get count of groups
    mov mod_header.mh_grpcount,ax   ; save it
    mov ax,communal_count   ; see if any communals
    or  al,ah               ; al nonzero if any communals
    je  wdm_seek            ; no communals
    or  BYTE PTR mod_header.mh_flags,80h    ; flag communals exist

; seek back to start of module
wdm_seek:
    mov bx,exe_handle
    mov ax,4200h            ; move file pointer, absolute byte offset
    int 21h
    call    write_ddl_modhead   ; write updated module header info
    call    reinit_variables

    pop es                  ; restore critical register
    pop si
    pop cx
    ret
write_ddl_module    ENDP

;*****************************
;* REINIT_VARIABLES          *
;*****************************

; reinitialize variables for DDLs
; destroys ax,cx,di,es

reinit_variables    PROC
    xor ax,ax               ; re-init total symbol count
    mov WORD PTR mod_header.mh_flags,ax ; zero out flags
    mov first_pdeclblk_ptr,ax   ; zero out block pointers
    mov alloc_pdeclblk_ptr,ax
    mov first_cdeclblk_ptr,ax
    mov alloc_cdeclblk_ptr,ax
    mov first_pdnameblk_ptr,ax
    mov alloc_pdnameblk_ptr,ax
    mov first_lnamesblk_ptr,ax
    mov alloc_lnamesblk_ptr,ax
    mov first_segdefblk_ptr,ax
    mov alloc_segdefblk_ptr,ax
    mov first_grpblk_ptr,ax
    mov alloc_grpblk_ptr,ax
    mov alloc_segpartblk_ptr,ax
    mov alloc_relblk_ptr,ax
    mov alloc_libent_ptr,ax
;***	mov first_local_ptr,ax
    mov alloc_ovlpubblk_ptr,ax
    mov first_fixblk_ptr,ax
    mov alloc_fixblk_ptr,ax
    mov first_binblk_ptr,ax
    mov alloc_binblk_ptr,ax
    mov modend_ptr,ax
    mov pubcount,ax
    mov comcount,ax
    mov extcount,ax

; zero out hashed pointers
    push    ds
    pop es                  ; es -> warplink data
    mov cx,HASH_ARRAY_SIZE
    mov di,OFFSET DGROUP:pubdecl_hash
    xor ax,ax
    rep stosw
    mov cx,HASH_ARRAY_SIZE
    mov di,OFFSET DGROUP:segdef_hash
    xor ax,ax
    rep stosw
    mov cx,HASH_ARRAY_SIZE
    shl cx,1                ; adjust for dword entries
    mov di,OFFSET DGROUP:lnames_hash
    xor ax,ax
    rep stosw
    ret
reinit_variables    ENDP

;*****************************
;* WRITE_BINARY_FPOS         *
;*****************************

; allocate room and write dummy file position for each segment
; destroys ax,bx,cx,dx

write_binary_fpos   PROC
    call    update_ddl_size ; get start of binary file position block (EOF)
    mov WORD PTR binfpos_table,ax   ; save pointer to absolute file position
    mov WORD PTR binfpos_table+2,dx
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_binfpos,ax   ; save pointer to start within module
    mov WORD PTR mod_header.mh_binfpos+2,dx
    mov bx,current_segdef   ; get count of segments
    mov cx,bx
    add bx,3                ; round up to next para
    shr bx,1                ; /2
    shr bx,1                ; /4, dword entries converted to paragraphs
    call    alloc_memory    ; get memory for block allocation
    mov binary_fpos,ax      ; save memory segment

    shl cx,1                ; x2
    mov dx,cx               ; save words to write
    shl cx,1                ; x4, count of bytes to write
    mov bx,exe_handle
    mov ds,ax               ; ds -> binary file position storage
    xchg    dx,cx           ; dx== byte count, cx==word count
    mov es,ax
    xor di,di               ; es:di -> binary file position storage
    mov ax,di
    rep stosw               ; zero out each entry
    mov cx,dx               ; restore byte count to cx

    xor dx,dx               ; ds:dx -> binary file position storage
    mov ah,40h              ; write to device
    int 21h
    jnc wbf_ret             ; no errors
    jmp NEAR PTR dos_error  ; error writing to file

wbf_ret:
    mov ax,DGROUP
    mov ds,ax
    ret
write_binary_fpos   ENDP

;*****************************
;* WRITE_SEGDEF_BLOCK        *
;*****************************

; write all segdef blocks into one big segdef block for DDL module
; destroys ax,bx,cx,dx,si,es

write_segdef_block  PROC
    call    update_ddl_size ; get start of segdef block (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_segdef,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_segdef+2,dx
    mov ax,first_segdefblk_ptr

wsb_loop:
    or  ax,ax               ; see if valid segdef block
    jne wsb_2               ; yes

wsb_done:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    ret

wsb_2:
    mov ds,ax               ; ds -> segdef block
    mov cx,ds:[0]           ; get count of entries in block
    inc ax
    mov ds,ax               ; ds -> first entry in segdef block
    xor si,si               ; si offsets into block

    or  cx,cx               ; see if any entries in block    
    jne wsb_3               ; yes
    jmp NEAR PTR wsb_nextblk    ; no entries in block

wsb_3:
    push    cx              ; save count of entries

; translate segment name seg:off to file position
wsb_transloop:
    mov ax,ds:[si+seg_namblk_ptr]   ; get segment of segment name
    mov di,OFFSET DGROUP:ddl_symbol_lookup  ; di -> start of lookup table
    mov dx,DGROUP
    mov es,dx               ; es -> warplink data

wsb_searchloop:
    scasw                   ; search for segment entry in lookup table
    je  wsb_found           ; found the entry
    add di,4                ; bump to next lookup entry
    jmp SHORT wsb_searchloop    ; keep looking

wsb_found:
    mov ax,es:[di]          ; get low word of file position
    mov di,es:[di+2]        ; get high word

; file position corresponding to segment name in di:ax
    add ax,ds:[si+seg_nament_ptr]   ; add in segment name offset
    adc di,0                ; carry to high word
    sub ax,4                ; adjust for 2 system info words not written
    sbb di,0                ; borrow to high word

    sub ax,WORD PTR es:mod_header.mh_lnames ; make relative to start of lnames block
    sbb di,WORD PTR es:mod_header.mh_lnames+2

    mov [si+seg_nament_ptr],ax  ; save low file position
    mov [si+seg_namblk_ptr],di  ; save high file position

; translate class name seg:off to file position
    mov ax,ds:[si+class_namblk_ptr] ; get segment of segment name
    mov di,OFFSET DGROUP:ddl_symbol_lookup  ; di -> start of lookup table

wsb_search2:
    scasw                   ; search for segment entry in lookup table
    je  wsb_found2          ; found the entry
    add di,4                ; bump to next lookup entry
    jmp SHORT wsb_search2   ; keep looking

wsb_found2:
    mov ax,es:[di]          ; get low word of file position
    mov di,es:[di+2]        ; get high word

; file position corresponding to segment name in di:ax
    add ax,ds:[si+class_nament_ptr] ; add in segment name offset
    adc di,0                ; carry to high word
    sub ax,4                ; adjust for 2 system info words not written
    sbb di,0                ; borrow to high word

    sub ax,WORD PTR es:mod_header.mh_lnames ; make relative to start of lnames block
    sbb di,WORD PTR es:mod_header.mh_lnames+2

    mov [si+class_nament_ptr],ax    ; save low file position
    mov [si+class_namblk_ptr],di    ; save high file position

wsb_nextent:
    add si,32               ; move to next entry
    loop    wsb_transloop   ; loop until all entries translated

    pop ax                  ; get count of entries
    mov cl,32
    mul cl                  ; get byte count of entries
    mov cx,ax               ; cx holds bytes to write
    xor dx,dx
    mov ah,40h              ; write to device
    int 21h
    jc  wsb_error

wsb_nextblk:
    mov ax,ds               ; get -> first entry in block
    dec ax                  ; back up to -> block start
    mov ds,ax               ; ds -> block
    mov ax,ds:[2]           ; get pointer to next block, if any
    jmp NEAR PTR wsb_loop   ; loop back and write it

wsb_error:
    jmp NEAR PTR dos_error  ; error opening file

write_segdef_block  ENDP

;*****************************
;* WRITE_GRPDEF_BLOCK        *
;*****************************

; write all grpdef blocks into one big grpdef block for DDL module

write_grpdef_block  PROC
    call    update_ddl_size ; get start of segdef block (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_grpdef,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_grpdef+2,dx
    mov ax,first_grpblk_ptr

wgb_loop:
    or  ax,ax               ; see if valid segdef block
    jne wgb_2               ; yes

wgb_done:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    ret

wgb_2:
    mov ds,ax               ; ds -> grpdef block
    mov cx,ds:[0]           ; get count of entries in block
    inc ax
    mov ds,ax               ; ds -> first entry in segdef block
    xor si,si               ; si offsets into block
    jcxz    wgb_nextblk     ; no entries in block

    push    cx              ; save count of entries

; translate group name seg:off to file position
wgb_transloop:
    mov ax,ds:[si+ges_grp_namblk_ptr]   ; get segment of group name
    mov di,OFFSET DGROUP:ddl_symbol_lookup  ; di -> start of lookup table
    mov dx,DGROUP
    mov es,dx               ; es -> warplink data

wgb_searchloop:
    scasw                   ; search for segment entry in lookup table
    je  wgb_found           ; found the entry
    add di,4                ; bump to next lookup entry
    jmp SHORT wgb_searchloop    ; keep looking

wgb_found:
    mov ax,es:[di]          ; get low word of file position
    mov di,es:[di+2]        ; get high word

; file position corresponding to segment name in di:ax
    add ax,ds:[si+ges_grp_nament_ptr]   ; add in group name offset
    adc di,0                ; carry to high word
    sub ax,4                ; adjust for 2 system info words not written
    sbb di,0                ; borrow to high word

    sub ax,WORD PTR es:mod_header.mh_lnames ; make relative to start of lnames block
    sbb di,WORD PTR es:mod_header.mh_lnames+2

    mov [si+ges_grp_nament_ptr],ax  ; save low file position
    mov [si+ges_grp_namblk_ptr],di  ; save high file position

wgb_nextent:
    add si,16               ; move to next entry
    loop    wgb_transloop   ; loop until all entries translated

    pop ax                  ; get count of entries
    mov cl,16
    mul cl                  ; get byte count of entries
    mov cx,ax               ; cx holds bytes to write
    xor dx,dx
    mov ah,40h              ; write to device
    int 21h
    jc  wsb_error

wgb_nextblk:
    mov ax,ds               ; get -> first entry in block
    dec ax                  ; back up to -> block start
    mov ds,ax               ; ds -> block
    mov ax,ds:[2]           ; get pointer to next block, if any
    jmp SHORT wgb_loop      ; loop back and write it

write_grpdef_block  ENDP

;*****************************
;* WRITE_MODEND_INFO         *
;*****************************

; translate modend start address, if any, and store it in mh_startup
; destroys ax,bx,dx,di,si,es

write_modend_info   PROC
    mov ax,DGROUP
    mov es,ax
    mov di,OFFSET DGROUP:mod_header.mh_startup
    mov ds,modend_ptr
    xor si,si
    lodsb                   ; get first byte of modend record
    and al,40h              ; see if contains start address
    je  wmi_ret             ; no

    or  BYTE PTR es:mod_header.mh_flags,1   ; set main module flag
    movsb                   ; transfer end dat field

    xor ah,ah
    lodsb                   ; get first byte of frame datum
    test    al,80h          ; see if two byte index
    je  wmi_2               ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte
    lodsb                   ; get frame datum/index second byte

wmi_2:
    stosw                   ; save frame datum

    xor ah,ah
    lodsb                   ; get first byte of target datum
    test    al,80h          ; see if two byte index
    je  wmi_3               ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte
    lodsb                   ; get target datum/index second byte

wmi_3:
    stosw                   ; save target datum
    movsw                   ; transfer target displacement

wmi_ret:
    mov ax,DGROUP
    mov ds,ax
    ret
write_modend_info   ENDP

;*****************************
;* WRITE_BINARY_BLOCK        *
;*****************************

; translate and write binary/fixup data
; destroys ax,bx,cx,dx,di,si,es

write_binary_block  PROC
    call    update_ddl_size ; get start of binary blocks (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_binary,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_binary+2,dx

    cmp first_binblk_ptr,0  ; see if first binary block exists
    jne wbb_2               ; yes
    ret                     ; no binary data in this module

wbb_2:
    push    bp              ; save critical register
    mov proc_segment,1      ; init number of segment being processed for binary data
    mov ax,first_segdefblk_ptr  ; get pointer to first segdef block

wbb_blkloop:
    mov bp,ax               ; save -> block
    inc ax                  ; ax -> first entry in block

wbb_entloop:
    mov es,ax               ; es -> entry in segdef block
    mov curr_segdefent_ptr,ax   ; save -> to current segdef entry
    push    es              ; save es -> segdef entry
    mov al,es:[sdes_acbp_byte]  ; get acbp byte
    and al,0e0h             ; get align field
    je  to_wbb_zeroseg      ; absolute segment, no bytes to save
    mov es,es:[seg_partent_firstptr]    ; es -> first segment partition entry
    mov al,es:[spes_acbp_byte]  ; get acbp byte
    and ax,2                ; get Big bit (zero high byte)
    or  ax,es:[spes_part_len]   ; merge in length of segment
    jne wbb_nonzero         ; nonzero length segment

to_wbb_zeroseg:
    jmp NEAR PTR wbb_zeroseg    ; zero, no data to write

; segment has nonzero length, check the binary blocks and see if any data was
; specified for the segment
wbb_nonzero:
    mov ax,es:[spes_part_len]
    mov seg_truelen,ax      ; save length of overlaid segment
    mov ax,buffer_base      ; ax -> i/o buffer
    sub ax,0fffh            ; ax -> 64K-16 area below i/o buffer (work i/o buffer)
    cmp ax,allocation_base  ; make sure haven't rammed into allocations building up
    jae wbb_workbuff        ; no

    mov ax,8                ; force out of memory error
    jmp NEAR PTR dos_error

wbb_workbuff:
    mov es,ax               ; es -> work i/o buffer
    xor ax,ax
    mov di,ax               ; offset within i/o buffer
    mov si,OFFSET DGROUP:bin_header
    stosw                   ; init flags
    mov ds:[si],ax          ; init for binary header structure too
    stosw
    mov ds:[si+2],ax
    mov ax,proc_segment     ; get segment number
    stosw                   ; init segment number
    mov ds:[si+4],ax
    xor ax,ax
    mov cx,7                ; zero init next 7 words (word and 3 dwords)
    rep stosw
    mov ds:[si+6],ax
    mov ds:[si+8],ax
    mov ds:[si+10],ax
    mov ds:[si+12],ax
    mov ds:[si+14],ax
    mov ds:[si+16],ax
    mov ds:[si+18],ax

    mov ax,first_binblk_ptr ; get pointer to first binary block

wbb_searchloop:
    mov si,12               ; si-> start of L?DATA record image
    mov ds,ax               ; ds -> binary block
    mov cx,ds:[3]           ; get record length-checksum
    lodsb                   ; get segment index of block
    dec cx                  ; adjust for first segment index byte
    test    al,80h          ; see if high bit set
    je  wbb_3               ; no
    lodsb                   ; get true index number
    dec cx                  ; adjust for second segment index byte

wbb_3:
    mov ds:[7],al           ; save segment index
    mov ds:[8],si           ; save start of iterated or enumerated data block-2
    add WORD PTR ds:[8],2   ; adjust for data offset
    mov ds:[10],cx          ; save record length not counting segment index
    cmp al,BYTE PTR es:[bhs_segind] ; see if segment indices match
    jne wbb_nextblk         ; no

; this block of binary data belongs to this segment
    mov al,ds:[0]           ; get type of record (LEDATA or LIDATA)
    cmp al,LEDATA           ; see if LEDATA
    jne wbb_lidata          ; no, LIDATA

    call    ddl_proc_ledata ; process LEDATA record
    jmp SHORT wbb_nextblk

; zero length segment, or no initialized data
wbb_zeroseg:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    mov di,proc_segment     ; get segment being processed
    dec di                  ; make relative zero
    shl di,1                ; convert to dword offset
    shl di,1
    mov es,binary_fpos
    xor ax,ax
    stosw
    stosw                   ; zero binary file position entry in table to show no init'ed data
    jmp SHORT wbb_nextseg   ; try next segment

wbb_lidata:
    call    ddl_proc_lidata ; process LIDATA record

; move to next binary block, if any
wbb_nextblk:
    mov ax,ds:[1]           ; get pointer to next block
    or  ax,ax               ; see if exists
    jne wbb_searchloop      ; yes

    mov cx,es:[bhs_length]  ; get length of init'ed binary data
    jcxz    wbb_zeroseg     ; zero, ignore

    mov ax,DGROUP
    mov ds,ax
    mov si,es:[bhs_offset]
    mov ds:[bin_header.bhs_offset],si   ; save offset of binary data
    mov ds:[bin_header.bhs_length],cx ; save length of binary data

    mov ax,seg_truelen
    mov ds:[bin_header.bhs_truelen],ax  ; save true length of segment (for DDL overlay manager)

    add cx,20               ; add in system words
    mov si,cx
    call    update_ddl_size ; get start of binary block (EOF)
    mov cx,si               ; restore byte count
    mov WORD PTR currbin_start,ax   ; save pointer to start
    mov WORD PTR currbin_start+2,dx
    mov si,proc_segment     ; get segment being processed
    dec si                  ; make relative zero
    shl si,1                ; convert to dword offset
    shl si,1
    mov ds,binary_fpos
    mov ds:[si],ax          ; save start of binary block to file position table
    mov ds:[si+2],dx
    push    es
    pop ds
    xor dx,dx               ; ds:dx -> i/o buffer
    mov ah,40h              ; write to file (bx already == handle)
    int 21h
    jc  wbb_error           ; error writing to file

; check if fixups/write them if they exist, update binary file position info
    call    ddl_proc_fixupp ; process FIXUPP records for segment, if any

wbb_nextent:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

wbb_nextseg:
    pop es                  ; restore es -> segdef entry
    mov ax,proc_segment     ; get number of segment last processed
    inc ax                  ; bump to next
    cmp ax,current_segdef   ; see if past number of segments
    ja  wbb_ret             ; yes
    mov proc_segment,ax     ; update segment being processed
    mov ax,es               ; ax -> current segdef entry
    inc ax                  ; move to next entry in block
    inc ax                  ; 32 bytes/entry
    mov bx,ax
    sub bx,bp               ; get difference between new entry and start of block
    cmp bx,SEG_DEFBLK_SIZE  ; see if all entries in block used
    jae wbb_getnext         ; yes
    jmp NEAR PTR wbb_entloop    ; no, keep pulling them

wbb_getnext:
    mov es,bp               ; es -> block
    mov ax,es:[2]           ; get -> next block
    jmp NEAR PTR wbb_blkloop; process it

wbb_ret:
    pop bp                  ; restore critical register
    ret

wbb_error:
    jmp NEAR PTR dos_error  ; error opening file

write_binary_block  ENDP

;*****************************
;* DDL_PROC_FIXUPP           *
;*****************************

; see if fixup records for current segment, process them if so
; upon entry es -> i/o buffer
; destroys ax,bx,cx,dx,di,si,es,ds

ddl_proc_fixupp PROC
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    xor ax,ax
    mov lobyte_count,ax     ; zero counters
    mov near_count,ax
    mov far_count,ax
    call    update_ddl_size ; get start of fixupp block (EOF)
    mov WORD PTR bin_header.bhs_fixptr,ax   ; update fixup block file position
    mov WORD PTR bin_header.bhs_fixptr+2,dx
    mov cx,proc_segment     ; get segment index currently being processed

    mov ax,first_fixblk_ptr
    or  ax,ax               ; see if any fixups
    jne dpf_2               ; yes

; update binary header, seek back to end of file
dpf_update:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    call    update_ddl_size ; get next binary file position
    mov WORD PTR bin_header.bhs_nextptr,ax  ; update next binary block file position
    mov WORD PTR bin_header.bhs_nextptr+2,dx

    mov es,curr_segdefent_ptr   ; es -> current segdef entry
    mov al,es:[27]          ; save overlay flag byte
    mov BYTE PTR bin_header.bhs_flags,al

    mov ax,lobyte_count
    or  ax,near_count
    or  ax,far_count        ; see if any fixups
    jne dpf_hdrseek         ; yes
    mov WORD PTR bin_header.bhs_fixptr,ax   ; zero out fixup pointer
    mov WORD PTR bin_header.bhs_fixptr+2,ax
    mov ax,seg_truelen
    mov bin_header.bhs_truelen,ax   ; save true length of segment (for DDL overlay manager)

; seek back to binary header
dpf_hdrseek:
    mov dx,WORD PTR currbin_start
    mov cx,WORD PTR currbin_start+2
    mov ax,4200h            ; move file pointer, offset from start
    int 21h

; write updated binary header
    mov cx,BIN_HEADER_SIZE  ; size of binary header
    mov dx,OFFSET DGROUP:bin_header ; ds:dx -> source
    mov ah,40h              ; write to file (bx already == handle)
    int 21h
    jc  wbb_error           ; error writing to file

    call    update_ddl_size ; seek to end of file
    ret

; update binary header info
dpf_2:
    xor di,di               ; zero offset in buffer
    mov dx,ax               ; save -> first fixup block

; init fixup file position info to start of fixup block
    mov ax,WORD PTR bin_header.bhs_fixptr
    mov WORD PTR es:[fhs_lowinfo],ax
    mov WORD PTR es:[fhs_lowloc],ax
    mov WORD PTR es:[fhs_nearinfo],ax
    mov WORD PTR es:[fhs_nearloc],ax
    mov WORD PTR es:[fhs_farinfo],ax
    mov WORD PTR es:[fhs_farloc],ax
    mov ax,WORD PTR bin_header.bhs_fixptr+2
    mov WORD PTR es:[fhs_lowinfo+2],ax
    mov WORD PTR es:[fhs_lowloc+2],ax
    mov WORD PTR es:[fhs_nearinfo+2],ax
    mov WORD PTR es:[fhs_nearloc+2],ax
    mov WORD PTR es:[fhs_farinfo+2],ax
    mov WORD PTR es:[fhs_farloc+2],ax
    xor ax,ax               ; zero init counts
    mov es:[fhs_lowcount],ax
    mov es:[fhs_nearcount],ax
    mov es:[fhs_farcount],ax

    mov di,30               ; 3 sets of word/dword/dword
    mov ch,6                ; six fixup passes needed; info for low-byte, near, far
                            ; and location for low-byte, near, and far
    mov ax,dx               ; restore ax -> first fixup block

dpf_typeloop:
    mov ds,ax               ; ds -> fixup block
    mov dx,ax               ; save -> fixup block
    mov ax,ds:[2]           ; get owning L?DATA block, if any
    or  ax,ax
    je  dpf_nextblk         ; no owner (thread fixup)
    mov ds,ax               ; ds -> owning L?DATA block
    cmp cl,ds:[7]           ; see if segment index matches current segment
    jne dpf_nextblk         ; no

; this fixup block goes with the current segment, save appropriate fixups
    mov cl,ds:[0]           ; get LEDATA/LIDATA fixup status
    mov bx,ds:[5]           ; get data offset
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    cmp ch,4                ; see if info pass
    jb  dpf_3               ; no

; info pass, keep fixup information, no discrimination between LIDATA and LEDATA
    call    save_fixup_info
    jmp SHORT dpf_nextblk

dpf_3:
    call    lxdata_fixupp   ; save fixups for LEDATA/LIDATA

dpf_nextblk:
    mov ax,DGROUP
    mov ds,ax
    mov cl,BYTE PTR proc_segment    ; get segment index currently being processed in cl

    mov ds,dx               ; ds -> fixup block
    mov ax,ds:[0]           ; get -> next block
    or  ax,ax               ; see if valid
    jne dpf_typeloop        ; yes

; all through fixups this pass
; update appropriate fixup info
    mov ax,DGROUP
    mov ds,ax
    cmp ch,6                ; see if low-byte info
    jne dpf_ni              ; no
    mov ax,lobyte_count     ; update low-byte fixup info
    mov es:[fhs_lowcount],ax
    add WORD PTR es:[fhs_lowinfo],30    ; update low-byte location
    adc WORD PTR es:[fhs_lowinfo+2],0   ; carry to high word
    add WORD PTR es:[fhs_nearinfo],di   ; update next (near fixup) info
    adc WORD PTR es:[fhs_nearinfo+2],0  ; carry to high word
    jmp SHORT dpf_passchk

dpf_ni:
    cmp ch,5                ; see if near info
    jne dpf_fi              ; no
    mov ax,near_count       ; update near fixup info
    mov es:[fhs_nearcount],ax
    add WORD PTR es:[fhs_farinfo],di    ; update start of next field
    adc WORD PTR es:[fhs_farinfo+2],0   ; carry to high word
    jmp SHORT dpf_passchk

dpf_fi:
    cmp ch,4                ; see if far info
    jne dpf_ll              ; no
    mov ax,far_count        ; update far fixup info
    mov es:[fhs_farcount],ax
    add WORD PTR es:[fhs_lowloc],di ; update start of next field
    adc WORD PTR es:[fhs_lowloc+2],0    ; carry to high word
    jmp SHORT dpf_passchk

dpf_ll:
    cmp ch,3                ; see if low-byte location
    jne dpf_nl              ; no
    add WORD PTR es:[fhs_nearloc],di    ; update start of next field
    adc WORD PTR es:[fhs_nearloc+2],0   ; carry to high word
    jmp SHORT dpf_passchk

dpf_nl:
    cmp ch,2                ; see if near location
    jne dpf_passchk         ; no
    add WORD PTR es:[fhs_farloc],di ; update start of next field
    adc WORD PTR es:[fhs_farloc+2],0    ; carry to high word

dpf_passchk:
    mov ax,first_fixblk_ptr ; reset pointer to start of fixup block
    dec ch                  ; see if done with all passes
    je  dpf_write           ; done
    jmp NEAR PTR dpf_typeloop   ; more fixup passes to perform

; write the saved fixups
dpf_write:
    mov bx,exe_handle
    push    es
    pop ds                  ; ds -> i/o buffer
    mov cx,di               ; get count of bytes to write
    cmp cx,30               ; see if only fixup header (no fixups)
    jbe dbf_to_update       ; yes, don't bother writing empty header
    xor dx,dx
    mov ah,40h              ; write to device
    int 21h
    jc  dpf_error

dbf_to_update:
    jmp NEAR PTR dpf_update ; update binary header with new info

dpf_error:
    jmp NEAR PTR dos_error  ; error writing to file

ddl_proc_fixupp ENDP

;*****************************
;* SAVE_FIXUP_INFO           *
;*****************************

; save fixup information from FIXUPP record
; upon entry ch holds pass number(4-6), dx-> fixup block
; cl holds L?DATA record type
; es:di -> destination buffer
; destroys ax,bx,si,ds

save_fixup_info PROC
    push    dx
    push    bp
    push    cx
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    mov bl,ch               ; save pass number in bl

; get the state of FIXUPP thread fields when using this fixupp record
    mov ax,es
    push    ds
    pop es                  ; es -> warplink data
    mov ds,dx               ; ds -> fixupp record
    push    di              ; save position in i/o buffer
    mov si,6                ; ds:si -> saved thread method and index info
    mov di,OFFSET DGROUP:frame_thrd_meth
    movsw                   ; get four frame thread method bytes
    movsw
    mov di,OFFSET DGROUP:target_thrd_meth
    movsw                   ; get four target thread method bytes
    movsw
    mov cx,4                ; four index words
    mov di,OFFSET DGROUP:frame_thrd_index
    rep movsw               ; get frame thread index
    mov cx,4                ; four index words
    mov di,OFFSET DGROUP:target_thrd_index
    rep movsw               ; get target thread index
    pop di                  ; restore position in i/o buffer
    mov es,ax               ; es -> i/o buffer

    mov cx,ds:[4]           ; get record length

sfi_loop:
    mov dx,ds               ; save -> fixupp record
    lodsb                   ; get first byte of thread or fixup field
    dec cx                  ; decrement record length to parse
    test    al,80h          ; fixup field if high bit set
    jne sfi_fix             ; fixup field

; thread field, parse it out
    call    ddl_save_thread
    jmp NEAR PTR sfi_chkcheck   ; check if at checksum byte

; fixup field
sfi_fix:
    mov BYTE PTR es:[di],0  ; zero out modified fixdat field
    mov bp,DGROUP
    mov ds,bp               ; ds -> warplink data
    mov bh,al
    and bh,1ch              ; get loc field

; bh holds loc field ==0 if low-order, ==4 if offset, ==8 if segment
; == 0ch if seg:off, ==14h if offset, others invalid
    jne sfi_notlow          ; loc field nonzero

; low-order byte fixup
    cmp bl,6                ; see if low-order fixup pass
    jne sfi_cont            ; no, leave bh==0 to flag not to save fixup
    inc bh                  ; bh!=0 flags save fixup
    inc lobyte_count        ; bump count of low-byte fixups
    jmp SHORT sfi_cont

sfi_notlow:
    cmp bh,4                ; see if offset
    je  sfi_offset          ; yes
    cmp bh,14h              ; see if offset
    je  sfi_offset          ; yes
    cmp bh,0ch              ; see if potentially offset
    jne sfi_segment         ; no

; pointer fixup, only keep as offset if pass 5 (near pass)
    cmp bl,5                ; see if near fixup pass
    jne sfi_segment         ; no

sfi_offset:
    xor bh,bh               ; init save fixup flag
    cmp bl,5                ; see if near fixup pass
    jne sfi_cont            ; no
    inc bh                  ; flag save fixup
    inc near_count          ; bump count of near fixups
    jmp SHORT sfi_cont

sfi_segment:
    cmp bh,8                ; see if segment
    je  sfi_far             ; yes
    cmp bh,0ch              ; see if potentially segment
    jne sfi_nosave          ; no

; pointer fixup, only keep as segment if pass 4 (far pass)
    cmp bl,4                ; see if far fixup pass
    jne sfi_nosave          ; no
    jmp SHORT sfi_pointer   ; keep bh at 0ch to flag segment portion of pointer fixup

sfi_far:
    xor bh,bh               ; init save fixup flag
    cmp bl,4                ; see if far fixup pass
    jne sfi_cont            ; no
    inc bh                  ; flag save fixup

sfi_pointer:
    inc far_count           ; bump count of far fixups
    jmp SHORT sfi_cont

sfi_nosave:
    xor bh,bh               ; flag no save of fixup

sfi_cont:
    mov save_fixup,bh       ; set status of save fixup flag
    mov bh,al               ; get locat low-byte
    and bh,40h              ; get M-bit value
    shl bh,1                ; shift to MSB
    mov es:[di],bh          ; save modified fix dat MSB

    mov ah,al               ; save first byte of locat field
    mov ds,dx               ; ds -> fixupp record
    lodsb                   ; get second byte of locat field
    mov bp,DGROUP
    mov ds,bp               ; ds -> warplink data
    dec cx                  ; decrement record length to parse
    and ah,3                ; ax holds data record offset
    cmp save_fixup,0ch      ; see if segment part of pointer fixup
    jne sfi_notpoint        ; no
    add ax,2                ; bump 2 past offset to segment portion

sfi_notpoint:
    mov data_rec_off,ax     ; save data record offset for LIDATA fixup
    mov ds,dx               ; ds -> fixupp record
    lodsb                   ; get fix dat field
    dec cx
    mov dl,al               ; save fix dat value

    test    al,80h          ; check if thread field for frame (fbit)
    je  sfi_notframe        ; no

; thread field for frame
    shr al,1                ; make frame field value relative zero
    shr al,1
    shr al,1
    shr al,1
    and al,7                ; mask off F bit

    mov bp,ds               ; save -> fixupp record
    mov bx,DGROUP
    mov ds,bx               ; ds -> warplink data

    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:frame_thrd_meth    ; bx -> frame thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get frame method
    shl bl,1                ; shift to frame field
    shl bl,1
    shl bl,1
    shl bl,1
    and dl,0fh              ; mask off F-bit, frame bits
    or  dl,bl               ; merge in frame bits from thread

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:frame_thrd_index   ; bx -> frame thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get frame index
    mov es:[di+1],ax        ; save frame index

    mov ds,bp               ; restore ds -> fixupp record
    jmp SHORT sfi_targ

; no thread field for frame
sfi_notframe:
    mov WORD PTR es:[di+1],0    ; zero init frame datum field (frame index)
    and al,70h              ; get frame field
    cmp al,20h              ; see if frame datum/index exists
    ja  sfi_targ            ; no

; frame datum/index exists
    xor ah,ah               ; zap high byte
    lodsb                   ; get frame datum/index first byte
    dec cx                  ; decrement record length to parse
    cmp al,80h              ; see if two byte field
    jb  sfi_frame           ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte
    lodsb                   ; scan past frame datum/index second byte
    dec cx

sfi_frame:
    mov es:[di+1],ax        ; save frame datum/index

sfi_targ:
    mov WORD PTR es:[di+3],0    ; zero init target datum field (target index)
    mov al,dl               ; get fix dat field
    test    al,8            ; check if thread field for target (T bit)
    jne sfi_thrdtarg        ; yes

; no thread field for target
    xor ah,ah               ; zap high byte
    lodsb                   ; get target datum first byte
    dec cx
    cmp al,80h              ; see if two byte field
    jb  sfi_savetarg        ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte
    lodsb                   ; get target datum second byte
    dec cx

sfi_savetarg:
    mov bp,ds               ; bp -> fixupp record
    jmp SHORT sfi_2

; thread field for target
sfi_thrdtarg:
    mov bp,ds               ; save -> fixupp record
    mov bx,DGROUP
    mov ds,bx               ; ds -> warplink data

    and al,3                ; get targt field value in al
    xor ah,ah               ; zap high byte
    mov bx,OFFSET DGROUP:target_thrd_meth   ; bx -> target thread method array base
    add bx,ax               ; bx -> proper byte array element
    mov bl,[bx]             ; get target method
    and dl,0fch             ; mask off targt bits
    or  dl,bl               ; merge in target bits from thread

    shl ax,1                ; convert to word offset
    mov bx,OFFSET DGROUP:target_thrd_index  ; bx -> target thread index array base
    add bx,ax               ; bx -> proper word array element
    mov ax,[bx]             ; get target index

sfi_2:
    mov es:[di+3],al        ; save target index low
    shl ah,1                ; merge in target index high
    shl ah,1
    shl ah,1
    shl ah,1                ; high 4 bits
    or  es:[di+2],ah

    mov ds,bp               ; restore ds -> fixupp record

sfi_pbit:
    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    jne sfi_savefix         ; set, no target displacement

; P bit is zero, scan past target displacement field
    inc si                  ; gobble target displacement word
    inc si
    dec cx
    dec cx

; check if should save fixup, if so, just increment di+5 so no overwrite next fixup
sfi_savefix:
    mov bp,ds
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    cmp save_fixup,0        ; see if should save fixup
    je  sfi_flagoff         ; no
    or  es:[di],dl          ; merge in updated fix dat (save M bit status)
    pop ax                  ; get cx entry value in ax (al holds L?DATA record type)
    push    ax              ; restore to stack
    cmp al,LEDATA           ; see if LEDATA
    je  sif_nextslot        ; yes

; LIDATA record, multiple fixups
    call    lidata_multfix_info ; need to save info on multiple fixups
    jmp SHORT sfi_flagoff

sif_nextslot:
    add di,4
    jc  sfi_memerr          ; buffer overflow

sfi_flagoff:
    mov ds,bp               ; restore ds -> fixupp record
    pop bx                  ; get cx entry value in bx, bh holds pass number
    push    bx              ; restore to stack
    mov bl,bh               ; get pass number in bl

; see if at checksum
sfi_chkcheck:
    cmp cx,1                ; check if at checksum byte
    jbe sfi_out             ; yes
    jmp NEAR PTR sfi_loop   ; no, loop back for next fixup/thread field

sfi_out:
    pop cx
    pop bp
    pop dx
    ret

sfi_memerr:
    mov ax,8                ; force out of memory error
    jmp NEAR PTR dos_error

save_fixup_info ENDP

;*****************************
;* LIDATA_MULTFIX_INFO       *
;*****************************

; save info for fixups occurring multiple times in LIDATA record
; upon entry es:di -> fixup info storage, currently at 4-byte entry
; to save as many times as fixup occurs
; ah holds information pass (4-6, low-byte, near, or far)
; ds -> DGROUP
; bp -> fixupp record
; data_rec_off has data record offset
; destroys ax,bx,dx

lidata_multfix_info PROC
    push    cx
    push    si
    push    ds
    cmp ah,6                ; see if low-byte info pass
    jne lmi_notlow          ; no
    dec lobyte_count        ; drop count of low-byte fixups incremented during LEDATA shared code
    jmp SHORT lmi_2

lmi_notlow:
    cmp ah,5                ; see if near info pass
    jne lmi_far             ; no
    dec near_count          ; drop count of near fixups incremented during LEDATA shared code
    jmp SHORT lmi_2

lmi_far:
    dec far_count           ; drop count of far fixups incremented during LEDATA shared code

lmi_2:
    mov pass_count,ah       ; save pass count information
    mov ds,bp               ; ds -> fixupp record
    mov ds,ds:[2]           ; ds -> owner LIDATA record
    push    ds              ; save -> LIDATA record
    mov bx,ds:[5]           ; get iterated data offset
    mov ax,ds:[8]           ; get start of iterated data block
    mov cx,ds:[10]          ; get length of record after segment index byte
    sub cx,2                ; adjust for data offset word
    mov dx,DGROUP
    mov ds,dx               ; ds -> warplink data
    add data_rec_off,ax     ; add start of iterated data block to data record offset
    mov si,ax               ; start scanning at iterated data block
    mov iter_data_off,bx    ; save iterated data offset
    mov is_loc,0            ; zero location parsing flag

    mov al,es:[di]          ; get fixdat value
    mov fixdat,al           ; save it
    mov ax,es:[di+1]        ; get frame datum

    and ah,0fh              ; mask off high bits of target
    mov frame_datum,ax      ; save it

    mov al,es:[di+3]        ; get target datum low byte
    mov ah,es:[di+2]        ; get high byte
    shr ah,1
    shr ah,1
    shr ah,1
    shr ah,1                ; convert 4 MSB to proper value
    mov target_datum,ax     ; save it

    pop ds                  ; ds -> LIDATA record

lmi_3:
    jcxz    lmi_ret         ; no more fixups
    call    fixup_recurse   ; save fixup info for LIDATA record
    jmp SHORT lmi_3         ; loop until no more bytes

lmi_ret:
    pop ds
    pop si
    pop cx
    ret
lidata_multfix_info ENDP

;*****************************
;* LIDATA_MULTFIX_LOC        *
;*****************************

; save location of fixups occurring multiple times in LIDATA record
; upon entry es:di -> fixup info storage
; ds -> fixup record
; ax == data record offset + LIDATA record (iterated) offset
; bp == LIDATA data offset
; destroys ax,bx,dx

lidata_multfix_loc  PROC
    push    cx
    push    si
    push    bp
    push    ds

    sub ax,bp               ; get data record offset in ax
    mov bp,ax               ; save data record offset in bp

    mov ds,ds:[2]           ; ds -> owner LIDATA record
    push    ds              ; save -> LIDATA record
    mov bx,ds:[5]           ; get iterated data offset
    mov ax,ds:[8]           ; get start of iterated data block
    mov cx,ds:[10]          ; get length of record after segment index byte
    sub cx,2                ; adjust for data offset word
    mov dx,DGROUP
    mov ds,dx               ; ds -> warplink data
    add bp,ax               ; add start of iterated data block to data record offset
    mov data_rec_off,bp     ; save it
    mov si,ax               ; start scanning at iterated data block
    mov iter_data_off,bx    ; save iterated data offset
    mov is_loc,1            ; flag fixup location parsing

    pop ds                  ; ds -> LIDATA record

lml_2:
    jcxz    lml_ret         ; no more fixups
    call    fixup_recurse   ; save fixup info for LIDATA record
    jmp SHORT lml_2         ; loop until no more bytes

lml_ret:
    pop ds
    pop bp
    pop si
    pop cx
    ret
lidata_multfix_loc  ENDP

;*****************************
;* FIXUP_RECURSE             *
;*****************************

; use recursion to get all fixups out of lidata record
; upon entry ds:[si] -> current data byte (initially iterated data block),
; cx == size of data block,
; es:[di] -> destination
; destroys ax,bx,cx,dx

fixup_recurse   PROC
    lodsw                   ; get repeat count
    dec cx                  ; decrement record length to parse
    dec cx
    mov bx,ax               ; save repeat count

    lodsw                   ; get block count
    dec cx                  ; decrement record length to parse
    dec cx
    mov dx,ax               ; save block count

    push    si              ; save buffer position
    push    cx              ; save record iterated data block length

fxr_reploop:
    pop cx                  ; restore record length
    pop si                  ; restore buffer position
    push    si              ; restore values to stack
    push    cx

    or  dx,dx               ; see if nested iterated data blocks
    je  fxr_data            ; no

; nested iterated data blocks
    mov ax,dx               ; get total number of blocks to loop through
fxr_blkloop:
    push    ax              ; save current number of blocks left to loop thru
    push    bx              ; save repeat value
    push    dx              ; save block value
    call    fixup_recurse   ; nest down one level
    pop dx                  ; restore block value
    pop bx                  ; restore repeat value
    pop ax                  ; restore number of blocks left to loop thru
    dec ax                  ; one iteration completed
    jne fxr_blkloop         ; loop is not complete, using Z flag from previous decrement
    jmp SHORT fxr_end_reploop   ; loop is complete, end this iteration of the repeat loop

; block size is zero, data follows
fxr_data:
    lodsb                   ; get length of data to write
    dec cx                  ; decrement record length to parse

; write the data bytes
    push    cx              ; save record's total data length (in case of repeat loop termination)
    mov cl,al               ; get data bytes to write in cl
    xor ch,ch               ; zap high byte
    push    cx              ; save data bytes written

    push    ds              ; save ds -> lidata record
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data

fxr_loop:
    cmp si,data_rec_off     ; see if matches fixup location
    jne fxr_cont            ; no

    cmp is_loc,0            ; see if parsing fixup for location
    jne fxr_loc             ; yes

    mov al,fixdat           ; save fixup info
    stosb
    mov ax,frame_datum
    stosw

    mov al,BYTE PTR target_datum+1
    shl al,1
    shl al,1
    shl al,1
    shl al,1                ; convert to high bits
    or  es:[di-1],al        ; merge into frame datum high byte

    mov al,BYTE PTR target_datum    ; save low byte of target datum
    stosb

    cmp pass_count,6        ; see if low-byte info pass
    jne fxr_notlow          ; no
    inc lobyte_count        ; bump count of low-byte fixups
    jmp SHORT fxr_cont

fxr_notlow:
    cmp pass_count,5        ; see if near info pass
    jne fxr_far             ; no
    inc near_count          ; bump count of near fixups
    jmp SHORT fxr_cont

fxr_far:
    inc far_count           ; bump count of far fixups
    jmp SHORT fxr_cont

fxr_loc:
    mov ax,iter_data_off    ; get current location of fixup
    stosw

fxr_cont:
    inc si                  ; move to next byte of record
    inc iter_data_off       ; bump location
    dec cx                  ; drop count of data bytes to parse
    jne fxr_loop            ; not done yet

    pop ds                  ; restore ds -> lidata record

    pop ax                  ; get data bytes written
    pop cx                  ; restore record's total data length (in case of repeat loop termination)
    sub cx,ax               ; subtract off data bytes written

fxr_end_reploop:
    dec bx                  ; an iteration of the repeat loop was completed
    je  fxr_end             ; repeat loop completed
    jmp NEAR PTR fxr_reploop    ; not complete, go back for next iteration

fxr_end:
    add sp,4                ; trash old record length and buffer position values on stack
    ret                     ; repeat loop complete, return to next highest level or proc2_lidata

fxr_memerr:
    mov ax,8                ; force out of memory error

fxr_error:
    jmp NEAR PTR dos_error

fixup_recurse   ENDP

;*****************************
;* LXDATA_FIXUPP             *
;*****************************

; save fixups for L?DATA record
; upon entry ch holds pass number(1-3), dx -> fixup block
; es:di -> destination buffer,
; cl holds L?DATA record type
; bx holds L?DATA data offset
; destroys ax,bx,si,ds

lxdata_fixupp   PROC
    push    dx
    push    bp
    push    cx
    mov ds,dx
    mov si,30               ; ds:si -> fixupp record
    mov bp,bx               ; save data offset in bp
    mov bl,ch               ; save pass number in bl
    mov cx,ds:[4]           ; get record length

lef_loop:
    lodsb                   ; get first byte of thread or fixup field
    dec cx                  ; decrement record length to parse
    test    al,80h          ; fixup field if high bit set
    jne lef_fix             ; fixup field

; thread field, parse it out
    call    ddl_save_thread
    jmp NEAR PTR lef_chkcheck   ; check if at checksum byte

; fixup field, scan past it
lef_fix:
    push    ds
    mov dx,DGROUP
    mov ds,dx               ; ds -> warplink data
    mov ah,al               ; save low byte of locat field
    and al,1ch              ; get loc field

; al holds loc field ==0 if low-order, ==4 if offset, ==8 if segment
; == 0ch if seg:off, ==14h if offset, others invalid
    jne lef_notlow          ; loc field nonzero

; low-order byte fixup
    cmp bl,3                ; see if low-order fixup pass
    jne lef_cont            ; no, leave bh==0 to flag not to save fixup
    inc al                  ; al!=0 flags save fixup
    jmp SHORT lef_cont

lef_notlow:
    cmp al,4                ; see if offset
    je  lef_offset          ; yes
    cmp al,14h              ; see if offset
    je  lef_offset          ; yes
    cmp al,0ch              ; see if potentially offset
    jne lef_segment         ; no

; pointer fixup, only keep as offset if pass 2 (near pass)
    cmp bl,2                ; see if near fixup pass
    jne lef_segment         ; no

lef_offset:
    xor al,al               ; init save fixup flag
    cmp bl,2                ; see if near fixup pass
    jne lef_cont            ; no
    inc al                  ; flag save fixup
    jmp SHORT lef_cont

lef_segment:
    cmp al,8                ; see if segment
    je  lef_far             ; yes
    cmp al,0ch              ; see if potentially segment
    jne lef_nosave          ; no

; pointer fixup, only keep as segment if pass 1 (far pass)
    cmp bl,1                ; see if far fixup pass
    jne lef_nosave          ; no
    jmp SHORT lef_cont      ; keep flag as 0ch to show segment in pointer location (offset 2)

lef_far:
    xor al,al               ; init save fixup flag
    cmp bl,1                ; see if far fixup pass
    jne lef_cont            ; no
    inc al                  ; flag save fixup
    jmp SHORT lef_cont

lef_nosave:
    xor al,al               ; flag no save of fixup

lef_cont:
    mov dl,al               ; set status of save fixup flag
    pop ds                  ; ds -> fixupp record
    lodsb                   ; get second byte of locat field
    dec cx                  ; decrement record length to parse

    and ah,3                ; ax holds data record offset
    add ax,bp               ; add in L?DATA data offset
    or  dl,dl               ; see if should save location
    je  lef_scan            ; no
    cmp dl,0ch              ; see if segment part of pointer fixup
    jne lef_saveit          ; no
    add ax,2                ; bump 2 past offset to segment portion

lef_saveit:
    pop dx                  ; get cx entry value in dx (dl holds L?DATA record type)
    push    dx              ; restore to stack
    cmp dl,LEDATA           ; see if LEDATA
    je  lef_lesave          ; yes

; LIDATA record, multiple fixups
    call    lidata_multfix_loc  ; need to save location of multiple fixups
    jmp SHORT lef_scan

lef_lesave:
    stosw                   ; save location

lef_scan:
    call    scan_to_targ_disp   ; scan to target displacement field, if any

    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    jne lef_nodisp          ; set, no target displacement

; P bit is zero, scan past target displacement field
    inc si                  ; gobble target displacement word
    inc si
    dec cx
    dec cx

lef_nodisp:
    pop bx                  ; get cx entry value in bx, bh holds pass number
    push    bx              ; restore to stack
    mov bl,bh               ; get pass number in bl

; see if at checksum
lef_chkcheck:
    cmp cx,1                ; check if at checksum byte
    jbe lef_ret             ; yes
    jmp NEAR PTR lef_loop   ; no, loop back for next fixup/thread field

lef_ret:
    pop cx
    pop bp
    pop dx
    ret
lxdata_fixupp   ENDP

;*****************************
;* DDL_PROC_LEDATA           *
;*****************************

; process a LEDATA record
; upon entry ds:si -> enumerated data offset of LEDATA record image,
; es:di -> current i/o buffer position, cx holds data bytes+2 (enumerated data offset)
; updates di,si
; destroys ax,bx,cx,dx

ddl_proc_ledata PROC
    lodsw                   ; get enumerated data offset
    dec cx
    dec cx                  ; adjust for enumerated data offset bytes
    push    es:[bhs_length] ; save old length

    cmp ax,es:[bhs_length]  ; see if matches current length of segment
    je  dple_2              ; yes
    ja  dple_fill           ; no, fill

; offset is LESS than current segment length, reduce current segment length
    mov dx,es:[bhs_length]
    sub dx,ax
    sub di,dx               ; back up i/o buffer position
    mov es:[bhs_length],ax
    jmp SHORT dple_2

; must zero fill to current offset
dple_fill:
    mov es:[bhs_offset],ax  ; save starting offset
    call    ddl_zero_fill

; ax == current length of segment (data offset)
; cx holds count of data bytes
; es:di -> destination
; ds:si -> source
dple_2:
    mov ds:[5],ax           ; save data offset for fixups
    add ax,cx               ; add in bytes to add to segment
    mov es:[bhs_length],ax  ; update segment length
    jc  dple_memerr         ; overflow, force out of memory error
    cmp ax,0fff0h-20        ; see if buffer will overflow (20 system bytes)
    ja  dple_memerr         ; yes, force out of memory error

; put any fixup target displacement value in record
    call    proc_targ_disp  ; process fixup target displacements into binary data

    shr cx,1                ; convert byte count to write to words
    rep movsw               ; move the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop ax                  ; get old segment length
    cmp ax,es:[bhs_length]  ; see if <=new length
    ja  dple_adjust         ; no, use old length
    ret

dple_adjust:
    mov bx,es:[bhs_length]  ; get new segment length (less than old)
    mov es:[bhs_length],ax  ; restore old segment length
    sub ax,bx               ; get amount to adjust di
    add di,ax               ; adjust di back to end of old segment
    ret

dple_memerr:
    mov ax,8                ; force out of memory error
    jmp NEAR PTR dos_error

ddl_proc_ledata ENDP

;*****************************
;* DDL_PROC_LIDATA           *
;*****************************

; process a LIDATA record
; upon entry ds:si -> iterated data offset of LIDATA record image,
; es:di -> current i/o buffer position, cx holds data bytes+2 (iterated data offset)
; updates di,si
; destroys ax,bx,cx,dx

ddl_proc_lidata PROC
    lodsw                   ; get iterated data offset
    dec cx
    dec cx                  ; adjust for iterated data offset bytes
    push    es:[bhs_length] ; save old length

    cmp ax,es:[bhs_length]  ; see if matches current length of segment
    je  dpli_2              ; yes
    ja  dpli_fill           ; no, fill

; offset is LESS than current segment length, reduce current segment length
    mov dx,es:[bhs_length]
    sub dx,ax
    sub di,dx               ; back up i/o buffer position
    mov es:[bhs_length],ax
    jmp SHORT dpli_2

; must zero fill to current offset
dpli_fill:
    mov es:[bhs_offset],ax  ; save starting offset
    call    ddl_zero_fill
    mov es:[bhs_length],ax  ; update segment length

; ax == current length of segment (data offset)
; cx == size of iterated data block
; ds:si -> source
; es:di -> destination
dpli_2:
    mov ds:[5],ax           ; save data offset for fixups

; put any fixup target displacement value in record
    call    proc_targ_disp  ; process fixup target displacements into binary data

dpli_3:
    jcxz    dpli_4
    call    ddl_recurse_lidata  ; parse out the lidata record
    jmp SHORT dpli_3        ; loop until no more bytes

dpli_4:
    pop ax                  ; get old segment length
    cmp ax,es:[bhs_length]  ; see if <=new length
    ja  dpli_adjust         ; no, use old length
    ret

dpli_adjust:
    mov bx,es:[bhs_length]  ; get new segment length (less than old)
    mov es:[bhs_length],ax  ; restore old segment length
    sub ax,bx               ; get amount to adjust di
    add di,ax               ; adjust di back to end of old segment
    ret
ddl_proc_lidata ENDP

;*****************************
;* DDL_RECURSE_LIDATA        *
;*****************************

; use recursion to get all data out of lidata record
; upon entry ds:[si] -> current data byte, cx == size of data block,
; es:[di] -> destination
; destroys ax,bx,cx,dx

ddl_recurse_lidata  PROC
    lodsw                   ; get repeat count
    dec cx                  ; decrement record length to parse
    dec cx
    mov bx,ax               ; save repeat count

    lodsw                   ; get block count
    dec cx                  ; decrement record length to parse
    dec cx
    mov dx,ax               ; save block count

    push    si              ; save buffer position
    push    cx              ; save record iterated data block length

drl_reploop:
    pop cx                  ; restore record length
    pop si                  ; restore buffer position
    push    si              ; restore values to stack
    push    cx

    or  dx,dx               ; see if nested iterated data blocks
    je  drl_data            ; no

; nested iterated data blocks
    mov ax,dx               ; get total number of blocks to loop through
drl_blkloop:
    push    ax              ; save current number of blocks left to loop thru
    push    bx              ; save repeat value
    push    dx              ; save block value
    call    ddl_recurse_lidata  ; nest down one level
    pop dx                  ; restore block value
    pop bx                  ; restore repeat value
    pop ax                  ; restore number of blocks left to loop thru
    dec ax                  ; one iteration completed
    jne drl_blkloop         ; loop is not complete, using Z flag from previous decrement
    jmp SHORT drl_end_reploop   ; loop is complete, end this iteration of the repeat loop

; block size is zero, data follows
drl_data:
    lodsb                   ; get length of data to write
    dec cx                  ; decrement record length to parse

; write the data bytes
    push    cx              ; save record's total data length (in case of repeat loop termination)
    mov cl,al               ; get data bytes to write in cl
    xor ch,ch               ; zap high byte
    push    cx              ; save data bytes written
    add es:[bhs_length],cx  ; update segment length
    jc  drl_memerr          ; overflow, force out of memory error
    cmp WORD PTR es:[bhs_length],0fff0h-20  ; see if buffer will overflow (20 system bytes)
    ja  drl_memerr          ; yes, force out of memory error

; write the data bytes to destination (i/o buffer)
; updates di,si
    shr cx,1                ; convert byte count to zero to words
    rep movsw               ; transfer the string
    rcl cx,1                ; pick up carry
    rep movsb               ; transfer leftover byte, if any

    pop ax                  ; get data bytes written
    pop cx                  ; restore record's total data length (in case of repeat loop termination)
    sub cx,ax               ; subtract off data bytes written

drl_end_reploop:
    dec bx                  ; an iteration of the repeat loop was completed
    jne drl_reploop         ; not complete, go back for next iteration

drl_end:
    add sp,4                ; trash old record length and buffer position values on stack
    ret                     ; repeat loop complete, return to next highest level or proc2_lidata

drl_memerr:
    mov ax,8                ; force out of memory error

drl_error:
    jmp NEAR PTR dos_error

ddl_recurse_lidata  ENDP

;*****************************
;* DDL_ZERO_FILL             *
;*****************************

; zero fill segment image until matches current offset
; upon es:di -> current i/o buffer position, ax holds offset
; updates di,si
; destroys dx

ddl_zero_fill   PROC
    push    ax              ; save critical register
    mov dx,cx               ; save cx count
    mov cx,ax
    sub cx,es:[bhs_length]  ; get difference between data offset and last segment stop
    xor ax,ax               ; zero fill
    shr cx,1                ; convert byte count to zero to words
    rep stosw               ; zero the string
    rcl cx,1                ; pick up carry
    rep stosb               ; zero leftover byte, if any
    mov cx,dx               ; restore cx count
    pop ax                  ; restore critical register
    ret
ddl_zero_fill   ENDP

;*****************************
;* PROC_TARG_DISP            *
;*****************************

; process target displacements in fixupp records directly into binary data
; upon entry ds holds segment of L?DATA record to match in fixup entry
; ds:si -> data field (LEDATA) or data block (LIDATA)
; destroys ax,bx,dx

proc_targ_disp  PROC
    push    bx
    push    cx
    push    di
    push    si
    push    bp
    mov bp,ds               ; get segment to match
    mov di,si               ; save -> start of data
    mov ax,DGROUP
    mov ds,ax               ; ds -> warplink data
    mov ax,first_fixblk_ptr

ptd_blkloop:
    or  ax,ax               ; see if valid fixup block
    je  ptd_ret             ; no, done

    mov ds,ax               ; ds -> block
    cmp bp,ds:[2]           ; see if owner block matches current L?DATA record
    jne ptd_nextblk         ; no

; owner block matches current L?DATA record, add any target displacments from fixups
    mov cx,ds:[4]           ; get record length
    mov si,30               ; si -> start of fixupp record

ptd_fixloop:
    lodsb                   ; get first byte of thread or fixup field
    dec cx                  ; decrement record length to parse
    test    al,80h          ; fixup field if high bit set
    jne ptd_fix             ; fixup field

; thread field, parse it out
    call    ddl_save_thread
    jmp SHORT ptd_chkcheck  ; check if at checksum byte

ptd_fix:
    mov ah,ds:[si]          ; get high byte of locat field
    inc si
    dec cx                  ; decrement record length to parse
    mov bx,ax               ; save locat field value

    call    scan_to_targ_disp   ; scan to target displacement field, if any

    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    jne ptd_chkcheck        ; set, no target displacement

; P bit is zero, target displacement field exists
    lodsw                   ; get target displacement word
    dec cx
    dec cx
    mov dx,bx               ; save locat field
    and bl,3                ; get 2 MSBs of data record offset
    xchg    bh,bl           ; bx holds data record offset

    and dl,1ch              ; get loc field
    je  ptd_low
    cmp dl,8                ; see if base fixup
    je  ptd_chkcheck        ; yes, don't add in target displacement
    jmp SHORT ptd_notlow    ; offset or pointer

; low-order byte location
ptd_low:
    mov dx,ds               ; save flag value from 'and', save -> fixupp record
    mov ds,bp               ; save flag value from 'and', ds-> L?DATA record
    add [bx+di],al          ; add in low byte of target displacement
    jmp SHORT ptd_2

; any type of location other than low-order
ptd_notlow:
    mov dx,ds               ; save flag value from 'and', save -> fixupp record
    mov ds,bp               ; save flag value from 'and', ds-> L?DATA record
    add [bx+di],ax          ; add in target displacement word

ptd_2:
    mov ds,dx               ; restore ds -> fixupp record

; see if at checksum
ptd_chkcheck:
    cmp cx,1                ; check if at checksum byte
    ja  ptd_fixloop         ; no, loop back for next fixup/thread field


ptd_nextblk:
    mov ax,ds:[0]           ; get pointer to next block
    jmp SHORT ptd_blkloop   ; loop back and check it

ptd_ret:
    mov ds,bp               ; restore ds -> L?DATA record
    pop bp
    pop si
    pop di
    pop cx
    pop bx
    ret
proc_targ_disp  ENDP

;*****************************
;* WRITE_LNAMES_BLOCK        *
;*****************************

; write all lnames blocks into one big lnames block for DDL module
; destroys ax,bx,cx,dx

write_lnames_block  PROC
    call    update_ddl_size ; get start of lnames block (EOF)
    mov di,OFFSET DGROUP:ddl_symbol_lookup
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov [di+2],ax           ; save low word of file position for lookup
    mov [di+4],dx           ; save high word for lookup
    mov WORD PTR mod_header.mh_lnames,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_lnames+2,dx

    push    ds
    pop es                  ; es -> warplink data
    mov ax,first_lnamesblk_ptr

wlb_loop:
    or  ax,ax               ; see if valid lnames block
    je  wlb_done            ; no, done

    mov es:[di],ax          ; save segment for lookup
    mov dx,4                ; start at offset 4 in block past 2 system words
    mov cx,16*LNAMES_BLK_SIZE-4 ; write entire block except system words
    mov ds,ax               ; ds:dx -> write buffer
    sub cx,ds:[0]           ; subtract off unused space
    mov ah,40h              ; write to device
    int 21h
    jc  wlb_error           ; error writing to file

    add ax,es:[di+2]        ; compute next lookup file position
    mov es:[di+8],ax        ; save it
    mov ax,es:[di+4]
    adc ax,0                ; carry to high word
    mov es:[di+10],ax
    add di,6                ; move to next lookup position
    mov ax,ds:[2]           ; get pointer to next block, if any
    jmp SHORT wlb_loop      ; loop back and write it

wlb_done:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    ret

wlb_error:
    jmp NEAR PTR dos_error  ; error writing to file

write_lnames_block  ENDP

;*****************************
;* WRITE_SYMBOLS_BLOCK       *
;*****************************

; write all pubdef (symbol) names blocks into one big symbols block for DDL module
; destroys ax,bx,cx,dx,di,es

write_symbols_block PROC
    call    update_ddl_size ; get start of symbol names block (EOF)
    mov di,OFFSET DGROUP:ddl_symbol_lookup
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov [di+2],ax           ; save low word of file position for lookup
    mov [di+4],dx           ; save high word for lookup
    mov WORD PTR mod_header.mh_symbols,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_symbols+2,dx

    push    ds
    pop es                  ; es -> warplink data
    mov ax,first_pdnameblk_ptr

wsym_loop:
    or  ax,ax               ; see if valid lnames block
    je  wsym_done           ; no, done

    mov es:[di],ax          ; save segment for lookup
    mov dx,4                ; start at offset 4 in block past 2 system words
    mov cx,16*PUB_NAMBLK_SIZE-4 ; write entire block except system words
    mov ds,ax               ; ds:dx -> write buffer
    sub cx,ds:[0]           ; subtract off unused space
    mov ah,40h              ; write to device
    int 21h
    jc  wsym_error          ; error writing to file

    add ax,es:[di+2]        ; compute next lookup file position
    mov es:[di+8],ax        ; save it
    mov ax,es:[di+4]
    adc ax,0                ; carry to high word
    mov es:[di+10],ax
    add di,6                ; move to next lookup position
    mov ax,ds:[2]           ; get pointer to next block, if any
    jmp SHORT wsym_loop     ; loop back and write it

wsym_done:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    ret

wsym_error:
    jmp NEAR PTR dos_error  ; error writing to file

write_symbols_block ENDP

;*****************************
;* WRITE_PUBDEF_BLOCK        *
;*****************************

; write pubdefs into one block, update high/low, name pointers
; destroys ax,bx,cx,dx,di,si,es

write_pubdef_block  PROC
    call    update_ddl_size ; get start of pubdef block (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_pubdef,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_pubdef+2,dx

    mov pubcom_flag,0       ; flag parsing publics
    call    sort_pubdefs
    mov cx,pubcount         ; get count of entries in sort buffer
    mov mod_header.mh_pubcount,cx   ; save count of publics
    or  cx,cx
    jne wpb_altent          ; entries exist
    ret                     ; no entries

; alternate entry for code shared with write_comdef_block
wpb_altent:
    mov si,OFFSET DGROUP:ddl_hold_buff  ; si -> holding buffer for pubdefs prior to write
    xor bx,bx               ; bx offsets into sort buffer

wpb_mainloop:
    mov es,allocation_base
    mov es,es:[bx]          ; es -> symbol pubdef entry
    mov dx,es               ; save -> entry
    mov ax,es:[6]           ; get segment of pubdef name
    mov di,OFFSET DGROUP:ddl_symbol_lookup  ; di -> start of lookup table
    push    ds
    pop es                  ; es -> warplink data

wpb_searchloop:
    scasw                   ; search for segment entry in lookup table
    je  wpb_found           ; found the entry
    add di,4                ; bump to next lookup entry
    jmp SHORT wpb_searchloop    ; keep looking

wpb_found:
    mov ax,es:[di]          ; get low word of file position
    mov di,es:[di+2]        ; get high word

; file position corresponding to segment in di:ax
    mov es,dx               ; restore es -> block entry
    add ax,es:[4]           ; add in pubdef name offset
    adc di,0                ; carry to high word
    sub ax,4                ; adjust for 2 system info words not written
    sbb di,0                ; borrow to high word

    sub ax,WORD PTR ds:mod_header.mh_symbols    ; make relative to start of symbols block
    sbb di,WORD PTR ds:mod_header.mh_symbols+2

    mov [si+4],ax           ; save low file position
    mov [si+6],di           ; save high file position
    mov ax,es:[0]           ; save segment index (not pointer since DDL)
    mov [si],ax
    mov ax,es:[2]           ; save frame number/group index/communal length high word
    mov [si+2],ax
    mov ax,es:[8]           ; save public offset/communal length low word
    mov [si+8],ax
    mov ax,es:[14]          ; save definition and general flags
    mov [si+14],ax

; update symbol name low segment pointer at es:[10],
; symbol name high segment pointer at es:[12]
; pointers must be converted by dividing by 6 and multiplying by 16 (10h)
    mov es,allocation_base
    mov ax,es:[bx+2]        ; get low pointer
    xor dx,dx
    mov di,6
    div di                  ; adjust for sort buffer 6 byte entry pointers
    shl ax,1                ; adjust for symbol entry length of 16 bytes, x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    mov [si+10],ax          ; save adjusted low pointer
    mov ax,es:[bx+4]        ; get high pointer
    xor dx,dx
    div di                  ; adjust for sort buffer 6 byte entry pointers
    shl ax,1                ; adjust for symbol entry length of 16 bytes, x2
    shl ax,1                ; x4
    shl ax,1                ; x8
    shl ax,1                ; x16
    mov [si+12],ax          ; save adjusted high pointer
    and BYTE PTR [si+15],0f5h   ; shut off extdef/comdef flag, overlay class bits

    add bx,6                ; move to next position in sort buffer
    add si,16               ; move to next position in holding buffer
    cmp si,2034+OFFSET DGROUP:ddl_hold_buff ; see if holding buffer is full
    jb  wpb_nextent         ; no

    call    flush_hold_buff ; flush the holding buffer

wpb_nextent:
    dec cx
    jcxz    wpb_out         ; done with all entries
    jmp NEAR PTR wpb_mainloop   ; loop until all entries in sort table are finished

wpb_out:
    call    flush_hold_buff ; flush the pubdef block to the DDL file

wpb_ret:
    ret
write_pubdef_block  ENDP

;*****************************
;* SORT_PUBDEFS              *
;*****************************

; sort all the public or communal declarations into 3-word entries
;  [segment pointer][low name pointer][high name pointer]
; destroys ax,bx,cx,dx,di,si,es

sort_pubdefs    PROC

; compute upper boundary of sort area
    mov ax,allocation_top
    sub ax,allocation_base  ; compute free memory segments
    shl ax,1                ; convert to bytes, x2
    jc  sp_over             ; overflow, set ax boundary to highest possible
    shl ax,1                ; x4
    jc  sp_over
    shl ax,1                ; x8
    jc  sp_over
    shl ax,1                ; x16
    jnc sp_2                ; ax holds upper boundary of free memory

sp_over:
    mov ax,65530            ; set boundary to highest possible

sp_2:
    mov sort_buff_bound,ax  ; save sort buffer boundary
    xor bx,bx               ; init offset into sort buffer
;**    mov block_flag,2        ; init to public entry parsing

sp_mainloop:
    mov ax,first_pdeclblk_ptr   ; get pointer to first public declarations block

sp_blkloop:
    or  ax,ax               ; see if valid block
    je  sp_done             ; no

    mov es,ax               ; es -> declarations block
    push    ax              ; save -> declarations block
    mov cx,es:[0]           ; get count of entries in block
    jcxz    sp_nextblock    ; no entries
    inc ax
    mov dx,ax               ; dx -> first entry in block

sp_entloop:
    mov es,dx               ; es -> block entry
    mov al,es:[15]          ; get general flags
    mov ah,al               ; save status

    mov al,es:[14]          ; get definitions flag
    and al,3                ; mask off unneeded bits
    cmp pubcom_flag,0       ; see if parsing publics or communals
    je  sp_public           ; publics

; parsing communals
    cmp al,1                ; see if communal/extdef
    jne sp_nextent          ; no
    and ah,40h              ; see if communal
    je  sp_nextent          ; no
    jmp SHORT sp_match      ; yes

; parsing publics
sp_public:
    cmp al,2                ; see if public or absolute
    jae sp_match            ; yes

sp_nextent:
    inc dx                  ; move to next entry in block
    loop    sp_entloop      ; go through all entries in loop

sp_nextblock:
    pop es                  ; restore es -> declarations block
    mov ax,es:[2]           ; get pointer to next block, if any
    jmp SHORT sp_blkloop

sp_done:

sp_ret:
    ret

; matched communal or public, update count and sort buffer
sp_match:
    cmp pubcom_flag,0       ; see if parsing publics or communals
    je  sp_bumppub          ; public, bump public count

    inc comcount            ; bump count of communal in module
    jmp SHORT sp_3

sp_bumppub:
    inc pubcount            ; bump count of publics in module

sp_3:
    mov di,es:[6]           ; get segment of name pointer
    mov si,es:[4]           ; get offset of name pointer
    mov es,allocation_base  ; es:bx -> sort buffer entry
    mov es:[bx],dx          ; save -> this entry
    xor ax,ax
    mov WORD PTR es:[bx+2],ax   ; zero low/high pointers
    mov WORD PTR es:[bx+4],ax
    mov ax,es               ; ax holds allocation base
    push    cx              ; save critical register
    or  bx,bx               ; see if first entry
    je  sp_saved            ; yes, no binary tree to search

; di:si -> name, place in binary tree in sort buffer
    mov ds,di               ; ds:si -> name
    xor di,di               ; di == search offset in sort buffer

sp_searchloop:
    push    si              ; save -> start of name
    push    di              ; save sort buffer search offset
    mov es,es:[di]          ; es -> entry in tree
    les di,es:[4]           ; es:di -> name to compare against

sp_byteloop:
    cmpsb                   ; compare new name and search entry namec
    je  sp_byteloop         ; bytes match, loop for next byte test
    jc  sp_setlow           ; old name greater than new name
    mov cl,1                ; set higher flag
    jmp SHORT sp_chkflag

sp_setlow:
    xor cl,cl               ; set lower flag

sp_chkflag:
    pop di
    pop si

    mov es,ax               ; es:di -> last searched sort buffer entry
    or  cl,cl               ; see if name was lower or higher than search entry
    je  sp_low              ; lower

; new name was higher than search entry
sp_high:
    mov cx,es:[di+4]        ; get pointer to high name
    or  cx,cx               ; make sure valid pointer
    je  sp_savehiptr        ; no, save pointer to current entry as high pointer
    mov di,cx               ; di -> next entry to try
    jmp SHORT sp_searchloop

sp_savehiptr:
    mov es:[di+4],bx        ; update search entry high pointer to current
    jmp SHORT sp_saved

; new name was lower than search entry
sp_low:
    mov cx,es:[di+2]        ; get pointer to low name
    or  cx,cx               ; make sure valid pointer
    je  sp_saveloptr        ; no, save pointer to current entry as low pointer
    mov di,cx               ; di -> next entry to try
    jmp SHORT sp_searchloop

sp_saveloptr:
    mov es:[di+2],bx        ; update search entry low pointer to current

sp_saved:
    pop cx                  ; restore critical register
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    add bx,6                ; bump to next
    cmp bx,sort_buff_bound  ; make sure sort buffer has not overflowed
    jae sp_memerr           ; overflow
    jmp NEAR PTR sp_nextent ; move to next entry

; overflow of boundary, too many publics
sp_memerr:
    mov ax,8                ; force out of memory error
    jmp NEAR PTR dos_error

sort_pubdefs    ENDP

;*****************************
;* WRITE_COMDEF_BLOCK        *
;*****************************

; write comdefs into one block, update high/low, name pointers
; destroys ax,bx,cx,dx,di,si,es

write_comdef_block  PROC
    call    update_ddl_size ; get start of comdef block (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_comdef,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_comdef+2,dx

    mov pubcom_flag,1       ; flag parsing communals
    call    sort_pubdefs
    mov cx,comcount         ; get count of entries in sort buffer
    mov mod_header.mh_comcount,cx   ; save count of communals
    jcxz    wcb_ret         ; no entries
    jmp NEAR PTR wpb_altent ; go to code shared with write_pubdef_block

wcb_ret:
    ret
write_comdef_block  ENDP

;*****************************
;* WRITE_EXTDEF_BLOCK        *
;*****************************

; write extdefs into one block, update name pointers
; destroys ax,bx,cx,dx,di,si,es

write_extdef_block  PROC
    call    update_ddl_size ; get start of extdef block (EOF)
    sub ax,WORD PTR module_start    ; adjust relative to start of module
    sbb dx,WORD PTR module_start+2  ; adjust high word
    mov WORD PTR mod_header.mh_extdef,ax   ; save pointer to start
    mov WORD PTR mod_header.mh_extdef+2,dx
    mov si,OFFSET DGROUP:ddl_hold_buff  ; si -> holding buffer for extdefs prior to write

    mov bx,OFFSET DGROUP:ext_defent_indptr  ; index into extdef/comdef entries
    mov cx,current_extdef   ; get total count of entries in array
    jcxz    web_done        ; no entries

web_entloop:
    mov es,ds:[bx]          ; es -> entry's public declaration

; valid extdef (or comdef), keep it
    and BYTE PTR es:[15],0fdh   ; shut off extdef/comdef flag bit
    inc extcount            ; bump count of externals in module
    mov ax,es:[6]           ; get segment of extdef name
    mov di,OFFSET DGROUP:ddl_symbol_lookup  ; di -> start of lookup table
    push    ds
    pop es                  ; es -> warplink data

web_searchloop:
    scasw                   ; search for segment entry in lookup table
    je  web_found           ; found the entry
    add di,4                ; bump to next lookup entry
    jmp SHORT web_searchloop    ; keep looking

web_found:
    mov ax,es:[di]          ; get low word of file position
    mov di,es:[di+2]        ; get high word

; file position corresponding to segment in di:ax
    mov es,ds:[bx]          ; restore es -> block entry
    add ax,es:[4]           ; add in extdef name offset
    adc di,0                ; carry to high word
    sub ax,4                ; adjust for 2 system info words not written
    sbb di,0                ; borrow to high word

    sub ax,WORD PTR ds:mod_header.mh_symbols    ; make relative to start of symbols block
    sbb di,WORD PTR ds:mod_header.mh_symbols+2

    mov [si],ax             ; save low file position
    mov [si+2],di           ; save high file position
    mov ax,es:[14]          ; get flag bytes
    mov [si+4],ax           ; save them

    add si,6                ; move to next position in holding buffer
    cmp si,2044+OFFSET DGROUP:ddl_hold_buff ; see if holding buffer is full
    jb  web_nextent         ; no

    call    flush_hold_buff ; flush the holding buffer

web_nextent:
    inc bx                  ; move to next word entry in block
    inc bx
    loop    web_entloop     ; go through all entries in loop

web_done:
    call    flush_hold_buff ; flush the extdef block to the DDL file
    mov ax,extcount
    mov mod_header.mh_extcount,ax   ; save count of externals
    ret
write_extdef_block  ENDP

;*****************************
;* FLUSH_HOLD_BUFF           *
;*****************************

; flush the holding block to DDL file
; destroys ax,si
; returns si -> start of hold buffer

flush_hold_buff PROC
    push    bx              ; save critical register
    push    cx
    push    dx

    mov dx,OFFSET DGROUP:ddl_hold_buff
    sub si,dx               ; compute bytes to write
    mov cx,si
    jcxz    feb_out         ; no bytes to write
    mov bx,exe_handle
    mov ah,40h              ; write to device
    int 21h
    jc  dp_error            ; error writing to file

feb_out:
    mov si,OFFSET DGROUP:ddl_hold_buff  ; reset di -> start of holding buffer
    pop dx                  ; restore critical register
    pop cx
    pop bx
    ret
flush_hold_buff ENDP

;*****************************
;* DDL_PAD                   *
;*****************************

; pad a module out to 16-byte boundary with zeros
; returns bx==file handle
; destroys ax,bx,cx,dx

ddl_pad PROC
    call    update_ddl_size ; get current file size
    mov cx,WORD PTR ddl_filesize    ; get low word of file size
    and cx,15               ; get remainder from sixteen byte boundary
    jcxz    dp_ret          ; no padding needed
    mov ax,16
    sub ax,cx               ; ax holds padding necessary
    mov cx,ax               ; get bytes to write in cx
    mov dx,OFFSET DGROUP:zero_table
    mov ah,40h
    int 21h
    jc  dp_error            ; error writing to file

dp_ret:
    ret

dp_error:
    jmp NEAR PTR dos_error  ; error writing to file

ddl_pad ENDP

;*****************************
;* WRITE_DDL_MODHEAD         *
;*****************************

; write module header to DDL file
; destroys ax,bx,cx,dx

write_ddl_modhead   PROC
    mov bx,exe_handle
    mov dx,OFFSET DGROUP:mod_header ; ds:dx -> write buffer area
    mov cx,MOD_HEADER_SIZE  ; size of module header
    mov ah,40h              ; write to file
    int 21h
    jc  dp_error            ; error writing to file

    ret
write_ddl_modhead   ENDP

;*****************************
;* PARSE_DDL_OPTION          *
;*****************************

; parse DDL option
; ds:si -> second  char of option (past 'D')
; cx -> start of option
; destroys ax,dx,di,es
; updates si

parse_ddl_option    PROC
    mov ah,[si]             ; peek at next char
    cmp ah,'D'              ; see if possible /ddl option
    je  pdo_chkddl          ; possible
    cmp ah,'d'              ; check lowercase option
    je  pdo_chkddl          ; possible
    cmp ah,'M'              ; see if /dm option
    je  pdo_chkdbm          ; possible
    cmp ah,'m'              ; check lowercase option
    je  pdo_chkdbm          ; possible

; assume DOSSEG option
    mov is_dosseg,al        ; set dosseg segment ordering flag
    ret

pdo_chkddl:
    lodsb                   ; gobble 'd'
    lodsb                   ; get char following 'dd'
    cmp al,'L'              ; see if /ddl option
    je  pdo_ddl             ; yes
    cmp al,'l'              ; check lowercase option
    jne pdo_bad             ; no, bad option

pdo_ddl:
    mov is_ddl,al           ; flag ddl creation
    ret

pdo_bad:                    ; bad DDL option specified
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

pdo_chkdbm:
    lodsb                   ; gobble 'B' or 'M'
    and ah,0dfh             ; force option char to uppercase
    lodsb                   ; get colon following DDL option char
    cmp al,':'              ; must be colon
    jne pdo_bad             ; not a colon

    cmp ah,'M'              ; see if DDL manager name
    je  pdo_getfile         ; yes

; DDL manager data file name
pdo_getfile:
    mov ax,ds
    mov es,ax               ; es -> warplink data segment
    mov di,OFFSET DGROUP:ddldat_filename    ; es:di -> place for DDL manager name
    mov dx,di               ; ds:dx -> file spec

pdo_loop:
    lodsb                   ; get name char
    cmp al,' '              ; see if whitespace char
    jbe pdo_done            ; yes, done
    cmp al,','              ; see if comma
    je  pdo_done            ; done
    cmp al,'/'              ; see if option char
    je  pdo_done            ; done
    cmp al,'('              ; see if left paren, begin overlay
    je  pdo_done            ; done
    cmp al,')'              ; see if right paren, end overlay
    je  pdo_done            ; done
    cmp al,'@'              ; see if @ (response file)
    je  pdo_done            ; done
    cmp al,';'              ; see if semicolon terminator
    je  pdo_done            ; done

    stosb                   ; name char, transfer it
    jmp SHORT pdo_loop      ; get next char

pdo_done:
    dec si                  ; drop si back to -> terminating char
    xor al,al               ; null terminate string
    stosb                   ; transfer it
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jc  pdo_notfound        ; didn't find manager file/DOS error
    ret                     ; done

pdo_notfound:
    jmp NEAR PTR dos_error  ; no, dos error

parse_ddl_option    ENDP

;*****************************
;* DDL_DATAREC               *
;*****************************

; process LEDATA, LIDATA records for DDL module
; upon entry cx=record length,bp=buffer_end
; si -> first byte of record past record length word
; destroys ax,bx,dx,di,es
; updates si,cx

ddl_datarec PROC
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne dd_okay             ; no
    ret                     ; yes, no processing to complete

dd_okay:
    mov dl,al               ; save type of record
    mov bx,cx               ; get record length
    cmp cx,1                ; return if blank record
    jbe dd_ret

; adjust for checksum byte(-1), record type byte, record length,
; next block pointer word, data offset word, segment index byte,
; start of iterated/enumerated data, record length-segment index byte(s),
; round to next para
    add bx,26
    shr bx,1                ; convert to paragraphs
    shr bx,1
    shr bx,1
    shr bx,1
    call    alloc_memory    ; get memory for block allocation
    mov bx,ax               ; bx -> new block
    mov ax,alloc_binblk_ptr ; see if there was a previous block
    or  ax,ax
    jne dd_prevblk          ; previous block existed
    mov first_binblk_ptr,bx ; save -> first block
    jmp SHORT dd_2          ; bypass previous block code

dd_prevblk:
    mov es,ax               ; es -> previous block
    mov es:[1],bx           ; save -> new block

dd_2:
    mov alloc_binblk_ptr,bx ; update last allocated block
    mov es,bx               ; es -> new block
    mov es:[0],dl           ; save type of block
    xor ax,ax
    mov es:[1],ax           ; zero -> next block
    mov ax,cx               ; get record length
    dec ax                  ; adjust for checksum byte
    mov es:[3],ax           ; save record length

; move the L?DATA record -> buffer_base:si into newly allocated block
    mov ds,buffer_base
    mov di,12               ; es:di -> newly allocated block

dd_loop:
    movsb                   ; move byte of record into block
    dec cx                  ; decrement record length
    cmp si,bp               ; check boundary conditions
    jb  dd_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
dd_3:
    cmp cx,1                ; see if at end of record
    ja  dd_loop             ; no

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

dd_ret:
    ret
ddl_datarec ENDP

;*****************************
;* DDL_FIXUPP                *
;*****************************

; process FIXUPP records for DDL module
; upon entry cx=record length,bp=buffer_end
; si -> first byte of record past record length word
; destroys ax,bx,dx,di,es
; updates si,cx

ddl_fixupp  PROC
    cmp udl_proc_pass,1     ; see if pass 1 udl processing
    jne df_okay             ; no
    ret                     ; yes, no processing to complete

df_okay:
    mov bx,cx               ; get record length
    cmp cx,1                ; return if blank record
    jbe df_ret

; adjust for checksum byte(-1), record length, next block pointer word, pointer to owner L?DATA block,
; 4 frame fixup method bytes, 4 target fixup method bytes,
; 4 frame fixup thread index words, 4 target fixup thread index words, round to next para
    add bx,44
    shr bx,1                ; convert to paragraphs
    shr bx,1
    shr bx,1
    shr bx,1
    call    alloc_memory    ; get memory for block allocation
    mov bx,ax               ; bx -> new block
    mov ax,alloc_fixblk_ptr ; see if there was a previous block
    or  ax,ax
    jne df_prevblk          ; previous block existed
    mov first_fixblk_ptr,bx ; save -> first block
    jmp SHORT df_2          ; bypass previous block code

df_prevblk:
    mov es,ax               ; es -> previous block
    mov es:[0],bx           ; save -> new block

df_2:
    mov alloc_fixblk_ptr,bx ; update last allocated block
    mov es,bx               ; es -> new block
    xor ax,ax
    mov es:[0],ax           ; zero -> next block
    mov ax,alloc_binblk_ptr ; get last allocated block (owning L?DATA record)
    mov es:[2],ax
    mov ax,cx               ; get record length
    dec ax                  ; adjust for checksum byte
    mov es:[4],ax           ; save record length

; save the current state of FIXUPP thread fields
    push    si              ; save si-> source block
    mov dx,cx               ; save record length
    mov di,6
    mov si,OFFSET DGROUP:frame_thrd_meth
    movsw                   ; save four frame thread method bytes
    movsw
    mov si,OFFSET DGROUP:target_thrd_meth
    movsw                   ; save four target thread method bytes
    movsw
    mov cx,4                ; four index words
    mov si,OFFSET DGROUP:frame_thrd_index
    rep movsw               ; save frame thread index
    mov cx,4                ; four index words
    mov si,OFFSET DGROUP:target_thrd_index
    rep movsw               ; save target thread index
    mov cx,dx               ; restore record length
    pop si                  ; restore si-> source

; move the FIXUPP record -> buffer_base:si into newly allocated block
    mov ds,buffer_base
    mov di,30               ; es:di -> newly allocated block

df_loop:
    movsb                   ; move byte of record into block
    dec cx                  ; decrement record length
    cmp si,bp               ; check boundary conditions
    jb  df_3                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
df_3:
    cmp cx,1                ; see if at end of record
    ja  df_loop             ; no

; save new thread field values, if any, by parsing through fixupp record
    call    ddl_parse_fixupp

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

df_ret:
    ret
ddl_fixupp  ENDP

;*****************************
;* DDL_PARSE_FIXUPP          *
;*****************************

; parse fixupp record looking for thread fields
; upon entry es-> fixupp block,dx holds record length
; destroys ax,bx,dx,di,es,ds

ddl_parse_fixupp    PROC
    push    si              ; save critical register
    push    cx

	push	es
	pop	ds					; ds -> fixupp block
    mov si,30               ; ds:si -> start of fixupp record
    mov ax,DGROUP
    mov es,ax               ; es -> warplink data
    mov cx,dx               ; cx == record length

dpr_loop:
    lodsb                   ; get first byte of thread or fixup field
    dec cx                  ; decrement record length to parse
    test    al,80h          ; fixup field if high bit set
    jne dpr_fix             ; fixup field

; thread field, parse it out
    call    ddl_save_thread
    jmp SHORT dpr_chkcheck  ; check if at checksum byte

; fixup field, scan past it
dpr_fix:
    inc si                  ; scan past second byte of locat field
    dec cx                  ; decrement record length to parse

    call    scan_to_targ_disp   ; scan to target displacement field, if any

    mov al,dl               ; get fix dat field
    and al,4                ; get P bit field
    jne dpr_chkcheck        ; set, no target displacement

; P bit is zero, scan past target displacement field
    inc si                  ; gobble target displacement word
    inc si
    dec cx
    dec cx

; see if at checksum
dpr_chkcheck:
    cmp cx,1                ; check if at checksum byte
    ja  dpr_loop            ; no, loop back for next fixup/thread field

    pop cx
    pop si                  ; restore critical register
    ret
ddl_parse_fixupp    ENDP

;*****************************
;* SCAN_TO_TARG_DISP         *
;*****************************

; scan from fix dat field to target displacement field, if any
; upon entry ds:si -> fix dat field, cx holds record length
; returns dl==fix dat field
; destroys ax,dx
; updates si,cx

scan_to_targ_disp   PROC
    lodsb                   ; get fix dat field
    dec cx
    mov dl,al               ; save fix dat value
    mov dh,al               ; dh == fix dat
    and dh,70h              ; get frame field in dh
    test    al,80h          ; check if thread field for frame (fbit)
    jne stt_2               ; yes, no frame data bytes

; no thread field for frame
    cmp dh,20h              ; see if index specified for this frame
    ja  stt_2               ; no

; index specified for this field
    lodsb                   ; get frame datum first byte
    dec cx                  ; decrement record length to parse
    cmp al,80h              ; see if two byte field
    jb  stt_2               ; no
    lodsb                   ; scan past frame datum second byte
    dec cx

stt_2:
    mov al,dl               ; get fix dat field
    test    al,8            ; check if thread field for target (tbit)
    jne stt_ret             ; yes (no target data bytes)

; no thread field for target, scan past target data bytes
    lodsb                   ; get target datum first byte
    dec cx
    cmp al,80h              ; see if two byte field
    jb  stt_ret             ; no
    lodsb                   ; scan past target datum second byte
    dec cx

stt_ret:
    ret
scan_to_targ_disp   ENDP

;*****************************
;* DDL_SAVE_THREAD           *
;*****************************

; parse and save thread information
; upon entry ds:si -> source, es-> warplink data, al holds thread data byte
; destroys ax,dx

ddl_save_thread PROC
    push    bp              ; save critical register
    push    bx
    push    di
    mov bl,al               ; save thread data byte
    and al,1ch              ; break out thread method
    shr al,1
    shr al,1                ; adjust method value to relative zero (right justify bit field)
    push    ax              ; save thread method value

    xor ah,ah               ; zero high byte of thread index
    lodsb                   ; get thread index byte low byte
    dec cx                  ; decrement record length to parse
    cmp al,80h              ; see if two byte field
    jb  st_2                ; no
    and al,7fh              ; strip high bit
    mov ah,al               ; move value to high byte
    lodsb                   ; get thread index second byte
    dec cx                  ; decrement record length to parse

st_2:
    mov bp,ax               ; save thread index value
    mov dl,bl
    and dl,3                ; get thread number in dl
    xor dh,dh               ; zap high byte
    and bl,40h              ; get D bit field value in bl
    je  st_targ             ; D bit reset, target thread field

; frame thread field
    mov bx,dx               ; bx -> proper array element (thread number'th)
    mov di,dx               ; di -> proper array element (thread number'th)
	add	di,di				; convert to word offset
    pop ax                  ; get thread method value
    mov es:[bx+OFFSET DGROUP:frame_thrd_meth],al	; save method type in array
    mov es:[di+OFFSET DGROUP:frame_thrd_index],bp	; save index type to array

    mov bx,6				; bx -> storage for frame thread method within fixup entry
    mov di,14				; di -> storage for frame thread index within fixup entry

; ax == thread method value, ds -> fixupp block
st_array_save:
    add bx,dx               ; bx -> proper array element (thread number'th)
    mov ds:[bx],al          ; save method type in array
    add dx,dx               ; convert dx to word offset
    add di,dx               ; di -> proper array element (thread number'th)
    mov ds:[di],bp          ; save index type to array

    pop di
    pop bx
    pop bp                  ; restore critical register
    ret

; target thread field
st_targ:
    mov bx,dx               ; bx -> proper array element (thread number'th)
    mov di,dx               ; di -> proper array element (thread number'th)
	add	di,di				; convert to word offset
    pop ax                  ; get thread method value
    mov es:[bx+OFFSET DGROUP:target_thrd_meth],al	; save method type in array
    mov es:[di+OFFSET DGROUP:target_thrd_index],bp	; save index type to array

    mov bx,10				; bx -> storage for target thread method within fixup entry
    mov di,22				; di -> storage for target thread index within fixup entry
    jmp SHORT st_array_save ; save thread method and index to arrays

ddl_save_thread ENDP 

;*****************************
;* DDL_MODEND                *
;*****************************

; process MODEND record for DDL module
; upon entry cx=record length,bp=buffer_end
; si -> first byte of record past record length word
; destroys ax,bx,dx,di
; updates si,cx

ddl_modend  PROC
    push    es              ; save critical register
    mov bx,cx               ; get record length

    add bx,14               ; round up to next paragraph, adjust for checksum byte
    shr bx,1                ; convert to paragraphs
    shr bx,1
    shr bx,1
    shr bx,1
    call    alloc_memory    ; get memory for block allocation
    mov modend_ptr,ax       ; save -> modend block
    mov es,ax               ; es -> new block

; move the MODEND record -> buffer_base:si into newly allocated block
    mov ds,buffer_base
    xor di,di               ; es:di -> newly allocated block

dm_loop:
    movsb                   ; move byte of record into block
    dec cx                  ; decrement record length
    cmp si,bp               ; check boundary conditions
    jb  dm_2                ; okay
    call    load_file       ; load next portion of file into buffer, at end position
dm_2:
    cmp cx,1                ; see if at end of record
    ja  dm_loop             ; no

    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data

dm_ret:
    pop es                  ; restore critical register
    ret
ddl_modend  ENDP

END
