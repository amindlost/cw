;*********************************************************************
;*   MLQUICK.ASM                                                     *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          01/01/93                                         *
;*   Model:         Small                                            *
;*   Version:       2.50                                             *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Quick linker code                                               *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlquick
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
PUBLIC	check_qlk,read_qlk_libname,read_qlk_mod_info,read_qlk_unres
PUBLIC	write_qlk_libname,write_qlk_mod_info,write_qlk_unres
PUBLIC	write_qlk_lib_info,is_qlk_file,write_qlk_modsize
PUBLIC	delete_qlk_file

; variables
PUBLIC	writing_qlk_flag,reading_qlk_flag,no_qlk_modules

;*****************************
;* Data begins               *
;*****************************

; initialized data     
.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   filename:BYTE
EXTRN	lib_page_size:WORD,lib_page_num:WORD
EXTRN	module_flag:BYTE,libmod_obj_flag:BYTE,lib_read_amt:WORD
EXTRN	parfilename:BYTE,exe_ext_ptr:WORD

EVEN

; bytes
writing_qlk_flag	DB	0	; nonzero if writing to QLK file
reading_qlk_flag	DB	0	; nonzero if reading from QLK file

; words
qlk_handle	DW	0			; handle of open QLK file
qlk_buffpos	DW	0			; current read/write position in qlk buffer
qlk_readpos	DW	0			; read position of qlk file

; byte strings
qlk_name    DB  128 DUP (0) ; QLK (QuickLinker) file name, including any path
qlk_buffer	DB	300 DUP (?)	; QLK read/write buffer

; uninitialized data
.DATA?

; bytes
no_qlk_modules	DB	?		; nonzero if no library modules for library in QLK file

;*****************************
;* Constant data             *
;*****************************

.CONST

qlk_text	DB	"QLK"

;*****************************
;* Code begins               *
;*****************************
     
.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR,restore_ems_map:NEAR
EXTRN	get_xxx_name:NEAR,file_not_found:NEAR,set_exe_name:NEAR

;*****************************
;* IS_QLK_FILE               *
;*****************************

; returns carry flag set is QLK does not exist, reset if does
; destroys ax,bx,dx

is_qlk_file	PROC
	push	cx				; save critical registers
	push	si
	push	di
	push	es

; copy parfilename to qlk_buffer variable
	push	ds
	pop	es
	mov	cx,64
	mov	si,OFFSET DGROUP:parfilename
	mov	di,OFFSET DGROUP:qlk_buffer
	rep	movsw

	push	WORD PTR exe_name	; save exe_name in case is null and have to set it
	push	exe_ext_ptr		; save exe name info
	cmp	BYTE PTR exe_name,0	; see if exe name supplied (for get_xxx_name use)
	jne	iqf_exe_ok			; yes
	call	set_exe_name	; set the EXE name temporarily

iqf_exe_ok:
	mov	bx,OFFSET DGROUP:qlk_text	; bx -> proper extension
    mov di,OFFSET DGROUP:qlk_name   ; di -> place to put ILF name
	mov	dx,di				; ds:dx -> filespec
    call    get_xxx_name    ; get QLK name
    mov ax,3d02h            ; open file for reading/writing
    int 21h
    call    restore_ems_map
	jnc	iqf_close			; success open, close it
	jmp	SHORT iqf_ret

; close the opened QLK file
iqf_close:
	mov	bx,ax
	mov	ah,3eh
	int	21h

iqf_ret:

; copy qlk_buffer to parfilename variable
	push	ds
	pop	es
	mov	cx,64
	mov	di,OFFSET DGROUP:parfilename
	mov	si,OFFSET DGROUP:qlk_buffer
	rep	movsw

	pop	exe_ext_ptr			; restore exe name info
	pop	WORD PTR exe_name	; restore exe_name to original null status if modified
	pop	es					; restore critical registers
	pop	di
	pop	si
	pop	cx
	ret
is_qlk_file	ENDP

;*****************************
;* CHECK_QLK                 *
;*****************************

; check if QLK file exists, if not create
; if exists, open
; destroys ax,bx,cx,dx,si,di,es

check_qlk	PROC
	mov	bx,OFFSET DGROUP:qlk_text	; bx -> proper extension
    mov di,OFFSET DGROUP:qlk_name   ; di -> place to put ILF name
	mov	dx,di				; ds:dx -> filespec
    call    get_xxx_name    ; get QLK name
    mov ax,3d02h            ; open file for reading/writing
    int 21h
    call    restore_ems_map
	mov	dx,1				; flag reading from qlk file, do NOT change flags
    jnc cq_qlk_found        ; file found okay

; QLK does not exist, build a new one
cq_build:
    mov dx,OFFSET DGROUP:qlk_name	; ds:dx -> filespec
    xor cx,cx				; normal file
    mov ah,3ch				; create or truncate file
    int 21h
    call    restore_ems_map
	mov	dx,100h				; flag writing to qlk file, do NOT change flags
	jc	to_doserr			; error occured

; QLK file exists
; ax holds file handle of QLK file
cq_qlk_found:
    mov qlk_handle,ax       ; save handle of QLK file
	mov	writing_qlk_flag,dh	; set qlk read/write flags
	mov	reading_qlk_flag,dl
	ret

to_doserr:
    jmp NEAR PTR dos_error	; dos error

check_qlk	ENDP

;*****************************
;* READ_QLK_LIBNAME          *
;*****************************

; read the QuickLinker file for library name
; transfer to filename variable
; returns carry flag set if no more library names
; destroys ax,bx,cx,dx,si,di,es

read_qlk_libname	PROC
	xor	al,al
	mov	no_qlk_modules,al	; init no library modules flag
	mov	bx,qlk_handle
	mov	dx,qlk_readpos
	xor	cx,cx
    mov ah,42h				; move file pointer, offset from file start
    int 21h

	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	cx,131
	mov	ah,3fh				; read file
	int	21h
    call    restore_ems_map
	jc	rql_doserr			; error occured

	cmp	WORD PTR qlk_buffer,0ff40h	; see if library name
	je	rql_islib			; yes

; no more libraries
rql_fail:
	mov	qlk_buffpos,0		; re-init qlk read/write buffer position
	stc						; flag failure
	ret

rql_islib:
	add	qlk_readpos,2			; adjust for prepended flag word

; transfer file name
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:filename
	mov	si,OFFSET DGROUP:qlk_buffer+2	; point past flag byte

rql_transloop:
	movsb
	inc	qlk_readpos
	cmp	BYTE PTR [si-1],0	; see if null terminator transferred
	jne	rql_transloop

; at library system info flag byte or start of next entry
	mov	ax,[si]				; get next entry value, if is next

; this could be the start of another library, or unresolved externals
; if there were no modules used from the library
; check for 0ff10h or 0ff40h word value
	cmp	ax,0ff10h			; check for unresolveds
	je	rql_scan			; yes
	cmp	ax,0ff40h			; check for next library
	jne	rql_update			; not next library
	mov	no_qlk_modules,al	; flag that no library modules for library
	jmp	SHORT rql_scan

rql_update:
	mov	ax,[si+1]			; get library page size
	inc	qlk_readpos			; adjust for prepended flag byte
	mov	lib_page_size,ax	; save it
	add	qlk_readpos,2		; adjust for library page size

; scan to start of module information in qlk file
rql_scan:
	mov	bx,qlk_handle
	mov	dx,qlk_readpos
	xor	cx,cx
    mov ax,4200h			; move file pointer, offset from file start
    int 21h

	mov	qlk_buffpos,300		; init buffer position to force read
	clc						; flag success
	ret

rql_doserr:
	jmp	NEAR PTR qlk_doserr

read_qlk_libname	ENDP

;*****************************
;* READ_QLK_MOD_INFO         *
;*****************************

; read the QuickLinker file for module info
; updates current_lib,lib_page_num
; returns carry flag set if no more modules, reset and variables updated otherwise
; destroys ax,bx,cx,dx

read_qlk_mod_info	PROC
	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_buffpos
	cmp	bx,293				; see if need to read in info
	jb	rqm_2				; no

	mov	bx,qlk_handle
	mov	dx,qlk_readpos
	xor	cx,cx
    mov ax,4200h			; move file pointer, offset from file start
    int 21h

	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	cx,300
	mov	ah,3fh				; read file
	int	21h
    call    restore_ems_map
	jc	rqm_doserr			; error occured

	xor	bx,bx
	mov	qlk_buffpos,bx		; init buffer position

rqm_2:
	xor	ax,ax				; pre-set no module info indicator
	add	bx,dx
	mov	obj_ovl_flag,al		; init variables
	mov	nonovl_rvect,al
	mov	libmod_obj_flag,al
	mov	al,[bx]				; get module info flag
	test	al,20h			; see if module info flag
	je	rqm_fail			; not module info, fail

; set appropriate flags for module
	test	al,4			; see if overlaid
	je	rqm_4				; no
	mov	obj_ovl_flag,0c0h	; flag overlaid

	test	al,2			; see if root vectoring
	jne	rqm_4				; no
	mov	nonovl_rvect,40h	; flag root vectoring

rqm_4:
	and	al,1				; see if module
	mov	module_flag,al		; flag module

	mov	ax,7
	add	qlk_buffpos,ax		; adjust buffer position
	add	qlk_readpos,ax		; adjust file position
	mov	ax,[bx+3]			; get current library number
	mov	current_lib,ax		; update variable
	mov	ax,[bx+1]			; get library page
	mov	lib_page_num,ax
	mov	ax,[bx+5]			; get # of bytes to read for file
	mov	lib_read_amt,ax
	clc						; flag success
	ret

rqm_fail:
	stc						; flag failure
	ret

rqm_doserr:
	jmp	NEAR PTR qlk_doserr

read_qlk_mod_info	ENDP

;*****************************
;* READ_QLK_UNRES            *
;*****************************

; read the QuickLinker file for unresolved externals
; if current unresolved > saved unresolved change reading qlk flag to writing
; destroys ax,bx,cx,dx

read_qlk_unres	PROC
	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_buffpos
	cmp	bx,296				; see if need to read in info
	jb	rqu_2				; no

	mov	bx,qlk_handle
	mov	dx,qlk_readpos
	xor	cx,cx
    mov ax,4200h			; move file pointer, offset from file start
    int 21h

	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_handle
	mov	cx,4
	mov	ah,3fh				; read file
	int	21h
    call    restore_ems_map
	jc	rqu_doserr			; error occured

	xor	bx,bx

rqu_2:
	add	bx,dx
	mov	ax,WORD PTR tot_sym_count
	sub	ax,WORD PTR pub_sym_count
	cmp	ax,[bx+2]			; check current unresolved count against saved
	ja	rqu_fail
	ret

; more unresolved externals than previously, append new lib information
; scan to start of unresolved info (end of module info) in qlk file
rqu_fail:
	mov	bx,qlk_handle
	mov	dx,qlk_readpos
	xor	cx,cx
    mov ax,4200h			; move file pointer, offset from file start
    int 21h
	mov	reading_qlk_flag,0	; change status of reading flags to writing
	mov	writing_qlk_flag,1
	ret

rqu_doserr:
	jmp	NEAR PTR qlk_doserr

read_qlk_unres	ENDP

;*****************************
;* WRITE_QLK_LIBNAME         *
;*****************************

; write library name to QuickLinker file
; destroys ax,bx,cx,dx

write_qlk_libname	PROC
	mov	bx,qlk_buffpos
	mov	dx,OFFSET DGROUP:qlk_buffer
	or	bx,bx				; see if module info in buffer to flush
	je	wql_2				; no

	call	flush_qlk_buffer	; flush remaining module info bytes

; write library word flag (0FF40h)
wql_2:
	mov	bx,qlk_handle
	mov	WORD PTR qlk_buffer,0ff40h
	mov	cx,2
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	wql_doserr			; error occured

; write library name
	mov	ax,OFFSET DGROUP:filename
	mov	dx,ax
	xchg	bx,ax			; bx -> filename, ax==file handle
	xor	cx,cx				; cx holds char count of filename

wql_loop:
	inc	cx
	cmp	BYTE PTR [bx],0		; see if at null terminator
	je	wql_3				; yes
	inc	bx
	jmp	SHORT wql_loop

wql_3:
	mov	bx,ax				; bx==file handle, cx==byte count, dx->filename
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	wql_doserr			; error occured
	ret

wql_doserr:
	jmp	NEAR PTR qlk_doserr

write_qlk_libname	ENDP

;*****************************
;* WRITE_QLK_LIB_INFO        *
;*****************************

; write library info to QuickLinker file
; destroys bx,cx,dx

write_qlk_lib_info	PROC
	push	ax				; save critical register
	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_handle

; write lib info byte flag (8)
	mov	qlk_buffer,8
	mov	cx,1
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	wqi_doserr			; error occured

; write library page size
	mov	ax,lib_page_size
	mov	WORD PTR qlk_buffer,ax
	mov	cx,2
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	wqi_doserr			; error occured
	pop	ax					; restore critical register
	ret

wqi_doserr:
	jmp	NEAR PTR qlk_doserr

write_qlk_lib_info	ENDP

;*****************************
;* WRITE_QLK_MOD_INFO        *
;*****************************

; write module info to QuickLinker file
; upon entry ax holds module number
; destroys cx,dx

write_qlk_mod_info	PROC
	push	bx
	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_buffpos
	cmp	bx,293				; see if at end of buffer
	jb	wqm_2				; no

	call	flush_qlk_buffer	; flush module info in buffer
	xor	bx,bx				; re-init buffer position

; write library module flag
wqm_2:
	add	bx,dx				; bx -> start of qlk buffer
	mov	dx,20h
	cmp	obj_ovl_flag,dh		; see if overlaid
	je	wqm_4				; no
	or	dl,4				; flag overlaid

	cmp	nonovl_rvect,dh		; see if root vectoring
	jne	wqm_4				; no
	or	dl,2				; flag root vectoring

wqm_4:
	cmp	module_flag,dh		; see if module
	je	wqm_5				; no
	inc	dx					; flag module (set bit 0 of dl)

wqm_5:
	mov	BYTE PTR [bx],dl	; set flag status
	mov	[bx+1],ax
	mov	dx,current_lib
	mov	[bx+3],dx
	add	qlk_buffpos,7		; adjust buffer position
	pop	bx
	ret

wqm_doserr:
	jmp	NEAR PTR qlk_doserr

write_qlk_mod_info	ENDP

;*****************************
;* WRITE_QLK_MODSIZE         *
;*****************************

; write library module size or zero if buffer overflow to
; QLK file read/write buffer
; upon entry si+file_pos_adj-lib_pos_adj is size of module
; destroys ax,bx,dx

write_qlk_modsize	PROC
	mov	bx,OFFSET DGROUP:qlk_buffer-2
	add	bx,qlk_buffpos		; bx -> library module size word slot
	mov	ax,WORD PTR file_pos_adj
	mov	dx,WORD PTR file_pos_adj+2
    sub ax,WORD PTR lib_pos_adj ; adjust for library module offset
    sbb dx,WORD PTR lib_pos_adj+2
	or	ax,dx
	jne	wqm_overflow		; buffer overflowed, use default read
	mov	ax,si
	dec	ax
	add	ax,lib_page_size
	jc	wqm_overflow
	and	al,0f0h
	mov	[bx],ax
	ret

; buffer overflowed reading library module
wqm_overflow:
	mov	ax,buffer_end		; read up to amount in buffer
	mov	[bx],ax
	ret
write_qlk_modsize	ENDP

;*****************************
;* FLUSH_QLK_BUFFER          *
;*****************************

; flush qlk buffer of module info
; upon entry bx == number of bytes to write, dx -> buffer
; destroys bx,cx

flush_qlk_buffer	PROC
	push	ax				; save module number
	mov	cx,bx				; get count of bytes to write
	mov	bx,qlk_handle
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	wqm_doserr			; error occured
	pop	ax					; restore module number
	mov	qlk_buffpos,0		; init qlk read/write buffer position
	ret
flush_qlk_buffer	ENDP

;*****************************
;* WRITE_QLK_UNRES           *
;*****************************

; write unresolved externals to the QuickLinker file
; destroys ax,bx,cx,dx

write_qlk_unres	PROC
	mov	dx,OFFSET DGROUP:qlk_buffer
	mov	bx,qlk_buffpos
	or	bx,bx				; see if pending module info to write
	je	wqu_2				; no
	call	flush_qlk_buffer	; flush remaining module info bytes

wqu_2:
	mov	bx,qlk_handle

; write unresolved word flag (0FF10h)
	mov	WORD PTR qlk_buffer,0ff10h
	mov	cx,2
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	qlk_doserr			; error occured

; write count of unresolved externals
	mov	ax,WORD PTR tot_sym_count
	sub	ax,WORD PTR pub_sym_count
	mov	WORD PTR qlk_buffer,ax
	mov	cx,2
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	qlk_doserr			; error occured

; write end byte flag (80h)
	mov	qlk_buffer,80h
	mov	cx,1
	mov	ah,40h				; write to file
	int	21h
    call    restore_ems_map
	jc	qlk_doserr			; error occured
	ret

qlk_doserr:
	mov	dx,OFFSET DGROUP:filename
    jmp NEAR PTR dos_error	; dos error

write_qlk_unres	ENDP

;*****************************
;* DELETE_QLK_FILE           *
;*****************************

; close and delete the qlk file if it exists and writing to it
; destroys ax,bx,cx,dx

delete_qlk_file PROC
	cmp	writing_qlk_flag,0	; see if writing to qlk file
	je	dqf_ret				; no, reading, okay to keep

    mov bx,qlk_handle
    or  bx,bx               ; see if file exists
    je  dqf_ret             ; no
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

    mov dx,OFFSET DGROUP:qlk_name
    mov ah,41h              ; delete file
    int 21h
    xor ax,ax               ; zero out file handle so no further deletion attempts
    mov qlk_handle,ax

dqf_ret:
    ret
delete_qlk_file ENDP

END
