;*********************************************************************
;*   MLPAROVL.ASM                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          11/05/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   Parse linker overlay options                                    *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlparovl
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
     
PUBLIC  parse_ovl_option,save_ovl_name

;*****************************
;* Data begins               *
;*****************************
     
.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   def_lib_flag:BYTE
EXTRN   exe_pathspec:BYTE

; initialized local variables

; byte values
EVEN
is_ovl_filename DB  0       ; nonzero if overlay file name specified

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
EXTRN   link_error:NEAR,dos_error:NEAR,alloc_memory:NEAR
EXTRN	is_terminator:NEAR

;*****************************
;* PARSE_OVL_OPTION          *
;*****************************

; parse overlay option
; ds:si -> second  char of option (past 'O')
; cx -> start of option
; destroys ax,dx,es
; updates si

parse_ovl_option    PROC
    mov ax,ds
    mov es,ax               ; es -> warplink data
    lodsb                   ; get overlay option char
    cmp al,'A'              ; check bounds
	jae	ovl_boundchk		; okay
	jmp	NEAR PTR ovl_bad_option	; too low

ovl_boundchk:
    cmp al,'z'              ; check bounds
    ja  ovl_bad1            ; too high

ovl_toupper:
    and al,0dfh             ; force char to uppercase

IFNDEF DEMO
    cmp al,'I'              ; see if internal overlays
    jne pop_chkumb          ; no
    mov is_internal,al      ; set internal overlays flag
    ret
ENDIF

pop_chkumb:
    cmp al,'U'              ; see if UMB use
    jne pop_swap			; no
    mov is_umb,al      		; set UMB flag
    ret

pop_swap:
    cmp al,'H'              ; see if overlay file stashing
    jne pop_stash           ; no
    lodsb                   ; get third option char
    and al,0dfh             ; force char to uppercase
    cmp al,'T'              ; see if extended file stashing
    jne pop_ohp             ; no
    jmp NEAR PTR ovl_get_oht	; yes

pop_ohp:
    cmp al,'P'              ; see if expanded filestashing
    jne ovl_bad1            ; no, bad option
    jmp NEAR PTR ovl_get_ohp	; yes

pop_stash:
    cmp al,'R'              ; see if active overlay stashing
    jne pop_chkems          ; no
    lodsb                   ; get third option char
    and al,0dfh             ; force char to uppercase
    cmp al,'T'              ; see if extended stashing
    jne pop_orp             ; no
    mov is_xtstash,al       ; flag extended memory stashing
    ret

pop_orp:
    cmp al,'P'              ; see if expanded stashing
    jne ovl_bad1            ; no, bad option
    mov is_xpstash,al
    ret

pop_chkems:
    cmp al,'X'              ; see if overlay pool in EMS
    jne eat_colon           ; no
    mov is_ems_ovlpool,al   ; set overlay pool in EMS flag
    mov ah,[si]             ; peek at next char
    cmp ah,':'              ; see if colon (e-var setting)
    je  eat_colon           ; yes
    ret

eat_colon:
    mov ah,al               ; save option in ah
    lodsb                   ; get colon following overlay option char
    cmp al,':'              ; must be colon
    jne ovl_bad1            ; not a colon

; check for valid option char in ah
ovl_check_valid:
IFNDEF DEMO
    cmp ah,'C'              ; check if overlay class option
    je  ovl_get_class       ; yes

    cmp ah,'L'              ; check if max overlay loaded count option
    jne pop_1               ; no
    jmp NEAR PTR ovl_get_load   ; yes
ENDIF

pop_1:
    cmp ah,'P'              ; check if overlay pool size option
    jne pop_2               ; no
    jmp NEAR PTR ovl_get_pool   ; yes

pop_2:
IFNDEF DEMO
    cmp ah,'S'              ; check if overlay stack size option
    je  ovl_get_stack       ; yes

pop_3:
    cmp ah,'N'              ; check if overlay file name
    jne pop_ox              ; no
    jmp NEAR PTR ovl_get_name   ; get overlay file name
ENDIF

pop_ox:
    cmp ah,'X'              ; check if /ox environment variable
    jne ovl_bad1            ; no, no more valid options
    jmp NEAR PTR ovl_get_evar   ; get ox environment variable

ovl_bad1:                   ; bad overlay option specified
	jmp	NEAR PTR ovl_bad_option

IFNDEF DEMO
; overlay stack size option
ovl_get_stack:
    xor dx,dx               ; dx == multiplier

ogs_2:
    lodsb                   ; get number
    cmp al,'0'              ; see nondigit
    jb  ogs_done            ; yes, done
    cmp al,'9'              ; see nondigit
    ja  ogs_done            ; yes

    and al,0fh              ; change from ASCII to actual value
    mov dh,al               ; save value
    mov al,dl               ; get pre-existing value
    mov ah,10
    mul ah                  ; multiply pre-existing value by 10 (shift placeholder)
    xchg    dx,ax           ; save shifted value back to dx, get new value in ah
    mov al,ah               ; get value in low byte
    xor ah,ah               ; zap high byte
    add dx,ax               ; add in new value
    or  dh,dh               ; see if value too large
    je  ogs_2               ; no, get next digit
    jmp NEAR PTR ovl_bad_option ; yes

ogs_done:
    cmp dx,63               ; check if absolute maximum exceeded
    ja  ovl_bad1            ; yes
    or  dx,dx               ; value cannot be zero
    je  ovl_bad1
    mov dh,dl
    xor dl,dl               ; effective multiply by 256
    shl dh,1                ; x512
    shl dh,1                ; x1024, convert stack size to kilobytes
    mov ovl_stack,dx        ; save stack size
    dec si                  ; drop si back to -> nondigit char
    ret                     ; done

; overlay class option
ovl_get_class:
    mov di,OFFSET DGROUP:ovl_class  ; es:di -> place for overlay class name
    mov al,[si]             ; peek ahead at next char
    cmp al,'.'              ; see if a period (exact match indicator)
    jne ogc_2               ; no
    lodsb                   ; gobble period
    mov exact_ovl_class,al  ; set exact class match flag

ogc_2:
    lodsb                   ; get name char
	call	is_terminator	; see if name parse terminator
    je 	ogc_done            ; yes, done

    cmp al,'a'              ; see if lowercase
    jb  ogc_3               ; no
    cmp al,'z'
    ja  ogc_3
    sub al,20h              ; change lowercase to uppercase

ogc_3:
    stosb                   ; name char, transfer it
    jmp SHORT ogc_2         ; get next char

ogc_done:
    dec si                  ; drop si back to -> terminating char
    xor al,al               ; null terminate string
    stosb                   ; transfer it
    ret                     ; done

; maximum loaded overlay count option
ovl_get_load:
    xor dx,dx               ; dx == multiplier

ogl_2:
    lodsb                   ; get number
    cmp al,'0'              ; see nondigit
    jb  ogl_done            ; yes, done
    cmp al,'9'              ; see nondigit
    ja  ogl_done            ; yes

    and al,0fh              ; change from ASCII to actual value
    or  dh,dh               ; see if value too large for shift
    jne ovl_bad2            ; yes
    mov dh,al               ; save value
    mov al,dl               ; get pre-existing value
    mov ah,10
    mul ah                  ; multiply pre-existing value by 10 (shift placeholder)
    xchg    dx,ax           ; save shifted value back to dx, get new value in ah
    mov al,ah               ; get value in low byte
    xor ah,ah               ; zap high byte
    add dx,ax               ; add in new value
    jmp SHORT ogl_2         ; get next digit

ogl_done:
    cmp dx,512              ; check if absolute maximum exceeded
    ja  ovl_bad2            ; yes
    or  dx,dx               ; value cannot be zero
    je  ovl_bad2
    mov ovl_max_load,dx     ; save maximum load count
    shl dx,1                ; x2
    shl dx,1                ; x4
    shl dx,1                ; x8
    shl dx,1                ; x16, size of load array (count*16)
    mov ovl_max_load_size,dx
    dec si                  ; drop si back to -> nondigit char
    ret                     ; done

ovl_bad2:                   ; bad overlay option specified
	jmp	NEAR PTR ovl_bad_option

; overlay file name
ovl_get_name:
    mov di,OFFSET DGROUP:ovl_nopath ; es:di -> place for overlay file name, without path
    mov is_ovl_filename,1   ; set flag to indicate overlay file name indicated

ogn_2:
    lodsb                   ; get name char
	call	is_terminator	; see if file name parse terminator
    je	ogn_done            ; yes, done

    cmp al,'\'              ; see if backslash, directory specifier, filename no good
    je  ovl_bad_option      ; yes, abort
    cmp al,'.'              ; see if extension was specified
    jne ogn_3               ; not yet
    mov is_ovl_filename,al  ; set flag to '.' value to indicate extension specified

ogn_3:
    stosb                   ; name char, transfer it
    jmp SHORT ogn_2         ; get next char
ENDIF

ogn_done:
    dec si                  ; drop si back to -> terminating char
    xor al,al               ; null terminate string
    stosb                   ; transfer it
    ret                     ; done

; /ox environment variable
ovl_get_evar:
    mov di,OFFSET DGROUP:ovl_ox_evar    ; es:di -> place for overlay file name, without path
oge_loop:
    lodsb                   ; get name char
    cmp al,' '              ; see if whitespace char
    jbe ogn_done            ; yes, done
    stosb                   ; transfer e-var setting
    cmp di,OFFSET DGROUP:ovl_ox_evar+32 ; see if overflowed maximum of 31 bytes
    jae ovl_bad_option      ; yes
    jmp SHORT oge_loop

; overlay pool size option
ovl_get_pool:
    xor dx,dx               ; dx == multiplier
    mov al,[si]             ; peek at next char

    cmp al,'M'              ; check if minimum pool size option
    je  ogp_minpool         ; yes
    cmp al,'m'              ; check if lowercase option
    jne ogp_notmin          ; no

ogp_minpool:
    lodsb                   ; gobble 'M'
    mov is_min_pool,al      ; set minimum pool size flag
    mov ovl_mem_alloc,al    ; flag to allocate specified memory for pool, not free memory
    ret

ogp_notmin:
    cmp al,'-'              ; see if minus, specified amount is free memory, not pool memory
    je  ogp_2               ; yes
    mov ovl_mem_alloc,al    ; flag to allocate specified memory for pool, not free memory
    jmp SHORT ogp_3         ; bypass '-' code

; - prepended to number, gobble it
ogp_2:
    lodsb                   ; gobble sign

ogp_3:
    lodsb                   ; get number
    cmp al,'0'              ; see nondigit
    jb  ogp_done            ; yes, done
    cmp al,'9'              ; see nondigit
    ja  ogp_done            ; yes

    and al,0fh              ; change from ASCII to actual value
    or  dh,dh               ; see if value too large for shift
    jne ovl_bad_option      ; yes
    mov dh,al               ; save value
    mov al,dl               ; get pre-existing value
    mov ah,10
    mul ah                  ; multiply pre-existing value by 10 (shift placeholder)
    xchg    dx,ax           ; save shifted value back to dx, get new value in ah
    mov al,ah               ; get value in low byte
    xor ah,ah               ; zap high byte
    add dx,ax               ; add in new value
    jmp SHORT ogp_3         ; get next digit

ogp_done:
    cmp dx,512              ; check if absolute maximum exceeded
    ja  ovl_bad_option      ; yes
    or  dx,dx               ; value cannot be zero
    je  ovl_bad_option

; convert memory allocated/free size to kilobytes
    shl dx,1                ; x2
    shl dx,1                ; x4
    mov WORD PTR ovl_pool+1,dx  ; store offset by one byte does effective multiply by 256
    dec si                  ; drop si back to -> nondigit char

pop_ret:
    ret

ovl_bad_option:             ; bad overlay option specified
    mov ax,BAD_OPTION_ERR   ; error value
    jmp NEAR PTR link_error ; transfer control to error handler

ovl_get_oht:
	call	parse_ohx_value	; size in ax, zero flag set if minus value
	mov	ovl_oht_size,ax
	mov	is_oht,1			; flag /oht setting
	je	pop_ret				; minus found
	mov	ovl_oht_alloc,1		; set allocation flag
	ret

ovl_get_ohp:
	mov	al,ds:[si]			; peek at following char
IFNDEF DEMO
	cmp	al,'3'				; see if explicit EMS 3.0 compatibility flag
	jne	ovl_parse_ohp		; no
	lodsb					; gobble the 3
	mov	ems3_flag,1			; set EMS 3.0 compatibility mode
ENDIF

ovl_parse_ohp:
	call	parse_ohx_value	; size in ax, zero flag set if minus value
	mov	ovl_ohp_size,ax
	mov	is_ohp,1			; flag /ohp setting
	je	pop_ret				; minus found
	mov	ovl_ohp_alloc,1		; set allocation flag
	ret

parse_ovl_option    ENDP

;*****************************
;* PARSE_OHX_VALUE           *
;*****************************

; parse out the value for /oht or /ohp
; returns zero flag set if '-' present, reset if no minus (no minus means allocate to, minus means leave free)
; returns size in ax
; updates si -> char after value
; destroys ax,dx

parse_ohx_value	PROC
	push	bx				; save critical register
    xor bx,bx               ; bx == value
	lodsb					; get colon
	cmp	al,':'				; make sure is colon
    jne pov_bad_option      ; bad option format
	mov	al,ds:[si]			; peek ahead at number or sign
    cmp al,'-'              ; see if minus, specified amount is free memory, not allocated
	pushf					; save flag status of minus compare
    jne pov_3               ; no
    lodsb                   ; gobble '-' char

pov_3:
    lodsb                   ; get number
    cmp al,'0'              ; see nondigit
    jb  pov_done            ; yes, done
    cmp al,'9'              ; see nondigit
    ja  pov_done            ; yes

    and al,0fh              ; change from ASCII to actual value
	test	bh,80h			; see if value too large
    jne pov_bad_option      ; yes
	xchg	ax,bx			; previous value in ax, ones digit in bl
	mov	dx,10
	mul	dx					; 10x previous in ax
	or	dx,dx				; see if any overflow to high word
	jne	pov_bad_option		; yes
	xor	bh,bh
	add	ax,bx				; 10x previous+new ones digit in ax
	mov	bx,ax				; update value storage
    jmp SHORT pov_3         ; get next digit

pov_done:
    dec si                  ; drop si back to -> nondigit char
    cmp bx,16383            ; check if absolute maximum exceeded
    ja  pov_bad_option      ; yes
    or  bx,bx               ; value cannot be zero
    je  pov_bad_option

	popf					; restore flag status of compare
	mov	ax,bx				; get size in ax
	pop	bx					; restore critical register
	ret

; bad size value given for /ohp, /oht
pov_bad_option:
	jmp	NEAR PTR ovl_bad_option

parse_ohx_value	ENDP

;*****************************
;* SAVE_OVL_NAME             *
;*****************************

; save overlay file name with .OVL extension
; destroys ax,bx,si,di,es

save_ovl_name   PROC
    mov ax,ds
    mov es,ax               ; es -> WarpLink data
    mov di,OFFSET DGROUP:ovl_filename   ; point to destination
    cmp is_ovl_filename,0   ; see if overlay file name specified
    jne sov_2               ; yes          
    mov si,OFFSET DGROUP:exe_name   ; point to filename to transfer to global string
    jmp SHORT ovl_loop      ; no

; explicitly set overlay file name
sov_2:
    mov si,OFFSET DGROUP:exe_pathspec

sov_pathloop:
    cmp BYTE PTR [si],0     ; see if done putting in path spec to exe file
    je  sov_3               ; yes
    movsb                   ; transfer path spec before overlay file name
    jmp SHORT sov_pathloop  ; loop until complete

sov_3:
    mov si,OFFSET DGROUP:ovl_nopath ; point to filename to transfer

ovl_loop:
    movsb                   ; transfer a char
    cmp is_ovl_filename,'.' ; see if overlay file extension was supplied
    je  sov_4               ; yes, bypass period check
    cmp BYTE PTR [si-1],'.' ; non-path period signals end of transfer
    jne sov_4
    cmp BYTE PTR [si],'.'   ; two periods indicate pathspec
    je  sov_4
    cmp BYTE PTR [si],'\'   ; period+backslash indicates pathspec
    jne sov_addext          ; real period extension, add .OVL extension

sov_4:
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne ovl_loop            ; not zero, keep transferring
    cmp is_ovl_filename,'.' ; see if overlay file extension was supplied
    jne sov_addext          ; no, add default extension
    ret                     ; yes, done with overlay filename setup

sov_addext:
    mov BYTE PTR [di-1],'.' ; add period
    mov BYTE PTR [di],'O'   ; add OVL extension
    mov BYTE PTR [di+1],'V'
    mov BYTE PTR [di+2],'L'
    mov BYTE PTR [di+3],0   ; null terminate string

    mov di,OFFSET DGROUP:ovl_nopath ; point to destination
    mov si,OFFSET DGROUP:ovl_filename   ; point to filename to transfer to global string

; find zero terminator in overlay name and then back up until
; hit beginning or '\' char
    xor bx,bx

sov_endloop:
    lodsb                   ; get char
    or  al,al
    je  sov_past_null       ; zero, found terminator
    inc bx                  ; bump count of chars in name
    jmp SHORT sov_endloop   ; loop for next char

; shot past null terminator, back si up to -> null terminator
sov_past_null:
    dec si

sov_at_end:
    dec si                  ; back up one char in name
    dec bx                  ; drop count of chars in name
    je  ovl_loop2           ; no more chars in name, at beginning
    mov al,[si]             ; get char in name
    cmp al,'\'              ; see if backslash, directory indicator
    jne sov_at_end          ; no, keep looking
    inc si                  ; point si past backslash in name

ovl_loop2:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; zero byte signals end of transfer
    jne ovl_loop2           ; not zero, keep tranferring

    ret
save_ovl_name   ENDP

END
