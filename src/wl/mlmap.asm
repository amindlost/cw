;*********************************************************************
;*   MLMAP.ASM                                                       *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          12/21/92                                         *
;*   Model:         Small                                            *
;*   Version:       2.5                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 2.x+                                      *
;*                                                                   *
;*   map file code                                                   *
;*                                                                   *
;*********************************************************************

TITLE   WARPLINK mlmap
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
     
PUBLIC  init_map,finish_map,map_segments,map_groups
PUBLIC  map_detail_seg

;*****************************
;* Data begins               *
;*****************************

.DATA

;*****************************
;* External declarations     *
;*****************************

; variables
EXTRN   segment_start:DWORD,segment_stop:DWORD,true_seg_len:DWORD
EXTRN   filename:BYTE

; initialized local variables

; byte values
EVEN
detail_flag DB  0           ; nonzero if not calling map_detail_seg for first time
EVEN
comm_loop_flag  DB  0       ; nonzero if publics written to map are in communal (vs public) declarations block

.DATA?

; uninitialized local variables

; word values
map_seg_ovl DW  ?           ; nonzero if mapping overlaid segment, holds overlay id

; byte buffer
EVEN
map_text_buffer DB  700 DUP (?) ; space for buffered line in map file

;*****************************
;* Constant data             *
;*****************************

.CONST

EVEN
mhtext_len      DB  mhtext_stop-map_header_text
map_header_text DB  CR,LF,'Start  Stop   Length Name                   Class',CR,LF
mhtext_stop =   $

prog_text       DB  CR,LF,'PROGRAM: '

date_text       DB  CR,LF,'DATE:    '

time_text       DB  CR,LF,'TIME:    '

metext_len      DB  metext_stop-map_entry_text
map_entry_text  DB  CR,LF,CR,LF,'Program entry point at '
map_entry_value DB  '0000:0000',CR,LF
metext_stop =   $

mgtext_len      DB  mgtext_stop-map_group_text
map_group_text  DB  CR,LF,'Origin   Group'
mgtext_stop =   $

msegtext_len      DB  msegtext_stop-map_detseg_text
map_detseg_text   DB  CR,LF,CR,LF,'Detailed Segment Map',CR,LF
                  DB  'Name             Ovl# Address   Length Align Combine Class    Group     Module       File',CR,LF
msegtext_stop   =   $

maddrtext_len   DB  maddrtext_stop-maddr_text
maddr_text      DB  CR,LF,' Address   Status   Symbol Name'
maddrtext_stop  =   $

mpub_ovl    DB  '  Ovl      '   ; overlaid public
mpub_abs    DB  '  Abs      '   ; absolute public
mpub_unres  DB  '  Unres    '   ; unresolved public
mpub_comm   DB  '  Comm     '   ; communal variable
mpub_res    DB  '  Res      '   ; resolved, resident public

align_byte  DB  'BYTE  '
align_word  DB  'WORD  '
align_para  DB  'PARA  '
align_page  DB  'PAGE  '

comb_priv   DB  'PRIVATE '
comb_pub    DB  'PUBLIC  '
comb_stack  DB  'STACK   '
comb_com    DB  'COMMON  '

;*****************************
;* Code begins               *
;*****************************

.CODE

;*****************************
;* External declarations     *
;*****************************

EXTRN   link_error:NEAR,dos_error:NEAR
EXTRN   restore_ems_map:NEAR

;*****************************
;* INIT_MAP                  *
;*****************************

; open map file and write header
; destroys ax,bx,cx,dx,es

init_map    PROC
    cmp is_mapfile,0        ; see if map file to be created
    jne im_1                ; yes
    ret

im_1:
    mov dx,OFFSET DGROUP:map_name
    xor cx,cx               ; normal file
    mov ah,3ch              ; create or truncate file
    call    NEAR PTR im_share2

    mov map_handle,ax       ; save handle of map file

; get date
    push    ds
    pop es
    mov di,OFFSET DGROUP:map_text_buffer    ; es:di -> buffer
    mov ah,2Ah              ;DOS function to get date
    int 21h
    xchg    ax,cx           ;CL=day-of-week (AX=year)
    xchg    ax,dx           ;DX=year; AX=month and day
    mov bx,ax
    mov al,bh               ;AL = month
    aam                     ;AX = month in BCD format
    add ax,"00"             ;AX = month in decimal digits
    xchg    al,ah
    stosw                   ;write month to string
    mov al,"/"
    stosb                   ;write "/" to string
    xchg    ax,bx           ;AL = day
    aam                     ;AX = day in BCD format
    add ax,"00"             ;AX = day in decimal digits
    xchg    al,ah
    stosw                   ;write day to string
    mov al,"/"
    stosb                   ;write "/" to string
    xchg    ax,dx           ;AX = year
    mov dl,100
    div dl                  ;DL = last two digits of year
    mov al,ah               ;AL = last two digits of year
    aam                     ;AX = year in BCD format
    add ax,"00"             ;AX = year in decimal digits
    xchg    al,ah
    stosw                   ;write year to string

; write program name
    mov bx,map_handle
    mov dx,OFFSET DGROUP:prog_text
    mov cx,11
    call    NEAR PTR im_sharerout

    mov di,OFFSET DGROUP:exe_name
im_nameloop:
    cmp BYTE PTR [di],0
    je  im_writedate
    mov dx,di
    mov cl,1
    call    NEAR PTR im_sharerout
    inc di
    jmp SHORT im_nameloop

; write date
im_writedate:
    mov dx,OFFSET DGROUP:date_text
    mov cl,11
    call    NEAR PTR im_sharerout

    mov dx,OFFSET DGROUP:map_text_buffer
    mov cl,8
    call    NEAR PTR im_sharerout

; get time
    mov di,OFFSET DGROUP:map_text_buffer    ; es:di -> buffer
    mov ah,2Ch              ;dos function to get time
    int 21h                 ;returns time in CX:DX
    xchg    ax,dx           ;move time to DX:AX...
    mov dx,cx
    mov al,dh               ;Al = hours (24 hr clock)
    mov dh,"p"              ;set DH for pm
    cmp al,12               ;is it pm?
    jae asc_time_100        ;  y: leave DH setup for pm
    mov dh,"a"              ;  n: set DH for am
asc_time_100:
    cmp al,13               ;is time 1pm or later?
    jb  asc_time_120        ;  n: skip 24 to 12 hr clock adjust
    sub al,12               ;AL = hours (12 hr clock)
asc_time_120:
    or  al,al               ;is time 12 am?
    jne asc_time_140        ;  n: skip 12am adjust
    mov al,12               ;  y: make hour 0 = 12am
asc_time_140:
    aam                     ;AX = hours in BCD format
    add ax,"00"             ;AX = hours in decimal digits
    xchg    al,ah
    stosw                   ;write hours to string
    mov al,":"
    stosb                   ;write ":" to string
    mov al,dl               ;AL = minutes
    aam                     ;AX = minutes in BCD format
    add ax,"00"             ;AX = minutes in decimal digits
    xchg    al,ah
    stosw                   ;write minutes to string
    mov al," "
    stosb                   ;write space character to string
    mov ah,"m"
    mov al,dh               ;AX = am/pm indicator
    stosw                   ;write am/pm indicator

; write time
    mov dx,OFFSET DGROUP:time_text
    mov cx,11
    call    NEAR PTR im_sharerout

    mov dx,OFFSET DGROUP:map_text_buffer
    mov cl,8
    call    NEAR PTR im_sharerout

; write segment header
    mov dx,OFFSET DGROUP:map_header_text
    mov cl,2                ; write CR/LF, fall through and write full text
    call    NEAR PTR im_sharerout

    mov cl,mhtext_len

im_sharerout:
    mov ah,40h              ; write to file

im_share2:
    int 21h
    call    restore_ems_map
    jc  im_error

    ret

im_error:
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

init_map    ENDP

;*****************************
;* MAP_SEGMENTS              *
;*****************************

; show map of segments
; called for each segment
; prints segment start, stop, length, name, and class in map file
; uses memory variables segment_start, segment_stop, true_seg_len
; destroys ax,bx,cx,dx,di

map_segments    PROC
    push    es              ; save critical register
	push	ds
	pop	es					; es -> warplink data

; convert all needed amount of map_text_buffer bytes to spaces
    mov cx,140              ; 140 words in field
    mov ax,2020h            ; spaces in high and low bytes
    mov di,OFFSET DGROUP:map_text_buffer

ms_loop:
    stosw                   ; spaces in two bytes
    loop    ms_loop         ; loop until complete

    xor dx,dx               ; zero count of chars in string
    mov bx,OFFSET DGROUP:map_text_buffer    ; bx -> place to put ascii chars

    mov cl,BYTE PTR segment_start+2 ; get value of LSB of MSW of segment start
    mov ah,1                ; 4 bit value
    call    hexxit

    mov cl,BYTE PTR segment_start+1 ; get value of MSB of LSW of segment start
    xor ah,ah               ; 8 bit value
    call    hexxit

    mov cl,BYTE PTR segment_start   ; get value of LSB of LSW of segment start
    call    hexxit

    mov BYTE PTR [bx],'H'   ; put a hexadecimal number indicator
    add bx,2                ; move to segment length portion of field
    add dx,2
    mov cl,BYTE PTR segment_stop+2  ; get value of LSB of MSW of segment stop
    mov ah,1                ; 4 bit value
    call    hexxit

    mov cl,BYTE PTR segment_stop+1  ; get value of LSB of LSW of segment stop
    xor ah,ah               ; 8 bit value
    call    hexxit

    mov cl,BYTE PTR segment_stop    ; get value of LSB of LSW of segment stop
    call    hexxit

    mov BYTE PTR [bx],'H'   ; put a hexadecimal number indicator
    add bx,2                ; move to segment length portion of field
    add dx,2
    mov cl,BYTE PTR true_seg_len+2  ; get value of LSB of MSW of segment length
    mov ah,1                ; 4 bit value
    call    hexxit

    mov cl,BYTE PTR true_seg_len+1  ; get value of LSB of LSW of segment length
    xor ah,ah               ; 8 bit value
    call    hexxit

    mov cl,BYTE PTR true_seg_len    ; get value of LSB of LSW of segment length
    call    hexxit
    mov BYTE PTR [bx],'H'   ; put a hexadecimal number indicator
    add bx,2                ; move to segment name part of field
    add dx,2

    pop es                  ; es -> segdef entry
    push    es              ; restore to stack
    mov di,es:[8]
    add di,8                ; bump past 2 doubleword pointers
    mov es,es:[10]          ; es:di -> segment name
    xor cx,cx               ; zero count of chars in name

ms_segloop:
    mov al,es:[di]          ; get name char
    or  al,al               ; check for zero terminator
    je  ms_2                ; done
    mov [bx],al             ; transfer char
    inc di
    inc bx
    inc cx                  ; bump count of chars in name
    inc dx                  ; bump count of chars in map string
    jmp SHORT ms_segloop    ; loop back for next char

ms_2:
    cmp cx,22               ; see if at least 22 chars were in name
    jae ms_3                ; yes
    mov ax,22
    sub ax,cx               ; get difference between length of char string and 22
    add bx,ax
    add dx,ax               ; move to class name field in map file

ms_3:
    inc bx                  ; move one space to class name field
    inc dx

    pop es                  ; es -> segdef entry
    push    es              ; restore to stack
    mov di,es:[12]
    add di,8                ; bump past 2 doubleword pointers
    mov es,es:[14]          ; es:di -> class name
    
ms_clloop:
    mov al,es:[di]          ; get name char
    or  al,al               ; check for zero terminator
    je  ms_4                ; done
    mov [bx],al             ; transfer char
    inc di
    inc bx
    inc dx                  ; bump count of chars in map string
    jmp SHORT ms_clloop    ; loop back for next char

ms_4:
    mov BYTE PTR [bx],CR    ; cr/lf terminate
    mov BYTE PTR [bx+1],LF
    add dx,2                ; bump count of chars in string

    mov cx,dx               ; get count of chars to write
    mov bx,map_handle
    mov dx,OFFSET DGROUP:map_text_buffer   ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc ms_ret              ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

ms_ret:
    pop es                  ; restore critical register
    ret
map_segments    ENDP

;*****************************
;* MAP_GROUPS                *
;*****************************

; show map of groups
; only called once
; destroys ax,bx,cx,dx,di,si,es

map_groups  PROC
    mov ax,first_grpblk_ptr ; get pointer to first group block
    mov di,ax               ; save pointer
    or  ax,ax               ; check if null
    jne mg_grp_exist        ; no, one or groups exist
    ret                     ; zero, no groups, return

; write group header
mg_grp_exist:
    mov bx,map_handle
    mov cl,mgtext_len       ; get length of text to write
    xor ch,ch               ; zap high byte
    mov dx,OFFSET DGROUP:map_group_text ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mg_2                ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mg_2:
    mov es,di               ; es -> block
    mov cx,es:[0]           ; get count
    mov ax,di
    inc ax
    mov es,ax               ; es -> first entry in block

mg_3:
    or  cx,cx               ; check entry count
    jne mg_3a
    jmp NEAR PTR mg_next_block  ; count is zero, get next group block, if any

mg_3a:
    dec cx                  ; decrement count
    push    cx              ; save critical register

; print group offset and  group name
    xor dx,dx               ; zero count of chars in string to print
    mov bx,OFFSET DGROUP:map_text_buffer    ; bx -> place to put ascii chars

    mov BYTE PTR [bx],CR    ; cr/lf prepend
    inc bx
    mov BYTE PTR [bx],LF
    inc bx
    add dx,2                ; bump count of chars in string

    mov cx,es:[0]           ; get group offset low word
    mov ax,es:[2]           ; get group offset high word

; convert offset value in ax:cx to paragraphs
    shr ax,1
    rcr cx,1                ; /2
    shr ax,1
    rcr cx,1                ; /4
    shr ax,1
    rcr cx,1                ; /8
    shr ax,1                ; ax should be zero by the final shift
    rcr cx,1                ; /16

    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit

    mov cl,ch               ; get low byte
    call    hexxit

    mov BYTE PTR [bx],':'    ; put in ':0   ' after group origin segment value
    inc bx
    mov BYTE PTR [bx],'0'
    inc bx
    mov al,' '
    mov BYTE PTR [bx],al
    inc bx
    mov BYTE PTR [bx],al
    inc bx
    mov BYTE PTR [bx],al
    inc bx
    add dx,5                ; bump count of chars in string

    mov si,es:[4]
    add si,8                ; bump past 2 doubleword pointers
    mov cx,es               ; save -> group entry
    mov es,es:[6]           ; es:si -> segment name

mg_grploop:
    mov al,es:[si]          ; get name char
    or  al,al               ; check for zero terminator
    je  mg_4                ; done
    mov [bx],al             ; transfer char
    inc si
    inc bx
    inc dx                  ; bump count of chars in map string
    jmp SHORT mg_grploop    ; loop back for next char

mg_4:
    mov es,cx               ; restore es -> group entry
    mov cx,dx               ; get count of chars to write
    mov bx,map_handle
    mov dx,OFFSET DGROUP:map_text_buffer    ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mg_5                ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mg_5:
    pop cx                  ; restore critical register
    mov ax,es
    inc ax
    mov es,ax               ; es -> next group entry
    jmp NEAR PTR mg_3       ; loop for next entry

mg_next_block:
    mov es,di               ; es -> group block
    mov di,es:[2]           ; get pointer to next block
    or  di,di               ; make sure that it isn't null
    je  mg_ret              ; null, return
    jmp NEAR PTR mg_2       ; non-null, another block exists, loop and print entries

mg_ret:
    ret
map_groups  ENDP

;*****************************
;* MAP_DETAIL_SEG            *
;*****************************

; detail map of segments
; upon entry es -> segment partition entry
; destroys ax,di

map_detail_seg  PROC
    push    bx              ; save critical register
    push    cx
    push    dx
    push    si

; convert all of map_text_buffer bytes to spaces
    mov bx,es               ; save es critical register
	push	ds
	pop	es					; es -> warplink data
    mov cx,350              ; 350 words in field
    mov ax,2020h            ; spaces in high and low bytes
    mov di,OFFSET DGROUP:map_text_buffer

mds_loop:
    stosw                   ; store spaces
    loop    mds_loop        ; loop until complete

    mov es,bx               ; restore es critical register
    mov al,detail_flag
    or  al,al               ; see if detail segment header text was printed
    jne mds_2               ; yes

; print detail segment header text
    inc al
    mov detail_flag,al      ; see flag to show printed this pass
    mov bx,map_handle
    mov cl,msegtext_len     ; get length of text to write in cx
    xor ch,ch
    mov dx,OFFSET DGROUP:map_detseg_text
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mds_2               ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mds_2:
    xor dx,dx               ; zero count of chars in string to print
    mov di,OFFSET DGROUP:map_text_buffer    ; di -> place to put ascii chars

; write segment name
    mov bx,es               ; save -> segment partition entry
    mov map_seg_ovl,0       ; zero overlaid segment flag

    mov ax,es:[4]           ; get master segdef entry/overlay identifier
    test    BYTE PTR es:[15],80h    ; see if overlaid segment
    je  mds_2a              ; no, ax holds master segdef entry

; overlaid segment, ax holds overlay identifier
    mov map_seg_ovl,ax      ; set overlaid segment flag with identifier
    mov si,ax
    dec si                  ; make relative zero
    shl si,1                ; convert to word offset
    mov es,master_segblk    ; es:si -> proper master segdef entry
    mov ax,es:[si]          ; get master segdef entry in ax

mds_2a:
    push    ax              ; save -> segdef entry on stack
    mov si,ds
    mov ds,ax               ; ds -> master segdef entry
    mov es,si               ; es:di -> write buffer
    lds si,ds:[8]           ; ds:si -> segment name
    add si,8                ; adjust past doubleword pointers

mds_loop1:
    mov al,[si]             ; see if null terminator reached
    or  al,al
    je  mds_3               ; yes
    movsb                   ; transfer byte of name
    inc dx                  ; no, bump count of chars in string
    jmp SHORT mds_loop1     ; loop for next char transfer

mds_3:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    xchg    di,bx           ; bx -> write buffer, di -> segment partition entry
    inc bx                  ; add space after name
    inc dx
    cmp map_seg_ovl,0       ; see if overlaid segment
    je  mds_3b              ; no

; add overlay number
    mov ax,17
    cmp dx,ax               ; see if at least 17 chars of name+space written
    jae mds_3a              ; yes
    sub ax,dx
    add bx,ax               ; position to overlay number field
    add dx,ax               ; adjust char count

mds_3a:
    mov cx,map_seg_ovl      ; get overlay identifier
    xchg    ch,cl           ; high byte in cl, low byte in ch
    xor ah,ah               ; flag 8 bit values
    call    hexxit          ; print high byte
    mov cl,ch               ; get low byte
    call    hexxit          ; print low byte
    inc bx                  ; add space after overlay identifier
    inc dx

mds_3b:
    mov ax,22
    cmp dx,ax               ; see if at least 22 chars of name+overlay #+space written
    jae mds_4               ; yes
    sub ax,dx
    add bx,ax               ; position to address field
    add dx,ax               ; adjust char count

; write address
mds_4:
    cmp map_seg_ovl,0       ; see if overlaid segment
    je  mds_4a
    mov es,di               ; es -> segment partition entry
    xor cx,cx               ; zero offset bytes
    jmp SHORT mds_4b

mds_4a:
    pop es                  ; es -> segdef entry
    push    es              ; replace -> segdef entry on stack
    call    seg_offset_to_para  ; convert segment offset to paragraphs in cx

mds_4b:
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit
    mov BYTE PTR [bx],':'   ; put in ':'
    inc bx
    inc dx

    cmp map_seg_ovl,0       ; see if overlaid segment
    je  mds_4c
    xor cx,cx               ; zero offset bytes
    jmp SHORT mds_4d

mds_4c:
    mov al,es:[2]           ; get low byte of offset
    and ax,0fh              ; only save lowest 4 bits of offset
    mov es,di               ; es -> segment partition entry
    add ax,es:[0]           ; add in segment partition offset
    mov cx,ax

mds_4d:
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit
    inc bx                  ; add space after address
    inc dx

; write length
    mov al,' '              ; assume length is less than 64K
    test BYTE PTR es:[11],2 ; test Big bit of acbp byte of segment partition
    je  mds_5

    mov al,'1'              ; Big bit set

mds_5:
    mov [bx],al             ; put in space or '1'
    inc bx
    inc dx
    mov cx,es:[12]          ; get segment partition length low word
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit
    mov BYTE PTR [bx],'h'   ; put in 'h '
    add bx,2
    add dx,2

; write alignment
    mov al,es:[11]          ; get segment partition entry's acbp byte
    and al,0e0h             ; get align field
    cmp al,20h              ; check if byte aligned
    je  mds_byte
    cmp al,40h              ; check if word aligned
    je  mds_word
    cmp al,60h              ; check if paragraph aligned
    je  mds_para

; assume page aligned (align field 80h)
    mov si,OFFSET DGROUP:align_page
    jmp SHORT mds_6

mds_para:
    mov si,OFFSET DGROUP:align_para
    jmp SHORT mds_6

mds_word:
    mov si,OFFSET DGROUP:align_word
    jmp SHORT mds_6

mds_byte:
    mov si,OFFSET DGROUP:align_byte

mds_6:
    mov cx,6                ; transfer 6 chars
    add dx,6                ; adjust total char count

mds_loop2:
    lodsb                   ; get align message char
    mov [bx],al             ; transfer char
    inc bx
    loop    mds_loop2       ; loop until all message chars transferred

; write combine
    mov al,es:[11]          ; get segment partition entry's acbp byte
    and al,1ch              ; get combine field
    je  mds_priv            ; private combine
    cmp al,8                ; check if public combine
    je  mds_public
    cmp al,10h              ; check if public combine
    je  mds_public
    cmp al,1ch              ; check if public combine
    je  mds_public
    cmp al,14h              ; check if stack combine
    je  mds_stack

; assume common combine (combine field 18h)
    mov si,OFFSET DGROUP:comb_com
    jmp SHORT mds_7

mds_stack:
    mov si,OFFSET DGROUP:comb_stack
    jmp SHORT mds_7

mds_public:
    mov si,OFFSET DGROUP:comb_pub
    jmp SHORT mds_7

mds_priv:
    mov si,OFFSET DGROUP:comb_priv

mds_7:
    mov cx,8                ; transfer 8 chars
    add dx,8                ; adjust total char count

mds_loop3:
    lodsb                   ; get combine message char
    mov [bx],al             ; transfer char
    inc bx
    loop    mds_loop3       ; loop until all message chars transferred

    mov si,ds

mds_7a:
    pop es                  ; es -> master segdef entry

; write class
mds_7b:
    xchg    di,bx           ; di -> write buffer, bx -> segment partition entry
    mov ax,es               ; get master segdef entry
    push    ax              ; save -> segdef entry on stack
    mov ds,ax               ; ds -> master segdef entry
    mov es,si               ; es:di -> write buffer
    lds si,ds:[12]          ; ds:si -> class name
    add si,8                ; adjust past doubleword pointers

mds_loop4:
    mov al,[si]             ; see if null terminator reached
    or  al,al
    je  mds_8               ; yes
    movsb                   ; transfer byte of name
    inc dx                  ; no, bump count of chars in string
    jmp SHORT mds_loop4     ; loop for next char transfer

mds_8:
    inc di                  ; add space after name, di still -> write buffer
    inc dx
    mov ax,62
    cmp dx,ax               ; see if at least 62 chars written
    jae mds_9               ; yes
    sub ax,dx
    add di,ax               ; position to class field
    add dx,ax               ; adjust char count

mds_9:
    mov ax,DGROUP
    mov ds,ax
    pop es                  ; es -> segdef entry

    cmp map_seg_ovl,0       ; see if overlaid segment
    jne mds_10              ; yes, no group association

; write group
    mov si,DGROUP
    mov ax,es               ; get master segdef entry
    mov ds,ax               ; ds -> master segdef entry
    mov es,si               ; es:di -> write buffer
    mov ax,ds:[16]          ; get pointer to group entry
    or  ax,ax               ; check if zero (no group)
    je  mds_10              ; zero, no group name
    mov ds,ax               ; ds -> group entry
    lds si,ds:[4]           ; ds:si -> group name
    add si,8                ; adjust past doubleword pointers

mds_loop5:
    mov al,[si]             ; see if null terminator reached
    or  al,al
    je  mds_10              ; yes
    movsb                   ; transfer byte of name
    inc dx                  ; no, bump count of chars in string
    jmp SHORT mds_loop5     ; loop for next char transfer

mds_10:
    inc di                  ; add space after name
    inc dx
    mov ax,72
    cmp dx,ax               ; see if at least 72 chars written
    jae mds_11              ; yes
    sub ax,dx
    add di,ax               ; position to class field
    add dx,ax               ; adjust char count

mds_11:
    mov ax,DGROUP
    mov ds,ax               ; restore ds -> warplink data
    mov es,ax               ; es -> warplink data

; write module
    mov si,OFFSET DGROUP:tmod_name

mds_loop6:
    mov al,[si]             ; see if null terminator reached
    or  al,al
    je  mds_12              ; yes
    movsb                   ; transfer byte of name
    inc dx                  ; no, bump count of chars in string
    jmp SHORT mds_loop6     ; loop for next char transfer

mds_12:
    inc di                  ; add space after name
    inc dx
    mov ax,85
    cmp dx,ax               ; see if at least 85 chars written
    jae mds_13              ; yes
    sub ax,dx
    add di,ax               ; position to class field
    add dx,ax               ; adjust char count

; write file
mds_13:
    mov si,OFFSET DGROUP:filename

mds_loop7:
    mov al,[si]             ; see if null terminator reached
    or  al,al
    je  mds_14              ; yes
    movsb                   ; transfer byte of name
    inc dx                  ; no, bump count of chars in string
    jmp SHORT mds_loop7     ; loop for next char transfer

mds_14:
    mov BYTE PTR [di],CR    ; cr/lf terminate
    mov BYTE PTR [di+1],LF
    add dx,2                ; bump count of chars in string

    mov es,bx               ; restore es -> segment partition entry

    mov cx,dx               ; get count of chars to write
    mov bx,map_handle
    mov dx,OFFSET DGROUP:map_text_buffer   ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mds_ret             ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mds_ret:
    pop si
    pop dx
    pop cx
    pop bx
    ret
map_detail_seg  ENDP

;*****************************
;* SEG_OFFSET_TO_PARA        *
;*****************************

; convert segment offset to paragraphs
; destroys ax
; returns value in cx

seg_offset_to_para  PROC
    mov cx,es:[2]           ; get low word of offset
    mov ax,es:[4]           ; get high word of offset
    shr ax,1                ; convert offset in ax:cx to paragraphs
    rcr cx,1                ; /2
    shr ax,1
    rcr cx,1                ; /4
    shr ax,1
    rcr cx,1                ; /8
    shr ax,1
    rcr cx,1                ; /16
    ret
seg_offset_to_para  ENDP

;*****************************
;* HEXXIT                    *
;*****************************

; convert 4 or 8 bit hex number to its ASCII representation
; upon entry cl contains number
; ah == 1 if 4 bit, == 0 if 8 bit
; bx -> place to put digits
; destroys al,cl
; updates bx,dx (number of chars in string)

hexxit  PROC
    or  ah,ah               ; check if 4 bit or 8 bit value
    jne hx_4bit             ; 4 bit
    mov al,cl
    and al,0f0h             ; get high nybble
    shr al,1                ; make value relative zero
    shr al,1
    shr al,1
    shr al,1                ; al has relative zero value
    cmp al,0ah              ; see if need to use hex numbers a-f
    jb  hx_2                ; no
    add al,7                ; adjust for ASCII jump to alpha chars

hx_2:
    add al,30h              ; make number an ASCII representation
    mov [bx],al             ; save it to number string
    inc bx                  ; point to next char slot
    inc dx                  ; bump count of chars in number string

hx_4bit:
    and cl,0fh              ; get low nybble
    cmp cl,0ah              ; see if hex number a-f
    jb  hx_5                ; no
    add cl,7                ; adjust for ASCII jump to alpha chars
hx_5:
    add cl,30h              ; make number an ASCII representation
    mov [bx],cl             ; save it to number string
    inc bx                  ; point to next char slot
    inc dx                  ; bump count of chars in number string
    ret
hexxit  ENDP

;*****************************
;* FINISH_MAP                *
;*****************************

; write remaining map info
; destroys ax,bx,cx,dx

finish_map  PROC
    cmp is_mapfile,0        ; see if map file
    je  fm_ret              ; no

    cmp is_mapexpand,0      ; check if expanded map file
    je  fm_2                ; no
    call    map_publics     ; write the public addresses and names to map file

fm_2:
    call    map_entry       ; write program entry point
    mov bx,map_handle       ; get handle of map file
    mov ah,3eh              ; close file
    int 21h
    call    restore_ems_map

fm_ret:
    ret
finish_map  ENDP

;*****************************
;* MAP_PUBLICS               *
;*****************************

; map public symbol addresses and names
; destroys ax,bx,cx,dx,di,si,es

map_publics PROC
    mov cl,maddrtext_len    ; get length of text to write
    xor ch,ch               ; zap high byte
    mov bx,map_handle
    mov dx,OFFSET DGROUP:maddr_text ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mp_2                ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mp_2:
    mov ax,first_pdeclblk_ptr   ; get pointer to first public declarations block
    or  ax,ax               ; check if non-null
    jne mp_blkloop          ; non-null, public declarations exist
    jmp NEAR PTR mp_communal_chk    ; null, try communal declarations block

mp_blkloop:
    push    ax              ; save -> block on stack
    mov es,ax               ; es -> declarations block
    mov si,es:[0]           ; get count of entries in block

mp_entloop:
    inc ax                  ; ax -> next entry in block
    mov es,ax               ; es -> declaration entry
    push    es              ; save -> declaration entry

; 12/21/92
;***	test    BYTE PTR es:[15],20h    ; see if local communal (don't list locals)
    test    BYTE PTR es:[15],24h    ; see if local

    je  mp_not_local        ; no

mp_noprint:
    pop es                  ; restore stack
    jmp NEAR PTR mp_next_entry  ; try next entry in block

mp_not_local:
    test    BYTE PTR es:[14],3  ; see if weak extdef
    je  mp_noprint          ; yes, don't print it

    mov ah,es:[15]          ; get general flags
    test    ah,1            ; get in overlay flag
    jne mp_overlaid         ; set, in overlay, zero segment offset
    mov al,es:[14]          ; get definition flag
    and al,3                ; get definition bits
    cmp al,3                ; see if absolute
    jne mp_not_abs          ; no

; absolute symbol
    mov cx,es:[2]           ; get frame number in cx

mp_zero_offset:
    xor di,di               ; di==low nybble of segment offset (always zero)
    jmp SHORT mp_3          ; bypass segment frame code

mp_not_abs:
    cmp al,2                ; see if resolved
    je  mp_2a               ; yes
    xor cx,cx               ; zero frame
    jmp SHORT mp_zero_offset    ; go to zero offset code

mp_overlaid:
    xor cx,cx               ; zero frame
    mov di,cx               ; zero public offset
    jmp SHORT mp_3          ; bypass segment frame code

mp_2a:
    mov es,es:[0]           ; es -> segment partition entry
    mov cx,es:[0]           ; get segment partition offset
    mov es,es:[4]           ; es -> segdef entry

    and ah,80h              ; see if public is in group
    je  mp_2b               ; no

    mov di,cx               ; di holds segment partition entry offset

    mov cx,es:[2]           ; get low word of segment offset
    mov ax,es:[4]           ; get high word of segment offset
    pop es                  ; es -> public declaration entry
    push    es              ; restore -> public declaration entry to stack
    mov es,es:[2]           ; es -> group entry
    sub cx,es:[0]           ; compute low word difference in group/segment offset
    sbb ax,es:[2]           ; compute high word difference in group/segment offset
    add di,cx               ; add difference into di, public offset

    mov ax,es:[0]           ; get low word of group offset
    mov cx,ax               ; save in cx
    and ax,0fh              ; get paragraph remainder
    add di,ax               ; add to public offset

    mov ax,es:[2]           ; get high word of group offset
    shr ax,1                ; convert offset in ax:cx to paragraphs
    rcr cx,1                ; /2
    shr ax,1
    rcr cx,1                ; /4
    shr ax,1
    rcr cx,1                ; /8
    shr ax,1
    rcr cx,1                ; /16
    jmp SHORT mp_3          ; bypass segment specific code

; non-group public
mp_2b:
    mov di,es:[2]           ; get low word of segment offset
    and di,0fh              ; di==low nybble of segment offset
    add di,cx               ; add in segment partition entry offset

    call    seg_offset_to_para  ; get segment offset in paragraphs (frame) in cx

mp_3:
    mov bx,OFFSET DGROUP:map_text_buffer
    mov BYTE PTR [bx],CR    ; prepend cr/lf pair
    inc bx
    mov BYTE PTR [bx],LF
    inc bx
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit
    mov BYTE PTR [bx],':'   ; put in ':'
    inc bx

    pop es                  ; es -> declaration entry
    add di,es:[8]           ; add in public offset
    mov cx,di
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit

    mov cx,11                ; nine bytes to write, CR,LF,nnnn:nnnn
    mov bx,map_handle
    mov dx,OFFSET DGROUP:map_text_buffer    ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc mp_4                ; no errors

mp_toerr:
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mp_4:
    mov al,es:[15]          ; get general flag
    and al,1                ; see if overlaid
    jne mp_ovl              ; yes
    mov al,es:[14]          ; get definitions flag
    and al,3                ; get definition bits
    cmp al,3                ; see if absolute
    je  mp_abs              ; no
    cmp al,2                ; see if resolved public/communal
    jne mp_unres            ; no
    test    BYTE PTR es:[15],40h    ; see if communal
    je  mp_res              ; no, resolved public

;communal variable
    mov dx,OFFSET DGROUP:mpub_comm
    jmp SHORT mp_print_pubtype

; overlaid variable
mp_ovl:
    push    es              ; save critical register
    mov es,es:[0]           ; es -> segment partition entry
    mov cx,es:[4]           ; get overlay identifier
    pop es                  ; restore critical register
    mov bx,OFFSET DGROUP:mpub_ovl+5 ; bx -> place to put overlay number
    xchg    ch,cl           ; get high byte in cl, save low byte in ch
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,ch               ; get low byte
    call    hexxit

    mov dx,OFFSET DGROUP:mpub_ovl
    jmp SHORT mp_print_pubtype

; absolute variable
mp_abs:
    mov dx,OFFSET DGROUP:mpub_abs
    jmp SHORT mp_print_pubtype

; resolved public
mp_res:
    mov dx,OFFSET DGROUP:mpub_res
    jmp SHORT mp_print_pubtype

; unresolved public
mp_unres:
    mov dx,OFFSET DGROUP:mpub_unres

mp_print_pubtype:
    mov cx,11
    mov bx,map_handle
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jc  mp_toerr            ; error occurred

    mov bx,map_handle
    push    ds              ; save critical register
    lds di,es:[4]           ; get -> pubdef name in es:bx
    mov dx,di               ; save -> name
    xor cx,cx               ; cx will hold of chars in name

mp_loop:
    cmp BYTE PTR [di],0     ; see if zero terminator in symbol name found
    je  mp_5                ; yes
    inc cx                  ; bump count of chars in string
    inc di                  ; move to next char slot to test
    jmp SHORT mp_loop       ; loop back to test next char

mp_5:
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    pop ds                  ; restore ds -> machlink data
    jc  mp_toerr2           ; error occurred

mp_next_entry:
    mov ax,es               ; ax -> current entry
    dec si                  ; drop count of entries in block
    je  mp_next_block       ; no more entries try next block
    jmp NEAR PTR mp_entloop ; print next entry

mp_toerr2:
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

mp_next_block:
    pop es                  ; restore es -> block
    mov ax,es:[2]           ; get pointer to next block, if any
    or  ax,ax               ; check if non-null
    je  mp_communal_chk     ; null, check communals
    jmp NEAR PTR mp_blkloop ; loop back for more publics

mp_communal_chk:
    inc comm_loop_flag      ; bump counter
    cmp comm_loop_flag,1    ; see if communal blocks printed yet
    ja  mp_ret              ; yes
    mov ax,first_cdeclblk_ptr   ; get pointer to first communal declarations block
    or  ax,ax               ; check if non-null
    je  mp_ret              ; null, done
    jmp NEAR PTR mp_blkloop ; non-null, print declarations in communal block

mp_ret:
    ret
map_publics ENDP

;*****************************
;* MAP_ENTRY                 *
;*****************************

; map entry point of program
; destroys ax,bx,cx,dx

map_entry   PROC
    mov bx,OFFSET DGROUP:map_entry_value    ; point to place to stuff entry value
    mov cl,BYTE PTR entry_segval+1  ; get MSB of entry segment
    xor ah,ah               ; 8 bit value
    call    hexxit
    mov cl,BYTE PTR entry_segval    ; get LSB of entry segment
    call    hexxit

    inc bx                  ; bump past colon
    mov cl,BYTE PTR entry_offval+1  ; get MSB of entry offset
    call    hexxit
    mov cl,BYTE PTR entry_offval    ; get LSB of entry offset
    call    hexxit

    mov cl,metext_len       ; get length of text to write
    xor ch,ch               ; zap high byte
    mov bx,map_handle
    mov dx,OFFSET DGROUP:map_entry_text    ; ds:dx -> text to write
    mov ah,40h              ; write to file
    int 21h
    call    restore_ems_map
    jnc me_ret              ; no errors
    mov dx,OFFSET DGROUP:map_name
    jmp NEAR PTR dos_error  ; error writing to file

me_ret:
    ret
map_entry   ENDP

END
