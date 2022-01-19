DDL_HEADER_SIZE EQU 96      ; size of DDL header

; sample DDL loader code

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
    dh_ox       DB  ?       ; 0==regular memory for op, nonzero==EMS page frame for op
    dh_r        DB  ?       ; /r option setting
    dh_cla      DB  ?       ; /cla option setting
    dh_db       DW  ?       ; /db option setting
    dh_pad      DW  ?       ; pads to dword boundary
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
    dh_reser1   DD  ?       ; reserved for future
    dh_reser2   DD  ?       ; reserved for future
    dh_reser3   DD  ?       ; reserved for future
    dh_reser4   DD  ?       ; reserved for future
DDL_HEADER_STRUC    ENDS

cseg    segment para 'CODE'
    assume  cs:cseg,ds:cseg
start:
    jmp SHORT past_name     ; jump past the DDL name/path
ddl_name    DB  'NONAME.DDL',0,0,0  ; room for 13 bytes (12+null terminator)
ddl_path    DB  'DDLPATH=',0
past_name:
    push    ds
    push    cs
    pop ds                  ; ds -> data in code segment
    mov dx,OFFSET ddl_name
    mov ax,3d00h            ; open for reading
    int 21h
    jc  not_cwd             ; error, not in current working directory
    jmp NEAR PTR load_2     ; no errors

; check if file not found related problem
not_cwd:
    cmp ax,2                ; check for file not found error
    je  scan_path           ; yes, scan the DDL path for file
    cmp ax,3                ; check if path not found or file doesn't exist error
    je  scan_path           ; yes
    cmp ax,5                ; check for access denied (not in current directory)
    jne to_loaderr          ; unexpected DOS error

; find DDL file using DDLPATH environment string
; prior to using DDLPATH, if using DOS 3.x+, check the EXE's path that is found
; after the local environment block (argv[0])
scan_path:
    mov ax,es:[2]           ; get top of allocated memory (from PSP)
    sub ax,8                ; subtract off paragraphs for fully qualified DDL name
    mov ds,ax
    push    es              ; put PSP value on stack
    mov ax,es:[2ch]         ; get environment segment from offset 2ch in PSP
    mov es,ax               ; es -> environment segment

    mov ah,30h              ; get MS-DOS version number
    int 21h
    cmp al,2                ; should be 3.x or above
    jbe dos_2x_entry        ; no, can't check path of EXE file after environment blockk

; search for end of environment block
    push    es
    push    ds
    pop es                  ; es -> file name buffer
    pop ds
    xor si,si               ; ds:si -> start of environment

end_loop:
    lodsb                   ; get environment char
    or  al,[si]             ; merge in next char
    jne end_loop            ; not at end of environment block

    add si,3                ; bump si -> start of exe file path
    mov di,si               ; save start pointer

path_loop:
    lodsb                   ; get char of file path
    or  al,al               ; see if at end
    je  calc_path           ; yes
    cmp al,'\'              ; see if directory indicator
    jne path_loop           ; no
    mov bx,si               ; save -> to char past directory
    jmp SHORT path_loop     ; get next char

calc_path:
    mov si,di               ; ds:si -> start  of file path
    xor di,di               ; es:di -> program name with path prefix slot

calc_loop:
    movsb                   ; transfer a char
    cmp si,bx               ; see if at end of path
    jne calc_loop           ; no

    mov bx,ds               ; save -> environment block
    push    cs
    pop ds                  ; ds -> loader data
    mov si,OFFSET ddl_name  ; append program name to end of path prefix

append_loop:
    movsb                   ; transfer a char
    cmp BYTE PTR [si-1],0   ; see if null terminator transferred
    jne append_loop         ; no

; have EXE file path and DDL file name
    push    es
    pop ds                  ; restore ds -> file name
    xor dx,dx
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jc  check_errors        ; program not found, make sure non-DOS error
    jmp NEAR PTR found_ddl  ; program found

check_errors:
    cmp ax,2                ; file not found
    je  try_path
    cmp ax,3                ; path not found
    je  try_path
    cmp ax,5                ; access denied error
    je  try_path

to_loaderr:
    jmp NEAR PTR load_err

; file wasn't found in EXE's path, try PATH environment variable
try_path:
    mov es,bx               ; es -> environment block

dos_2x_entry:
    mov bx,OFFSET ddl_path  ; bx holds target string address for compares
    xor si,si               ; starting location for target string check

ddl_find_path:
    xor di,di               ; offset into target string

ddl_loop2:
    mov al,es:[si]          ; get byte from environment string
    inc si                  ; point to next char in environment
    cmp al,cs:[bx+di]       ; does environment char match PATH string char
    je  ddl_byte_match      ; yes, try next location
    or  al,es:[si]          ; two zero values in a row mean the end of the environment
    jne ddl_find_path       ; not the end of the environment

ddl_not_found:
    mov ax,2                ; force file not found error
    jmp SHORT to_loaderr    ; transfer to error handler

; check that PATH is not part of another environment string
ddl_byte_match:
    or  di,di               ; di is zero if first char is matched
    jne ddl_2               ; not first char, test already done
    cmp si,1                ; si equals one if DDLPATH is first string in environment block
    je  ddl_2               ; no previous environment string
    cmp BYTE PTR es:[si-2],0    ; check if char before DDLPATH was nonzero
    jne ddl_find_path       ; yes, LIB is a subset of another string, keep looking

ddl_2:
    inc di                  ; a match, move to next byte of target string
    cmp di,8                ; check if all bytes matched
    jb  ddl_loop2           ; not yet, keep comparing

; es -> environment block
; ds -> file name buffer
; cs -> loader data
ddl_3:
    xor di,di               ; offset into path prefix

ddl_4:
    mov al,es:[si]          ; get path character
    cmp al,';'              ; check if path terminator character
    je  ddl_prefix_complete ; yes, check file's existence with the current path prefix
    cmp al,' '              ; anything less than a space is also a terminator character
    jb  ddl_prefix_complete
    mov [di],al             ; save path character
    inc di                  ; move to next name slot
    inc si                  ; move to next byte location
    jmp SHORT ddl_4         ; loop for next character

ddl_prefix_complete:
    push    si              ; save si -> current environment position
    mov si,OFFSET ddl_name  ; append program name to end of path prefix
    cmp BYTE PTR [di-1],'\' ; check for backslash already in place
    je  ddl_5
    mov BYTE PTR [di],'\'   ;put a backslash between the path and program name
    inc di                  ; point di past backslash

ddl_5:
    mov al,cs:[si]          ; get program name character
    mov [di],al             ; transfer program name
    or  al,al               ; stop transfer after first zero byte transfer
    je  ddl_search          ; now see if file exists in this path
    inc si                  ; move to next name character slot
    inc di
    jmp SHORT ddl_5         ; loop for next character

ddl_search:
    mov ax,4300h            ; get file attributes (check for file existence)
    int 21h
    jnc ddl_prog_found      ; found the program
    cmp ax,2                ; file not found
    je  ddl_6
    cmp ax,3                ; path not found
    je  ddl_6
    cmp ax,5                ; access denied
    jne to_loaderr

ddl_6:
    pop si                  ; restore si -> environment block position
    xor al,al
    cmp es:[si],al          ; check last terminator
    je  ddl_not_found       ; if zero then no more path prefixes to try
    cmp es:[si+1],al        ; check character following last terminator
    je  ddl_not_found       ; if zero then no more path prefixes to try

ddl_search_again:
    inc si                  ; point to first character after last terminator
    jmp SHORT ddl_3         ; try next path

ddl_prog_found:
    pop ax                  ; trash the values stored in the stack

found_ddl:
    mov ax,3d00h            ; open for reading
    int 21h
    jc  load_err
    pop es                  ; restore es -> PSP

load_2:
    mov bx,ax               ; save file handle
    mov ax,es:[2]           ; get top of allocated memory (from PSP)
    sub ax,1                ; subtract off paragraphs for partial DDL header read
    mov ds,ax
    xor dx,dx
    mov cx,10h              ; bytes to read
    mov ah,3fh              ; read from file
    int 21h
    jc  load_err

    mov dx,ds:[12]          ; get file position of manager low word
    mov cx,ds:[14]          ; get file position of manager high word
    mov ax,4200h            ; move file pointer, absolute position
    int 21h
    jc  load_err

    mov cx,ds:[10]          ; get manager size (bytes to read)
    mov dx,cx
    add dx,15               ; round up to next para boundary
    shr dx,1                ; convert to paras, /2
    shr dx,1                ; /4
    shr dx,1                ; /8
    shr dx,1                ; /16
    mov ax,es:[2]           ; get top of memory
    sub ax,dx               ; subtract off paragraphs for read of loader
    mov ds,ax

    xor dx,dx               ; ds:dx -> read buffer
    mov ah,3fh              ; read from file
    int 21h
    jc  load_err

; NOP this out, so file still open and manager code will have file handle in bx
    mov ah,3eh              ; close file
    int 21h
    jc  load_err

    mov ax,ds               ; get segment of transfer address
    pop ds                  ; restore ds -> PSP
    push    ax
    xor ax,ax               ; offset of transfer address is zero
    push    ax
    retf                    ; go to DDL manager code

; error occurred
load_err:
    mov ah,4ch
    int 21h
cseg    ends
end start
