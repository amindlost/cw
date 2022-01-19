; assemble with favorite assembler, e.g. TASM ERRHAND
; and link the resulting object module into your program for plain
; English feedback on errors that occur within the overlay manager

STDERR  EQU 2	; standard error device
CR  	EQU 0DH	; carriage return
LF  	EQU 0AH	; line feed
BELL    EQU 7	; bell

errhand_seg SEGMENT WORD PRIVATE '$$_OVL_MANAGER'
    ASSUME cs:errhand_seg,ds:errhand_seg

PUBLIC  _ovlmgr_error_vector

_ovlmgr_error_vector    DW  OFFSET disp_err
                        DW  SEG disp_err

gripe	DB	BELL
crlf	DB	CR,LF	; must immediately follow 'gripe'

; Following is a partial list of common error messages returned by DOS

			DB	errmess1_stop-errmess1
errmess1	DB	'Invalid function number.'
errmess1_stop	=	$

			DB	errmess2_stop-errmess2
errmess2    DB	'File not found.'
errmess2_stop	=	$

			DB	errmess3_stop-errmess3
errmess3    DB	'Path not found.'
errmess3_stop	=	$

			DB	errmess4_stop-errmess4
errmess4    DB	'No handle available.'
errmess4_stop	=	$

			DB	errmess5_stop-errmess5
errmess5    DB	'Access denied.'
errmess5_stop	=	$

			DB	errmess6_stop-errmess6
errmess6    DB	'Invalid handle.'
errmess6_stop	=	$

			DB	errmess7_stop-errmess7
errmess7    DB	'Memory control blocks destroyed.'
errmess7_stop	=	$

			DB	errmess8_stop-errmess8
errmess8    DB	'Insufficient memory.'
errmess8_stop	=	$

			DB	errmess9_stop-errmess9
errmess9    DB	'Invalid memory block address.'
errmess9_stop	=	$

			DB	errmess10_stop-errmess10
errmess10   DB	'Invalid environment.'
errmess10_stop	=	$

			DB	errmess11_stop-errmess11
errmess11   DB	'Invalid format.'
errmess11_stop	=	$

			DB	errmessxx_stop-errmessxx
errmessxx   DB	'Unknown error code '
codeval     DB  'XXX (decimal).'
errmessxx_stop	=	$

; An error has occurred, find which one and return an error message
; to the user
disp_err    PROC    FAR
    push    cs
    pop ds                  ; make sure ds -> program data
    cmp ax,1                ; invalid function error?
    jne err2                ; no, try the next error
    mov dx,OFFSET errmess1  ; dx -> error message
    jmp SHORT to_print_err  ; jump to common error code
err2:
    cmp ax,2                ; file not found error
    jne err3
    mov dx,OFFSET errmess2
    jmp SHORT to_print_err
err3:
    cmp ax,3                ; path not found error
    jne err4
    mov dx,OFFSET errmess3
    jmp SHORT to_print_err
err4:
    cmp ax,4                ; no handle available error
    jne err5
    mov dx,OFFSET errmess4
    jmp SHORT to_print_err
err5:
    cmp ax,5                ; access denied error
    jne err6
    mov dx,OFFSET errmess5
    jmp SHORT to_print_err
err6:
    cmp ax,6                ; Invalid handle error
    jne err7
    mov dx,OFFSET errmess6
to_print_err:
    jmp SHORT print_err
err7:
    cmp ax,7                ; Memory control blocks destroyed error.
    jne err8
    mov dx,OFFSET errmess7
    jmp SHORT print_err
err8:
    cmp ax,8                ; Insufficient memory error
    jne err9
    mov dx,OFFSET errmess8
    jmp SHORT print_err
err9:
    cmp ax,9                ; Invalid memory block address error
    jne err10
    mov dx,OFFSET errmess9
    jmp SHORT print_err
err10:
    cmp ax,10               ; Invalid environment error
    jne err11
    mov dx,OFFSET errmess10
    jmp SHORT print_err
err11:
    cmp ax,11               ; invalid format error
    jne errxx               ; unknown error code
    mov dx,OFFSET errmess11
    jmp SHORT print_err

; not particularly efficient routine to make error code an ASCII value
; cleanup, if desired, is left as an exercise for the reader
errxx:
	push	ax			; save error value
    mov bx,OFFSET codeval
    mov cl,100
    mov ch,al
    div cl
    mov dl,al
    add dl,30h
    mov [bx],dl
    mul cl
    sub ch,al
    mov al,ch

    inc bx
    mov cl,10
    div cl
    mov dl,al
    add dl,30h
    mov [bx],dl
    mul cl
    sub ch,al
    mov al,ch

    inc bx
    add al,30h
    mov [bx],al

	pop	ax				; restore ax == error value
    mov dx,OFFSET errmessxx

; deisplay the feedback
print_err:
	push	ax			; save error value
	mov	si,dx			; si -> error message to print
	mov	dx,OFFSET gripe	; BEEP, CR, LF chars
    mov bx,STDERR       ; print error message to standard error device
	mov	cx,3
    mov ah,40h          ; write to device
    int 21h

	mov	dx,si			; restore dx -> error message to print
	mov	cl,[si-1]		; get length of string to print, ch == 0
    mov ah,40h          ; write to device
    int 21h

	mov	dx,OFFSET crlf	; CR/LF chars
	mov	cl,2
    mov ah,40h          ; write to device
    int 21h

	pop	ax				; restore error value
    mov ah,4ch          ; terminate with return code
    int 21h
    ret
disp_err    ENDP

errhand_seg ENDS
END
