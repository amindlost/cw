		page	60, 132
; ***********************************************************************
; *									*
; *	File:		strmatch.asm					*
; *	By:		David Rifkind					*
; *	Date:		29 Jan 91					*
; *	Model:		Compact						*
; *	Version:	1.00						*
; *	Assembler:	TASM 2.0					*
; *	Environment:	80x86						*
; *									*
; *	Compare a string to a pattern.  The pattern may contain		*
; *	wildcards "?" and "*".  Not DOS filename wildcards!  "*"	*
; *	can appear anywhere in the pattern, any number of times,	*
; *	and matches any substring of zero or more characters.		*
; *									*
; *	Called from wsprun.c processing "include" and "exclude"		*
; *	options.  Can be called a number of times for each symbol	*
; *	in the map file; thus, it's worth some effort to make this	*
; *	fast.								*
; *									*
; ***********************************************************************

; $Header: d:/warpspd/RCS/strmatch.asm 1.1 91/03/11 04:12:33 dave Exp Locker: dave $


		.model	large, c
		locals

		.code

; *******************************************************
; *	int strmatch(char *s, char *p, int nocase);	*
; *******************************************************
;
;	Compare string "s" to pattern "p".  If "nocase" is non-
;	zero, match is case-insensitive.  Returns 1 on match,
;	0 on failure.
;

		public	strmatch
strmatch	proc	uses si di ds
		arg	s:dword, p:dword, nocase:word

		lds	si, p		; DS:SI is "p"
		les	di, s		; ES:DI is "s"
		xor	cx, cx		; CX non-zero if we can back up

@@1:		lodsb			; Next pattern character

		cmp	al, '?'		; '?' matches any non-null character
		jne	@@2
		inc	di		; Next target character...
		cmp	byte ptr es:[di-1], 0 ; ...unless end of string
		jne	@@1		; Loop to next character
		jmp	short @@7	; AL non-zero means failure

@@2:		cmp	al, '*'		; '*' matches any string zero or more
		jne	@@3
		cmp	byte ptr [si], 0 ; Check for end of string
		mov	al, 0		; Trivial optimization: '*' at end of
		jz	@@7		;   pattern always matches
		inc	cx		; Increment restart count
		mov	bx, si		; Save "s" and "p" for restart after
		mov	dx, di		;   failure
		jmp	@@1		; Next pattern char, same source char

@@3:		mov	ah, es:[di]	; Anything else is literal match
		inc	di
		cmp	nocase, 0	; Case sensitive?
		jnz	@@4
		sub	al, ah		; Yes - must be equal
		jnz	@@7		;   or we fail (AL non-zero)
		test	ah, ah		; Hit end of string?
		jnz	@@1		; No - next character
		jmp	short @@7	; Yes - succeed (AL zero)

@@4:		sub	al, 'A'		; ASCII minus 'A'
		cmp	al, 'Z'-'A'+1	; Is AL upper-case alpha?
		jnc	@@5
		add	al, 20h		; Convert to lower case minus 'A'
@@5:		sub	ah, 'A'		; ASCII minus 'A'
		cmp	ah, 'Z'-'A'+1	; Is AH upper-case alpha?
		jnc	@@6
		add	ah, 20h		; Convert to lower case minus 'A'
@@6:		sub	al, ah
		jnz	@@7		; Equal (AL zero) means match
		add	ah, 'A'		; Else convert back to ASCII
		jnz	@@1		;   and check for end of string

@@7:		test	al, al		; Match succeeded if AL zero
		jz	@@8
		jcxz	@@8		; Failed if no '*' to retry

		mov	si, bx		; Back up to last previous '*'
		inc	dx		; Advance to next source character
		mov	di, dx
		cmp	byte ptr es:[di], 0
		jnz	@@1		; Restart if not exhausted

@@8:		cmp	al, 1		; Convert any non-zero AL to 1
		mov	al, 0		;   in AX
		rcl	al, 1
		cbw

		ret

strmatch	endp

		end
