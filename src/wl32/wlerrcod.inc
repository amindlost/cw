;*********************************************************************
;*   WLERRCOD.INC                                                    *
;*                                                                   *
;*   By:            Michael Devore                                   *
;*   Date:          06/21/94                                         *
;*   Model:         Small                                            *
;*   Version:       1.0                                              *
;*   Assembler:     MASM 5.0                                         *
;*   Environment:   MS-DOS 3.0+                                      *
;*                                                                   *
;*   Error and warning code equates                                  *
;*                                                                   *
;*********************************************************************

; linker error codes
counter	=	80h

INTERNALERRORCODE	=	counter
counter	=	counter+1
DOSVERSIONERRORCODE	=	counter
counter	=	counter+1
RSPLINELENERRORCODE	=	counter
counter	=	counter+1
BADOPTIONERRORCODE	=	counter
counter	=	counter+1
RSPNESTLEVELERRORCODE	=	counter
counter	=	counter+1
MEMALLOCFAILERRORCODE	=	counter
counter	=	counter+1
MEMSIZEFAILERRORCODE	=	counter
counter	=	counter+1
NOOBJFILEERRORCODE	=	counter
counter	=	counter+1
BADOBJRECERRORCODE	=	counter
counter	=	counter+1
UNSUPOBJRECERRORCODE	=	counter
counter	=	counter+1
MEMRELEASEFAILERRORCODE	=	counter
counter	=	counter+1
BADOBJRECLENERRORCODE	=	counter
counter	=	counter+1
POORFORMOBJERRORCODE	=	counter
counter	=	counter+1
SEGLEN64KERRORCODE	=	counter
counter	=	counter+1
BADLIBERRORCODE	=	counter
counter	=	counter+1
SEG32BITEXEERRORCODE	=	counter
counter	=	counter+1
CONFIGLINELENERRORCODE	=	counter
counter	=	counter+1
BADCONFIGLINEERRORCODE	=	counter
counter	=	counter+1
BADSYMBOLTOKENERRORCODE	=	counter
counter	=	counter+1

