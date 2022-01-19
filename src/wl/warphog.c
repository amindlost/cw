
/*
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³WarpHog 2.00                                                               ³
³                                                                           ³
³Copyright (c) 1991-1993 Doug Amaral                                        ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/


#include <stdio.h>
#include <dos.h>
#include <bios.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <malloc.h>

#include <ctype.h>
#include <errno.h>
#include <process.h>

#include <ttsr.h>                       /* Include file for TTSR        */
                                        /*    structures and prototypes */

unsigned idnum;                         /* TSR Identification Number       */

void main (int argc, char **argv)
{
    unsigned int freemem=0;
    unsigned int useramt=500;

    TsSetDefStk(0x0);                   /* code assumes a 4K stack!!!!     */
                                        /*   this will assign 2K to each   */
                                        /*   foreground and background!    */

		puts("");
		puts(" WARPHOG v2.60  -  The Memory Eater ");
		puts(" Copyright (c) 1990-1993 Michael Devore");
		puts(" Written by Doug Amaral");
		puts("");

                                        /* Are we already here?            */
    if(TsCheckResident("WARPHOG2",&idnum) == 0xffff)
    {
        TsRelease(idnum);
				puts(" WarpHog and extra allocation have been removed from memory.");
				exit(0);
    }

		if ( argc == 1 )
		{
			puts(" SYNTAX  :  WARPHOG <memory size in k>");
			puts("");
			puts(" Warphog hogs memory leaving only the requested size free.");
			puts("");
			puts(" Example :  WARPHOG 500");
			puts(" Warphog hogs all but 500k of memory.");
			puts("");
			puts(" Execute WARPHOG with no parameters to unload.");
			puts("");
			exit(1);
		}

		useramt = atoi ( argv[1] );

		if ( useramt < 200 || useramt > 640 )
		{
			puts(" Invalid memory size!! ");
			puts("");
			puts(" Valid range is 200 to 640");
			puts("");
			puts(" SYNTAX  :  WARPHOG <memory size in k>");
			puts("");
			puts(" Warphog hogs memory leaving only the requested size free.");
			puts("");
			puts(" Example :  WARPHOG 500");
			puts(" Warphog hogs all but 500k of memory.");
			puts("");
			puts(" Execute WARPHOG with no parameters to unload.");
			puts("");
			exit(1);

		}

		_heapmin();

		asm	{
			mov ah, 48h
			mov bx, 0FFFFh
			int 21h
			mov freemem,bx
		}

		freemem = freemem / 64 ;

		if ( freemem < useramt )
		{
		 printf(" Could not make %uk available, only %uk DOS memory is free.\n",useramt,freemem);
		 exit (1);
		}

		printf(" DOS memory available   :   %uk\n",freemem+27);
		printf(" Memory to HOG          :   %uk\n",freemem - useramt+27);
		printf(" DOS memory remaining   :   %uk\n\n",useramt);

		useramt = (freemem - useramt) * 64 ;

		TsSwapType(4); //No Swapping

  	puts(" Execute WARPHOG with no parameters to unload.");
	  puts("");

		if( TsDoInit(
        0,
        0,
        0,
        1728 + useramt) )
				 {
          puts("Warphog could not load, try a new memory size");
          exit(1);
				 }
	exit(0);
}
