#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>

char LineBuffer[128];

FILE *fpin;

void main(void)
{
	int ovlnum;
	char *feedback;
	char *loadstring="Loaded Overlay ";
	char *callstring="Called Overlay ";
	char *retstring="Returned from Overlay ";
	char *instring="Swapin Overlay ";
	char *outstring="Swapout Overlay ";
	char *xstring="Unknown Process Overlay ";
	char *exestring="Executing info ";
	char *jumpstring="Jump info ";
	char *transtring="Tranferring info ";
	char *donestring="Done info ";

	fpin=fopen("OVLMGR.LOG","rb");
	if(fpin==NULL){
		exit(1);
	}

    while(fread(LineBuffer,3,1,fpin)==1){   // read until file end
		ovlnum=*(int *)(&LineBuffer[1]);
		switch(LineBuffer[0]){
			case 'L':
				feedback=loadstring;
				break;
			case 'C':
				feedback=callstring;
				break;
			case 'R':
				feedback=retstring;
				break;
			case 'O':
				feedback=outstring;
				break;
			case 'I':
				feedback=instring;
				break;
			case 'E':
				feedback=exestring;
				break;
			case 'J':
				feedback=jumpstring;
				break;
			case 'T':
				feedback=transtring;
				break;
			case 'D':
				feedback=donestring;
				break;
			default:
				feedback=xstring;
				break;
		}
		printf("%s %xh\n",feedback,ovlnum);
	}

	fclose(fpin);
}
