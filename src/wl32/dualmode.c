// DUALMODE.C

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#include <ctype.h>

#define BITFPOS	0xb5f0

char ProcessFileName[81];
char IOBuffer[256];

FILE *fpprocess;

void main(int argc, char *argv[])
{
	GetFileInfo(argc,argv);
	ProcessFile();
}

void GetFileInfo(int argc, char *argv[])
{
	char temp[81];
	int len;

	if(argc<2){
		printf("\nNeed file name to process\n");
		exit(1);
	}
	else{
		strcpy(temp,argv[1]);
	}
	len=strlen(temp);
	if(len && temp[len-1]<=' '){
		temp[len-1]=0;
	}
	strcpy(ProcessFileName,temp);
}

void ProcessFile(void)
{
	char ByteValue1=0;
	char ByteValue2=0x40;
	char ByteValue3=1;

	fpprocess=fopen(ProcessFileName,"rb+");
	if(fpprocess==NULL){
		printf("\n\007Error opening file %s.",ProcessFileName);
		exit(1);
	}
	fseek(fpprocess,BITFPOS,SEEK_SET);
	fwrite(&ByteValue1,1,1,fpprocess);
	fwrite(&ByteValue2,1,1,fpprocess);
	fwrite(&ByteValue3,1,1,fpprocess);
	fclose(fpprocess);
}
