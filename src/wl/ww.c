/*
    WARPWRAP.C

    Copyright 1993, Michael E. Devore, All rights reserved

    programmed in the large memory model of Turbo C++ version 2.0
    started 06-11-93
    last changed 06-16-93

    Modify EXE for environment variable burn-in
*/

#include <stdio.h>
#include <dos.h>
#include <string.h>
#include <stdlib.h>
#include <dir.h>
#include <conio.h>

//#define DEBUG

void credits(void);
void GetCommandLine(int argc, char *argv[]);
void OpenEXEFile(void);
void OpenDEFFile(void);
void OpenDataFile(void);
void ReadEXEHeader(void);
void ComputeWWSize(void);
void ParseDEFFile(void);
void ParseString(void);
void REPLACEString(int);
void BadLine(void);
void OutOfMemory(void);
void NEWString(int);
void DELETEString(int);
void NEWCommand(void);
void REPLACECommand(void);
void DELETECommand(void);
void SAVECASECommand(void);
void NOSAVECASECommand(void);
void CLEARCommand(void);
void WriteEXEBytes(void);
void CheckRelocationEntries(void);
void WriteWWBytes(void);

char EXEFileName[128],DEFFileName[128],DataFileName[128];
char LineBuffer[129],ParseBuffer[128],SingleCommandBuffer[128];
char *OriginalEXEBytes;
unsigned char *NewEXEBytes;
int *NewRelocationEntryStorage;
int NewRelocationEntryCount=0;
int REPLACEStringCount=0,NEWStringCount=0,DELETEStringCount=0;
int WWSize;
int SingleCommandFlag=0;
int BytesRelocated;
long EXEImageSize,DataFileSize;
FILE *fpexe,*fpdef,*fpdata;

struct StringStruct{
	char *NamePtr;
	struct StringStruct *NextPtr;
};

struct StringStruct *FirstREPLACEString,*FirstNEWString,*FirstDELETEString;
struct StringStruct *LastREPLACEString,*LastNEWString,*LastDELETEString;

struct {
	char Sig1;	// EXE signature byte 1, 'M'
	char Sig2;	// EXE signature byte 2, 'Z'
	unsigned int FileLenMod512;	// length of file modulo 512
	unsigned int FileLen512Page;	// length of file in 512-byte pages
	unsigned int RelocCount;	// count of relocation items
	unsigned int HeaderParaSize;	// size of header in paragraphs
	unsigned int MinAlloc;	// minimum number of paragraphs needed
	unsigned int MaxAlloc;	// maximum number of paragraphs needed
	unsigned int SSRegister;	// SS register value at entry
	unsigned int SPRegister;	// SP register value at entry
	unsigned int CheckSum;	// checksum of file
	unsigned int IPRegister;	// IP register value at entry
	unsigned int CSRegister;	// CS register value at entry
	unsigned int RelocOffset;	// offset to first relocation item in file
} EXEHeader,DataFileHeader;

struct {
	unsigned char NOP1;	// nop opcode
	unsigned char NOP2;	// nop opcode
	unsigned char JMP[2];	// jmp short opcode
	unsigned char sig[9];	// WARPWRAP signature
	unsigned int WrapperSize;	//size of wrappered code and data (for replacement)
	unsigned int RelocationEntryCount;	//count of relocation entries (word sized)
	unsigned int OriginalCS;	//original CS value
	unsigned int OriginalIP;	//original IP value
	unsigned int NewStringCount;	//count of new strings
	unsigned int NewStringLocation;	//offset to new strings
	unsigned int ReplaceStringCount;	//count of replace strings
	unsigned int ReplaceStringLocation;	//offset to replace strings
	unsigned int DeleteStringCount;	//count of delete strings
	unsigned int DeleteStringLocation;	//offset to delete strings
	unsigned char MemoryFillFlag;	//nonzero if memory fill required
	unsigned int MemoryFillValue;	//memory fill value
	unsigned int EXEImageSizePara;	// EXE image (physically loaded from file) size in paras
	unsigned int OverwrittenByteCount;	// count of overwritten bytes
} WrapData;

char *DefaultEXEExtension=".EXE";
char *DefaultDEFExtension=".DEF";
char *DefaultDEFName="WARPWRAP.DEF";
char *BaseDataFileName="WW";
char *DataFileExtension=".DTA";
char *OptionalPrefix="SET ";
char *WWSignature="WARPWRAP1";
char *commands[6]={
	"~NEW",
	"~REPLACE",
	"~DELETE",
	"~SAVECASE",
	"~NOSAVECASE",
	"~CLEAR"
};

int NEWFlag=0;
int REPLACEFlag=1;
int DELETEFlag=0;
int SAVECASEFlag=0;
int NOSAVECASEFlag=1;
int CLEARFlag=0;
unsigned int CLEARValue=0;
unsigned int TotalStringBytes=0;
unsigned int TotalREPLACEBytes=0;
unsigned int TotalNEWBytes=0;
unsigned int TotalDELETEBytes=0;

void main(int argc, char *argv[])
{
    credits();
	GetCommandLine(argc,argv);
	OpenEXEFile();
	OpenDEFFile();
	OpenDataFile();
	ReadEXEHeader();
	ComputeWWSize();
	ParseDEFFile();
	WriteEXEBytes();
	CheckRelocationEntries();
	WriteWWBytes();
}

void credits(void)
{
    printf("\nWARPWRAP, Version 2.0, Copyright 1993-1996, Michael Devore.\nAll rights reserved.");
    printf("\nEXE wrapper program for environment string variable burn-in and more.");
	printf("\nWARPWRAP written using CauseWay wetware technology.\n");
}

// get any command line options and arguments
void GetCommandLine(int argc, char *argv[])
{
	char Buffer[67];
	char FileDrive[MAXDRIVE];
	char FileDir[MAXDIR];
	char FileName[MAXFILE];
	char FileExt[MAXEXT];

	if(argc<2){	// no command line arguments
		printf("\nFile name? ");
		Buffer[0]=65;	// 64 chars in file name allowed
		cgets(Buffer);
		printf("\n");
	}
	else{	// first command line argument is file name
		strcpy(&Buffer[2],argv[1]);	// keep in same position as cgets() name
	}

	// check for valid file name
	if(!strlen(&Buffer[2])){	// null file name, assume termination desired
		exit(0);
	}
	fnsplit(&Buffer[2],FileDrive,FileDir,FileName,FileExt);

	if(argc>2 && Buffer[2]=='@' && Buffer[3]>' '){	// DEF file name specified
		strcpy(DEFFileName,&Buffer[3]);
		if(!FileExt[0]){	// no extension supplied, supply default
			strcat(DEFFileName,DefaultDEFExtension);
		}
		strupr(DEFFileName);	// make all caps
		strcpy(&Buffer[2],argv[2]);	// get EXE name in buffer
	}
	else if(argc>2 && Buffer[2]=='!' && Buffer[3]>' '){	// REPLACE command supplied
		SingleCommandFlag=1;	// flag single command only
		strcpy(SingleCommandBuffer,&Buffer[3]);
		strcpy(&Buffer[2],argv[2]);	// get EXE name in buffer
	}
	else{	// no DEF file name specified, use default
		strcpy(DEFFileName,DefaultDEFName);
	}

	strcpy(EXEFileName,&Buffer[2]);
	fnsplit(&Buffer[2],FileDrive,FileDir,FileName,FileExt);
	if(!FileExt[0]){	// no extension supplied, supply default
		strcat(EXEFileName,DefaultEXEExtension);
	}
	strupr(EXEFileName);	// make all caps

	strcpy(DataFileName,argv[0]);	// get data file name (WARPWRAP name)
}

void OpenEXEFile(void)
{
	fpexe=fopen(EXEFileName,"r+b");	// open EXE file
	if(fpexe==NULL){
		printf("Error opening %s\007\007\007\n",EXEFileName);
		exit(1);	// fatal error
	}
}

void OpenDEFFile(void)
{
	if(SingleCommandFlag){	// burn-in one REPLACE e-var only
		return;
	}
	fpdef=fopen(DEFFileName,"rb");	// open DEF file
	if(fpdef==NULL){
		printf("Error opening %s\007\007\007\n",DEFFileName);
		exit(1);	// fatal error
	}
}

void OpenDataFile(void)
{
	fpdata=fopen(DataFileName,"rb");	// open data file
	if(fpdata==NULL){
		printf("Error opening %s\007\007\007\n",DataFileName);
		exit(1);	// fatal error
	}
}

void ReadEXEHeader(void)
{
	char *InvalidEXEString="Invalid EXE File Format";
	int bytes;

	rewind(fpexe);
	bytes=fread(&EXEHeader,1,sizeof(EXEHeader),fpexe);	// read EXE header info
	if(bytes<sizeof(EXEHeader) || EXEHeader.Sig1!='M' || EXEHeader.Sig2!='Z'){
		// short read on EXE header or signature bytes don't match
		printf("File %s is %s\007\007\007\n",EXEFileName,InvalidEXEString);
		exit(1);	// fatal error
	}

	EXEImageSize=(long)EXEHeader.FileLen512Page*512L;	// total size of EXE file in 512-byte pages
	EXEImageSize-=(long)EXEHeader.HeaderParaSize*16L;	// subtract off header
	if(EXEHeader.FileLenMod512){	// odd byte count on last page
		EXEImageSize-=512L;	// back off last page amount
		EXEImageSize+=EXEHeader.FileLenMod512;	// add in odd bytes
	}
}

void ComputeWWSize(void)
{
	rewind(fpdata);
	fread(&DataFileHeader,1,sizeof(DataFileHeader),fpdata);	// read data file EXE header info
	DataFileSize=(long)DataFileHeader.FileLen512Page*512L;	// total size of EXE file in 512-byte pages
	if(DataFileHeader.FileLenMod512){	// odd byte count on last page
		DataFileSize-=512L;	// back off last page amount
		DataFileSize+=DataFileHeader.FileLenMod512;	// add in odd bytes
	}
	// seek to end of EXE file
	fseek(fpdata,0L,SEEK_END);
	// compute size of appended WARPWRAP data file
	WWSize=(int)(ftell(fpdata)-DataFileSize);
#ifdef DEBUG
	printf("\nDataFileName=%s  WWSize=%d  DataFileSize=%d\n",DataFileName,WWSize,DataFileSize);
#endif

	NewEXEBytes=malloc(WWSize-sizeof(WrapData));	// allocate space for new EXE bytes besides WARPWRAP data
	if(NewEXEBytes==NULL){
		printf("Out of memory.\007\007\007\n");
		exit(1);
	}
	// seek to start of WARPWRAP data
	fseek(fpdata,-WWSize,SEEK_END);
	// read first 13 bytes (instruction opcodes and signature) into WrapData structure
	fread(&WrapData,1,13,fpdata);
#ifdef DEBUG
	printf("WrapData first four bytes == %x %x %x %x\n",WrapData.NOP1,\
		WrapData.NOP2,WrapData.JMP[0],WrapData.JMP[1]);
#endif

	// seek past WARPWRAP data to new code
	fseek(fpdata,sizeof(WrapData)-13,SEEK_CUR);
	// read the new code bytes
	fread(NewEXEBytes,1,WWSize-sizeof(WrapData),fpdata);
}

void ParseDEFFile(void)
{
	int i,pos,LineLen,CommandLen;
	int CommandFlag;

	if(SingleCommandFlag){	// no DEF file, single REPLACE command only
		REPLACECommand();
		NOSAVECASECommand();
		strcpy(ParseBuffer,SingleCommandBuffer);
		strcpy(LineBuffer,SingleCommandBuffer);
		ParseString();
		return;
	}
    while(fgets(LineBuffer,129,fpdef)!=NULL){	// read until file end
		LineLen=strlen(LineBuffer);
		if(LineLen>=128){
			printf("Line in file %s too long:\n%s\007\007\007\n",DEFFileName,LineBuffer);
			exit(1);
		}
		pos=CommandFlag=0;
		while(LineBuffer[pos]<=' ' && LineBuffer[pos]){	// scan past leading whitespace
			pos++;
		}
		strcpy(ParseBuffer,&LineBuffer[pos]);
		for(i=0;i<6;i++){
			CommandLen=strlen(commands[i]);
			if(!strncmpi(ParseBuffer,commands[i],CommandLen)){	// command match
				switch(i){
					case 0:	// new
						NEWCommand();
						CommandFlag=1;
						break;
					case 1:	// replace
						REPLACECommand();
						CommandFlag=1;
						break;
					case 2:	// delete
						DELETECommand();
						CommandFlag=1;
						break;
					case 3:	// savecase
						SAVECASECommand();
						CommandFlag=1;
						break;
					case 4:	// nosavecase
						NOSAVECASECommand();
						CommandFlag=1;
						break;
					case 5:	// clear
						CLEARCommand();
						CommandFlag=1;
						break;
				}
			}
		}
		if(!CommandFlag && ParseBuffer[0]!='#' && ParseBuffer[0]){
			// parse as a noncommand, noncomment, non-null line
			ParseString();
		}
	}
}

void ParseString(void)
{
	int len,pos=0;

	// kill optional prefix on environment variable strings
	len=strlen(OptionalPrefix);
	if(!strncmpi(ParseBuffer,OptionalPrefix,len)){
		pos+=len;
	}

	if(NOSAVECASEFlag){	// turn to all caps
		strupr(ParseBuffer);
	}

	// scan off leading whitespace, could occur with optional prefix
	while(ParseBuffer[pos]<=' ' && ParseBuffer[pos]){
		pos++;
	}
	if(ParseBuffer[pos]){	// have a non-null string
		if(REPLACEFlag){
			REPLACEString(pos);
		}
		else if(NEWFlag){
			NEWString(pos);
		}
		else{	// assume delete
			DELETEString(pos);
		}
	}
}

void REPLACEString(int pos)
{
	char c,string[128];
	char *sptr;
	int EqualsPos=0,i,len;
	struct StringStruct *ss,*oldptr;

	i=0;
	while((c=ParseBuffer[pos])>=' '){
		string[i++]=c;
		if(c=='='){
			if(!EqualsPos){
				EqualsPos=i-1;
			}
			else{	// multiple '=' in string, not allowed
				BadLine();
			}
		}
		pos++;
	}
	string[i]=0;

	len=strlen(string);
	if(EqualsPos==0 || EqualsPos>=len){	// no equals, equals at start, or equals at end
		BadLine();
	}

	// allocate space for string structure
	ss=calloc(1,sizeof(struct StringStruct));
	sptr=malloc(strlen(string)+1);
	if(ss==NULL || sptr==NULL){
		OutOfMemory();
	}

	if(REPLACEStringCount){	// previous string structures exist
		oldptr=LastREPLACEString;
		oldptr->NextPtr=ss;
	}
	else{
		FirstREPLACEString=ss;
	}

	strcpy(sptr,string);
	ss->NamePtr=sptr;
	LastREPLACEString=ss;

	TotalStringBytes+=strlen(sptr)+1;
	TotalREPLACEBytes+=strlen(sptr)+1;
	REPLACEStringCount++;	// bump count of replace strings
#ifdef DEBUG
	printf("\nreplace string = %s",sptr);
#endif
}

void BadLine(void)
{
	printf("Bad setting in %s%s, Line:\n%s\007\007\007\n",SingleCommandFlag?"":"file ",\
		SingleCommandFlag?"command string":DEFFileName,LineBuffer);
	exit(1);
}

void OutOfMemory(void)
{
	printf("Out of memory parsing %s%s, Line:\n%s\007\007\007\n",SingleCommandFlag?"":"file ",\
		SingleCommandFlag?"command string":DEFFileName,LineBuffer);
	exit(1);
}

void NEWString(int pos)
{
	char c,string[128];
	char *sptr;
	int EqualsPos=0,i,len;
	struct StringStruct *ss,*oldptr;

	i=0;
	while((c=ParseBuffer[pos])>=' '){
		string[i++]=c;
		if(c=='='){
			if(!EqualsPos){
				EqualsPos=i-1;
			}
			else{	// multiple '=' in string, not allowed
				BadLine();
			}
		}
		pos++;
	}
	string[i]=0;

	len=strlen(string);
	if(EqualsPos==0 || EqualsPos>=len){	// no equals, equals at start, or equals at end
		BadLine();
	}

	// allocate space for string structure
	ss=calloc(1,sizeof(struct StringStruct));
	sptr=malloc(strlen(string)+1);
	if(ss==NULL || sptr==NULL){
		OutOfMemory();
	}

	if(NEWStringCount){	// previous string structures exist
		oldptr=LastNEWString;
		oldptr->NextPtr=ss;
	}
	else{
		FirstNEWString=ss;
	}

	strcpy(sptr,string);
	ss->NamePtr=sptr;
	LastNEWString=ss;

	TotalStringBytes+=strlen(sptr)+1;
	TotalNEWBytes+=strlen(sptr)+1;
	NEWStringCount++;	// bump count of new strings
#ifdef DEBUG
	printf("\nnew string = %s",sptr);
#endif
}

void DELETEString(int pos)
{
	char c,string[128];
	char *sptr;
	int EqualsPos=0,i,len;
	struct StringStruct *ss,*oldptr;

	i=0;
	while((c=ParseBuffer[pos])>=' '){
		string[i++]=c;
		if(c=='='){
			if(!EqualsPos){
				EqualsPos=i-1;
				if(i==1){	// can't have starting equals
					BadLine();
				}
			}
			else{	// multiple '=' in string, not allowed
				BadLine();
			}
		}
		pos++;
	}
	string[i]=0;

	len=strlen(string);
	if(EqualsPos && EqualsPos!=len-1){	// equals given and NOT at end
		BadLine();
	}
	if(!EqualsPos){
		strcat(string,"=");	// add an equals on the end if none
	}

	// allocate space for string structure
	ss=calloc(1,sizeof(struct StringStruct));
	sptr=malloc(strlen(string)+1);
	if(ss==NULL || sptr==NULL){
		OutOfMemory();
	}

	if(DELETEStringCount){	// previous string structures exist
		oldptr=LastDELETEString;
		oldptr->NextPtr=ss;
	}
	else{
		FirstDELETEString=ss;
	}

	strcpy(sptr,string);
	ss->NamePtr=sptr;
	LastDELETEString=ss;

	TotalStringBytes+=strlen(sptr)+1;
	TotalDELETEBytes+=strlen(sptr)+1;
	DELETEStringCount++;	// bump count of delete strings
#ifdef DEBUG
	printf("\ndelete string = %s",sptr);
#endif
}

void NEWCommand(void)
{
	NEWFlag=1;
	REPLACEFlag=0;
	DELETEFlag=0;
}

void REPLACECommand(void)
{
	NEWFlag=0;
	REPLACEFlag=1;
	DELETEFlag=0;
}

void DELETECommand(void)
{
	NEWFlag=0;
	REPLACEFlag=0;
	DELETEFlag=1;
}

void SAVECASECommand(void)
{
	SAVECASEFlag=1;
	NOSAVECASEFlag=0;
}

void NOSAVECASECommand(void)
{
	SAVECASEFlag=0;
	NOSAVECASEFlag=1;
}

void CLEARCommand(void)
{
	CLEARFlag=1;
	sscanf(&ParseBuffer[6],"%x",&CLEARValue);
#ifdef DEBUG
	printf("\n&ParseBuffer[6]=%s  CLEARValue= %x\n",&ParseBuffer[6],CLEARValue);
	exit(1);
#endif
}

void WriteEXEBytes(void)
{
	// compute # of bytes to write over in EXE
	BytesRelocated=WWSize+TotalStringBytes;
	if(BytesRelocated>EXEImageSize){	// EXE too small for changes
		printf("File %s too small to hold modification code.\007\007\007\n",EXEFileName);
		exit(1);
	}
	OriginalEXEBytes=malloc(BytesRelocated);	// allocate space for original EXE bytes for their transfer
	if(OriginalEXEBytes==NULL){
		printf("Out of memory.\007\007\007\n");
		exit(1);
	}

	// seek to start of bytes to move to end of EXE file and write over
	fseek(fpexe,(long)EXEHeader.HeaderParaSize*16L,SEEK_SET);
	// read the bytes
	fread(OriginalEXEBytes,1,BytesRelocated,fpexe);

	// check for WARPWRAP signature
	if(!strncmp(&OriginalEXEBytes[4],WWSignature,9)){
		printf("File %s already WARPWRAP processed.\007\007\007\n",EXEFileName);
		exit(1);
	}

	// seek to end of EXE file and append the original bytes
	fseek(fpexe,0L,SEEK_END);
	// write the bytes
	fwrite(OriginalEXEBytes,1,BytesRelocated,fpexe);
}

void CheckRelocationEntries(void)
{
	int i,j,ChangeFlag=0;
	unsigned long RelocationValue;
	unsigned char *rptr;

	if(EXEHeader.RelocCount){
		rptr=malloc(EXEHeader.RelocCount*4);
		NewRelocationEntryStorage=malloc(EXEHeader.RelocCount*2);
		if(rptr==NULL || NewRelocationEntryStorage==NULL){
			printf("Out of memory\007\007\007\n");
			exit(1);
		}
		// seek to doubleword relocation entries, read them in
		fseek(fpexe,EXEHeader.RelocOffset,SEEK_SET);
		fread(rptr,1,EXEHeader.RelocCount*4,fpexe);
	}

	for(i=0;i<EXEHeader.RelocCount;i++){
		RelocationValue=(unsigned long)*(rptr+(i*4));
		RelocationValue+=256L*(unsigned long)(*(rptr+(i*4)+1));
		RelocationValue+=16L*(unsigned long)(*(rptr+(i*4)+2));
		RelocationValue+=4096L*(unsigned long)(*(rptr+(i*4)+3));
		if(RelocationValue<=BytesRelocated+1){	// relocation entry within target zone
			ChangeFlag=1;	// flag changed in relocation entries occurred
			NewRelocationEntryStorage[NewRelocationEntryCount]=(int)RelocationValue;
			NewRelocationEntryCount++;
			// zero out relocation entry in EXE header image
			for(j=0;j<3;j++){
				*(rptr+(i*4))=0;
				*(rptr+(i*4)+1)=0;
				*(rptr+(i*4)+2)=0;
				*(rptr+(i*4)+3)=0;
			}
#ifdef DEBUG
	printf("\ni=%x  NewRelocationEntryStorage[i]=%x\n",i,NewRelocationEntryStorage[i]);
#endif
		}
	}

	// save original CS:IP from EXE
	WrapData.OriginalCS=EXEHeader.CSRegister;
	WrapData.OriginalIP=EXEHeader.IPRegister;

	// update EXE header with new CS:IP (0:2)
	EXEHeader.CSRegister=0;
	EXEHeader.IPRegister=2;
	rewind(fpexe);
	fwrite(&EXEHeader,1,sizeof(EXEHeader),fpexe);	// write new EXE header info

	if(ChangeFlag){	// must update EXE relocation entry tables
		// seek to doubleword relocation entries, write updated image
		fseek(fpexe,EXEHeader.RelocOffset,SEEK_SET);
		fwrite(rptr,1,EXEHeader.RelocCount*4,fpexe);
	}
	if(EXEHeader.RelocCount){
		free(rptr);	// free unneeded image
	}
	// seek to end of EXE file and append relocation entries
	fseek(fpexe,0L,SEEK_END);
	// write relocation entries
	fwrite(&NewRelocationEntryStorage[0],1,NewRelocationEntryCount*2,fpexe);
}

void WriteWWBytes(void)
{
	int i,len;
	struct StringStruct *ss;

	// seek to start of bytes to write over
	fseek(fpexe,(long)EXEHeader.HeaderParaSize*16L,SEEK_SET);

	// update WARPWRAP data, CS:IP already updated
	WrapData.WrapperSize=WWSize;
	WrapData.RelocationEntryCount=NewRelocationEntryCount;
	WrapData.NewStringCount=NEWStringCount;
	WrapData.NewStringLocation=WWSize;
	WrapData.ReplaceStringCount=REPLACEStringCount;
	WrapData.ReplaceStringLocation=WrapData.NewStringLocation+TotalNEWBytes;
	WrapData.DeleteStringCount=DELETEStringCount;
	WrapData.DeleteStringLocation=WrapData.ReplaceStringLocation+TotalREPLACEBytes;
	WrapData.MemoryFillFlag=CLEARFlag;
	WrapData.MemoryFillValue=CLEARValue;
	WrapData.EXEImageSizePara=(int)((EXEImageSize+15L)/16L);
	WrapData.OverwrittenByteCount=BytesRelocated;

	// write wrapper data
	fwrite(&WrapData,1,sizeof(WrapData),fpexe);
	// write wrapper code
	fwrite(NewEXEBytes,1,WWSize-sizeof(WrapData),fpexe);
	// write new strings
	ss=FirstNEWString;
	for(i=0;i<NEWStringCount;i++){
		len=strlen(ss->NamePtr)+1;
		fwrite(ss->NamePtr,1,len,fpexe);
		ss=ss->NextPtr;
	}
	// write replace strings
	ss=FirstREPLACEString;
	for(i=0;i<REPLACEStringCount;i++){
		len=strlen(ss->NamePtr)+1;
		fwrite(ss->NamePtr,1,len,fpexe);
		ss=ss->NextPtr;
	}
	// write delete strings
	ss=FirstDELETEString;
	for(i=0;i<DELETEStringCount;i++){
		len=strlen(ss->NamePtr)+1;
		fwrite(ss->NamePtr,1,len,fpexe);
		ss=ss->NextPtr;
	}
}
