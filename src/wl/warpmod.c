/*
    WARPMOD.C

    Copyright 1992, Michael E. Devore, All rights reserved

    programmed in the small memory model of Turbo C++ version 2.0
    started 07-23-92
    last changed 04-12-94

    Modify WarpLink 2.5+ linked application settings
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dir.h>
#include <conio.h>
#include <io.h>
#include <time.h>
#include <dos.h>
#include <ctype.h>
#include <sys\stat.h>
#include "warpmod.h"
#include "keys.h"

#define	ON	1
#define OFF	0
#define BLANKS(A) (&blanks[80-A])

char blanks[81]={
"                                                                                "
};

char WarpModFileName[128],IOBuffer[8192];
int DateOptionCount;
int ASOptionCount;
int STOptionCount;
long OMOptionPtrFilePos;	// file position of overlay manager option pointers
struct stat statbuff;
struct ftime FileTime;
FILE *fp;

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
} EXEHeader;

int OptionInfoCount=0;
int BatchMode=OFF;
int OverlayManagerPresent=OFF;
int ChangesMade=OFF;
int PreserveDateFlag=OFF;

int BackgroundColor=BLUE;
int ForegroundColor=LIGHTGRAY;
int CommandBackColor=LIGHTGRAY;
int CommandForeColor=BLUE;
int IntensityColor=WHITE;
int HighlightColor=RED;

char *DefaultExt=".EXE";

struct {
	char *OptionString;	// option name
	int OptionStringX;
	int OptionStringY;
	char HighlightChar;	// highlighted char in option name for quick key
	int HighlightCharX;
	int HighlightCharY;
	struct {	// option value flags;
		unsigned char BooleanOnOff:1;	// on/off
		unsigned char BooleanDate:1;	// Change/Preserve
		unsigned char SignedInt:1;	// signed int value
		unsigned char OptionalSignedInt:1;	// optional value (zero value != blank)
		unsigned char UnsignedInt:1;	// unsigned int value
		unsigned char RequiredString:1;	// string for option
		unsigned char BooleanAndString:1;	// on/off and optional string
	} flags;
	union {
		signed int SignedValue;
		unsigned int UnsignedValue;
	} OldValues;
	union {
		signed int SignedValue;
		unsigned int UnsignedValue;
	} NewValues;
	union {	// low boundary for numeric value
		signed int SignedValue;
		unsigned int UnsignedValue;
	} LowBoundary;
	union {	// high boundary for numeric value
		signed int SignedValue;
		unsigned int UnsignedValue;
	} HighBoundary;
	long ValueMultiplier;	// amount to multiply value by to get real (saved) value (/op, /os)
	int OldValueExists;	// nonzero if optional value and exists
	int OldValueX;
	int OldValueY;
	int NewValueX;
	int NewValueY;
	char *ValueFormatString;	// printf() format string for values/strings
	char *OldString;	// only used by /ox, /on option
	char *NewString;
	int OldStringX;
	int OldStringY;
	int NewStringX;
	int NewStringY;
	char *CursorUpOption;	// option moved to for appropriate cursor move
	char *CursorDownOption;
	char *CursorLeftOption;
	char *CursorRightOption;
} OptionInfo[14];

int BatchDStatus=OFF;
int BatchASStatus=OFF;
int BatchSTStatus=OFF;
int BatchOPStatus=OFF;
int BatchOSStatus=OFF;
int BatchOHPStatus=OFF;
int BatchOLStatus=OFF;
int BatchOHP3Status=OFF;
int BatchORPStatus=OFF;
int BatchOHTStatus=OFF;
int BatchOUStatus=OFF;
int BatchORTStatus=OFF;
int BatchONStatus=OFF;
int BatchOXStatus=OFF;
int BatchOXStringStatus=OFF;
unsigned int BatchDValue;
int BatchASValue;
int BatchSTValue;
int BatchOPValue;
unsigned int BatchOSValue;
int BatchOHPValue;
unsigned int BatchOLValue;
int BatchOHP3Value;
unsigned int BatchORPValue;
int BatchOHTValue;
unsigned int BatchOUValue;
unsigned int BatchORTValue;
unsigned int BatchONValue;
unsigned int BatchOXValue;

char ONStringValue[13]={0};
char OXStringValue[32]={0};
char BatchONString[13]={0};
char BatchOXString[32]={0};

char *DateString="Date/timestamp";
char *ASString="AS:";
char *STString="ST:";
char *OPString="OP:";
char *OSString="OS:";
char *OHPString="OHP:";
char *OLString="OL:";
char *OHP3String="OHP3:";
char *ORPString="ORP";
char *OHTString="OHT:";
char *OUString="OU";
char *ORTString="ORT";
char *ONString="ON:";
char *OXString="OX:";
char *BooleanONString="[ON ]";
char *BooleanOFFString="[OFF]";
char *BooleanCHANGEString=" CHANGE ";
char *BooleanPRESERVEString="PRESERVE";

char *Format2u="%2u";
char *Format3u="%3u";
char *Format5u="%5u";
char *Format4d="%4d";
char *Format6d="%6d";
char *Format12s="%-12s";
char *Format31s="%-31s";

char *MainPromptString="Press Esc to exit WarpMod, F10 to reset options to original values";

// offsets (pointers) to overlay manager options, after signature bytes
struct OMOptionOffsets{
	char Pad[133];	// overlay file name+sig chars
	int OverlayPool;	// -512 to 512, long
	int PoolAllocFlag;	// char
	int OHPUsedFlag;	// char
	int OHPValue;	// -16383 to 16383, int
	int OHPAllocFlag;	// char
	int OHP3UsedFlag;	// char
	int OHTUsedFlag;	// char
	int OHTValue;	// -16383 to 16383, int
	int OHTAllocFlag;	// char
	int OUFlag;		// char
	int OSValue;	// 1-63, int
	int OLValue;	// 1-512, int
	int ORPFlag;	// char
	int ORTFlag;	// char
	int ONName;	// on string, 12 chars
	int OXFlag;	// char
	int OXEnvVar;	// ox environment variable string, 32 chars
};

struct OMOptionOffsets *opt;

void main(int argc, char *argv[])
{
	char *NoChangeString="No changes made to option settings.";

	memset(OptionInfo,0,sizeof(OptionInfo));	// zero init option information
	GetCommandLine(argc,argv);
	GetNonOMInfo();	// get nonoverlay manager info
	SearchForOMText();	// search for and read in overlay manager text, if exists
	ClearFeedBackLine();
	if(OverlayManagerPresent){
		GetOMOptions();	// get the overlay manager options
	}
	ShowInfo();	// show the information
	if(!BatchMode){	// not running batch mode, allow info editing
		EditInfo();
	}
	else{
		ShowNewValues();	// running batch mode, show specified changes
	}
	CheckChanges();	// see if any changes to information
	if(ChangesMade){	// changes made to file options, save them
		SaveInfo();
	}
	else{
		DisplayFeedBack(NoChangeString);
	}
	QuitWarpMod();
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
	}
	else{	// first command line argument is file name
		strcpy(&Buffer[2],argv[1]);	// keep in same position as cgets() name
	}

	// check for valid file name
	if(!strlen(&Buffer[2])){	// null file name, assume termination desired
		exit(0);
	}
	fnsplit(&Buffer[2],FileDrive,FileDir,FileName,FileExt);
	strcpy(WarpModFileName,&Buffer[2]);
	if(!FileExt[0]){	// no extension supplied, supply default
		strcat(WarpModFileName,DefaultExt);
	}
	strupr(WarpModFileName);	// make all caps
	if(argc>2){	// edit options exist
		GetCommandOptions(argc,argv);
	}
	ShowShell();	// show the WarpMod display shell
}

// get command options
void GetCommandOptions(int argc, char *argv[])
{
	char string[81];
	int loop,i,ValidFlag;
	long tlong;
	char *FollowString,FollowChar;
	char *BatchStrings[]={
		"D","AS:","ST:","OP:",
		"OS:","OHP:","OL:","OHP3:",
		"ORP","OHT:","OU","ORT",
		"ON:","OX","MONO"
	};

	BatchMode=OFF;	// turn off batch mode flag (interactive)
	for(loop=2;loop<argc;loop++){
		ValidFlag=OFF;
		for(i=0;i<15;i++){
			if(!strncmpi(BatchStrings[i],argv[loop],strlen(BatchStrings[i]))){
				FollowString=&argv[loop][strlen(BatchStrings[i])];
				ValidFlag=ON;
				break;
			}
			else if(!strncmpi(BatchStrings[i],&argv[loop][1],strlen(BatchStrings[i])) && \
				(argv[loop][0]=='/' || argv[loop][0]=='-')){
				FollowString=&argv[loop][1+strlen(BatchStrings[i])];
				ValidFlag=ON;
				break;
			}
		}
		if(!ValidFlag){	// invalid option
			sprintf(string,"Invalid option: %s",argv[loop]);
			DisplayError(string);
			QuitWarpMod();
		}

		ValidFlag=OFF;	// shut off for value check
		FollowChar=*FollowString;	// get first char of following string
		switch(i){	// check which option was specified, act on it
			case 0:	// d
				BatchDStatus=ON;
				BatchDValue=ON;
				ValidFlag=ON;	// always valid
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 1:	// as:
				BatchASStatus=ON;
				if(isdigit(FollowChar)){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong<65536L){	// make sure in range
						BatchASValue=(unsigned int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 2: // st:
				BatchSTStatus=ON;
				if(isdigit(FollowChar)){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong<65536L){	// make sure in range
						BatchSTValue=(unsigned int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 3:	// op:
				BatchOPStatus=ON;
				if(isdigit(FollowChar) || FollowChar=='-'){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong>=-512 && tlong<=512){	// make sure in range
						BatchOPValue=(signed int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 4:	// os:
				BatchOSStatus=ON;
				if(isdigit(FollowChar)){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong<63 && tlong>0){	// make sure in range
						BatchOSValue=(unsigned int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 5:	// ohp
				BatchOHPStatus=ON;
				if(isdigit(FollowChar) || FollowChar=='-'){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong>=-16383 && tlong<=16383){	// make sure in range
						BatchOHPValue=(signed int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 6:	// ol
				BatchOLStatus=ON;
				if(isdigit(FollowChar)){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong<=512 && tlong>0){	// make sure in range
						BatchOLValue=(unsigned int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 7:	// ohp3
				BatchOHP3Status=ON;
				if(isdigit(FollowChar) || FollowChar=='-'){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong>=-16383 && tlong<=16383){	// make sure in range
						BatchOHP3Value=(signed int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 8:	// orp
				BatchORPStatus=ON;
				if(FollowChar=='+' || FollowChar<=' '){	// turn on option (explicit or implicit)
					BatchORPValue=ON;
					ValidFlag=ON;
				}
				else if (FollowChar=='-'){	// turn off option
					BatchORPValue=OFF;
					ValidFlag=ON;
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 9:	// oht:
				BatchOHTStatus=ON;
				if(isdigit(FollowChar) || FollowChar=='-'){	// valid digit char
					tlong=atol(FollowString);	// get numeric string following
					if(tlong>=-16383 && tlong<=16383){	// make sure in range
						BatchOHTValue=(signed int)tlong;
						ValidFlag=ON;
					}
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 10:	// ou
				BatchOUStatus=ON;
				if(FollowChar=='+' || FollowChar<=' '){	// turn on option (explicit or implicit)
					BatchOUValue=ON;
					ValidFlag=ON;
				}
				else if (FollowChar=='-'){	// turn off option
					BatchOUValue=OFF;
					ValidFlag=ON;
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 11:	// ort
				BatchORTStatus=ON;
				if(FollowChar=='+' || FollowChar<=' '){	// turn on option (explicit or implicit)
					BatchORTValue=ON;
					ValidFlag=ON;
				}
				else if (FollowChar=='-'){	// turn off option
					BatchORTValue=OFF;
					ValidFlag=ON;
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 12:	// on:
				BatchONStatus=ON;
				ValidFlag=ON;
				strncpy(BatchONString,FollowString,12);
				BatchONString[12]=0;
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 13:	// ox
				BatchOXStatus=ON;
				if (FollowChar=='-'){	// turn off option
					BatchOXValue=OFF;
					ValidFlag=ON;
				}
				else if(FollowChar==':'){	// supplied string, turn on
					BatchOXValue=ON;
					BatchOXStringStatus=ON;
					ValidFlag=ON;
					strncpy(BatchOXString,FollowString+1,31);
					BatchOXString[31]=0;
				}
				BatchMode=ON;	// turn on batch mode flag (not interactive)
				break;
			case 14:	//MONO
				BackgroundColor=BLACK;
				HighlightColor=WHITE;
				CommandBackColor=BLACK;
				CommandForeColor=WHITE;
				ValidFlag=ON;	// always valid
				break;
		}
		if(!ValidFlag){	// bad option value
			sprintf(string,"Bad option setting: %s",argv[loop]);
			DisplayError(string);
			QuitWarpMod();
		}
	}
}

// show the warpmod shell while reading file
void ShowShell(void)
{
	int i,len;
	char *WarpModSkeleton[23]={
		" ษอ FILE: ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป",
		" บ                                                                            บ",
		" ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน",
		" บ                                                                            บ",
		" ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" บ                                                                            บ",
		" ศออออออออออออออ WARPMOD 2.7, Copyright 1992-94, Michael Devore ออออออออออออออผ",
	};

	_setcursortype(_NOCURSOR);	// turn off cursor
	window(1,1,80,23);
	textcolor(ForegroundColor);
	textbackground(BackgroundColor);
	clrscr();

	// stuff file name in skeleton
	len=strlen(WarpModFileName);
	for(i=0;i<len;i++){
		WarpModSkeleton[0][10+i]=WarpModFileName[i];
	}
	WarpModSkeleton[0][10+i]=' ';	// trailing space after name

	for(i=0;i<23;i++){
		gotoxy(1,i+1);
		cprintf(WarpModSkeleton[i]);
	}
	ClearFeedBackLine();
}

// clear the feedback line with inverse
void ClearFeedBackLine(void)
{
	textcolor(BackgroundColor);
	textbackground(ForegroundColor);
	gotoxy(5,20);
	cprintf(BLANKS(72));

	// restore default color scheme
	textcolor(ForegroundColor);
	textbackground(BackgroundColor);
}

void GetNonOMInfo(void)
{
	char string[128];

	fp=fopen(WarpModFileName,"r+b");	// open warpmod'ed file
	if(fp==NULL){
		sprintf(string,"Error opening %s\007\007\007",WarpModFileName);
		DisplayError(string);
		QuitWarpMod();	// fatal error
	}

	sprintf(string,"Reading %s...",WarpModFileName);
	DisplayFeedBack(string);
	delay(1000);	// make sure feedback is readable

	fstat(fileno(fp),&statbuff);	// get file statistics
	ReadEXEHeader();
	DisplayLoadImageSize();
	DisplayEXESize();

	// set up option information
	GetDateOptionInfo();
	GetASOptionInfo();
	GetSTOptionInfo();

	DisplayNonOMOptions();
}

void GetDateOptionInfo(void)
{
	// datestamp info
	DateOptionCount=OptionInfoCount;	// keep pointer in case of overlays, changing cursor movement
	OptionInfo[OptionInfoCount].OptionString=DateString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=4;
	OptionInfo[OptionInfoCount].HighlightChar='D';
	OptionInfo[OptionInfoCount].HighlightCharX=5;
	OptionInfo[OptionInfoCount].HighlightCharY=4;
	OptionInfo[OptionInfoCount].flags.BooleanDate=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=OFF;
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchDStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchDValue;
	}
	OptionInfo[OptionInfoCount].OldValueX=51;
	OptionInfo[OptionInfoCount].OldValueY=4;
	OptionInfo[OptionInfoCount].NewValueX=51;
	OptionInfo[OptionInfoCount].NewValueY=4;
	OptionInfo[OptionInfoCount].CursorRightOption=ASString;
	OptionInfo[OptionInfoCount].CursorLeftOption=STString;
	OptionInfo[OptionInfoCount].CursorUpOption=STString;
	OptionInfo[OptionInfoCount].CursorDownOption=ASString;
	OptionInfoCount++;	// move to next option
}

void GetASOptionInfo(void)
{
	// as option info
	ASOptionCount=OptionInfoCount;	// keep pointer in case of overlays, changing cursor movement
	OptionInfo[OptionInfoCount].OptionString=ASString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=8;
	OptionInfo[OptionInfoCount].HighlightChar='A';
	OptionInfo[OptionInfoCount].HighlightCharX=5;
	OptionInfo[OptionInfoCount].HighlightCharY=8;
	OptionInfo[OptionInfoCount].flags.UnsignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=EXEHeader.MaxAlloc;
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchASStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchASValue;
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format5u;
	OptionInfo[OptionInfoCount].LowBoundary.UnsignedValue=0;
	OptionInfo[OptionInfoCount].HighBoundary.UnsignedValue=65535U;
	OptionInfo[OptionInfoCount].OldValueX=18;
	OptionInfo[OptionInfoCount].OldValueY=8;
	OptionInfo[OptionInfoCount].NewValueX=10;
	OptionInfo[OptionInfoCount].NewValueY=8;
	OptionInfo[OptionInfoCount].CursorRightOption=STString;
	OptionInfo[OptionInfoCount].CursorLeftOption=DateString;
	OptionInfo[OptionInfoCount].CursorUpOption=DateString;
	OptionInfo[OptionInfoCount].CursorDownOption=STString;
	OptionInfoCount++;	// move to next option
}

void GetSTOptionInfo(void)
{
	// st option info
	STOptionCount=OptionInfoCount;	// keep pointer in case of overlays, changing cursor movement
	OptionInfo[OptionInfoCount].OptionString=STString;
	OptionInfo[OptionInfoCount].OptionStringX=54;
	OptionInfo[OptionInfoCount].OptionStringY=8;
	OptionInfo[OptionInfoCount].HighlightChar='S';
	OptionInfo[OptionInfoCount].HighlightCharX=54;
	OptionInfo[OptionInfoCount].HighlightCharY=8;
	OptionInfo[OptionInfoCount].flags.UnsignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=EXEHeader.SPRegister;
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchSTStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchSTValue;
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format5u;
	OptionInfo[OptionInfoCount].LowBoundary.UnsignedValue=0;
	OptionInfo[OptionInfoCount].HighBoundary.UnsignedValue=65535U;
	OptionInfo[OptionInfoCount].OldValueX=67;
	OptionInfo[OptionInfoCount].OldValueY=8;
	OptionInfo[OptionInfoCount].NewValueX=59;
	OptionInfo[OptionInfoCount].NewValueY=8;
	OptionInfo[OptionInfoCount].CursorRightOption=DateString;	// possibly changed to OPString
	OptionInfo[OptionInfoCount].CursorLeftOption=ASString;
	OptionInfo[OptionInfoCount].CursorUpOption=ASString;
	OptionInfo[OptionInfoCount].CursorDownOption=DateString;
	OptionInfoCount++;	// move to next option
}

void ReadEXEHeader(void)
{
	char *InvalidEXEString="Invalid EXE File Format";
	int bytes;

	bytes=fread(&EXEHeader,1,sizeof(EXEHeader),fp);	// read EXE header info
	if(bytes<sizeof(EXEHeader) || EXEHeader.Sig1!='M' || EXEHeader.Sig2!='Z'){
		// short read on EXE header or signature bytes don't match
		DisplayError(InvalidEXEString);
		QuitWarpMod();	// fatal error
	}
}

void DisplayLoadImageSize(void)
{
	long LoadImageSize;

	LoadImageSize=(long)EXEHeader.FileLen512Page*512L;	// total size of file in 512-byte pages
	LoadImageSize-=(long)EXEHeader.HeaderParaSize*16L;	// subtract off header
	if(EXEHeader.FileLenMod512){	// odd byte count on last page
		LoadImageSize-=512L;	// back off last page amount
		LoadImageSize+=EXEHeader.FileLenMod512;	// add in odd bytes
	}
	LoadImageSize+=(long)EXEHeader.MinAlloc*16L;	// add in amount needed above program
	gotoxy(5,2);
	cprintf("Load Image Size: %ld",LoadImageSize);
}

void DisplayEXESize(void)
{
	gotoxy(31,2);
	cprintf("EXE File Size: %ld",statbuff.st_size);
}

void DisplayNonOMOptions(void)
{
	char *NonOMOptStrings[]={
		"ออออออออออออออออออัออออออออออออออออออออัออออออออออออออออออออออ",
		" New     Old      ณ NONOVERLAY OPTIONS ณ          New     Old ",
		"ฤฤฤฤฤ   ฤฤฤฤฤ     ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู         ฤฤฤฤฤ   ฤฤฤฤฤ"
	};
	int i;

	for(i=0;i<3;i++){
		gotoxy(10,5+i);
		cprintf(NonOMOptStrings[i]);
	}

	ShowOptionStrings(OFF);	// show strings AND values

	getftime(fileno(fp),&FileTime);	// get file date/time
	gotoxy(22,4);
	cprintf("%02u-%02u-%02u  %2u:%02u%s",FileTime.ft_month,FileTime.ft_day,FileTime.ft_year+80, \
		FileTime.ft_hour,FileTime.ft_min,FileTime.ft_hour<12?"a":"p");

}

void SearchForOMText(void)
{
    int i,FailMatchFlag,BytesRead;
    int pos;
	long SigLength;
	char Signature[]={
		"\r\nFATAL DOS Error, Code "
		"\r\nFATAL Overlay Manager Error, Code "
		"0000h\r\n"
		"PATH="
//		"…"	// ^M^E^D, high bit set
//		"V1"
	};
	char *SearchOverlayString="Searching for overlay information...";

	DisplayFeedBack(SearchOverlayString);
	delay(1000);	// make sure feedback is readable

	SigLength=sizeof(Signature)-1L;
	fseek(fp,EXEHeader.HeaderParaSize*16,SEEK_SET); // position past header bytes
    while((BytesRead=fread(IOBuffer,1,sizeof(IOBuffer),fp))>0){
        pos=0;
        while(pos<BytesRead-SigLength){	// check possible positions up to maximum start of signature
            FailMatchFlag=OFF;
            for(i=0;i<SigLength;i++){	// check all bytes in signature
                if((Signature[i] & 0x7f) != IOBuffer[pos+i]){	// mask off signature high bits
                    FailMatchFlag=ON;	// signature check failed
                    break;
                }
            }
            if(!FailMatchFlag){  // success finding signature
				OMOptionPtrFilePos=ftell(fp);	// adjust past overlay name+sig chars
				OMOptionPtrFilePos-=(BytesRead-SigLength-pos);
				fseek(fp,OMOptionPtrFilePos,SEEK_SET); // position to just past signature
				fread(IOBuffer,1,sizeof(IOBuffer),fp);    // read pointer bytes and later values
				opt=(struct OMOptionOffsets *)IOBuffer;	// options pointer structure -> start of info
				CursorOMMovement();	// update nonoverlay options for overlay option cursor movement
				OverlayManagerPresent=ON;	// flag overlay manager is present
				return;	// return on successful find
            }
            pos++;	// try next byte position
        }
        if(BytesRead==sizeof(IOBuffer)){	// another block to read
            fseek(fp,-SigLength,SEEK_CUR);	// adjust in case of signature spanning blocks
		}
        else{	// final block tested and failed
			return;
		}
    }
	return;	// final block tested and failed
}

// update nonoverlay options for overlay option cursor movement
void CursorOMMovement(void)
{
	OptionInfo[STOptionCount].CursorRightOption=OPString;
	OptionInfo[STOptionCount].CursorDownOption=OSString;
	OptionInfo[STOptionCount].CursorUpOption=OXString;
	OptionInfo[ASOptionCount].CursorDownOption=OPString;
	OptionInfo[DateOptionCount].CursorUpOption=ORTString;
	OptionInfo[DateOptionCount].CursorLeftOption=OXString;
}

void DisplayFeedBack(char *string)
{
	ClearFeedBackLine();
	textcolor(BackgroundColor);
	textbackground(ForegroundColor);
	gotoxy(6,20);
	cprintf(string);

	// restore default color scheme
	textcolor(ForegroundColor);
	textbackground(BackgroundColor);
}

void DisplayError(char *string)
{
	ClearFeedBackLine();
	textcolor(HighlightColor);
	textbackground(ForegroundColor);
	gotoxy(6,20);
	cprintf(string);

	// restore default color scheme
	textcolor(ForegroundColor);
	textbackground(BackgroundColor);
}

void GetOMOptions(void)
{
	GetOPOptionInfo();
	GetOSOptionInfo();
	GetOHPOptionInfo();
	GetOLOptionInfo();
	GetOHP3OptionInfo();
	GetORPOptionInfo();
	GetOHTOptionInfo();
	GetOUOptionInfo();
	GetORTOptionInfo();
	GetONOptionInfo();
	GetOXOptionInfo();

}

void GetOHPOptionInfo(void)
{
	// ohp option info
	OptionInfo[OptionInfoCount].OptionString=OHPString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=13;
	OptionInfo[OptionInfoCount].HighlightChar='P';
	OptionInfo[OptionInfoCount].HighlightCharX=7;
	OptionInfo[OptionInfoCount].HighlightCharY=13;
	OptionInfo[OptionInfoCount].flags.OptionalSignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.SignedValue=*(int *)(IOBuffer+opt->OHPValue);
	if(!*(char *)(IOBuffer+opt->OHPAllocFlag)){	// flip sign
		OptionInfo[OptionInfoCount].OldValues.SignedValue*=-1;
	}
	OptionInfo[OptionInfoCount].OldValueExists=!!*(char *)(IOBuffer+opt->OHPUsedFlag);
	if(!OptionInfo[OptionInfoCount].OldValueExists){
		// zero value if doesn't exist
		OptionInfo[OptionInfoCount].OldValues.SignedValue=0;
	}
	OptionInfo[OptionInfoCount].NewValues.SignedValue=OptionInfo[OptionInfoCount].OldValues.SignedValue;
	if(BatchOHPStatus){
		OptionInfo[OptionInfoCount].NewValues.SignedValue=BatchOHPValue;
		if(!BatchOHPValue){	// zero batch value turns off
			OptionInfo[OptionInfoCount].OldValueExists=OFF;
		}
		else{
			OptionInfo[OptionInfoCount].OldValueExists=ON;
		}
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format6d;
	OptionInfo[OptionInfoCount].LowBoundary.SignedValue=-16383;
	OptionInfo[OptionInfoCount].HighBoundary.SignedValue=16383;
	OptionInfo[OptionInfoCount].OldValueX=20;
	OptionInfo[OptionInfoCount].OldValueY=13;
	OptionInfo[OptionInfoCount].NewValueX=12;
	OptionInfo[OptionInfoCount].NewValueY=13;
	OptionInfo[OptionInfoCount].CursorRightOption=OLString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OSString;
	OptionInfo[OptionInfoCount].CursorUpOption=OPString;
	OptionInfo[OptionInfoCount].CursorDownOption=OHP3String;
	OptionInfoCount++;	// move to next option
}

void GetOLOptionInfo(void)
{
	// ol option info
	OptionInfo[OptionInfoCount].OptionString=OLString;
	OptionInfo[OptionInfoCount].OptionStringX=51;
	OptionInfo[OptionInfoCount].OptionStringY=13;
	OptionInfo[OptionInfoCount].HighlightChar='L';
	OptionInfo[OptionInfoCount].HighlightCharX=52;
	OptionInfo[OptionInfoCount].HighlightCharY=13;
	OptionInfo[OptionInfoCount].flags.UnsignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=*(unsigned int *)(IOBuffer+opt->OLValue);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchOLStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchOLValue;
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format3u;
	OptionInfo[OptionInfoCount].LowBoundary.UnsignedValue=1;
	OptionInfo[OptionInfoCount].HighBoundary.UnsignedValue=512;
	OptionInfo[OptionInfoCount].OldValueX=64;
	OptionInfo[OptionInfoCount].OldValueY=13;
	OptionInfo[OptionInfoCount].NewValueX=57;
	OptionInfo[OptionInfoCount].NewValueY=13;
	OptionInfo[OptionInfoCount].CursorRightOption=OHP3String;
	OptionInfo[OptionInfoCount].CursorLeftOption=OHPString;
	OptionInfo[OptionInfoCount].CursorUpOption=OSString;
	OptionInfo[OptionInfoCount].CursorDownOption=ORPString;
	OptionInfoCount++;	// move to next option
}

void GetOHP3OptionInfo(void)
{
	// ohp3 option info
	OptionInfo[OptionInfoCount].OptionString=OHP3String;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=14;
	OptionInfo[OptionInfoCount].HighlightChar='3';
	OptionInfo[OptionInfoCount].HighlightCharX=8;
	OptionInfo[OptionInfoCount].HighlightCharY=14;
	OptionInfo[OptionInfoCount].flags.OptionalSignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.SignedValue=*(int *)(IOBuffer+opt->OHPValue);
	if(!*(char *)(IOBuffer+opt->OHPAllocFlag)){	// flip sign
		OptionInfo[OptionInfoCount].OldValues.SignedValue*=-1;
	}
	OptionInfo[OptionInfoCount].OldValueExists=!!*(char *)(IOBuffer+opt->OHP3UsedFlag);
	if(!OptionInfo[OptionInfoCount].OldValueExists){
		// zero value if doesn't exist
		OptionInfo[OptionInfoCount].OldValues.SignedValue=0;
	}
	OptionInfo[OptionInfoCount].NewValues.SignedValue=OptionInfo[OptionInfoCount].OldValues.SignedValue;
	if(BatchOHP3Status){
		OptionInfo[OptionInfoCount].NewValues.SignedValue=BatchOHP3Value;
		if(!BatchOHP3Value){	// zero batch value turns off
			OptionInfo[OptionInfoCount].OldValueExists=OFF;
		}
		else{
			OptionInfo[OptionInfoCount].OldValueExists=ON;
		}
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format6d;
	OptionInfo[OptionInfoCount].LowBoundary.SignedValue=-16383;
	OptionInfo[OptionInfoCount].HighBoundary.SignedValue=16383;
	OptionInfo[OptionInfoCount].OldValueX=20;
	OptionInfo[OptionInfoCount].OldValueY=14;
	OptionInfo[OptionInfoCount].NewValueX=12;
	OptionInfo[OptionInfoCount].NewValueY=14;
	OptionInfo[OptionInfoCount].CursorRightOption=ORPString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OLString;
	OptionInfo[OptionInfoCount].CursorUpOption=OHPString;
	OptionInfo[OptionInfoCount].CursorDownOption=OHTString;
	OptionInfoCount++;	// move to next option
}

void GetORPOptionInfo(void)
{
	// orp option info
	OptionInfo[OptionInfoCount].OptionString=ORPString;
	OptionInfo[OptionInfoCount].OptionStringX=51;
	OptionInfo[OptionInfoCount].OptionStringY=14;
	OptionInfo[OptionInfoCount].HighlightChar='P';
	OptionInfo[OptionInfoCount].HighlightCharX=53;
	OptionInfo[OptionInfoCount].HighlightCharY=14;
	OptionInfo[OptionInfoCount].flags.BooleanOnOff=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=!!*(char *)(IOBuffer+opt->ORPFlag);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=!!OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchORPStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchORPValue;
	}
	OptionInfo[OptionInfoCount].OldValueX=63;
	OptionInfo[OptionInfoCount].OldValueY=14;
	OptionInfo[OptionInfoCount].NewValueX=56;
	OptionInfo[OptionInfoCount].NewValueY=14;
	OptionInfo[OptionInfoCount].CursorRightOption=OHTString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OHP3String;
	OptionInfo[OptionInfoCount].CursorUpOption=OLString;
	OptionInfo[OptionInfoCount].CursorDownOption=ORTString;
	OptionInfoCount++;	// move to next option
}

void GetOHTOptionInfo(void)
{
	// oht option info
	OptionInfo[OptionInfoCount].OptionString=OHTString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=15;
	OptionInfo[OptionInfoCount].HighlightChar='T';
	OptionInfo[OptionInfoCount].HighlightCharX=7;
	OptionInfo[OptionInfoCount].HighlightCharY=15;
	OptionInfo[OptionInfoCount].flags.OptionalSignedInt=ON;
	OptionInfo[OptionInfoCount].OldValues.SignedValue=*(int *)(IOBuffer+opt->OHTValue);
	if(!*(char *)(IOBuffer+opt->OHTAllocFlag)){	// flip sign
		OptionInfo[OptionInfoCount].OldValues.SignedValue*=-1;
	}
	OptionInfo[OptionInfoCount].OldValueExists=!!*(char *)(IOBuffer+opt->OHTUsedFlag);
	if(!OptionInfo[OptionInfoCount].OldValueExists){
		// zero value if doesn't exist
		OptionInfo[OptionInfoCount].OldValues.SignedValue=0;
	}
	OptionInfo[OptionInfoCount].NewValues.SignedValue=OptionInfo[OptionInfoCount].OldValues.SignedValue;
	if(BatchOHTStatus){
		OptionInfo[OptionInfoCount].NewValues.SignedValue=BatchOHTValue;
		if(!BatchOHTValue){	// zero batch value turns off
			OptionInfo[OptionInfoCount].OldValueExists=OFF;
		}
		else{
			OptionInfo[OptionInfoCount].OldValueExists=ON;
		}
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format6d;
	OptionInfo[OptionInfoCount].LowBoundary.SignedValue=-16383;
	OptionInfo[OptionInfoCount].HighBoundary.SignedValue=16383;
	OptionInfo[OptionInfoCount].OldValueX=20;
	OptionInfo[OptionInfoCount].OldValueY=15;
	OptionInfo[OptionInfoCount].NewValueX=12;
	OptionInfo[OptionInfoCount].NewValueY=15;
	OptionInfo[OptionInfoCount].CursorRightOption=OUString;
	OptionInfo[OptionInfoCount].CursorLeftOption=ORPString;
	OptionInfo[OptionInfoCount].CursorUpOption=OHP3String;
	OptionInfo[OptionInfoCount].CursorDownOption=ONString;
	OptionInfoCount++;	// move to next option
}

void GetOUOptionInfo(void)
{
	// ou option info
	OptionInfo[OptionInfoCount].OptionString=OUString;
	OptionInfo[OptionInfoCount].OptionStringX=30;
	OptionInfo[OptionInfoCount].OptionStringY=15;
	OptionInfo[OptionInfoCount].HighlightChar='U';
	OptionInfo[OptionInfoCount].HighlightCharX=31;
	OptionInfo[OptionInfoCount].HighlightCharY=15;
	OptionInfo[OptionInfoCount].flags.BooleanOnOff=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=!!*(char *)(IOBuffer+opt->OUFlag);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=!!OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchOUStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchOUValue;
	}
	OptionInfo[OptionInfoCount].OldValueX=41;
	OptionInfo[OptionInfoCount].OldValueY=15;
	OptionInfo[OptionInfoCount].NewValueX=34;
	OptionInfo[OptionInfoCount].NewValueY=15;
	OptionInfo[OptionInfoCount].CursorRightOption=ORTString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OHTString;
	OptionInfo[OptionInfoCount].CursorUpOption=ONString;
	OptionInfo[OptionInfoCount].CursorDownOption=OXString;
	OptionInfoCount++;	// move to next option
}

void GetORTOptionInfo(void)
{
	// ort option info
	OptionInfo[OptionInfoCount].OptionString=ORTString;
	OptionInfo[OptionInfoCount].OptionStringX=51;
	OptionInfo[OptionInfoCount].OptionStringY=15;
	OptionInfo[OptionInfoCount].HighlightChar='T';
	OptionInfo[OptionInfoCount].HighlightCharX=53;
	OptionInfo[OptionInfoCount].HighlightCharY=15;
	OptionInfo[OptionInfoCount].flags.BooleanOnOff=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=!!*(char *)(IOBuffer+opt->ORTFlag);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=!!OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchORTStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchORTValue;
	}
	OptionInfo[OptionInfoCount].OldValueX=63;
	OptionInfo[OptionInfoCount].OldValueY=15;
	OptionInfo[OptionInfoCount].NewValueX=56;
	OptionInfo[OptionInfoCount].NewValueY=15;
	OptionInfo[OptionInfoCount].CursorRightOption=ONString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OUString;
	OptionInfo[OptionInfoCount].CursorUpOption=ORPString;
	OptionInfo[OptionInfoCount].CursorDownOption=DateString;
	OptionInfoCount++;	// move to next option
}

void GetONOptionInfo(void)
{
	// on option info
	OptionInfo[OptionInfoCount].OptionString=ONString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=17;
	OptionInfo[OptionInfoCount].HighlightChar='N';
	OptionInfo[OptionInfoCount].HighlightCharX=6;
	OptionInfo[OptionInfoCount].HighlightCharY=17;
	OptionInfo[OptionInfoCount].flags.RequiredString=ON;
	OptionInfo[OptionInfoCount].OldString=(IOBuffer+opt->ONName);
//	OptionInfo[OptionInfoCount].OldString=IOBuffer;
	strcpy(ONStringValue,OptionInfo[OptionInfoCount].OldString);
	if(BatchONStatus){
		strcpy(ONStringValue,BatchONString);
	}
	OptionInfo[OptionInfoCount].NewString=ONStringValue;
	OptionInfo[OptionInfoCount].ValueFormatString=Format12s;
	OptionInfo[OptionInfoCount].OldStringX=10;
	OptionInfo[OptionInfoCount].OldStringY=18;
	OptionInfo[OptionInfoCount].NewStringX=10;
	OptionInfo[OptionInfoCount].NewStringY=17;
	OptionInfo[OptionInfoCount].CursorRightOption=OXString;
	OptionInfo[OptionInfoCount].CursorLeftOption=ORTString;
	OptionInfo[OptionInfoCount].CursorUpOption=OHTString;
	OptionInfo[OptionInfoCount].CursorDownOption=OUString;
	OptionInfoCount++;	// move to next option
}

void GetOXOptionInfo(void)
{
	// ox option info
	OptionInfo[OptionInfoCount].OptionString=OXString;
	OptionInfo[OptionInfoCount].OptionStringX=33;
	OptionInfo[OptionInfoCount].OptionStringY=17;
	OptionInfo[OptionInfoCount].HighlightChar='X';
	OptionInfo[OptionInfoCount].HighlightCharX=34;
	OptionInfo[OptionInfoCount].HighlightCharY=17;
	OptionInfo[OptionInfoCount].flags.BooleanAndString=ON;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue=!!*(char *)(IOBuffer+opt->OXFlag);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=!!OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchOXStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchOXValue;
	}
	OptionInfo[OptionInfoCount].OldValueX=38;
	OptionInfo[OptionInfoCount].OldValueY=18;
	OptionInfo[OptionInfoCount].NewValueX=38;
	OptionInfo[OptionInfoCount].NewValueY=17;
	OptionInfo[OptionInfoCount].OldString=(IOBuffer+opt->OXEnvVar);
	strcpy(OXStringValue,OptionInfo[OptionInfoCount].OldString);
	if(BatchOXStringStatus){
		strcpy(OXStringValue,BatchOXString);
	}
	OptionInfo[OptionInfoCount].NewString=OXStringValue;
	OptionInfo[OptionInfoCount].ValueFormatString=Format31s;
	OptionInfo[OptionInfoCount].OldStringX=45;
	OptionInfo[OptionInfoCount].OldStringY=18;
	OptionInfo[OptionInfoCount].NewStringX=45;
	OptionInfo[OptionInfoCount].NewStringY=17;
	OptionInfo[OptionInfoCount].CursorRightOption=DateString;
	OptionInfo[OptionInfoCount].CursorLeftOption=ONString;
	OptionInfo[OptionInfoCount].CursorUpOption=OUString;
	OptionInfo[OptionInfoCount].CursorDownOption=STString;
	OptionInfoCount++;	// move to next option
}

void GetOPOptionInfo(void)
{
	// op option info
	OptionInfo[OptionInfoCount].OptionString=OPString;
	OptionInfo[OptionInfoCount].OptionStringX=5;
	OptionInfo[OptionInfoCount].OptionStringY=12;
	OptionInfo[OptionInfoCount].HighlightChar='O';
	OptionInfo[OptionInfoCount].HighlightCharX=5;
	OptionInfo[OptionInfoCount].HighlightCharY=12;
	OptionInfo[OptionInfoCount].flags.SignedInt=ON;
	OptionInfo[OptionInfoCount].ValueMultiplier=1024L;
	OptionInfo[OptionInfoCount].OldValues.SignedValue= \
		(unsigned int)(*(long *)(IOBuffer+opt->OverlayPool)/(long)OptionInfo[OptionInfoCount].ValueMultiplier);
	if(*(char *)(IOBuffer+opt->PoolAllocFlag)==0){
		OptionInfo[OptionInfoCount].OldValues.SignedValue*=-1;
	}
	OptionInfo[OptionInfoCount].NewValues.SignedValue=OptionInfo[OptionInfoCount].OldValues.SignedValue;
	if(BatchOPStatus){
		OptionInfo[OptionInfoCount].NewValues.SignedValue=BatchOPValue;
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format4d;
	OptionInfo[OptionInfoCount].LowBoundary.SignedValue=-512;
	OptionInfo[OptionInfoCount].HighBoundary.SignedValue=512;
	OptionInfo[OptionInfoCount].OldValueX=22;
	OptionInfo[OptionInfoCount].OldValueY=12;
	OptionInfo[OptionInfoCount].NewValueX=14;
	OptionInfo[OptionInfoCount].NewValueY=12;
	OptionInfo[OptionInfoCount].CursorRightOption=OSString;
	OptionInfo[OptionInfoCount].CursorLeftOption=STString;
	OptionInfo[OptionInfoCount].CursorUpOption=ASString;
	OptionInfo[OptionInfoCount].CursorDownOption=OHPString;
	OptionInfoCount++;	// move to next option
}

void GetOSOptionInfo(void)
{
	// os option info
	OptionInfo[OptionInfoCount].OptionString=OSString;
	OptionInfo[OptionInfoCount].OptionStringX=51;
	OptionInfo[OptionInfoCount].OptionStringY=12;
	OptionInfo[OptionInfoCount].HighlightChar='S';
	OptionInfo[OptionInfoCount].HighlightCharX=52;
	OptionInfo[OptionInfoCount].HighlightCharY=12;
	OptionInfo[OptionInfoCount].flags.UnsignedInt=ON;
	OptionInfo[OptionInfoCount].ValueMultiplier=1024L;
	OptionInfo[OptionInfoCount].OldValues.UnsignedValue= \
		(unsigned int)(*(unsigned int *)(IOBuffer+opt->OSValue)/OptionInfo[OptionInfoCount].ValueMultiplier);
	OptionInfo[OptionInfoCount].NewValues.UnsignedValue=OptionInfo[OptionInfoCount].OldValues.UnsignedValue;
	if(BatchOSStatus){
		OptionInfo[OptionInfoCount].NewValues.UnsignedValue=BatchOSValue;
	}
	OptionInfo[OptionInfoCount].ValueFormatString=Format2u;
	OptionInfo[OptionInfoCount].LowBoundary.UnsignedValue=1;
	OptionInfo[OptionInfoCount].HighBoundary.UnsignedValue=63;
	OptionInfo[OptionInfoCount].OldValueX=65;
	OptionInfo[OptionInfoCount].OldValueY=12;
	OptionInfo[OptionInfoCount].NewValueX=58;
	OptionInfo[OptionInfoCount].NewValueY=12;
	OptionInfo[OptionInfoCount].CursorRightOption=OHPString;
	OptionInfo[OptionInfoCount].CursorLeftOption=OPString;
	OptionInfo[OptionInfoCount].CursorUpOption=STString;
	OptionInfo[OptionInfoCount].CursorDownOption=OLString;
	OptionInfoCount++;	// move to next option
}

// show the EXE's nonoverlay and overlay information
void ShowInfo(void)
{
	char *OMOptStrings1[]={
		"อออออออออออออออออัอออออออออออออออออัออออออออออออออออออออ",
		" New     Old     ณ OVERLAY OPTIONS ณ         New    Old ",
		"ฤฤฤฤฤฤ  ฤฤฤฤฤฤ   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู        ฤฤฤฤฤ  ฤฤฤฤฤ"
	};
	char *OMOptStrings2[]={
		" New    Old",
		"ฤฤฤฤฤ  ฤฤฤฤฤ"
	};
	char *OMOptStrings3[]={
		"<- New ->",
		"<- Old ->"
	};
	int i;

	if(OverlayManagerPresent){
		for(i=0;i<3;i++){
			gotoxy(12,9+i);
			cprintf(OMOptStrings1[i]);
		}
		for(i=0;i<2;i++){
			gotoxy(34,13+i);
			cprintf(OMOptStrings2[i]);
		}
		for(i=0;i<2;i++){
			gotoxy(23,17+i);
			cprintf(OMOptStrings3[i]);
		}
	}

	ShowOptionStrings(OFF);	// show strings AND values
	DisplayPrompt(MainPromptString);
}

// display description line
void DisplayDescription(int which)
{
	char *DescriptString[14]={
		"Preserve original EXE file time and date or change to last modified",
		"Set maximum program allocation space in paragraphs (16 bytes)",
		"Set program stack size in bytes",
		"Set overlay pool size in kilobytes, '-' leave amount free",
		"Set overlay manager internal stack size in kilobytes",
		"Set overlay stash in EMS expanded memory, '-' leave amount free",
		"Set overlay maximum simultaneously loaded count",
		"Set overlay stash in EMS, forced version 3.0 compatibility",
		"Set swap of active overlays to EMS version 4.0 expanded memory",
		"Set overlay stash in XMS extended memory, '-' leave amount free",
		"Place overlay pool in upper memory blocks (UMBs) if possible",
		"Set swap of active overlays to XMS version 4.0 extended memory",
		"Change overlay file name (not for internal overlays)",
		"Place overlay pool in EMS page frame, optional e-variable control"
	};

	gotoxy(5,21);
	cprintf(BLANKS(72));	// clear pre-existing description line
	textcolor(IntensityColor);	// highlight the description
	gotoxy(8,21);
	cprintf(DescriptString[which]);
	textcolor(ForegroundColor);	// restore default nonhighlight text
}


// display a prompt
void DisplayPrompt(char *string)
{
	gotoxy(5,22);
	cprintf(BLANKS(72));	// clear pre-existing prompt
	gotoxy(8,22);
	cprintf(string);
}

// show warpmod option strings, flag set if only showing option values,
void ShowOptionStrings(int JustValuesFlag)
{
	int	i;

	// show nonoverlaid options, overlaid only if appropriate (overlays present)
	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		if(!JustValuesFlag){
			gotoxy(OptionInfo[i].OptionStringX,OptionInfo[i].OptionStringY);
			cprintf(OptionInfo[i].OptionString);
			ShowHighlightChar(i);	// show the highlight char of option
		}
		// show old value
		gotoxy(OptionInfo[i].OldValueX,OptionInfo[i].OldValueY);
		if(OptionInfo[i].flags.SignedInt || (OptionInfo[i].flags.OptionalSignedInt && OptionInfo[i].OldValueExists)){
			cprintf(OptionInfo[i].ValueFormatString,OptionInfo[i].OldValues.SignedValue);
		}
		else if(OptionInfo[i].flags.UnsignedInt){
			cprintf(OptionInfo[i].ValueFormatString,OptionInfo[i].OldValues.UnsignedValue);
		}
		else if(OptionInfo[i].flags.BooleanOnOff || OptionInfo[i].flags.BooleanAndString){
			cprintf("%s",OptionInfo[i].OldValues.UnsignedValue?BooleanONString:BooleanOFFString);
		}
		else if(OptionInfo[i].flags.OptionalSignedInt && !OptionInfo[i].OldValueExists){
			cprintf(BooleanOFFString);
		}
		else if(OptionInfo[i].flags.BooleanDate){
			cprintf(BooleanCHANGEString);
		}
		if(OptionInfo[i].flags.RequiredString || OptionInfo[i].flags.BooleanAndString){
			gotoxy(OptionInfo[i].OldStringX,OptionInfo[i].OldStringY);
			cprintf(OptionInfo[i].OldString);
		}

		// blank new values
		gotoxy(OptionInfo[i].NewValueX,OptionInfo[i].NewValueY);
		if(OptionInfo[i].flags.SignedInt || (OptionInfo[i].flags.OptionalSignedInt && OptionInfo[i].OldValueExists)){
			cprintf(BLANKS(6));
		}
		else if(OptionInfo[i].flags.UnsignedInt){
			cprintf(BLANKS(6));
		}
		else if(OptionInfo[i].flags.BooleanOnOff){
			cprintf(BLANKS(6));
		}
		else if(OptionInfo[i].flags.OptionalSignedInt){
			cprintf(BLANKS(6));
		}
		gotoxy(OptionInfo[i].NewStringX,OptionInfo[i].NewStringY);
		if(OptionInfo[i].flags.RequiredString){
			cprintf(BLANKS(12));
		}
		else if(OptionInfo[i].flags.BooleanAndString){
			cprintf(BLANKS(32));
		}
	}
}

void ShowHighlightChar(int which)
{
	if(OptionInfo[which].HighlightChar){	// allow for no highlight chars
		textcolor(HighlightColor);
		gotoxy(OptionInfo[which].HighlightCharX,OptionInfo[which].HighlightCharY);
		cprintf("%c",OptionInfo[which].HighlightChar);
		textcolor(ForegroundColor);
	}
}

void EditInfo(void)
{
	unsigned int KeyValue=0;
	int which=0;
	char *InstructString="Move highlight bar to option to modify and press Enter.";

	DisplayFeedBack(InstructString);
	HighlightOption(which);
	while(KeyValue!=ESCAPE){
		KeyValue=GetKeypress();
		switch(KeyValue){
			case RIGHTARROW:
				MoveToOption(&which,OptionInfo[which].CursorRightOption);
				break;
			case LEFTARROW:
				MoveToOption(&which,OptionInfo[which].CursorLeftOption);
				break;
			case UPARROW:
				MoveToOption(&which,OptionInfo[which].CursorUpOption);
				break;
			case DOWNARROW:
				MoveToOption(&which,OptionInfo[which].CursorDownOption);
				break;
			case ESCAPE:	// bugout
				break;
			case ENTER:	// change option
				NewOptionValue(which);
				DisplayPrompt(MainPromptString);
				DisplayFeedBack(InstructString);
				break;
			case F10:	// reset options to original values
				ResetOptions();
				break;
			default:
				CheckQuickKey(KeyValue,&which);	// see if a quick key press
				break;
		}
	}
}

// reset options to original values
void ResetOptions(void)
{
	int i;

	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		if(OptionInfo[i].flags.SignedInt || OptionInfo[i].flags.OptionalSignedInt){
			OptionInfo[i].NewValues.SignedValue=OptionInfo[i].OldValues.SignedValue;
		}
		else if(OptionInfo[i].flags.UnsignedInt){
			OptionInfo[i].NewValues.UnsignedValue=OptionInfo[i].OldValues.UnsignedValue;
		}
		else if(OptionInfo[i].flags.BooleanOnOff || OptionInfo[i].flags.BooleanAndString){
			OptionInfo[i].NewValues.UnsignedValue=OptionInfo[i].OldValues.UnsignedValue;
		}
		else if(OptionInfo[i].flags.BooleanDate){
			OptionInfo[i].NewValues.UnsignedValue=OptionInfo[i].OldValues.UnsignedValue;
		}
		else if(OptionInfo[i].flags.RequiredString){
			strcpy(ONStringValue,OptionInfo[i].OldString);
		}
		if(OptionInfo[i].flags.BooleanAndString){
			strcpy(OXStringValue,OptionInfo[i].OldString);
		}
	}

	ShowOptionStrings(ON);	// show values ONLY
}

// change option value or string
void NewOptionValue(int which)
{
	char string[81],InputField[35];
	char *ToggleString="Press Space Bar to change status, Enter to keep.";
	char *OnOffString="Turn option ON or OFF.";
	char *DateChangeString="Select CHANGE or PRESERVE EXE file date.";
	char *InputString="Type in the new value and press Enter.";
	char *InputOffString="Type in the new value or 0 to turn off, then press Enter.";
	char *NewNameString="Type in the new overlay file name.";
	char *EnvironmentString="Type in the optional environment variable setting.";
	char *EnterDoneString="Press Enter when done.";
	int KeyValue;
	long InputValue;

	ClearFeedBackLine();
	_setcursortype(_SOLIDCURSOR);	// turn on block cursor
	if(OptionInfo[which].flags.BooleanOnOff ||OptionInfo[which].flags.BooleanDate \
		 || OptionInfo[which].flags.BooleanAndString){
		// toggle values
		if(OptionInfo[which].flags.BooleanDate){
			DisplayFeedBack(DateChangeString);
		}
		else{
			DisplayFeedBack(OnOffString);
		}
		DisplayPrompt(ToggleString);
		KeyValue=SPACEBAR;	// entry setup
		OptionInfo[which].NewValues.UnsignedValue^=1;	// pre-toggle value
		do {
			if(KeyValue==SPACEBAR){
				OptionInfo[which].NewValues.UnsignedValue^=1;	// toggle value
				if(OptionInfo[which].NewValues.UnsignedValue != OptionInfo[which].OldValues.UnsignedValue){
					// new value different from old
					textcolor(IntensityColor);	// high intensity
				}
				gotoxy(OptionInfo[which].NewValueX,OptionInfo[which].NewValueY);
				if(OptionInfo[which].flags.BooleanOnOff || OptionInfo[which].flags.BooleanAndString){
					// on/off
					cprintf("%s",OptionInfo[which].NewValues.UnsignedValue?BooleanONString:BooleanOFFString);
				}
				else{	// change/preserve
					cprintf("%s",OptionInfo[which].NewValues.UnsignedValue?BooleanPRESERVEString:BooleanCHANGEString);
				}
				textcolor(ForegroundColor);	// restore colors
			}
			else{
				cprintf("\007");	// invalid keypress
			}
		} while((KeyValue=GetKeypress())!=ENTER);
	}
	else if(OptionInfo[which].flags.SignedInt || OptionInfo[which].flags.UnsignedInt || \
		OptionInfo[which].flags.OptionalSignedInt){
		if(OptionInfo[which].flags.SignedInt || OptionInfo[which].flags.OptionalSignedInt){
			sprintf(string,"Value must be in range %d to %d.",OptionInfo[which].LowBoundary.SignedValue,\
				OptionInfo[which].HighBoundary.SignedValue);
		}
		else if(OptionInfo[which].flags.UnsignedInt){
			sprintf(string,"Value must be in range %u to %u.",OptionInfo[which].LowBoundary.UnsignedValue,\
				OptionInfo[which].HighBoundary.UnsignedValue);
		}
		DisplayFeedBack(string);
		if(OptionInfo[which].flags.OptionalSignedInt){
			DisplayFeedBack(InputOffString);
		}
		else{
			DisplayPrompt(InputString);
		}

		// get length of input from format string, %#
		InputField[0]=atoi(OptionInfo[which].ValueFormatString+1)+1;

		while(1){	// get input until correct
			//  clear previous value
			gotoxy(OptionInfo[which].NewValueX,OptionInfo[which].NewValueY);
			cprintf(BLANKS(InputField[0]));

			textcolor(IntensityColor);	// high intensity for new input
			gotoxy(OptionInfo[which].NewValueX,OptionInfo[which].NewValueY);
			cgets(InputField);
			if(InputField[1]<1){	// no input, set to old value
				OptionInfo[which].NewValues.SignedValue = OptionInfo[which].OldValues.SignedValue;
				break;
			}
			else{	// check if valid output
				InputValue=atol(&InputField[2]);	// get the input value
				if(OptionInfo[which].flags.SignedInt || OptionInfo[which].flags.OptionalSignedInt){
					if(InputValue < OptionInfo[which].LowBoundary.SignedValue || \
						InputValue > OptionInfo[which].HighBoundary.SignedValue){
						// out of bounds value
						sprintf(string,"\007ERROR: Value must be in range %d to %d.",OptionInfo[which].LowBoundary.SignedValue,\
							OptionInfo[which].HighBoundary.SignedValue);
						DisplayError(string);
					}
					else{	// value within bounds
						OptionInfo[which].NewValues.SignedValue=(int)InputValue;
						break;
					}
				}
				else if(OptionInfo[which].flags.UnsignedInt){
					if(InputValue < OptionInfo[which].LowBoundary.UnsignedValue || \
						InputValue > OptionInfo[which].HighBoundary.UnsignedValue){
						// out of bounds value
						sprintf(string,"\007ERROR: Value must be in range %u to %u.",OptionInfo[which].LowBoundary.UnsignedValue,\
							OptionInfo[which].HighBoundary.UnsignedValue);
						DisplayError(string);
					}
					else{	// value within bounds
						OptionInfo[which].NewValues.UnsignedValue=(unsigned int)InputValue;
						break;
					}
				}
			}
		}

		// clear field and display value properly formatted, if not blank
		if(InputField[1]>0){
			gotoxy(OptionInfo[which].NewValueX,OptionInfo[which].NewValueY);
			cprintf(BLANKS(InputField[0]));
			if(OptionInfo[which].NewValues.SignedValue == OptionInfo[which].OldValues.SignedValue){
				// values match, no intensity
				textcolor(ForegroundColor);	// restore colors
			}
			gotoxy(OptionInfo[which].NewValueX,OptionInfo[which].NewValueY);
			// optional value shows OFF on zero value
			if(!OptionInfo[which].NewValues.SignedValue && OptionInfo[which].flags.OptionalSignedInt){
				cprintf(BooleanOFFString);
			}
			else{
				cprintf(OptionInfo[which].ValueFormatString,OptionInfo[which].NewValues.SignedValue);
			}
		}
	}
	if(OptionInfo[which].flags.RequiredString || OptionInfo[which].flags.BooleanAndString){
		if(OptionInfo[which].flags.RequiredString){
			DisplayFeedBack(NewNameString);
		}
		else if(OptionInfo[which].flags.BooleanAndString){
			DisplayFeedBack(EnvironmentString);
		}
		DisplayPrompt(EnterDoneString);

		// get length of input from format string, %-#
		InputField[0]=atoi(OptionInfo[which].ValueFormatString+2)+1;
		//  clear previous value
		gotoxy(OptionInfo[which].NewStringX,OptionInfo[which].NewStringY);
		cprintf(BLANKS(InputField[0]));

		textcolor(IntensityColor);	// high intensity for new input
		gotoxy(OptionInfo[which].NewStringX,OptionInfo[which].NewStringY);
		cgets(InputField);
		if(InputField[1]<1){	// no input, set to old value
			strcpy(OptionInfo[which].NewString,OptionInfo[which].OldString);
		}
		else{	// clear field and display string properly formatted
			strcpy(OptionInfo[which].NewString,&InputField[2]);
			gotoxy(OptionInfo[which].NewStringX,OptionInfo[which].NewStringY);
			cprintf(BLANKS(InputField[0]));
			if(!strcmp(OptionInfo[which].NewString,OptionInfo[which].OldString)){
				// values match, no intensity
				textcolor(ForegroundColor);	// restore colors
			}
			gotoxy(OptionInfo[which].NewStringX,OptionInfo[which].NewStringY);
			cprintf(OptionInfo[which].ValueFormatString,OptionInfo[which].NewString);
		}
	}
	textcolor(ForegroundColor);	// restore colors
	_setcursortype(_NOCURSOR);	// turn off cursor
}

// check if a quick key press, update if so
void CheckQuickKey(int KeyValue,int *current)
{
	int i,check;

	// find option that has matching quick key that isn't current option
	check=*current;	// check following options first
	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		if(OptionInfo[check].HighlightChar == KeyValue && check!=*current){
			DeHighlightOption(*current);
			*current=check;	// update current option
			HighlightOption(*current);	// highlight new option
			return;	// done
		}
		check=(check<2+11*!!OverlayManagerPresent)?check+1:0;
	}
	cprintf("\007");	// invalid keypress
}

void MoveToOption(int *fromwhich,char *towhich)
{
	int i;

	DeHighlightOption(*fromwhich);
	// find option string that matches towhich pointer
	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		if(OptionInfo[i].OptionString == towhich){
			*fromwhich=i;	// update current option
			break;
		}
	}
	HighlightOption(*fromwhich);	// highlight new option
}

// highlight an option
// display descriptive text
void HighlightOption(int which)
{
	char string[81];

	textcolor(BackgroundColor);	// inverse colors
	textbackground(ForegroundColor);
	// pad displayed string front and back with space
	sprintf(string," %s ",OptionInfo[which].OptionString);
	gotoxy(OptionInfo[which].OptionStringX-1,OptionInfo[which].OptionStringY);
	cprintf(string);
	ShowHighlightChar(which);
	textcolor(ForegroundColor);	// restore colors
	textbackground(BackgroundColor);
	DisplayDescription(which);
}

// remove highlight of an option
void DeHighlightOption(int which)
{
	char string[81];

	// pad displayed string front and back with space
	sprintf(string," %s ",OptionInfo[which].OptionString);
	gotoxy(OptionInfo[which].OptionStringX-1,OptionInfo[which].OptionStringY);
	cprintf(string);
	ShowHighlightChar(which);
}

unsigned int GetKeypress(void)
{
	unsigned char c=0;
	unsigned int retval=0;
	int ExtendFlag=OFF;

	while(1){
		c=getch();
		if(!ExtendFlag){	// uppercase regular keypresses
			c=toupper(c);
		}
		if(!c){	// extended char
			ExtendFlag=ON;
			retval=1000;
			continue;
		}
		retval+=c;
		return(retval);
	}
}

// show batch mode new values
void ShowNewValues(void)
{
	int	i;

	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		// show new values
		gotoxy(OptionInfo[i].NewValueX,OptionInfo[i].NewValueY);
		if(OptionInfo[i].flags.SignedInt || (OptionInfo[i].flags.OptionalSignedInt && !!OptionInfo[i].NewValues.SignedValue)){
			if(OptionInfo[i].NewValues.SignedValue != OptionInfo[i].OldValues.SignedValue){
				textcolor(IntensityColor);	// high intensity for new input value
			}
			cprintf(OptionInfo[i].ValueFormatString,OptionInfo[i].NewValues.SignedValue);
		}
		else if(OptionInfo[i].flags.UnsignedInt){
			if(OptionInfo[i].NewValues.UnsignedValue != OptionInfo[i].OldValues.UnsignedValue){
				textcolor(IntensityColor);	// high intensity for new input value
			}
			cprintf(OptionInfo[i].ValueFormatString,OptionInfo[i].NewValues.UnsignedValue);
		}
		else if(OptionInfo[i].flags.BooleanOnOff || OptionInfo[i].flags.BooleanAndString){
			if(OptionInfo[i].NewValues.UnsignedValue != OptionInfo[i].OldValues.UnsignedValue){
				textcolor(IntensityColor);	// high intensity for new input value
			}
			cprintf("%s",OptionInfo[i].NewValues.UnsignedValue?BooleanONString:BooleanOFFString);
		}
		else if(OptionInfo[i].flags.OptionalSignedInt && !OptionInfo[i].NewValues.SignedValue){
			if(OptionInfo[i].NewValues.SignedValue != OptionInfo[i].OldValues.SignedValue){
				textcolor(IntensityColor);	// high intensity for new input value
			}
			cprintf(BooleanOFFString);
		}
		else if(OptionInfo[i].flags.BooleanDate && BatchDValue){
			textcolor(IntensityColor);	// high intensity for new input value
			cprintf(BooleanPRESERVEString);
		}
		if(OptionInfo[i].flags.RequiredString || OptionInfo[i].flags.BooleanAndString){
			if(strcmp(OptionInfo[i].NewString,OptionInfo[i].OldString)){
				textcolor(IntensityColor);	// high intensity for new input value
			}
			gotoxy(OptionInfo[i].NewStringX,OptionInfo[i].NewStringY);
			cprintf(OptionInfo[i].NewString);
		}
		textcolor(ForegroundColor);	// restore colors for next pass
	}
	textcolor(ForegroundColor);	// restore colors
}

// see if any changes made to original values
void CheckChanges(void)
{
	int i;

	for(i=0;i<3+11*!!OverlayManagerPresent;i++){
		if(OptionInfo[i].flags.SignedInt || OptionInfo[i].flags.OptionalSignedInt){
			if(OptionInfo[i].NewValues.SignedValue!=OptionInfo[i].OldValues.SignedValue){
				ChangesMade=ON;	// option changed from original
				if(OptionInfo[i].OptionString==OPString){
					if(OptionInfo[i].NewValues.SignedValue>=0){
						*(char *)(IOBuffer+opt->PoolAllocFlag)=1;
					}
					else{
						*(char *)(IOBuffer+opt->PoolAllocFlag)=0;
						OptionInfo[i].NewValues.SignedValue*=-1;
					}
					*(long *)(IOBuffer+opt->OverlayPool)=OptionInfo[i].NewValues.SignedValue * \
						OptionInfo[i].ValueMultiplier;
				}
				else if(OptionInfo[i].OptionString==OHPString){
					if(OptionInfo[i].NewValues.SignedValue>0){
						*(int *)(IOBuffer+opt->OHPValue)=OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHPAllocFlag)=1;
						*(char *)(IOBuffer+opt->OHPUsedFlag)=1;
					}
					else if(OptionInfo[i].NewValues.SignedValue<0){
						*(int *)(IOBuffer+opt->OHPValue)=-OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHPAllocFlag)=0;
						*(char *)(IOBuffer+opt->OHPUsedFlag)=1;
					}
					else{	// turned off
						*(char *)(IOBuffer+opt->OHPUsedFlag)=0;
					}
				}
				else if(OptionInfo[i].OptionString==OHP3String){
					if(OptionInfo[i].NewValues.SignedValue>0){
						*(int *)(IOBuffer+opt->OHPValue)=OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHPAllocFlag)=1;
						*(char *)(IOBuffer+opt->OHP3UsedFlag)=1;
					}
					else if(OptionInfo[i].NewValues.SignedValue<0){
						*(int *)(IOBuffer+opt->OHPValue)=-OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHPAllocFlag)=0;
						*(char *)(IOBuffer+opt->OHP3UsedFlag)=1;
					}
					else{	// turned off
						*(char *)(IOBuffer+opt->OHP3UsedFlag)=0;
					}
				}
				else if(OptionInfo[i].OptionString==OHTString){
					if(OptionInfo[i].NewValues.SignedValue>0){
						*(int *)(IOBuffer+opt->OHTValue)=OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHTAllocFlag)=1;
						*(char *)(IOBuffer+opt->OHTUsedFlag)=1;
					}
					else if(OptionInfo[i].NewValues.SignedValue<0){
						*(int *)(IOBuffer+opt->OHTValue)=-OptionInfo[i].NewValues.SignedValue;
						*(char *)(IOBuffer+opt->OHTAllocFlag)=0;
						*(char *)(IOBuffer+opt->OHTUsedFlag)=1;
					}
					else{	// turned off
						*(char *)(IOBuffer+opt->OHTUsedFlag)=0;
					}
				}
			}
		}
		else if(OptionInfo[i].flags.UnsignedInt){
			if(OptionInfo[i].NewValues.UnsignedValue!=OptionInfo[i].OldValues.UnsignedValue){
				ChangesMade=ON;	// option changed from original
				if(OptionInfo[i].OptionString==ASString){
					EXEHeader.MaxAlloc=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==STString){
					EXEHeader.SPRegister=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==OLString){
					*(unsigned int *)(IOBuffer+opt->OLValue)=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==OSString){
					*(unsigned int *)(IOBuffer+opt->OSValue)=(unsigned int)(OptionInfo[i].NewValues.UnsignedValue * \
						OptionInfo[i].ValueMultiplier);
				}
			}
		}
		else if(OptionInfo[i].flags.BooleanOnOff || OptionInfo[i].flags.BooleanAndString){
			if(OptionInfo[i].NewValues.UnsignedValue!=OptionInfo[i].OldValues.UnsignedValue){
				ChangesMade=ON;	// option changed from original
				if(OptionInfo[i].OptionString==ORPString){
					*(char *)(IOBuffer+opt->ORPFlag)=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==OUString){
					*(char *)(IOBuffer+opt->OUFlag)=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==ORTString){
					*(char *)(IOBuffer+opt->ORTFlag)=OptionInfo[i].NewValues.UnsignedValue;
				}
				else if(OptionInfo[i].OptionString==OXString){
					*(char *)(IOBuffer+opt->OXFlag)=OptionInfo[i].NewValues.UnsignedValue;
				}
			}
		}
		else if(OptionInfo[i].flags.BooleanDate){
			if(OptionInfo[i].NewValues.UnsignedValue!=OptionInfo[i].OldValues.UnsignedValue){
//				ChangesMade=ON;	// option changed from original
				PreserveDateFlag=ON;
			}
		}
		else if(OptionInfo[i].flags.RequiredString){
			if(strcmp(ONStringValue,OptionInfo[i].OldString)){
				ChangesMade=ON;	// option changed from original
				strcpy(OptionInfo[i].OldString,ONStringValue);
			}
		}
		if(OptionInfo[i].flags.BooleanAndString){
			if(strcmp(OXStringValue,OptionInfo[i].OldString)){
				ChangesMade=ON;	// option changed from original
				strcpy(OptionInfo[i].OldString,OXStringValue);
			}
		}
	}
}

void SaveInfo(void)
{
	char *UpdateString="Updating new option values in EXE.";

	DisplayFeedBack(UpdateString);
	fseek(fp,0L,SEEK_SET); // position to start of header
	fwrite(&EXEHeader,1,sizeof(EXEHeader),fp);	// write EXE header info

	if(OverlayManagerPresent){
		fseek(fp,OMOptionPtrFilePos,SEEK_SET); // position to overlay manager info
		fwrite(IOBuffer,1,sizeof(IOBuffer),fp);    // write pointer bytes and later values
	}

	if(PreserveDateFlag){
		// change file date back to original
		setftime(fileno(fp),&FileTime);
	}
}

// quit the warpmod program
void QuitWarpMod(void)
{
	if(fp!=NULL){	// file was open, close it
		fclose(fp);	// close warpmod'ed file
	}
	_setcursortype(_NORMALCURSOR);	// restore normal cursor
	window(1,1,80,25);	// restore normal screen
	gotoxy(1,25);
	exit(0);
}
