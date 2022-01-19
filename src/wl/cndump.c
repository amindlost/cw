#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <conio.h>
#include <dos.h>
#include <dir.h>
#include <ctype.h>

char MapFileName[101];
char LineBuffer[128];
int ModuleCount=0;
struct SegmentInfo {
	unsigned int _dtlen;
	unsigned int _datlen;
	unsigned int codelen;
	unsigned int datalen;
	unsigned int totallen;
	unsigned char *name;
	struct SegmentInfo *NextPtr;
};
struct SegmentInfo *firstptr=NULL,*lastptr=NULL;
struct SegmentInfo **sparray;
FILE *fp;

void GetFileInfo(int argc, char *argv[]);
void CheckSegment(void);
void SaveSegmentInfo(unsigned int,unsigned int);
void SortSegmentInfo(void);
int SortFunction(const void *,const void *);
void ShowSegmentInfo(void);

void main(int argc, char *argv[])
{
	GetFileInfo(argc,argv);
	fp=fopen(MapFileName,"ra");
	if(fp==NULL){
		exit(1);
	}
    while(fgets(LineBuffer,127,fp)!=NULL){   // read until file end
		if(!strncmp(LineBuffer,"Name             Ovl#",21)){
		    while(fgets(LineBuffer,127,fp)!=NULL){   // read until file end
				if(LineBuffer[0]=='\n'){	// end of detailed segment map portion
					break;
				}
				CheckSegment();
			}
			break;
		}
	}
	fclose(fp);
	SortSegmentInfo();
	ShowSegmentInfo();
}

void GetFileInfo(int argc, char *argv[])
{
	int len;
	char FileDrive[MAXDRIVE];
	char FileDir[MAXDIR];
	char FileName[MAXFILE];
	char FileExt[MAXEXT];

	if(argc<2){
		cprintf("\nFile name? ");
		fgets(MapFileName,13,stdin);
		len=strlen(MapFileName);
		if(!len || len==1){	// no map file name given
			exit(0);
		}
		MapFileName[len-1]=0;	// kill newline
	}
	else{
		strcpy(MapFileName,argv[1]);
	}
	fnsplit(MapFileName,FileDrive,FileDir,FileName,FileExt);
	MapFileName[0]=0;
	if(FileDrive[0]){
		strcat(MapFileName,FileDrive);
	}
	if(FileDir[0]){
		strcat(MapFileName,FileDir);
	}
	strcat(MapFileName,FileName);
	if(!FileExt[0]){	// no extension, supply default
		strcat(MapFileName,".");
		strcat(MapFileName,"MAP");
	}
	else{
		strcat(MapFileName,FileExt);
	}
}

// check if _DT, _DAT, overlaid _CODE segment
void CheckSegment(void)
{
	static unsigned int dtlen,datlen;
	int pos=0;

	if(!strncmp(LineBuffer,"_DT_",4)){
		sscanf(LineBuffer,"%*s%*x%*c%*x%x",&dtlen);
	}
	else if(!strncmp(LineBuffer,"_DAT_",5)){
		sscanf(LineBuffer,"%*s%*x%*c%*x%x",&datlen);
	}
	else if(!strncmp(LineBuffer,"_CODE_",6)){
		while(LineBuffer[pos++]>' ')
			;
		while(LineBuffer[pos]==' ')
			pos++;
		if(isdigit(LineBuffer[pos]) && LineBuffer[pos+4]==' '){	// overlay
			SaveSegmentInfo(dtlen,datlen);
		}
	}
}

void SaveSegmentInfo(unsigned int dtlen,unsigned int datlen)
{
	char ModuleName[81];
	unsigned char *nptr;
	unsigned int codelen,datalen,totallen;
	struct SegmentInfo *siptr;

	sscanf(LineBuffer,"%*s%*x%*x%*c%*x%x%*c%*s%*s%*s%s",&codelen,ModuleName);
	siptr=calloc(1,sizeof(struct SegmentInfo));
	if(siptr==NULL){
		exit(1);
	}
	nptr=malloc(strlen(ModuleName)+1);
	if(nptr==NULL){
		exit(1);
	}
	strcpy(nptr,ModuleName);	// save file name text
	if(firstptr==NULL){
		firstptr=siptr;
	}
	else{
		lastptr->NextPtr=siptr;
	}

	dtlen+=1023;
	dtlen/=1024;
	datlen+=1023;
	datlen/=1024;
	datalen=dtlen+datlen;
	codelen+=1023;
	codelen/=1024;
	totallen=dtlen+datlen+codelen;

	siptr->_dtlen=dtlen;
	siptr->_datlen=datlen;
	siptr->codelen=codelen;
	siptr->datalen=datalen;
	siptr->totallen=totallen;
	siptr->name=nptr;
	siptr->NextPtr=NULL;
	lastptr=siptr;
	ModuleCount++;
}

void SortSegmentInfo(void)
{
	int count=0;
	struct SegmentInfo *siptr;

	sparray=calloc(ModuleCount,sizeof(struct SegmentInfo *));
	if(sparray==NULL){
		exit(1);
	}

	siptr=firstptr;
	while(siptr!=NULL){
		*(sparray+count++)=siptr;
		siptr=siptr->NextPtr;
	}
	qsort((void *)sparray,ModuleCount,sizeof(struct SegmentInfo *),SortFunction);
}

int SortFunction(const void *ptr1,const void *ptr2)
{
	struct SegmentInfo **s1,**s2;
	struct SegmentInfo *siptr1,*siptr2;

	s1=(struct SegmentInfo **)ptr1;
	s2=(struct SegmentInfo **)ptr2;
	siptr1=*s1;
	siptr2=*s2;
//	printf("\n- %s - %s -",siptr1->name,siptr2->name);
	if(siptr1->totallen<siptr2->totallen){
		return(1);
	}
	if(siptr1->totallen>siptr2->totallen){
		return(-1);
	}
	return(0);
}

void ShowSegmentInfo(void)
{
	int	i;
	struct SegmentInfo *siptr;
	char *head1="Overlaid Module Name            Total Size   Code Size   Data size";
	char *head2="--------------------            ----------   ---------   ---------";

	printf("\n%s",head1);
	printf("\n%s",head2);

	for(i=0;i<ModuleCount;i++){
		siptr=*(sparray+i);
		printf("\n%-36s%2dK%11dK%11dK",siptr->name,siptr->totallen,siptr->codelen,siptr->datalen);
	}
	printf("\n");
}
