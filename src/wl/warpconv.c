/* WARPCONV.C

    Copyright 1990-94, Michael E. Devore, All rights reserved

    programmed in the large memory model of Turbo C++ version 1.0
    started 8-23-90
    last changed 01-30-94

    convert Plink/.RTLink script file to WarpLink compatible script file

*/

#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <dos.h>
#include <string.h>
#include <ctype.h>

#define NO  0
#define YES 1
#define OFF 0
#define ON  1

void credits(void);
int is_extension(char *);
void parse_command(void);
void write_warplink_file(void);
void write_options(void);
void write_objs(void);
void write_libs(void);
int match(char *,const char *,int);
void add_obj_name(char *);
void add_lib_name(char *);
void alloc_memerr(int);
void get_filenames(int,int);
void preprocess(void);
void splitout(void);
void summary(void);
void warnings(void);
void comments(const char *string);

extern unsigned _stklen=2048;

const char *text_noequiv="# *** NO EQUIVALENT COMMAND, program structure or behavior MAY change\n";
const char *text_unknown="# *** UNKNOWN COMMAND, program structure or behavior MAY change\n";
const char *text_ignore="# No equivalent command, program behavior will NOT change\n";
const char *text_success="# Successful translation of command\n";
const char *text_into="# Separate overlay files not supported, all overlays go into one .OVL file.\n";
const char *def_rsp_ext=".LNK";

FILE *fpin,*fpout;

struct fileinfo {
    struct fileinfo *nextptr;
    char *nameptr;
    struct {
        is_overlaid:1;
        is_library:1;
    } flags;
};

struct fileinfo *first_libptr=NULL;
struct fileinfo *last_libptr=NULL;
struct fileinfo *first_objptr=NULL;
struct fileinfo *last_objptr=NULL;

char infile[128],outfile[128],prefile[128];
char workbuff[128],saveline[128];
char mapfile[128]={0};
char exefile[128]={0};
char workfile[128]={0};

int use_extdict=NO;    /* default behavior is to NOT use library extended dictionary */
int use_expmap=NO;  /* default is no map file */
int use_stack=NO;   /* default is no set stack */
unsigned int stack_value;    /* stack value if specified */
int use_dosseg=NO;  /* default is not to explicitly  use DOSSEG ordering */
int use_deflib=YES; /* default is to use default libraries */
int use_maxalloc=NO;    /* default is to not specify maximum program allocation space */
unsigned int maxalloc_value; /* maximum allocation value if specified */
int use_case=NO;    /* default is case insensitive links */
int use_ovlclass=NO;    /* default is not to specify overlay class */
int use_info=NO;    /* default is not to provide info during link */
char ovlclass[128]={'C','O','D','E'};   /* overlay class */
char comment_char='#';  /* Plink default comment character */

int is_overlays=NO; /* changed to yes if any overlays were specified */
int obj_count=0;    /* count of object modules */
int lib_count=0;    /* count of libraries */
int ovl_nest=0; /* overlay nest level for plink file */
int comma_flag=0;   /* nonzero if trailing comma on file or library or seach command */
int libfile_flag=0; /* nonzero if last command was file, library, search, or allocate */

int no_warn_flag=OFF;   /* set if no warnings specfied */
int no_comment_flag=OFF;    /* set if no comments specified */
int no_ovlmgr_flag=OFF; /* set if no ovlmgr.obj specified  */
int firstcom=0; /* command line parameter count of first file */
int secondcom=0;  /* command line parameter count of second file */

int is_clipperlib=OFF;  /* nonzero if CLIPPER.LIB detected */
int is_clarionlib=OFF;  /* nonzero if CLARIONx.LIB detected */
int is_qslib=OFF;       /* nonzero if QS.LIB detected */
int is_overlayxxx=OFF;  /* nonzero if OVERLAY.XXX detected */
int is_noequiv=OFF; /* nonzero if commands with no WarpLink equivalent were found */
int is_unknown=OFF; /* nonzero if unknown commands were found */

void main(int argc, char *argv[])
{
    int i,len,numargs;
    char temp[128];

    credits();
    numargs=argc-1; /* get number of command line parameters specified */
    if(numargs>=1 && argv[1][0]=='?'){  /* list syntax and options */
        summary();
        exit(0);
    }
    for(i=1;i<=numargs;i++){
        strcpy(temp,argv[i]);
        strupr(temp);
        if(temp[0]=='/'){   /* option */
            if(!strcmp(temp,"/NC")){    /* no comments option */
                no_comment_flag=ON;
            }
            else if (!strcmp(temp,"/NO")){  /* no ovlmgr.obj option */
                no_ovlmgr_flag=ON;
            }
            else if (!strcmp(temp,"/NW")){  /* no warnings option */
                no_warn_flag=ON;
            }
            else{   /* bad option */
                printf("\n\007Bad option: %s",temp);
                exit(1);
            }
        }
        else{   /* not an option, assume file name */
            if(firstcom){   /* already have position of first file */
                if(secondcom){  /* already have position of second file too */
                    printf("\n\007Too many files specified on command line.");
                    exit(1);
                }
                else{
                    secondcom=i;
                }
            }
            else{
                firstcom=i;
            }
        }
    }
    if(!firstcom){ /* input file name not provided */
        cprintf("\n\rPlink script file name (.LNK default extension)? ");
        gets(infile);
    }
    else{   /* input file name was provided */
        strcpy(infile,argv[firstcom]);
    }
    len=strlen(infile);
    if(!len){   /* exit on null file name */
        exit(0);
    }
    if(!is_extension(infile) && len<123) {   /* no extension, add default */
        strcpy(prefile,infile); /* get base filename in preprocessed file name first */
        strcat(infile,def_rsp_ext);
        strcat(prefile,".$$$"); /* give proper extension to preprocessed file */
    }
    else{   /* file extension exists */
        strcpy(prefile,infile); /* get base filename in preprocessed file name first */
        i=strlen(infile);
        while(i && infile[i]!='.'){
            i--;
        }
        if(infile[i]=='.' && infile[i+1]=='$' && infile[i+2]=='$' && infile[i+3]=='$'){
            /* illegal file name, keep '$$$' extension for future work */
            printf("\n\007Source file extension '$$$' not allowed.");
            exit(1);
        }
        prefile[i]='.'; /* replace with proper preprocessed file extension */
        prefile[i+1]='$';
        prefile[i+2]='$';
        prefile[i+3]='$';
        prefile[i+4]=0;
    }

    if(!secondcom){ /* output file name not provided */
        cprintf("\n\rWarpLink response file name (.LNK default extension)? ");
        gets(outfile);
    }
    else{   /* output file name was provided */
        strcpy(outfile,argv[secondcom]);
    }
    len=strlen(outfile);
    if(!len){   /* exit on null file name */
        exit(0);
    }
    if(!is_extension(outfile) && len<123) {   /* no extension, add default */
        strcat(outfile,def_rsp_ext);
    }

    if(!strcmp(infile,outfile)){ /* names cannot match */
        printf("\n\007Input file name cannot be same as output file name");
        exit(1);
    }

    if((fpin=fopen(infile,"r"))==NULL){  /* error opening file */
        printf("\n\007Error opening input file %s",infile);
        exit(1);
    }

    preprocess();   /* preprocess input file to .$$$ file */
    fclose(fpin);   /* close input file */

    /* open .$$$ file, translate commands */
    if((fpin=fopen(prefile,"r"))==NULL){  /* error opening file */
        printf("\n\007Error opening preprocessing file %s",prefile);
        exit(1);
    }

    if((fpout=fopen(outfile,"w"))==NULL){ /* error opening file */
        printf("\n\007Error opening output file %s",outfile);
        exit(1);
    }

    while(fgets(workbuff,128,fpin)!=NULL){   /* read Plink commands until file end */
        sprintf(saveline,"# %s#\n",workbuff);
        strupr(workbuff);
        parse_command();
        comments(saveline);
    }
    fclose(fpin);
    unlink(prefile);    /* erase preprocess file */
    if(is_overlays && no_ovlmgr_flag==OFF){ /* overlays specified, add WarpLink's overlay manager */
        ovl_nest=0; /* make sure overlay manager won't be stuck in an overlay */
        if(is_clarionlib){  /* add clarion specific overlay manager */
            add_obj_name("CNOVLMGR.OBJ");
        }
        else if(is_qslib){  /* add quicksilver specific overlay manager */
            add_obj_name("QSOVLMGR.OBJ");
        }
        else{
            add_obj_name("OVLMGR.OBJ");
        }
    }
    write_warplink_file();
    fclose(fpout);
    warnings();
    exit(0);
}

void credits(void)
{
    printf("\nWARPCONV, PLINK86/.RTLink script file to WarpLink response file converter");
    printf("\nVersion 2.52, Copyright 1990-1994, Michael Devore.  All rights reserved.");
    printf("\nType WARPCONV ? for options\n");
}

void summary(void)
{
    printf("\nSYNTAX:   WARPCONV [options] [input_file] [output_file]\n");
    printf("\n          Items in brackets are optional, WARPCONV will prompt");
    printf("\n          for file names if necessary.  If no file extension is");
    printf("\n          supplied, WARPCONV will default to the extension .LNK.");
    printf("\n          End a file name with a period for no extension.\n");
    printf("\nOPTIONS:  /nc  Delete translator comments in WarpLink response file.");
    printf("\n          /no  Do not automatically add OVLMGR.OBJ to object module list.");
    printf("\n          /nw  Do not give any warnings following translation.\n");
}

void warnings(void)
{
    if (no_warn_flag){
        return;
    }
    if(is_overlays){
        if(!is_clipperlib && !is_clarionlib){
            printf("\nOverlays have been specified in the script file.  You may need to");
            printf("\nexplicitly set WarpLink's overlay pool size using the /op:<size>");
            printf("\noption.  Consult the WarpLink documentation for more information");
            printf("\non the /op:<size> option.\n");
        }
        if(is_overlayxxx){  /* OVERLAY.OBJ or OVERLAY.LIB detected */
            printf("\nOne of the files OVERLAY.OBJ or OVERLAY.LIB has been listed in");
            printf("\nthe script file.  These files are not needed by WarpLink IF they");
            printf("\nprovide overlay support for use by other linkers.\n");
        }
    }
    if(is_noequiv){
        printf("\nOne or more commands are in the original script file that are not");
        printf("\nduplicated by WarpLink commands.  Make sure that your program will");
        printf("\nlink and operate correctly without these commands.\n");
    }
    if(is_unknown){
        printf("\nOne or more commands in the original script file are not recognized");
        printf("\nby WarpConv.  You may need to make modifications to the WarpLink");
        printf("\nresponse file for the program to link or operate correctly.\n");
    }
}

int is_extension(char *filename)
{
    int pos,backslash_flag=0;

    pos=strlen(filename);
    while(pos>0){
        if(filename[pos]=='\\'){
            backslash_flag=1;   /* track directory indicators, for periods immediately preceding */
        }
        else if(filename[pos]=='.'){
            if(!backslash_flag){ /* char after this period was not backslash, filename extension */
                return(1);  /* flag extension exists */
            }
        }
        else{
            backslash_flag=0;
        }
        pos--;
    }
    return(0);  /* flag no extension exists */
}

void parse_command(void)
{
    char buffchar,command[20],numarray[8],numbase[3];
    int i,len,commandpos,chars_matched,numarray_pos,nest_store;
    int buffpos=0;

    len=strlen(workbuff);
    while(workbuff[buffpos]<=' ' && buffpos<len)
        buffpos++;   /* strip leading whitespace */
    if(buffpos==len){
        comments(text_success);   /* successful translation (of blank line) */
        return; /* blank line, no commands */
    }
    command[0]=commandpos=0;
    while(buffpos<len && commandpos<19){
        if(comma_flag && libfile_flag){
            /* trailing comma for last line, with file/lib/search last command */
            switch(libfile_flag){
                case 1: /* file */
                    strcpy(command,"FILE");
                    commandpos=4;
                    break;
                case 2: /* library */
                    strcpy(command,"LIBRARY");
                    commandpos=7;
                    break;
                case 3: /* search */
                    strcpy(command,"SEARCH");
                    commandpos=6;
                    break;
                case 4: /* allocate */
                    strcpy(command,"ALLOCATE");
                    commandpos=6;
                    break;
            }
            libfile_flag=0; /* re-init flag */
            break;  /* have command, blow loop */
        }
        buffchar=workbuff[buffpos];
        if(isalpha(buffchar)){ /* alphabetic, part of command */
            command[commandpos++]=buffchar;
            buffpos++;
        }
        else if(buffchar==comment_char){    /* line starting here is a comment */
            if(!commandpos){    /* no commands */
                comments(text_success);   /* successful translation (of comment line) */
                return;
            }
            else{   /* command prior to comment */
                break;
            }
        }
        else{   /* end of alphabetic command */
            break;
        }
    }
    if(!command[0]){    /* line did not begin with alpha command */
        is_unknown=ON;
        comments(text_unknown);   /* unknown or bad command */
        return;
    }
    command[commandpos]=0;  /* null terminate string */

    /* make buffpos -> first non-whitespace char after command string, or null terminator */
    while(workbuff[buffpos] && workbuff[buffpos]<=' '){   /* gobble white space */
        buffpos++;
    }

    /* translatable commands */
    if(!strncmp(command,"FI",2)){   /* FILE command */
        if(!(chars_matched=match(&command[2],"LE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=1; /* flag last command was FILE */
            get_filenames(buffpos,NO);  /* get the object module name(s) */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"LI",2)){  /* LIBRARY command */
        if(!(chars_matched=match(&command[2],"BRARY",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=2; /* flag last command was library */
            get_filenames(buffpos,YES); /* get the library name(s) */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"SEARCH",6)){  /* SEARCH command */
        libfile_flag=3; /* flag last command was search */
        get_filenames(buffpos,YES); /* get the library name(s) */
        comments(text_success);   /* successful translation */
        return;
    }
    else if(!strncmp(command,"ALLOCATE",8)){  /* ALLOCATE command */
        libfile_flag=4; /* flag last command was allocate */
        nest_store=ovl_nest;    /* save current overlay nest value */
        ovl_nest=1; /* make sure nest value is nonzero so ALLOCATED lib always overlaid */
        get_filenames(buffpos,YES); /* get the library name(s) */
        ovl_nest=nest_store;    /* restore previous overlay nest value */
        comments(text_success);   /* successful translation */
        return;
    }
    else if(!strncmp(command,"MA",2)){  /* MAP command */
        if(!(chars_matched=match(&command[2],"P",YES))){
            return; /* didn't match rest of command */
        }
        else{
            buffchar=' ';
            while(buffchar!='=' && buffchar!=comment_char){   /* scan for equals sign */
                buffchar=workbuff[buffpos];
                if(!buffchar){  /* at end of line */
                    break;
                }
                if(buffchar>' '){   /* break on non-whitespace */
                    break;
                }
                buffpos++;
            }
            if(buffchar=='='){   /* advance past '=', if at it */
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
            if(buffchar=='='){  /* MAP file name supplied */
                if(workbuff[buffpos]){  /* not at end of line */
                    i=0;
                    while(workbuff[buffpos]>' '){
                        mapfile[i++]=workbuff[buffpos++];
                    }
                    mapfile[i]=0;   /* null terminate map file name */
                }
            }
            use_expmap=YES; /* flag use of expanded MAP file */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"ST",2)){  /* STACK command */
        if(!(chars_matched=match(&command[2],"ACK",YES))){
            return; /* didn't match rest of command */
        }
        else{
            numarray[0]=numarray_pos=0;
            numbase[0]='%'; /* set up to default read in hex number */
            numbase[1]='x';
            numbase[2]=0;
            while(numarray_pos<7){  /* can't have more than 6 digits plus base specifier */
                buffchar=workbuff[buffpos++];
                if(!isdigit(buffchar)){  /* not a digit, check for base specifier */
                    if(buffchar=='.'){  /* decimal */
                        numbase[1]='u';
                        break;
                    }
                    else if(buffchar=='D'){ /* decimal */
                        numbase[1]='u';
                        break;
                    }
                    else if (buffchar=='O'){    /* octal */
                        numbase[1]='o';
                        break;
                    }
                    else{   /* assume hex base */
                        numbase[1]='x';
                        break;
                    }
                }
                else{   /* is a digit, add to string */
                    numarray[numarray_pos++]=buffchar;
                }
            }
            numarray[numarray_pos]=0;   /* null terminate number string */
            sscanf(numarray,numbase,&stack_value);
            use_stack=YES;
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"DOS",3)){ /* DOSSEG command */
        if(!(chars_matched=match(&command[3],"SEG",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_dosseg=YES; /* command matched, set dosseg flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"ME",2)){  /* MEMORY command */
        if(!(chars_matched=match(&command[2],"MORY",YES))){
            return; /* didn't match rest of command */
        }
        else{
            numarray[0]=numarray_pos=0;
            numbase[0]='%'; /* set up to default read in hex number */
            numbase[1]='h';
            numbase[2]=0;
            while(numarray_pos<7){  /* can't have more than 6 digits plus base specifier */
                buffchar=workbuff[buffpos++];
                if(!isdigit(buffchar)){  /* not a digit, check for base specifier */
                    if(buffchar=='.'){  /* decimal */
                        numbase[1]='u';
                        break;
                    }
                    else if(buffchar=='D'){ /* decimal */
                        numbase[1]='u';
                        break;
                    }
                    else if (buffchar=='O'){    /* octal */
                        numbase[1]='o';
                        break;
                    }
                    else{   /* assume hex base */
                        numbase[1]='x';
                        break;
                    }
                }
                else{   /* is a digit, add to string */
                    numarray[numarray_pos++]=buffchar;
                }
            }
            numarray[numarray_pos]=0;   /* null terminate number string */
            sscanf(numarray,numbase,&maxalloc_value);
            use_maxalloc=YES;
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"NOD",3)){ /* NODEFLIB command */
        if(!(chars_matched=match(&command[3],"EFLIB",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_deflib=NO;  /* command matched, set no default libraries flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"NOE",3)){ /* NOEXTDICTIONARY command */
        if(!(chars_matched=match(&command[3],"XTDICTIONARY",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_extdict=NO; /* command matched, set no extended dictionary flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"EXT",3)){ /* EXTDICTIONARY command */
        if(!(chars_matched=match(&command[3],"DICTIONARY",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_extdict=YES;    /* command matched, set extended dictionary flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"RESE",4)){    /* RESEARCH command */
        if(!(chars_matched=match(&command[4],"ARCH",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation, nothing done */
            return;
        }
    }
    else if(!strncmp(command,"BE",2)){  /* BEGINAREA command */
        if(!(chars_matched=match(&command[2],"GINAREA",YES))){
            return; /* didn't match rest of command */
        }
        else{
            is_overlays=YES;    /* flag overlays specified */
            ovl_nest++; /* increase overlay nesting level */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"EN",2)){  /* ENDAREA command */
        if(!(chars_matched=match(&command[2],"DAREA",YES))){
            return; /* didn't match rest of command */
        }
        else{
            ovl_nest--; /* decrease overlay nesting level */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"SECTION",7)){ /* SECTION command */
        libfile_flag=0; /* flag last command was not file or library or search */
        comments(text_ignore);    /* ignore sections */
        if(strstr(workbuff," INTO ")!=NULL){
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_into);    /* give INTO feedback as well */
        }
        return;
    }
    else if(!strncmp(command,"OU",2)){  /* OUTPUT command */
        if(!(chars_matched=match(&command[2],"TPUT",YES))){
            return; /* didn't match rest of command */
        }
        else{
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
            if(workbuff[buffpos]){  /* not at end of line */
                i=0;
                while(workbuff[buffpos]>' '){
                    exefile[i++]=workbuff[buffpos++];
                }
                exefile[i]=0;   /* null terminate exe file name */
            }
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"WO",2)){  /* WORKFILE command */
        if(!(chars_matched=match(&command[2],"RKFILE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
            if(workbuff[buffpos]=='='){ /* gobble equals sign */
                buffpos++;
                while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                    buffpos++;
                }
            }
            if(workbuff[buffpos]){  /* not at end of line */
                i=0;
                while(workbuff[buffpos]>' '){
                    workfile[i++]=workbuff[buffpos++];
                }
                workfile[i]=0;   /* null terminate work file name */
            }
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"MI",2)){  /* MIXCASE command */
        if(!(chars_matched=match(&command[2],"XCASE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_case=YES;   /* command matched, set case sensitive flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"IG",2)){  /* IGNORECASE command */
        if(!(chars_matched=match(&command[2],"NORECASE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            use_case=NO;    /* command matched, reset case sensitive flag */
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"OV",2)){  /* OVERLAY command */
        if(!(chars_matched=match(&command[2],"ERLAY",YES))){
            return; /* didn't match rest of command */
        }
        else{
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
            if(workbuff[buffpos]){  /* not at end of line */
                i=0;
                while(workbuff[buffpos]>' '){
                    ovlclass[i++]=workbuff[buffpos++];
                }
                ovlclass[i]=0;   /* null terminate overlay class name */
            }
            use_ovlclass=YES;
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"COMM",4)){  /* COMMENT command */
        if(!(chars_matched=match(&command[4],"ENT",YES))){
            return; /* didn't match rest of command */
        }
        else{
            if(workbuff[buffpos]){
                comment_char=workbuff[buffpos];
            }
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"COMP",4)){  /* COMPATIBLE command */
        if(!(chars_matched=match(&command[4],"ATIBLE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            comment_char='%';
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    else if(!strncmp(command,"VE",2)){  /* VERBOSE command */
        if(!(chars_matched=match(&command[2],"RBOSE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            use_info=ON;    /* flag to turn on linker info display */
            comments(text_success);   /* successful translation */
            return;
        }
    }
    /* no equivalent commands, possibility of program change */
    else if(!strncmp(command,"NORESE",6)){  /* NORESEARCH command */
        if(!(chars_matched=match(&command[6],"ARCH",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            is_noequiv=ON;
            comments(text_noequiv);   /* no equivalent */
            return;
        }
    }
    else if(!strncmp(command,"LO",2)){  /* LOWERCASE command */
        if(!(chars_matched=match(&command[2],"WERCASE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            is_noequiv=ON;
            comments(text_noequiv);   /* no equivalent */
            return;
        }
    }
    else if(!strncmp(command,"UP",2)){  /* UPPERCASE command */
        if(!(chars_matched=match(&command[2],"PERCASE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            is_noequiv=ON;
            comments(text_noequiv);   /* no equivalent */
            return;
        }
    }
    /* ignore without program change commands */
    else if(!strncmp(command,"PRE",3)){ /* PRELOAD command */
        if(!(chars_matched=match(&command[3],"LOAD",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_ignore);   /* ignore command */
            return;
        }
    }
/*
    else if(!strncmp(command,"WO",2)){  /* WORKFILE command */
        if(!(chars_matched=match(&command[2],"RKFILE",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_ignore);   /* ignore command */
            return;
        }
    }
*/
    else if(!strncmp(command,"BATCH",5)){   /* BATCH command */
        libfile_flag=0; /* flag last command was not file or library or search */
        comments(text_ignore);   /* ignore command */
        return;
    }
    else if(!strncmp(command,"DE",2)){  /* DEBUG command */
        if(!(chars_matched=match(&command[2],"BUG",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_ignore);   /* ignore command */
            return;
        }
    }
    else if(!strncmp(command,"H",1)){   /* HEIGHT or HELP command */
        if(!(chars_matched=match(&command[1],"EIGHT",NO))){ /* no match on HEIGHT, try HELP */
            if(!(chars_matched=match(&command[1],"ELP",NO))){   /* no match on HELP */
                libfile_flag=0; /* flag last command was not file or library or search */
                is_unknown=ON;
                comments(text_unknown);
            }
            else{
                libfile_flag=0; /* flag last command was not file or library or search */
                comments(text_ignore);   /* ignore command */
            }
            return;
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_ignore);   /* ignore command */
            return;
        }
    }
    else if(!strncmp(command,"NOBELL",6)){  /* NOBELL command */
        libfile_flag=0; /* flag last command was not file or library or search */
        comments(text_ignore);   /* ignore command */
        return;
    }
    else if(!strncmp(command,"WA",2)){  /* WARNING command */
        if(!(chars_matched=match(&command[2],"RNING",YES))){
            return; /* didn't match rest of command */
        }
        else{
            libfile_flag=0; /* flag last command was not file or library or search */
            comments(text_ignore);   /* ignore command */
            return;
        }
    }
    else{   /* unknown or bad command */
        libfile_flag=0; /* flag last command was not file or library or search */
        is_unknown=ON;
        comments(text_unknown);
        return;
    }
}

/*
    return zero if string doesn't match tomatch string for as many chars
    as is in string.  Return number of chars matched + 1 if match.  Offset
    by +1 so that zero will not be returned upon successful match of null
    string.
*/

int match(char *string,const char *tomatch,int mustmatch_flag)
{
    int tomatchpos=0,stringpos=0;

    while(string[stringpos]>' '){   /* check remaining chars in command string */
        if(string[stringpos++]!=tomatch[tomatchpos++]){ /* match failure */
            if(mustmatch_flag){ /* strings must match, give failure feedback */
                is_unknown=ON;
                comments(text_unknown);   /* unknown or bad command */
            }
            return(0);
        }
    }
    return(tomatchpos+1);  /* at end of command string, success */
}

void write_warplink_file(void)
{
    if(no_comment_flag==OFF){
        fputs("#----------------------------------------------------------------------\n",fpout);
        fputs("#\n",fpout);
        fputs("# WarpLink options\n",fpout);
        fputs("#\n",fpout);
    }
    write_options();
    if(no_comment_flag==OFF){
        fputs("#\n",fpout);
        fputs("# Object module list\n",fpout);
        fputs("#\n",fpout);
    }
    write_objs();
    if(no_comment_flag==OFF){
        fputs("#\n",fpout);
        fputs("# Executable file name\n",fpout);
        fputs("#\n",fpout);
    }
    fputs(exefile,fpout);
    fputs("\n",fpout);
    if(no_comment_flag==OFF){
        fputs("#\n",fpout);
        fputs("# Map file name\n",fpout);
        fputs("#\n",fpout);
    }
    if(use_expmap){
        fputs(mapfile,fpout);
    }
    fputs("\n",fpout);
    if(no_comment_flag==OFF){
        fputs("#\n",fpout);
        fputs("# Library list\n",fpout);
        fputs("#\n",fpout);
    }
    write_libs();
}

void write_options(void)
{
    char tbuff[128];
    char tnum[6];

    tbuff[0]=0;
    strcpy(tbuff,"/w0 ");   /* always make warnings return code of zero */
    if(use_info){
        strcat(tbuff,"/i ");
    }
    if(use_extdict){
        strcat(tbuff,"/em ");
    }
    if(is_clipperlib && is_overlays){
        strcat(tbuff,"/op:m /r ");
    }
    if(is_clarionlib && is_overlays){
        strcat(tbuff,"/op:m /cla ");
    }
    if(is_qslib && is_overlays){
        strcat(tbuff,"/op:m /oc:PROG ");
    }
    if(use_expmap){
        strcat(tbuff,"/mx ");
    }
    if(use_stack){
        strcat(tbuff,"/st:");
        sprintf(tnum,"%u",stack_value);
        strcat(tbuff,tnum);
        strcat(tbuff," ");
    }
    if(use_dosseg){
        strcat(tbuff,"/d ");
    }
    if(!use_deflib){
        strcat(tbuff,"/nd ");
    }
    if(use_case){
        strcat(tbuff,"/s ");
    }
    if(use_maxalloc){
        strcat(tbuff,"/as:");
        sprintf(tnum,"%u",maxalloc_value);
        strcat(tbuff,tnum);
        strcat(tbuff," ");
    }
    if(use_ovlclass && !is_clarionlib){
        strcat(tbuff,"/oc:");
        strcat(tbuff,ovlclass);
        strcat(tbuff," ");
    }
    if(workfile[0]){
        strcat(tbuff,"/tf:");
        strcat(tbuff,workfile);
        strcat(tbuff," ");
    }
    strcat(tbuff," +\n");
    fputs(tbuff,fpout);
}

void write_objs(void)
{
    char tbuff[132];
    int len;
    struct fileinfo *curr_objptr;
    static int objs_written=0;

    curr_objptr=first_objptr;
    tbuff[0]=0;
    while(objs_written<obj_count){  /* cycle thru all entries */
        tbuff[0]=0;
        if(curr_objptr->flags.is_overlaid){ /* overlaid, put in left paren */
            strcat(tbuff,"(");
        }
        strcat(tbuff,curr_objptr->nameptr); /* get file name in tbuff */

        len=strlen(curr_objptr->nameptr);
        if(len>=7){    /* check for OVERLAY.OBJ*/
            if(len==11 || curr_objptr->nameptr[len-12]==':' || curr_objptr->nameptr[len-12]=='\\'){
                if(!strcmp(&curr_objptr->nameptr[len-11],"OVERLAY.OBJ")){
                    is_overlayxxx=YES;
                }
            }
            if(len==7 || curr_objptr->nameptr[len-8]==':' || curr_objptr->nameptr[len-8]=='\\'){
                if(!strcmp(&curr_objptr->nameptr[len-7],"OVERLAY")){
                    is_overlayxxx=YES;
                }
            }
        }

        if(curr_objptr->flags.is_overlaid){ /* overlaid, put in right paren */
            strcat(tbuff,")");
        }
        if(objs_written+1<obj_count){   /* not last name entry */
            strcat(tbuff,"+");  /* add plus for next line */
        }
        fputs(tbuff,fpout);
        fputs("\n",fpout);
        curr_objptr=curr_objptr->nextptr;   /* move to next entry, if any */
        objs_written++;
    }
    if(!obj_count){ /* check for silly case of not specifying object modules */
        fputs("\n",fpout);
    }
}

void write_libs(void)
{
    char tbuff[132];
    int len;
    struct fileinfo *curr_libptr;
    static int libs_written=0;

    curr_libptr=first_libptr;
    tbuff[0]=0;
    while(libs_written<lib_count){  /* cycle thru all entries */
        tbuff[0]=0;
        if(curr_libptr->flags.is_overlaid){ /* overlaid, put in left paren */
            strcat(tbuff,"(");
        }
        strcat(tbuff,curr_libptr->nameptr); /* get file name in tbuff */

        len=strlen(curr_libptr->nameptr);
        if(len>=7){    /* check for OVERLAY.LIB */
            if(len==11 || curr_libptr->nameptr[len-12]==':'|| curr_libptr->nameptr[len-12]=='\\'){
                if(!strcmp(&curr_libptr->nameptr[len-11],"OVERLAY.LIB")){
                    is_overlayxxx=YES;
                }
            }
            if(len==7 || curr_libptr->nameptr[len-8]==':' || curr_libptr->nameptr[len-8]=='\\'){
                if(!strcmp(&curr_libptr->nameptr[len-7],"OVERLAY")){
                    is_overlayxxx=YES;
                }
            }
        }

        if(curr_libptr->flags.is_overlaid){ /* overlaid, put in right paren */
            strcat(tbuff,")");
        }
        if(libs_written+1<lib_count){   /* not last name entry */
            strcat(tbuff,"+");  /* add plus for next line */
        }
        fputs(tbuff,fpout);
        fputs("\n",fpout);
        curr_libptr=curr_libptr->nextptr;   /* move to next entry, if any */
        libs_written++;
    }
}

void get_filenames(int buffpos,int islib)
{
    int i;
    char filename[128];

    while(1){
         /* gobble whitespace and commas */
        while((workbuff[buffpos] && workbuff[buffpos]<=' ') || workbuff[buffpos]==','){
            if(workbuff[buffpos]==','){ /* trailing comma */
                comma_flag=1;   /* set comma flag */
            }
            buffpos++;
        }
        if(!workbuff[buffpos] || workbuff[buffpos]==comment_char){ /* at end of line */
            break;  /* done */
        }
        i=0;
        /* grab filename until comma or whitespace */
        comma_flag=0;   /* init to no trailing comma */
        while(workbuff[buffpos]>' ' && workbuff[buffpos]!=','){
            filename[i++]=workbuff[buffpos++];
        }
        filename[i]=0;   /* null terminate filename string */
        if(workbuff[buffpos]==','){ /* trailing comma */
            comma_flag=1;   /* set comma flag */
        }
        if(islib){  /* parsing library name */
            add_lib_name(filename);
        }
        else{   /* parsing object module name */
            add_obj_name(filename);
        }
    }
}

void add_obj_name(char *filename)
{
    int len;
    struct fileinfo *prev_objptr;

    prev_objptr=last_objptr;
    last_objptr=(struct fileinfo *)malloc(sizeof(struct fileinfo)); /* allocate new entry */
    if(last_objptr==NULL){  /* error allocating memory */
        alloc_memerr(1);    /* exit with code */
    }
    len=strlen(filename);
    last_objptr->nameptr=(char *)malloc(len+1); /* allocate room for name plus null terminator */
    if(last_objptr->nameptr==NULL){ /* error allocating memory */
        alloc_memerr(2);    /* exit with code */
    }
    strcpy(last_objptr->nameptr,filename);  /* copy name into memory allocation */

    if(!obj_count){ /* first object module */
        first_objptr=last_objptr;   /* keep pointer to first object module */
    }
    else{   /* not first object module, save pointer to new in previous */
        prev_objptr->nextptr=last_objptr;
    }
    if(ovl_nest){   /* in an overlay */
        last_objptr->flags.is_overlaid=1;   /* set overlaid flag */
    }
    else{
        last_objptr->flags.is_overlaid=0;   /* reset overlaid flag */
    }
    last_objptr->flags.is_library=0;    /* flag not in library */
    obj_count++;    /* bump count of object modules */
}

void add_lib_name(char *filename)
{
    int len;
    struct fileinfo *prev_libptr;

    len=strlen(filename);
    if(len==6 || filename[len-7]==':' || filename[len-7]=='\\'){
        if(!strcmp(&filename[len-6],"QS.LIB")){
            is_qslib=YES;
        }
    }
    if(len==11 || filename[len-12]==':' || filename[len-12]=='\\'){
        if(!strcmp(&filename[len-11],"CLIPPER.LIB")){
            is_clipperlib=YES;
        }
        if(!strcmp(&filename[len-11],"CLARION.LIB")){
            is_clarionlib=YES;
        }
    }
    if(len==12 || filename[len-13]==':' || filename[len-13]=='\\'){
        if(!strcmp(&filename[len-12],"CLARION1.LIB")){
            is_clarionlib=YES;
        }
        if(!strcmp(&filename[len-12],"CLARION2.LIB")){
            is_clarionlib=YES;
        }
    }
    if(len==2 || filename[len-3]==':' || filename[len-3]=='\\'){
        if(!strcmp(&filename[len-2],"QS")){
            is_qslib=YES;
        }
    }
    if(len==7 || filename[len-8]==':' || filename[len-8]=='\\'){
        if(!strcmp(&filename[len-7],"CLIPPER")){
            is_clipperlib=YES;
        }
        if(!strcmp(&filename[len-7],"CLARION")){
            is_clarionlib=YES;
        }
    }
    if(len==8 || filename[len-9]==':' || filename[len-9]=='\\'){
        if(!strcmp(&filename[len-8],"CLARION1")){
            is_clarionlib=YES;
        }
        if(!strcmp(&filename[len-8],"CLARION2")){
            is_clarionlib=YES;
        }
    }

    prev_libptr=last_libptr;
    last_libptr=(struct fileinfo *)malloc(sizeof(struct fileinfo)); /* allocate new entry */
    if(last_libptr==NULL){  /* error allocating memory */
        alloc_memerr(3);    /* exit with code */
    }
    len=strlen(filename);
    last_libptr->nameptr=(char *)malloc(len+1); /* allocate room for name plus null terminator */
    if(last_libptr->nameptr==NULL){ /* error allocating memory */
        alloc_memerr(4);    /* exit with code */
    }
    strcpy(last_libptr->nameptr,filename);  /* copy name into memory allocation */

    if(!lib_count){ /* first library */
        first_libptr=last_libptr;   /* keep pointer to first library */
    }
    else{   /* not first library, save pointer to new in previous */
        prev_libptr->nextptr=last_libptr;
    }
    if(ovl_nest){   /* in an overlay */
        last_libptr->flags.is_overlaid=1;   /* set overlaid flag */
    }
    else{
        last_libptr->flags.is_overlaid=0;   /* reset overlaid flag */
    }
    last_libptr->flags.is_library=1;    /* flag in library */
    lib_count++;    /* bump count of libraries */
}

void alloc_memerr(int code)
{
    printf("\n\007Error allocating memory: code %u",code);
    exit(1);
}

void preprocess(void)
{
    if((fpout=fopen(prefile,"w"))==NULL){ /* error opening file */
        printf("\n\007Error opening preprocessor file %s",prefile);
        exit(1);
    }

    while(fgets(workbuff,128,fpin)!=NULL){   /* read Plink commands until file end */
        if(strlen(workbuff)>125){   /* string too long for response file */
            printf("\n\007Input line too long:\n%s",workbuff);
            exit(1);
        }
        strupr(workbuff);
        splitout(); /* split out any merged commands, put each on separate line */
    }

    fclose(fpout);
}

void splitout(void)
{
    int startpos,buffpos=0;
    int com_flag=OFF,secflag=OFF,mapflag=OFF;
    char comstring[128];

    startpos=buffpos; /* track start of string */
    while(1){

        while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
            workbuff[buffpos]=' ';  /* change all whitespace to space */
            buffpos++;
        }

        if(!workbuff[buffpos]){ /* at end of line */
            if(com_flag){    /* write command parsed */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            return;
        }

        if(!strncmp(&workbuff[buffpos],"BLINKER",7)){   /* blinker specific crap */
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
            while(workbuff[buffpos]){ /* eat chars until null terminator */
                buffpos++;
            }
            continue;   /* back to start of parse loop */
        }

        if(workbuff[buffpos]=='='){ /* gobble equals and first following string */
            buffpos++;
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
                buffpos++;
            }
            if(mapflag){    /* check for map options */
                while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                    if(workbuff[buffpos] && workbuff[buffpos]<' '){
                        workbuff[buffpos]=' ';  /* change all whitespace to space */
                    }
                    buffpos++;
                }
                /* gobble MAP options */
                while(workbuff[buffpos] && (workbuff[buffpos+1]<=' ' || workbuff[buffpos+1]==',')){
                    buffpos++;
                    if(workbuff[buffpos] && workbuff[buffpos]<' '){
                        workbuff[buffpos]=' ';  /* change all whitespace to space */
                    }
                }
                mapflag=OFF;
            }
            if(!secflag){   /* not a SECTION command */
                if(com_flag){    /* write command parsed */
                    strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                    comstring[buffpos-startpos]=0;  /* null terminate string */
                    fprintf(fpout,"%s\n",comstring);
                }
                com_flag=OFF;   /* done writing command */
                startpos=buffpos; /* track start of string */
            }
            continue;   /* back to start of parse loop */
        }

        if(workbuff[buffpos]==','){   /* gobble comma and first following string */
            buffpos++;
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                workbuff[buffpos]=' ';  /* change all whitespace to space */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            continue;   /* back to start of parse loop */
        }

        if(isdigit(workbuff[buffpos])){    /* number, eat it */
             /* gobble non-whitespace */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
                buffpos++;
            }
            if(com_flag){    /* write command parsed */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            mapflag=OFF;
            com_flag=OFF;   /* done writing command */
            startpos=buffpos; /* track start of string */
            continue;   /* back to start of parse loop */
        }

        /* check if INTO of SECTION INTO */
        if(secflag && workbuff[buffpos]=='I' && workbuff[buffpos+1]=='N' && \
            workbuff[buffpos+2]=='T' && workbuff[buffpos+3]=='O'){
             /* gobble non-whitespace */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        if(workbuff[buffpos]==comment_char){   /* at comment, stop parsing */
            if(com_flag){    /* write command parsed */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            return;
        }

        /* check if start of FILE, set command flag if so */
        if(workbuff[buffpos]=='F' && workbuff[buffpos+1]=='I'){
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
             /* gobble non-whitespace non-comma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        /* check if start of LIBRARY or SEARCH or ALLOCATE, set command flag if so */
        if((workbuff[buffpos]=='L' && workbuff[buffpos+1]=='I') || !strncmp(&workbuff[buffpos],"SEARCH",6) ||
            !strncmp(&workbuff[buffpos],"ALLOCATE",8)){
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
             /* gobble non-whitespace non-comma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        /* check if start of OVERLAY, set command flag if so */
        if(workbuff[buffpos]=='O' && workbuff[buffpos+1]=='V'){
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
             /* gobble non-whitespace non-comma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        /* check if start of WORKFILE, set command flag if so */
        if(workbuff[buffpos]=='W' && workbuff[buffpos+1]=='O'){
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
             /* gobble non-whitespace */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
            if(workbuff[buffpos]=='='){ /* equals sign specified with spaces */
                buffpos++;
                while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                    buffpos++;
                }
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        /* check if start of OUTPUT, set command flag if so */
        if(workbuff[buffpos]=='O' && workbuff[buffpos+1]=='U'){
            if(com_flag){   /* write previous command */
                strncpy(comstring,&workbuff[startpos],buffpos-startpos);
                comstring[buffpos-startpos]=0;  /* null terminate string */
                fprintf(fpout,"%s\n",comstring);
            }
            else{   /* set com_flag */
                com_flag=ON;
            }
            startpos=buffpos; /* track start of string */
             /* gobble non-whitespace */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
                buffpos++;
            }
            while(workbuff[buffpos] && workbuff[buffpos]<=' '){ /* gobble whitespace */
                buffpos++;
            }
             /* gobble non-whitespace noncomma */
            while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char && workbuff[buffpos]!=','){
                buffpos++;
            }
            mapflag=OFF;
            secflag=OFF;
            continue;   /* back to start of parse loop */
        }

        /* assume command, eat until whitespace or end, set comflag */
        if(com_flag){   /* write previous command */
            strncpy(comstring,&workbuff[startpos],buffpos-startpos);
            comstring[buffpos-startpos]=0;  /* null terminate string */
            fprintf(fpout,"%s\n",comstring);
        }
        else{   /* set com_flag */
            com_flag=ON;
        }
        startpos=buffpos; /* track start of string */
        mapflag=OFF;
        secflag=OFF;

        /* check if SECTION command, if so set secflag */
        if(workbuff[buffpos]=='S' && workbuff[buffpos+1]=='E' && workbuff[buffpos+2]=='C' && workbuff[buffpos+3]=='T' && \
            workbuff[buffpos+4]=='I' && workbuff[buffpos+5]=='O' && workbuff[buffpos+6]=='N'){
            secflag=ON;
        }

        /* check if MAP command, if so set mapflag */
        if(workbuff[buffpos]=='M' && workbuff[buffpos+1]=='A'){
            mapflag=ON;
        }
         /* gobble non-whitespace */
        while(workbuff[buffpos] && workbuff[buffpos]>' ' && workbuff[buffpos]!=comment_char){
            buffpos++;
        }
    }
}

void comments(const char *string)
{
    if (no_comment_flag){
        return;
    }
    fprintf(fpout,"%s",string);
}
