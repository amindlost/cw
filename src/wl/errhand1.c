/*
    C example of using the overlay manager error handler vector, internal
    overlay file name variable, and overlay file open and close routines.

    To try it out, compile in the large model (or make appropriate
    substitutions) and then link.  Only works with large model code or,
    at minimum, large code model strcpy() function when copying to the
    overlay file name (both segment and offset of address are necessary).
    When using Borland C, for example, use this command line:

        warplink /op:1 c0l errhand1 (errhand2) ovlmgr,test,,cl

    Make any necessary changes for your brand of compiler.
*/

#include <stdio.h>
#include <dos.h>
#include <stdlib.h>
#include <string.h>

void test2(void);
void far error_handler(int);

/*
    Initialize error handler vector to desired value, so errors will transfer
    to error handler even at program startup before any variable assignments.
    The prepended underscore to _ovlmgr_error_vector is automatically handled
    by the C compiler.

    If your compiler doesn't like this prototype declaration, rewrite the
    prototype appropriately.  It should be a pointer to a far function
    that gets passed an int on the stack and which returns no value (void).
*/
void far (*ovlmgr_error_vector)(int)=error_handler;

/*
    Prototype of overlay manager routine to close the overlay file.
    Prepended underscore automatically handled by the compiler.
*/
int far ovlmgr_close_ovl_file(void);

/*
    Function prototype of overlay manager routine to open the overlay file
    Prepended underscore automatically handled by the compiler.
*/
int far ovlmgr_open_ovl_file(void);

/*
    This is the file name the overlay manager uses when attempting to open
    the overlay file, INCLUDING full drive and path specification, if
    necessary (i.e. overlay file is not in current directory).

    Prepended underscore automatically handled by the compiler.
*/
extern char far ovlmgr_overlay_filename;

void main(void)
{
    printf("\nHello from main!");

/*
    Now play a dirty trick on the overlay manager by closing the overlay file
    and renaming it.
*/
    /* assume close is successful, don't check return code */
    ovlmgr_close_ovl_file();

    unlink("test.ovr"); /* kill any pre-existing file with same name as new */
    rename("test.ovl","test.ovr");

/*
    The overlay manager will not be able to find the overlay file for a call
    to test2() and will transfer control to the error handler.

    If you have previously called an overlay and stashed it to EMS or XMS via
    the /ohp, /ohp3, or /oht options, then the second and subsequent calls to
    the overlay will not cause an error since the (now renamed) overlay file
    is not accessed to load the overlay.  The same is true of previously
    called overlays that have not yet been unloaded by overlay manager to make
    way for more recently called overlays.
*/
    test2();

    printf("\nHello from main again!");
}

/*
    The parameter errcode in the function error_handler() is the DOS error
    code unless the high bit (bit 15) is set, in which case the error is
    an overlay manager internal error.  DOS errors unrelated to the
    overlay file not being found are unrecoverable, do NOT return from the
    error handler in an unrecoverable situation.  All internal overlay
    manager errors are unrecoverable.

    The error handler should ALWAYS be in the root, NOT overlaid.
*/
void far error_handler(int errcode)
{
    int ret;

    printf("\nError handler reports that error code %d occurred",errcode);

/*
    The error code should be 6 (invalid handle) unless a real error unrelated
    to closing and renaming the overlay file occurred.
*/
    /* Check if invalid handle error */
    if(errcode==6){
        /* copy new filename into overlay manager internal filename location */
        /* include drive and pathspec, if not in current directory */
        /* this strcpy() must be large model, handling segment and offset */
        strcpy(&ovlmgr_overlay_filename,"test.ovr");

        /* try opening the overlay file */
        ret=ovlmgr_open_ovl_file();
        if(!ret){   /* no errors opening file, return and retry */
            return;
        }
        else{   /* error opening overlay file */
            printf("\nOut of luck, can't open new overlay file.");
        }
    }

    /*
        NOTE: exit() will fail (crash the application) if the error
        occurred at startup before main() is called.  This is because the
        startup code hasn't made the proper initialization for the exit()
        function.  To exit from a high level language such as C prior to
        startup code being executed, you must supply your own exit routine.
    */
    exit(errcode);  /* crash if main() hasn't begun executing yet */
}
