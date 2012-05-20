#ifndef _POSIX_H_
#define _POSIX_H_
#define HAVE_INTTYPES_H 1
#define OS "Linux"
#include <setjmp.h>
#include <pthread.h>
#include <sys/time.h>
#ifdef __cplusplus
extern "C" {
#endif
/***
   Set this jump and the flag below if the handler should always jump; e.g., for interrupting a slow 3rd party or system library routine *
***/
extern sigjmp_buf interrupt_jump; 
extern int interrupt_jump_set;
extern int reading_from_readline;
#ifdef DONTCOMPILE
/***
    Internal M2 function for calling exec.
    Return code may be buggy.
    @param argv The arguments for exec as an M2 string.
    @return Always ERROR
 ***/
extern int system_exec(M2_ArrayString argv);

/***
    Wait on processes.
    This is for POSIX systems only.
    This should really be depracated.  Its not very compatible with threading.  
    @param pid M2 array of PIDs.
    @return Array of return codes from wait.
***/
extern M2_arrayint system_waitNoHang(M2_arrayint pids);
/***
    Unlink the given file.
    This is for POSIX systems only.
    @param name Path of the file to be unlinked.
    @return 0 on success, nonzero on error.
 ***/
extern int system_unlink(M2_string name);
/***
    Link the old file name to the new file name.
    This is for POSIX systems only.
    @param oldfilename File to be linked to.
    @param newfilename Path of link to be created.
    @return 0 on success, nonzero on error.
***/
extern int system_link(M2_string oldfilename,M2_string newfilename);
/***
    Symlink the old file name to the new file name.
    This is for POSIX systems only.
    @param oldfilename File to be linked to.
    @param newfilename Path of link to be created.
    @return 0 on success, nonzero on error.
 ***/
extern int system_symlink(M2_string oldfilename,M2_string newfilename);
/***
    This is for POSIX systems only.
 ***/
extern int system_fileMode(M2_string name);
/***
    This is for POSIX systems only.
 ***/
extern int system_fileModeFD(int fd);
/***
    chmod the given file to a new mode.
    This is for POSIX systems only.
    @param name Path to file.
    @param mode New mode for the file.  See POSIX specs for details. 
    @return 0 on success, nonzero on error.
***/
extern int system_chmod(M2_string name,int mode);
/***
    Return True if the file is a regular file.
    This is for POSIX systems only.
    @param name Path to file.
    @return True if the file is a regular file, false if it is not or the path does not exist.
***/
extern int system_isRegularFile(M2_string name);
#endif
#ifdef __cplusplus
}
#endif

/* stupid ANSI forces some systems to put underscores in front of useful identifiers */
#if !defined(S_ISREG)
#if defined(_S_ISREG)
#define S_ISREG _S_ISREG
#elif defined(S_IFREG)
#define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#elif defined(_S_IFREG)
#define S_ISREG(m)	(((m) & _S_IFMT) == _S_IFREG)
#endif
#endif


#endif
