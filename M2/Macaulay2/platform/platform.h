#ifndef _PLATFORM_H_
#define _PLATFORM_H_
/**
   The idea of this file is that we build platform files for the various platforms.
   We choose not to do the configure ifdef route because it produces too much code base fragmentation.
   Using configure effectively produces n^2 code bases where n is number of configure options.
   Furthermore, in most cases, deviations from say, the POSIX standard, are small.
   So we can choose to just say the system is POSIX and then special case the few exceptions.
   By comparison differences between posix and win32/64 are large and so should not even be in the same file.
   Style dictates that if there are to be ifdefs, a whole new function is created.
   No inline ifdefs are permitted.  
**/

#define PACKAGE_VERSION "1.4.0.1"
/* complete machine description (to appear in name of tar file) */
#define MACHINE "x86_64-Linux-Ubuntu-11.10"

static const char* newline = "\n";

#include "types.h"
typedef char M2_bool;
typedef struct M2_string_struct * M2_string;
typedef struct M2_arrayint_struct * M2_arrayint;
typedef M2_arrayint M2_arrayintOrNull;
typedef struct M2_stringCell_struct * M2_stringCell;
typedef struct M2_ArrayString_struct * M2_ArrayString;
typedef M2_ArrayString M2_ArrayStringOrNull;
typedef char * M2_charstar;
typedef unsigned char * M2_ucharstar;
typedef char ** M2_charstarstar;
typedef const char * M2_constcharstar;
typedef const unsigned char * M2_constucharstar;
typedef const char ** M2_constcharstarstar;
typedef char * M2_charstarOrNull;
typedef const char * M2_constcharstarOrNull;
typedef const unsigned char * M2_constucharstarOrNull;
typedef const char ** M2_constcharstarstarOrNull;

#include "../system/supervisorinterface.h"
#if ARCH == x86_64
#include "posix.h"
#endif
#ifdef __cplusplus
extern "C" {
#endif
  /***
      Returns the program global environment array.
      @return The program global enrionment array or NULL if not applicable on the platform.
  ***/
  extern char** getEnviron();
  /***
      @param cname Path to directory.
      @return 1 if path is a directory, 0 otherwise or if the path does not exist.
  ***/
  extern int platformIsDirectory(const char *cname);
  /***
      Return the number of seconds elapsed of cputime.
      @return number of seconds or 0 on error.
  ***/
  extern double system_cpuTime(void);
  /***
      Initialize the system cpuTime.
      This should always be called on program start.
      All systems should implement this.
  ***/
  extern void system_cpuTime_init(void);
  /***
      Execute the given program in the current thread.
      All systems should implement this.
      @param argv Argument string with program as first argument.
      @return 0 on success, other on failure.
  ***/
  extern int system_exec(M2_ArrayString argv);
  /***
      Return if the file exists.
      All systems should implement this.
      @param name path of the file.
      @return True if the file exists, false otherwise.
  ***/
  extern M2_bool system_fileExists(M2_string name);
  /***
      Make directory.
      All systems should implement this.
      @param name Path of the directory.
      @return 0 on success, nonzero for failure.
  ***/
  extern int system_mkdir(M2_string name);
  /***
      Remove the directory.
      All systems should implement this.
      @param name Path of the directory
      @return 0 on success, nonzero for failure.
  ***/
  extern int system_rmdir(M2_string name);
  /***
      Returns if the path is a directory.
      All systems should implement this.
      @param name Path of the directory.
      @return True if it is a directory, false otherwise or if path does not exist.
  ***/
  extern int system_isDirectory(M2_string name);
  /***
      Change the directory of the current process.
      Note that this may be process or thread specific depending on the platform.
      On POSIX it is process specific and thus not thread safe.
      @param name Path of the new working directory.
      @return 0 on success, nonzero for failure.
  ***/
  int system_chdir(M2_string filename);
  /***
      Return the contents of the directory as an array of strings.
      All systems should implement this.
      @param name Path of directory whose contents should be listed.
      @return An array of strings, NULL on error.
  ***/
  extern M2_ArrayString system_readDirectory(M2_string name);
  /***
      Returns the length of the file.
      All systems should implement this.
      @param fd File descriptor of file.
      @return Length of the file or ERROR(-1).
  ***/
  extern int system_fileLength(int fd);
  /***
      Returns the length of the file.
      All systems should implement this.
      @param filename Path to file.
      @return Length of the file or ERROR(-1).
  ***/
  extern int system_fileLength_1(M2_string filename);
  /***
      Not very sure what this does.
      This is implemented once for all systems.
      @return NULL on error, otherwise directory path ending in '/'.
  ***/
  extern M2_string system_realpath(M2_string filename);
#ifdef __cplusplus
}
#endif
#endif
