/*		Copyright 1994 by Daniel R. Grayson		*/

#include <dirent.h>

#include "expr-exports.h"
#include "types.h"
#include "M2mem.h"
#include "../c/compat.c"
#include "debug.h"

#include "../system/supervisorinterface.h"

int reading_from_readline = FALSE;

/***
   Display the string and arguments to stderr and exit with status 1.
   @param s printf style formatting string.
***/
void fatal(const char *s,...)   {
     va_list ap;
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
	 assert(0);
     exit(1);
     }

/***
    Fatal error on array index out of bounds.
    @param indx Index of array (0...length-1)
    @param len length of array.
    @param file The file that the error was in.
    @param line The line of the file.
    @param column The column of the file that the error was in, or -1 for not present.
 ***/
void fatalarrayindex(int indx, int len, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"array index %d out of bounds 0 .. %d",indx,len-1);
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     /* eventually when there is an interpreter we will have break loop here */
     }

/***
    Fatal error on array length less than zero.
    @param len length of array.
    @param file The file that the error was in.
    @param line The line of the file.
    @param column The column of the file that the error was in, or -1 for not present.
 ***/
void fatalarraylen(int len, const char *file, int line, int column)
{
     char msg[100];
     sprintf(msg,"new array length %d less than zero",len);
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     }
/***
    Fatal error on invalid interperter type
    @param typecode The invalid typecode.
    @param file The file that the error was in.
    @param line The line of the file.
    @param column The column of the file that the error was in, or -1 for not present.
***/
void invalidTypeTag(int typecode, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"internal error: unrecognized type code: %d\n",typecode);
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     }
/***
    Fatal null pointer dereference exception.
    @param file The file that the error was in.
    @param line The line of the file.
    @param column The column of the file that the error was in, or -1 for not present.
***/
void invalidNullPointer(const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"internal error: invalid null pointer\n");
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     }
/***
    Return the directory part of the pathname.
    @param s The filename to parse
    @return The directory part of the file name, not empty.
 ***/
M2_string interp_dirname(M2_string s) {
  char *t = M2_tocharstar(s);
  char *u = t;
  char *v = u;
  for (; *u; u++)
    if (*u == '/') 
      v=u+1;	/* on MacOS?? */
  if (v != NULL)
    *v = '\0';
  if (*t == '\0')
    return M2_tostring("./"); /* on MacOS?? */
  return M2_tostring(t);
}
/***
    Normal C strcmp but on M2 strings.
    @return 1 if s > t, -1 if s < t, 0 if s == t.
 ***/
int system_strcmp(M2_string s, M2_string t) {
  int slen = s->len, tlen = t->len, i;
  int ret = 0;
  int len = slen < tlen ? slen : tlen;
  char *sarray = s->array;
  char *tarray = t->array;
  for (i=0; i<len; i++) {
    unsigned char c = sarray[i];
    unsigned char d = tarray[i];
    if (isalnum(c)) {
      if (isalnum(d)) {
	if (toupper(c) < toupper(d)) return -1;
	if (toupper(c) > toupper(d)) return 1;
	if (ret == 0) {
	  if (c < d) ret = -1;
	  if (c > d) ret = 1;
	}
      }
      else return 1;
    }
    else {
      if (isalnum(d)) return -1;
      else {
	if (c < d) return -1;
	if (c > d) return 1;
      }
    }
  }
  if (slen > tlen) return 1;
  if (slen < tlen) return -1;
  return ret;
}
/***
    String comparison assuming the strings are natural numbers.
    @return 1 if s > t, -1 if s < t, 0 if s == t.
***/
int system_strnumcmp(M2_string s,M2_string t) {
     int slen = s->len, tlen = t->len, i;
     int ret = 0;
     int len = slen < tlen ? slen : tlen;
     int innumber = FALSE;
     char *sarray = s->array;
     char *tarray = t->array;
     for (i=0; i<len; i++) {
	  unsigned char c = sarray[i];
	  unsigned char d = tarray[i];

	  if (isdigit(c) && isdigit(d)) {
		 if (!innumber) {
		      int sn, tn;
		      sn=i+1; while(sn<slen && isdigit((int)sarray[sn])) sn++;
		      tn=i+1; while(tn<tlen && isdigit((int)tarray[tn])) tn++;
		      if (sn > tn) return  1;
		      if (sn < tn) return -1;
		      innumber = TRUE;
		 }
	  }
	  else innumber = FALSE;

	  if (isalnum(c)) {
	       if (isalnum(d)) {
		    unsigned char C = toupper(c), D = toupper(d);
		    innumber = FALSE;
		    if (C < D) return -1;
		    if (C > D) return 1;
		    if (ret == 0) {
			 if (c < d) ret = -1;
			 if (c > d) ret = 1;
		    }
	       }
	       else return 1;
	  }
	  else {
	       if (isalnum(d)) return -1;
	       else {
		    innumber = FALSE;
		    if (c < d) return -1;
		    if (c > d) return 1;
	       }
	  }
     }
     if (slen > tlen) return 1;
     if (slen < tlen) return -1;
     return ret;
}

int system_hash(double x){
     unsigned int h = 0;
     unsigned char *p = (unsigned char *)&x;
     unsigned int i;
     for (i=0; i<sizeof(x); i++) {
	  h = 231*h + p[i];
	  }
     return h;
     }

M2_string system_errfmt(M2_string filename, int lineno, int colno, int loaddepth) {
	char *s = getmem_atomic(filename->len+strlen(posfmt)+10);
	char *fn = M2_tocharstar(filename);
	M2_string ret;
	sprintf(s,posfmt,fn,lineno,colno,loaddepth);
	ret = M2_tostring(s);
	GC_FREE(s);
	GC_FREE(fn);
	return ret;
}



extern int errno;

int system_errno(void) {
  return 0;
#if HAVE_HERROR
    return h_errno > 0 ? h_errno : 
#endif
    return errno;
}

char const *system_strerror(void) {
     if (errno > 0) {
	  char const *msg = strerror(errno);
	  errno = 0;
	  return msg;
     }
     if (test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field))) return "interrupted";
     return "no system error indication found";
}

M2_string system_syserrmsg()
{
     return M2_tostring(system_strerror());
}

int system_run(M2_string command){
     char *c = M2_tocharstar(command);
     int r = system(c);
     GC_FREE(c);
     return r;
     }

struct FUNCTION_CELL *pre_final_list, *final_list, *thread_prepare_list;

void system_atend(void (*f)()){
     struct FUNCTION_CELL *this_final = (struct FUNCTION_CELL *)getmem(sizeof(struct FUNCTION_CELL));
     this_final -> fun = f;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

int system_strncmp(M2_string s,M2_string t,int n) {
  return strncmp(s->array,t->array,n);
}

int gotArg(const char *arg, const char **argv) {
  /* used in M2lib.c, but we put them here to prevent it from being optimized away: */
  for (; *argv; argv++) if (0 == strcmp(arg,*argv)) return TRUE;
  return FALSE;
}

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
