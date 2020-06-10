/* this file (or a replacement) gets linked into each program created using scc1 */
#include <scc-core.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#define ERROR (-1)
#define STDERR 2
#define TRUE 1
#define FALSE 0

static char errfmt[] = "%s:%d:%d: %s";
static char errfmtnc[] = "%s:%d: %s";

struct FUNCTION_CELL *pre_final_list, *final_list, *thread_prepare_list;

void outofmem(void) {
  const char msg[] = "\n\n *** out of memory, exiting ***\n";
  int r = write(STDERR,msg,sizeof(msg));
  if (r == ERROR) exit(1);
  exit(1);
}
void outofmem2(size_t new) {
  const char *msg = "\n\n *** out of memory trying to allocate %ld bytes, exiting ***\n";
  static char buf[sizeof(msg) + 100];
  sprintf(buf,msg,(long)new);
  int r = write(STDERR,buf,strlen(buf));
  if (r == ERROR) exit(1);
  exit(1);
}

void fatal(const char *s,...)   {
     va_list ap;
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     exit(1);
     }

void fatalarrayindex(int indx, int len, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"array index %d out of bounds 0 .. %d",indx,len-1);
     if (column == -1) fatal(errfmtnc,file,line,msg);
     else fatal(errfmt,file,line,column,msg);
     }

void fatalarraylen(int len, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"new array length %d less than zero",len);
     if (column == -1) fatal(errfmtnc,file,line,msg);
     else fatal(errfmt,file,line,column,msg);
     }

void invalidTypeTag(int typecode, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"internal error: unrecognized type code: %d\n",typecode);
     if (column == -1) fatal(errfmtnc,file,line,msg);
     else fatal(errfmt,file,line,column,msg);
     }

void invalidNullPointer(const char *file, int line, int column) {
     static char msg[] = "internal error: invalid null pointer\n";
     if (column == -1) fatal(errfmtnc,file,line,msg);
     else fatal(errfmt,file,line,column,msg);
     }

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/c scc-core.o "
 End:
*/




