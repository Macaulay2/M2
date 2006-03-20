/* error printing routines that don't use the heap or static memory */

#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include "warning.h"
#include "std.h"

static void warning1(char *s, va_list ap) {
  char buf[1000];
  vsprintf(buf,s,ap);
  write(STDERR,buf,strlen(buf));
}

void warning(char *s,...) {
  va_list ap;
  va_start(ap,s);
  warning1(s,ap);
  va_end(ap);
}


/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
