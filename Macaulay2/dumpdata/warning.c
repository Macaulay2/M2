/* error printing routines that don't use the heap or static memory */

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

volatile void fatal(char *s,...) {
  va_list ap;
  va_start(ap,s);
  warning1(s,ap);
  va_end(ap);
  exit(1);
}
