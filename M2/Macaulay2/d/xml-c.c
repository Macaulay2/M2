#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <string.h>
#include "M2mem.h"

static char *copystring(const char *s) {
  char *p = (char *)getmem(strlen(s)+1);
  strcpy(p,s);
  return p;
}

void initxml() __attribute__ ((constructor));
void initxml() {
  xmlGcMemSetup(freemem,(void *(*)(size_t))getmem,(void *(*)(size_t))getmem_atomic,(void *(*)(void *,size_t))getmoremem1,copystring);
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d xml-c.o "
 End:
*/
