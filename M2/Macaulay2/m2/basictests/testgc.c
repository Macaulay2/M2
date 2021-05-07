/* we test gc to whether it properly marks pointers found in registers */

#include "M2/config.h"
#include <M2/gc-include.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static void uniq(void *p, ...) {
  va_list a;
  void *q[100];
  int n = 0, i, j;
  q[n++] = p;
  va_start(a,p);
  for (;(q[n] = va_arg(a,void *));n++) ;
  va_end(a);
  for (i=0; i<n; i++) for (j=0; j<i; j++) if (q[i] == q[j]) {
    fprintf(stderr,
	    "error: gc library doesn't find all the active pointers!\n"
	    "       Perhaps GC_push_regs was configured incorrectly.\n"
	    );
    exit(1);
  }
}

int main () {
  GC_INIT();
  uniq(
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       (void *)0);
  return 0;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/basictests "
 End:
*/
