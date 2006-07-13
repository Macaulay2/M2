#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static int had_error = 0;

/* we test gc to see whether it properly marks pointers found in registers */
static void uniq(char *msg, void *p, ...) {
  va_list a;
  void *q[100];
  int n = 0, i, j;
  q[n++] = p;
  va_start(a,p);
  for (;(q[n] = va_arg(a,void *));n++) ;
  va_end(a);
  for (i=0; i<n; i++) for (j=0; j<i; j++) if (q[i] == q[j]) {
       fprintf(stderr, msg, __FILE__, __LINE__ );
       had_error = 1;
       return;
  }
}

static void *x[100];

int main() {
  int i;

#include "gc_fixes.h"

  uniq("%s:%d: error: gc library doesn't find all the active pointers in registers or on the stack!\n"
       "           Perhaps GC_push_regs was configured incorrectly.\n",
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       (void *)0);

  for (i=0; i<20; i++) {
       if (i%4 == 3) GC_gcollect();
       x[i] = GC_malloc(12);
  }
  uniq("%s:%d: error: gc library doesn't find all the active pointers in static memory!\n"
        "           Perhaps GC_add_roots needs to be told about static memory.\n",
       x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[14],x[15],x[16],x[17],x[18],x[19], (void *)0);

  return had_error;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
