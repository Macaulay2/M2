#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static int had_error = 0;
static char *msg;

/* we test gc to see whether it properly marks pointers found in registers */
static void uniq(void *p, ...) {
  va_list a;
  void *q[100];
  int n = 0, i, j;
  q[n++] = p;
  va_start(a,p);
  for (;(q[n] = va_arg(a,void *));n++) ;
  va_end(a);
  for (i=0; i<n; i++) for (j=0; j<i; j++) if (q[i] == q[j]) fprintf(stderr,msg, __FILE__, __LINE__ ), had_error = 1;
}

static void *x[100];

int main() {
  int i=0;
  msg = "%s:%d: error: gc library doesn't find all the active pointers in registers or on the stack!\n"
        "           Perhaps GC_push_regs was configured incorrectly.\n";
  uniq(
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       (void *)0);
  msg = "%s:%d: error: gc library doesn't find all the active pointers in static memory!\n"
        "           Perhaps GC_add_roots needs to be told about static memory.\n";
  uniq(
       x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), (GC_gcollect(),x[i++] = GC_malloc(12)),
       x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), (GC_gcollect(),x[i++] = GC_malloc(12)),
       x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), (GC_gcollect(),x[i++] = GC_malloc(12)),
       x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), (GC_gcollect(),x[i++] = GC_malloc(12)),
       x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), x[i++] = GC_malloc(12), (GC_gcollect(),x[i++] = GC_malloc(12)),
       (void *)0);
  return had_error;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
