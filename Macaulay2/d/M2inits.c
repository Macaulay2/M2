/* this file contains all initializations needed to get going, even if Macaulay2_main() is not called at all */

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <gc.h>
#include "config.h"

extern void outofmem();
extern void IM2_initialize();

char *progname;
void arginits(int argc, char **argv) {
  progname = argv[0];
}

static void init_gc(void) {
#ifdef MEM_DEBUG
     GC_all_interior_pointers = TRUE; /* set this before using gc routines!  (see gc.h) */
#endif
     GC_free_space_divisor = 2;	/* this was intended to be used only when we are about to dump data */
     GC_init();
     if (getenv("GC_free_space_divisor")) {
	  GC_free_space_divisor = atoi(getenv("GC_free_space_divisor"));
	  if (GC_free_space_divisor <= 0) {
	       fprintf(stderr, "%s: non-positive GC_free_space_divisor value, %ld\n", 
		    progname, GC_free_space_divisor);
	       exit (1);
	       }
	  }
     if (getenv("GC_enable_incremental") && atoi(getenv("GC_enable_incremental"))==1) {
	  GC_enable_incremental();
	  fprintf(stderr,"GC_enable_incremental()\n");
	  }
     if (getenv("GC_expand_hp")) {
	  GC_expand_hp(atoi(getenv("GC_expand_hp")));
	  }
#ifdef NDEBUG
     GC_set_warn_proc(dummy_GC_warn_proc);
#endif
     }

/* we test gc to whether it properly marks pointers found in registers */
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
	    "%s: error: gc library doesn't find all the active pointers!\n"
	    "           Perhaps GC_push_regs was configured incorrectly.\n",
	    progname
	    );
    exit(1);
  }
}
static void test_gc (void) {
  uniq(
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       (void *)0);
}

static void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_MALLOC_UNCOLLECTABLE(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }

static void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     return p;
     }

static void GC_free2 (void *s, size_t old) {
     GC_FREE(s);
     }

static void init_gmp(void) {
     mp_set_memory_functions(GC_malloc1,GC_realloc3,GC_free2);
     }

/* these next three functions are simply aliases for libcf in case it was configured 
   with --with-memman-old.  The three functions are declared in libcfmem.a, but we don't
   want to use their memory manager.  We make our own call to mp_set_memory_functions() 
   in any case, see M2lib.c.
   As a side effect, linking with libcfmem.a will cause an error about a duplicate definition,
   which is good, since we don't want to link with libcfmem.a. */
void*     getBlock ( size_t size                                  ) { return GC_malloc1(size);                   }
void* reallocBlock ( void * block, size_t oldsize, size_t newsize ) { return GC_realloc3(block,oldsize,newsize); }
void     freeBlock ( void * block, size_t size                    ) { return GC_free2(block, size);              }

void M2inits(void) {
  init_gc();
  test_gc();
  init_gmp();
  IM2_initialize();
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
// End:
*/
