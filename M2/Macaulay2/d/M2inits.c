/* this file contains all initializations needed to get going, even if Macaulay2_main() is not called at all */

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <gc.h>
#include "config.h"
#include "M2types.h"
#include "M2inits.h"
#include "M2mem.h"
#include "debug.h"
#include "gmp_init.h"

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
     GC_INIT();
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


#if 0
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
#endif

#ifndef drg

#if 0
static void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_MALLOC(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }

static void GC_free2 (void *s, size_t old) {
     GC_FREE(s);
     }
#endif

static void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     return p;
     }

#else

static void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_MALLOC(size_in_bytes);
     if (p == NULL) outofmem();
     trapchk(p);
     return p;
     }

static void GC_free2 (void *s, size_t old) {
     trapchk(s);
     GC_FREE(s);
     }

static void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p;
     trapchk(s);
     p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     trapchk(p);
     return p;
     }

void drg_GC_malloc_free() {}

#endif

static void M2_mp_set_memory_functions(void) {
#ifdef GC_DEBUG
     mp_set_memory_functions((void *(*) (size_t))GC_debug_malloc,GC_realloc3,(void (*) (void *, size_t))GC_debug_free);
#else
     mp_set_memory_functions((void *(*) (size_t))GC_malloc      ,GC_realloc3,(void (*) (void *, size_t))GC_free      );
#endif
     }

extern int initializeGMP();

static void *	(*save_gmp_allocate_func)(size_t);
static void *	(*save_gmp_reallocate_func)(void *, size_t, size_t);
static void	(*save_gmp_free_func)(void *, size_t);

#ifdef FACTORY
static void factory_mp_get_memory_functions() {
  save_gmp_allocate_func = __gmp_allocate_func;
  save_gmp_reallocate_func = __gmp_reallocate_func;
  save_gmp_free_func = __gmp_free_func;
}
static void factory_mp_set_memory_functions() {
  __gmp_allocate_func = save_gmp_allocate_func;
  __gmp_reallocate_func = save_gmp_reallocate_func;
  __gmp_free_func = save_gmp_free_func;
}
#endif

void factory_setup() {
  factory_mp_set_memory_functions();
}

void M2_setup() {
  M2_mp_set_memory_functions();
}

int M2inits_run = 0;

void M2inits(void) {
  /* this routine gets called twice, once by M2inits1 and once by M2inits2 -- the other constructors are called in between */
  if (M2inits_run) return;
#ifdef DEBUG
  trap();
#endif
  init_gc();
#if 0
  test_gc();			/* this takes some time, so skip it */
#endif
#ifdef FACTORY
  factory_gmp_init(), factory_mp_get_memory_functions();
#endif
  M2_mp_set_memory_functions();
  IM2_initialize();
  M2inits_run = 1;
  M2inits1(), M2inits2();	/* just to ensure that M2inits1.o and M2inits2.o were actually linked in! */
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
