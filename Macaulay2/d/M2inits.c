/* this file contains all initializations needed to get going, even if Macaulay2_main() is not called at all */

#include <assert.h>
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
#define TRUE 1
#define FALSE 0

extern void IM2_initialize();

char *progname;
void arginits(int argc, char **argv) { progname = argv[0]; }

static void init_gc(void) {
     GC_all_interior_pointers = TRUE;
     GC_free_space_divisor = 2;
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

static void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     return p;
     }

static void M2_mp_set_memory_functions(void) {
#    ifdef GC_DEBUG
     mp_set_memory_functions((void *(*) (size_t))GC_debug_malloc,GC_realloc3,(void (*) (void *, size_t))GC_debug_free);
#    else
     mp_set_memory_functions((void *(*) (size_t))GC_malloc      ,GC_realloc3,(void (*) (void *, size_t))GC_free      );
#    endif
     }

extern int initializeGMP();

static void *(*save_gmp_allocate_func  )(size_t);
static void *(*save_gmp_reallocate_func)(void *, size_t, size_t);
static void  (*save_gmp_free_func      )(void *, size_t);

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
# ifdef FACTORY
  factory_mp_set_memory_functions();
# endif
}

void M2_setup() { M2_mp_set_memory_functions(); }

int M2inits_run = 0;

void M2inits(void) {
  /* this routine gets called twice, once by M2inits1 and once by M2inits2 -- the other constructors are called in between */
  if (M2inits_run) return;
# ifdef DEBUG
  trap();
# endif
  init_gc();
# ifdef FACTORY
  factory_gmp_init(), factory_mp_get_memory_functions();
# endif
  M2_mp_set_memory_functions();
  IM2_initialize();
  M2inits_run = 1;
  M2inits1(), M2inits2();	/* just to ensure that M2inits1.o and M2inits2.o were actually linked in! */
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
