#include "factor.h"
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gmp.h>

#include "config.h"
#include <M2/gc-include.h>

extern void outofmem();
extern void IM2_initialize();
extern void dummy_GC_warn_proc(char *msg, GC_word arg);

char *progname;
void arginits(int argc, char **argv) {
  progname = argv[0];
}

#if USE_GC
static void init_gc(void) {
     GC_all_interior_pointers = INTERIOR_POINTERS || REPLACE_GETBLOCK && REPLACE_MALLOC_BY_GC_IN_GETBLOCK; /* getBlock uses interior pointers! */
     GC_free_space_divisor = 2;
     GC_init();
     // GC_enable_incremental();
     }
#endif

#if GMP_USES_GC
inline static void GC_free2 (void *s, size_t old) { GC_FREE(s); }
#if GMP_TESTS_RETURN_VALUE_FROM_GC
inline static void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_MALLOC(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }
inline static void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     return p;
     }
#else
inline static void *GC_malloc1 (size_t size_in_bytes) { return GC_MALLOC(size_in_bytes); }
inline static void *GC_realloc3(void *s, size_t old, size_t new) { return GC_REALLOC(s,new); }
#endif
#endif

static void init_gmp(void) {
#if GMP_USES_GC
#if GMP_TESTS_RETURN_VALUE_FROM_GC
  mp_set_memory_functions(GC_malloc1,GC_realloc3,GC_free2);
#else
  mp_set_memory_functions(GC_malloc,GC_realloc3,GC_free);
#endif
#endif
}

#if FACTORY_USES_GC_DIRECTLY
#if FACTORY_TESTS_RETURN_VALUE_FROM_GC
void*     getBlock ( size_t size                                  ) { return GC_malloc1(size); }
void* reallocBlock ( void * block, size_t oldsize, size_t newsize ) { return GC_realloc3(block,oldsize,newsize); }
void     freeBlock ( void * block, size_t size                    ) { GC_FREE(block); }
#else
void*     getBlock ( size_t size                                  ) { return GC_MALLOC(size); }
void* reallocBlock ( void * block, size_t oldsize, size_t newsize ) { return GC_REALLOC(block,newsize); }
void     freeBlock ( void * block, size_t size                    ) { GC_FREE(block); }
#endif
#else
#if REPLACE_GETBLOCK
# if REPLACE_MALLOC_BY_GC_IN_GETBLOCK
#  define malloc GC_MALLOC_UNCOLLECTABLE
#  define free GC_FREE
#  define realloc GC_REALLOC
# endif
    typedef struct dummy_le { struct dummy_le* next; } listentry;
    static listentry * blocklist[7] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
#   define GETBLOCK( list, size ) { if ( blocklist[list] ) { \
					  listentry* retval = blocklist[list]; \
					  blocklist[list] = retval->next; \
					  return (void*)retval; \
				      } else { \
					  char* retval = (char*)malloc( size ); \
					  *((int*)retval) = (size)-4; \
					  return (void*)(retval+4); \
				      } }
#   define FREEBLOCK( list, block ) { \
					 listentry* dummy = (listentry*)(block); \
					 dummy->next = blocklist[list]; \
					 blocklist[list] = dummy; \
				     }

    void* getBlock ( size_t size )
    {
	if ( size <= 4 ) GETBLOCK( 0, 8 )
	else if ( size <= 12 ) GETBLOCK( 1, 16 )
	else if ( size <= 28 ) GETBLOCK( 2, 32 )
	else if ( size <= 60 ) GETBLOCK( 3, 64 )
	else if ( size <= 124 ) GETBLOCK( 4, 128 )
	else if ( size <= 252 ) GETBLOCK( 5, 256 )
	else if ( size <= 508 ) GETBLOCK( 6, 512 )
	else {
	    char* retval = (char*)malloc( size+4 );
	    *((int*)retval) = size;
	    retval += 4;
	    return retval;
	}
    }

    void freeBlock ( void* block, size_t size )
    {
	char* dum = (char*)block;
	if ( block == NULL ) return;
	dum -= 4;
	size = *((int*)dum);
	if ( size == 4 ) FREEBLOCK( 0, block )
	else if ( size == 12 ) FREEBLOCK( 1, block )
	else if ( size == 28 ) FREEBLOCK( 2, block )
	else if ( size == 60 ) FREEBLOCK( 3, block )
	else if ( size == 124 ) FREEBLOCK( 4, block )
	else if ( size == 252 ) FREEBLOCK( 5, block )
	else if ( size == 508 ) FREEBLOCK( 6, block )
	else free( dum );
    }

    void* reallocBlock ( void* block, size_t oldsize, size_t newsize )
    {
      // improvement here saves calls to getBlock and freeBlock 10% of the time:
      char* dum = (char*)block - 4;
      int size = *((int*)dum);
      if (newsize <= size) return block;
      void* dummy = getBlock( newsize );
      memcpy( dummy, block, newsize < oldsize ? newsize : oldsize );
      freeBlock( block, oldsize );
      return dummy;
    }

#undef malloc
#undef free
#undef realloc

#endif
#endif

void M2inits(void) {
  static int M2inits_run;
  extern void M2inits1(void), M2inits2(void);
  if (M2inits_run) return;
#if USE_GC
  init_gc();
#endif
  init_gmp();
  // IM2_initialize();
  M2inits_run = 1;
  M2inits1(), M2inits2();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/engine benchmark"
// End:
