#include <stdlib.h>
#include <unistd.h>

#include "config.h"
#include <gc/gc.h>

#include "debug.h"
#include "M2mem.h"
#include <string.h>
#include "factory_allocator.h"

/* this is code from factory's file memutil.c, slightly sped up */

/* we must use GC_MALLOC_UNCOLLECTABLE instead of GC_MALLOC, because
   factory stores pointers to memory allocated here in memory areas
   allocated by "operator new[]", for example, in the function initPT().
*/

#  define malloc GC_MALLOC_UNCOLLECTABLE
#  define free GC_FREE
#  define realloc GC_REALLOC

typedef struct dummy_le { 
     struct dummy_le* next; 
} listentry;

static listentry * blocklist[7] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };

#ifdef DEBUG
#define RECORD_COUNT(p) (*((int*)(p)+2) = trapcount)
#define HEADER_WORDS 3
#define GOOD_MAGIC 0xbeef
#define DEAD_MAGIC 0xdead
#define MARK_GOOD(p) (*((int*)(p)+1) = GOOD_MAGIC, trapchk(p), RECORD_COUNT(p))
#define MARK_DEAD(p) (*((int*)(p)+1) = DEAD_MAGIC, trapchk(p))
#define CHECK_GOOD(p) (*((int*)(p)+1) != GOOD_MAGIC ? badBlock() : 0, trapchk(p))
#define CHECK_DEAD(p) (*((int*)(p)+1) != DEAD_MAGIC ? badBlock() : 0, trapchk(p))
#else
#define HEADER_WORDS 1
#define MARK_GOOD(p)
#define MARK_DEAD(p)
#define CHECK_GOOD(p)
#define CHECK_DEAD(p)
#endif

#define HEADER_BYTES (HEADER_WORDS * sizeof(int))


#   define GETBLOCK( list, size ) { if ( blocklist[list] ) { \
					  listentry* retval = blocklist[list]; \
					  blocklist[list] = retval->next; \
					  CHECK_DEAD((char *)retval-HEADER_BYTES); \
					  MARK_GOOD((char *)retval-HEADER_BYTES); \
					  return (void*)retval; \
				      } else { \
					  char* dum = (char*)malloc( size+HEADER_BYTES ); \
                                          if (dum == NULL) outofmem(); \
					  *((int*)dum) = size; \
					  MARK_GOOD(dum); \
					  return (void*)(dum+HEADER_BYTES); \
				      } }

#   define FREEBLOCK( list, block ) { \
					 listentry* dummy = (listentry*)(block); \
					 dummy->next = blocklist[list]; \
					 MARK_DEAD((char *)block-HEADER_BYTES); \
					 blocklist[list] = dummy; \
				     }

#define SIZE0 (sizeof(void *))
#define SIZE1 12
#define SIZE2 20
#define SIZE3 60
#define SIZE4 124
#define SIZE5 252
#define SIZE6 508

void* getBlock ( size_t size )
{
    if      ( size <= SIZE0 ) GETBLOCK( 0, SIZE0 )
    else if ( size <= SIZE1 ) GETBLOCK( 1, SIZE1 )
    else if ( size <= SIZE2 ) GETBLOCK( 2, SIZE2 )
    else if ( size <= SIZE3 ) GETBLOCK( 3, SIZE3 )
    else if ( size <= SIZE4 ) GETBLOCK( 4, SIZE4 )
    else if ( size <= SIZE5 ) GETBLOCK( 5, SIZE5 )
    else if ( size <= SIZE6 ) GETBLOCK( 6, SIZE6 )
    else {
	char* dum = (char*)malloc( size+HEADER_BYTES );
	if (dum == NULL) outofmem();
	*((int*)dum) = size;
	MARK_GOOD(dum);
	return dum + HEADER_BYTES;
    }
}

void* getBlockClear ( size_t numb, size_t size ) {
     size_t n = numb*size;
     void *p = getBlock(n);
     memset(p,0,n);
     return p;
}

void freeBlockN ( void* block )	/* N means no size given */
{
    char* dum;
    unsigned size;
    if ( block == NULL ) return;
    dum = (char*)block - HEADER_BYTES;
    size = *((int*)dum);
    CHECK_GOOD(dum);
    MARK_DEAD(dum);
    switch(size) {
       case SIZE0 : FREEBLOCK( 0, block ); break;
       case SIZE1 : FREEBLOCK( 1, block ); break;
       case SIZE2 : FREEBLOCK( 2, block ); break;
       case SIZE3 : FREEBLOCK( 3, block ); break;
       case SIZE4 : FREEBLOCK( 4, block ); break;
       case SIZE5 : FREEBLOCK( 5, block ); break;
       case SIZE6 : FREEBLOCK( 6, block ); break;
       default    : free( dum ); break;
    }
}

void freeBlock ( void* block, size_t size ) { 
     freeBlockN(block) ; 
}

void* reallocBlockN ( void* block, size_t newsize )
{
  char* dum = (char*)block - HEADER_BYTES;
  int size = *((int*)dum);
  CHECK_GOOD(dum);
  if (newsize <= size) return block;
  void* retval = getBlock( newsize );
  memcpy( retval, block, newsize < size ? newsize : size );
  freeBlock( block, size );
  return retval;
}

void* reallocBlock ( void* block, size_t oldsize, size_t newsize )
{
  char* dum = (char*)block - HEADER_BYTES;
  int size = *((int*)dum);
  CHECK_GOOD(dum);
  if (newsize <= size) return block;
  void* retval = getBlock( newsize );
  memcpy( retval, block, newsize < oldsize ? newsize : oldsize );
  freeBlock( block, oldsize );
  return retval;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
