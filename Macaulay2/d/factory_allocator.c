#include <gc.h>
#include <string.h>

/* this is code from factory's file memutil.c, slightly sped up */

/* amazingly, GC_MALLOC is faster than malloc even for the few times getBlock needs to get brand new memory */
#  define malloc GC_MALLOC_UNCOLLECTABLE
#  define free GC_FREE
#  define realloc GC_REALLOC

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
  /* improvement here saves calls to getBlock and freeBlock 10% of the time by noticing we already have a block big enough ! */  
  char* dum = (char*)block - 4;
  int size = *((int*)dum);
  if (newsize <= size) return block;
  void* dummy = getBlock( newsize );
  memcpy( dummy, block, newsize < oldsize ? newsize : oldsize );
  freeBlock( block, oldsize );
  return dummy;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
