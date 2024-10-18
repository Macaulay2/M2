#ifndef M2mem_included
#define M2mem_included

#include <stdlib.h>
#include "scc-core.h"

#ifdef NDEBUG
#define NVALGRIND 1
#endif

#include <valgrind/memcheck.h>

#if defined(__cplusplus)
extern "C" {
#endif

extern void outofmem2(size_t);
extern char *getmem(size_t);
extern void freemem(void *);
extern void freememlen(void *, size_t);
extern char *getmem_clear(size_t);
extern char *getmem_atomic(size_t);
extern char *getmem_malloc(size_t);
extern char *getmem_atomic_clear(size_t);
extern char *getmoremem(char *, size_t oldsize, size_t newsize);
extern char *getmoremem1(char *, size_t newsize);
extern char *getmoremem_atomic(char *, size_t oldsize, size_t newsize);

/* Valgrind helper functions,
 * NVALGRIND can be defined to explicitly disable this
 * But also valgrind.h (included by memcheck.h) will define NVALGRIND
 * if we are on a platform it doesn't understand
 */
#ifndef NVALGRIND
void *I_WRAP_SONAME_FNNAME_ZU(libgcZdsoZd1,GC_malloc)(size_t);
void *I_WRAP_SONAME_FNNAME_ZU(libgcZdsoZd1,GC_malloc_atomic)(size_t);
void *I_WRAP_SONAME_FNNAME_ZU(libgcZdsoZd1,GC_malloc_ignore_off_page)(size_t);
void *I_WRAP_SONAME_FNNAME_ZU(libgcZdsoZd1,GC_malloc_atomic_ignore_off_page)(size_t);
void *I_WRAP_SONAME_FNNAME_ZU(libgcZdsoZd1,GC_realloc)(void*, size_t);
#endif /* NVALGRIND */



#define sizeofarray(s,len) (sizeof(*(s)) + (len)*sizeof((s)->array[0]))
#define sizeofarraytype(S,len) sizeofarray((S)0,len)
#define sizeofstruct(s) sizeof(*(s))
#define sizeofstructtype(S) sizeofstruct((S)0)

#if defined(__cplusplus)
#define getmemarraytype(S,len) reinterpret_cast<S>(getmem(sizeofarraytype(S,len)))
#define getmemstructtype(S) reinterpret_cast<S>(getmem(sizeofstructtype(S)))
#define getmematomicarraytype(S,len) reinterpret_cast<S>(getmem_atomic(sizeofarraytype(S,len)))
#define getmematomicstructtype(S) reinterpret_cast<S>(getmem_atomic(sizeofstructtype(S)))
#define getmemvectortype(S,len) reinterpret_cast<S*>(getmem(sizeof(S)*len))
#define getmematomicvectortype(S,len) reinterpret_cast<S*>(getmem_atomic(sizeof(S)*(len)))
#else
#define getmemarraytype(S,len) (S)(getmem(sizeofarraytype(S,len)))
#define getmemstructtype(S) (S)(getmem(sizeofstructtype(S)))
#define getmematomicarraytype(S,len) (S)(getmem_atomic(sizeofarraytype(S,len)))
#define getmematomicstructtype(S) (S)(getmem_atomic(sizeofstructtype(S)))
#define getmemvectortype(S,len) (S*)(getmem(sizeof(S)*len))
#define getmematomicvectortype(S,len) (S*)(getmem_atomic(sizeof(S)*(len)))
#endif

#if defined(__cplusplus)
}
#endif

#endif

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
