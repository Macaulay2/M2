#ifndef NEWDELETE_H
#define NEWDELETE_H 1

// get declarations of outofmem and getmem
#include "../d/M2mem.h"
#include <gc.h>
#include "../d/debug.h"

// this replaces all uses of the construction "new T[n]":
#define newarray(T,len) reinterpret_cast<T*>(getmem((len) * sizeof(T)))
// this replaces all uses of the construction "new T":
#define newitem(T) reinterpret_cast<T*>(getmem(sizeof(T)))
// this replaces all uses of the construction "delete [] x":
#ifdef DEBUG
#define deletearray(x) (trapchk(x), GC_FREE(x))
#else
#define deletearray(x) GC_FREE(x)
#endif
// this replaces all uses of the construction "delete x":
#ifdef DEBUG
#define deleteitem(x) (TRAPCHK(x), GC_FREE(x))
#else
#define deleteitem(x) GC_FREE(x)
#endif

// this replaces all uses of the construction "new T[n]", with T containing NO pointers
#define newarray_atomic(T,len) reinterpret_cast<T*>(getmem_atomic((len) * sizeof(T)))
// this replaces all uses of the construction "new T":
#define newitem_atomic(T) reinterpret_cast<T*>(getmem_atomic(sizeof(T)))

// This is used instead of newitem(T) when the size is not known to the c++ compiler
// Caution: this uses the pointer type, not the struct type.
#define GETMEM(T,size) reinterpret_cast<T>(getmem(size))
#define GETMEM_ATOMIC(T,size) reinterpret_cast<T>(getmem_atomic(size))

struct our_new_delete {
  static inline void* operator new    ( size_t size ) { void *p = GC_MALLOC( size ); if (p == NULL) outofmem(); TRAPCHK(p); return p; }
  static inline void* operator new [] ( size_t size ) { void *p = GC_MALLOC( size ); if (p == NULL) outofmem(); TRAPCHK(p); return p; }

  static inline void* operator new    ( size_t size, void *existing_memory ) { return existing_memory; }
  static inline void* operator new [] ( size_t size, void *existing_memory ) { return existing_memory; }

  static inline void operator delete    ( void* obj ) { TRAPCHK(obj); if (obj != NULL) GC_FREE( obj ); }
  static inline void operator delete [] ( void* obj ) { TRAPCHK(obj); if (obj != NULL) GC_FREE( obj ); }
};

#include <gc/gc_allocator.h>

// struct gc_malloc_alloc {
//   static void* allocate(size_t n) { void* p = GC_MALLOC(n); if (p == NULL) outofmem(); return p; }
//   static void deallocate(void* p, size_t /* n */) { GC_FREE(p); }
//   static void* reallocate(void* p, size_t /* old size */, size_t newsize) { void* r = GC_REALLOC(p, newsize); if (NULL == r) outofmem(); return r; }
// };

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
