#ifndef NEWDELETE_H
#define NEWDELETE_H 1

#define GC_REDIRECT_TO_LOCAL
// get declarations of outofmem and getmem
#include "../d/M2mem.h"
#include "../d/debug.h"

#include <M2/gc-include.h>
#include <gc/gc_allocator.h>
#include <vector>

#ifdef MEMDEBUG
#include <memdebug.h>
#include <M2mem.h>
#endif

/**
  @brief a version of the STL vector, which allocates its backing memory with gc.

  This should be used instead of std::vector<T> in the following instances:
  (1) T contains (or is a pointer to) memory allocated with GC, and
  (2a) The object containing the vector is in GC collected memory
      that is, it is in a struct or class inheriting from our_new_delete, or gc_cleanup,
  or
  (2b) The vector is in local memory, and has not been allocated on the heap.

  It is a (probably hard to detect) error if exactly (1) or (2a) holds.
  In that case, redesign your class!
 */
#define VECTOR(T) std::vector<T, gc_allocator<T>>

// these replace all uses of the construction "new T[n]" (unless constructors
// have to be run!):
#define newarray(T, len) reinterpret_cast<T *>(getmem((len) * sizeof(T)))
#define newarray_clear(T, len) \
  reinterpret_cast<T *>(getmem_clear((len) * sizeof(T)))
// this replaces all uses of the construction "new T":
#define newitem(T) reinterpret_cast<T *>(getmem(sizeof(T)))
#define newitem_clear(T) reinterpret_cast<T *>(getmem_clear(sizeof(T)))

// this replaces all uses of the construction "new T[n]", with T containing NO
// pointers
#define newarray_atomic(T, len) \
  reinterpret_cast<T *>(getmem_atomic((len) * sizeof(T)))
#define newarray_atomic_clear(T, len) \
  reinterpret_cast<T *>(getmem_atomic_clear((len) * sizeof(T)))
// this replaces all uses of the construction "new T":
#define newitem_atomic(T) reinterpret_cast<T *>(getmem_atomic(sizeof(T)))

// This is used instead of newitem(T) when the size is not known to the c++
// compiler
// Caution: this uses the pointer type, not the struct type.
#define GETMEM(T, size) reinterpret_cast<T>(getmem(size))
#define GETMEM_ATOMIC(T, size) reinterpret_cast<T>(getmem_atomic(size))

struct our_new_delete
{
  static inline void *operator new(size_t size)
  {
    TRAPCHK_SIZE(size);
    void *p = getmem(size);
    if (p == NULL) outofmem2(size);
    TRAPCHK(p);
    return p;
  }
  static inline void *operator new[](size_t size)
  {
    TRAPCHK_SIZE(size);
    void *p = getmem(size);
    if (p == NULL) outofmem2(size);
    TRAPCHK(p);
    return p;
  }

  static inline void operator delete(void *obj)
  {
    TRAPCHK(obj);
    if (obj != NULL) freemem(obj);
  }
  static inline void operator delete[](void *obj)
  {
    TRAPCHK(obj);
    if (obj != NULL) freemem(obj);
  }

  static inline void *operator new(size_t size, void *existing_memory)
  {
    return existing_memory;
  }
  static inline void *operator new[](size_t size, void *existing_memory)
  {
    return existing_memory;
  }

  static inline void operator delete(void *obj, void *existing_memory)
  {
    TRAPCHK(obj);
  }
  static inline void operator delete[](void *obj, void *existing_memory)
  {
    TRAPCHK(obj);
  }

  
#if !defined(__GNUC__) || defined(__INTEL_COMPILER)
// see Scott Meyers, Effective C++, item 14!  This avoids something really bad
// in the c++ standard.
// ... but it slows down destructors in every class inheriting from this one
// gnu cc does it right, running all the destructors, so we don't bother with
// this.
#if 0
      // But this will put a pointer to the table of virtual methods at the start of every instance,
      // and we don't want that, because we want to pass these pointers to C routines.
      // So we can't have any virtual methods here.
      virtual ~our_new_delete() {}
#endif
#endif
};


class our_gc_cleanup: public our_new_delete
{
public:
  our_gc_cleanup();
  virtual ~our_gc_cleanup()
  {
  if (0 == GC_base(this)) return; // Non-heap object.
#ifdef MEMDEBUG
    GC_REGISTER_FINALIZER_IGNORE_SELF(M2_debug_to_outer((void*)this), 0, 0, 0, 0);
#else
    GC_REGISTER_FINALIZER_IGNORE_SELF(this,                           0, 0, 0, 0);
#endif
  }
};

static inline void cleanup(void* obj, void* displ)
{
#ifdef MEMDEBUG
  obj = M2_debug_to_inner(obj);
#endif
  ((our_gc_cleanup*) obj) -> ~our_gc_cleanup();
}

inline our_gc_cleanup::our_gc_cleanup()
{
  if (0 == GC_base(this)) return; // Non-heap object.
#ifdef MEMDEBUG
  GC_REGISTER_FINALIZER_IGNORE_SELF(M2_debug_to_outer(this), (GC_finalization_proc) cleanup, 0, 0, 0);
#else
  GC_REGISTER_FINALIZER_IGNORE_SELF(this,                    (GC_finalization_proc) cleanup, 0, 0, 0);
#endif
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
