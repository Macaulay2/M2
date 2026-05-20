#ifndef NEWDELETE_H
#define NEWDELETE_H 1

/**
 * @file newdelete.hpp
 * @brief `our_new_delete` --- per-class opt-in routing of `new` / `delete` through bdwgc.
 *
 * Declares the `our_new_delete` base class, the
 * destructor-firing `our_gc_cleanup` subclass, the `gc_vector<T>`
 * alias for `std::vector<T, gc_allocator<T>>`, and the
 * allocation macro family --- `newitem` / `newitem_clear` /
 * `newitem_atomic` for single objects, `newarray` /
 * `newarray_clear` / `newarray_atomic` / `newarray_atomic_clear`
 * for sized blocks, the size-known-at-runtime `GETMEM(T*,
 * size)` / `GETMEM_ATOMIC(T*, size)`, the `alloca`-backed
 * `ARRAY_ON_STACK(type, nelems)`, and `VECTOR(T)` as a legacy
 * alias for `gc_vector<T>`. A class inheriting from
 * `our_new_delete` gets `operator new` / `new[]` routed through
 * the engine's `getmem` (a `GC_malloc` wrapper from
 * `interface/m2-mem.h`, with `outofmem2` on failure) and
 * `operator delete` / `delete[]` routed through `freemem`, so
 * every `new Foo(...)` of a participating class lands on the
 * GC heap without any per-allocation glue at the call site;
 * placement-`new` overloads exist for the standard
 * `new(buffer) T(...)` idiom. `our_gc_cleanup` adds a
 * `GC_REGISTER_FINALIZER_IGNORE_SELF` registration in its
 * constructor and a `cleanup` trampoline that runs the C++
 * destructor when the collector reclaims the object.
 *
 * `gc_vector<T>` is the right choice whenever `T` holds GC
 * pointers and the vector itself either lives in a
 * `our_new_delete`-derived object or stays on the stack. The
 * engine deliberately avoids globally overriding `operator new`
 * because mixed memory regimes are in play (FLINT, MPFR, GMP,
 * and `std::vector` buffers all bring their own allocators);
 * subclassing is the opt-in mechanism that lets each class
 * choose. Production engine code uses `our_new_delete` as the
 * standard base; the legacy `stash` / `doubling_stash`
 * size-class allocators in `mem.hpp` are now stubbed, with
 * `new_elem`/`delete_elem` short-circuiting to
 * `newarray_clear` / `freemem`.
 *
 * @see hash.hpp
 * @see mem.hpp
 */

#include "interface/m2-mem.h"  // for freemem, getmem, outofmem2
//#include "debug.h"  // for TRAPCHK, TRAPCHK_SIZE

#include "M2/gc-include.h"  // for GC_base, NULL, size_t, GC_REGISTER_FINALI...
// IWYU pragma: begin_exports
#include <gc/gc_allocator.h>
// IWYU pragma: end_exports
#include <vector>

// #ifdef MEMDEBUG
// #include <memdebug.h>
// #endif

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
template <typename T>
using gc_vector = typename std::vector<T, gc_allocator<T>>;
// TODO: eventually replace all uses of VECTOR(T) with gc_vector<T>
#define VECTOR(T) gc_vector<T>

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

/// ARRAY_ON_STACK
// This allocates POD objects onto the stack.  They are then removed automatically
// when exiting the enclosing function.  This does NOT zero the corresponding memory.
// Example uses:
//    int* exp = ARRAY_ON_STACK(int, n);
//    auto exp =  ARRAY_ON_STACK(int, n);
// In gcc or clang, we could instead use:
//    int[n] exp;
// But that doesn't appear to be standard c++...
#define ARRAY_ON_STACK(type, nelems) static_cast<type*>(alloca(nelems * sizeof(type)))

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
    (void) size;
    return existing_memory;
  }
  static inline void *operator new[](size_t size, void *existing_memory)
  {
    (void) size;
    return existing_memory;
  }

  static inline void operator delete(void *obj, void *existing_memory)
  {
    (void) existing_memory;
    TRAPCHK(obj);
  }
  static inline void operator delete[](void *obj, void *existing_memory)
  {
    (void) existing_memory;
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
  (void) displ;
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
