// (c) 1995  Michael E. Stillman
#ifndef _mem_hh_
#define _mem_hh_

#include <cassert>
#include "newdelete.hpp"
// for spinLock:
#include "../system/mutex.h"
class buffer;

// 2*2^NDOUBLES = Largest stash size.
const int NDOUBLES = 25;
// const int slab_size = 2040;
const int slab_size = 2032;
// const int slab_size = 262134;
const char bad_pattern = '\245';
const int word_size =
    static_cast<int>(sizeof(void *));  // g++-4.8.0 complains without the cast.

extern size_t engine_allocated;
extern size_t engine_highwater;

// Each type should include something like the following:
#if 0
// // in .hpp file:
// friend void i_stashes();
// static stash *mystash;
// void *operator new(size_t size) { return mystash->new_elem(); }
// void operator delete(void *p) { mystash->delete_elem(p); }
// // in .cc file
// stash *matrix_rec::mystash;
// // in object.cc in i_stashes:
// matrix_rec::mystash = new stash("matrix", sizeof(matrix_rec));
#endif

class slab : public our_new_delete
{
  friend class stash;
  static int n_slabs;
  slab *next;
  char s[slab_size];

  slab() : next(NULL) { n_slabs++; }
  ~slab() { n_slabs--; }
};

class stash : public our_new_delete
{
 public:
  stash(const char *s, size_t len);
  ~stash();

  void *new_elem();
  void delete_elem(void *p);

  void text_out(buffer &o) const;  // Display statistics about this stash.
  static void stats(buffer &o);

 private:
  const char *name;
  size_t element_size;  // In bytes
  // n_per_slab provides the number of elements of element_size in each slab.
  // If 0, elements are new'ed directly.
  int n_per_slab;

  // List of slabs
  // Uses slab::next to indicate next element in list
  // This will be 0 if n_per_slab is 0.
  slab *slabs;

  // Free list for this stash.
  // Currently: if n_per_slab is 0, then elements are deleted'd directly.
  // Note that this essentially a list of the elements from various slabs.
  // a pointer to the next element in the list is in the first sizeof(void*)
  // bytes.
  void *free_list;

  // statistics
  size_t n_allocs;
  size_t n_inuse;
  size_t highwater;
  size_t n_frees;

  // private routines
  void chop_slab();

  // spinlock for modifying member lists
  spinLock list_spinlock;
};

inline void *stash::new_elem()
// Allocate space for an object from this stash.
{
  return newarray_clear(char, element_size);
  acquireSpinLock(&list_spinlock);
  n_allocs++;
  n_inuse++;
  if (n_inuse > highwater) highwater = n_inuse;
  if (free_list == NULL)
    {
      if (n_per_slab == 0)
        {
          void *result = newarray_clear(char, element_size);
          // allocated_amount += element_size;
          releaseSpinLock(&list_spinlock);
          return result;
        }
      chop_slab();
    }
  assert(free_list != NULL);  // chop_slab should not let this happen.
  void *result = free_list;
  free_list = *(reinterpret_cast<void **>(free_list));
  releaseSpinLock(&list_spinlock);
  return result;
}

inline void stash::delete_elem(void *p)
// Delete the object 'p', placing it on the free list for this stash.
{
  if (p == NULL) return;
  freemem(p);
  return;
  //  if (trace_bad_deletes)
  //    {
  //      for (void *q = free_list; q != NULL; q = *(reinterpret_cast<void
  //      **>(q)))
  //    if (q == p)
  //      assert(0);
  //    }

  n_inuse--;
  n_frees++;
  if (n_per_slab == 0)
    {
      //      deleted_amount += element_size;
      char *q = reinterpret_cast<char *>(p);
      freemem(q);
      return;
    }
  acquireSpinLock(&list_spinlock);
  memset(p, 0, element_size);  // we clear this element because it's free, and
                               // it may contain words that look like pointers
                               // to gc
  *(reinterpret_cast<void **>(p)) = free_list;
  free_list = p;
  releaseSpinLock(&list_spinlock);
}

/**
The doubling stash essentially is a list of stashes of different sizes.
The sizes start at 2 and run to 2*2^NDOUBLES
**/

class doubling_stash : public our_new_delete
{
  doubling_stash(const doubling_stash &) { assert(0); }
  void operator=(const doubling_stash &) { assert(0); }
 public:
  doubling_stash();
  ~doubling_stash();
  // Get a new element of given size.  Essentially this just dispatches to the
  // correct stash
  void *new_elem(size_t size);
  // Delete an element of a given size.  Essentially this just deletes an
  // element
  void delete_elem(void *p);
  // return the allocated size.
  size_t allocated_size(void *p);

 private:
  stash *doubles[NDOUBLES];
  size_t double_size[NDOUBLES];
};

extern doubling_stash *doubles;

static inline void engine_alloc(size_t n)
{
  engine_allocated += n;
  if (engine_allocated > engine_highwater) engine_highwater = engine_allocated;
}

static inline void engine_dealloc(size_t n) { engine_allocated -= n; }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
