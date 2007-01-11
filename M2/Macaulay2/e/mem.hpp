// (c) 1995  Michael E. Stillman
#ifndef _mem_hh_
#define _mem_hh_

#include "style.hpp"
#include "newdelete.hpp"

const int NDOUBLES = 25;
//const int slab_size = 2040;
const int slab_size = 2032;
//const int slab_size = 262134;
const char bad_pattern = '\245';
const int word_size = sizeof(void *);

extern unsigned int engine_allocated;
extern unsigned int engine_highwater;

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
  stash(char *s, size_t len);
  ~stash();

  void *new_elem();
  void delete_elem(void *p);

  void text_out(buffer &o) const; // Display statistics about this stash.
  static void stats(buffer &o);
private:
  stash *next;

  const char *name;
  size_t element_size;		// In bytes
  int n_per_slab;		// If 0, elements are new'ed directly.

  slab *slabs;			// This will be 0 if n_per_slab is 0.

  void *free_list;		// Free list for this stash.
				// Currently: if n_per_slab is 0, then
				// elements are 'delete'd directly.

  // statistics
  int n_allocs;
  int n_inuse;
  int highwater;
  int n_frees;

  static stash *stash_list;
  static slab *slab_freelist;
  static int num_slab_freelist();
  static long n_new_slabs;

  // private routines
  void chop_slab();
};

inline void *stash::new_elem()
     // Allocate space for an object from this stash.
{
  n_allocs++;
  n_inuse++;
  if (n_inuse > highwater) highwater = n_inuse;
  if (free_list == NULL) 
    {
      if (n_per_slab == 0) 
	{
	  void *result = newarray(char,element_size);
	  //allocated_amount += element_size;
	  return result;
	}
      chop_slab();
    }
  assert(free_list != NULL);	// chop_slab should not let this happen.
  void *result = free_list;
  free_list = *(reinterpret_cast<void **>(free_list));
  return result;
}

inline void stash::delete_elem(void *p)
     // Delete the object 'p', placing it on the free list for this stash.
{
  if (p == NULL) return;
  //  if (trace_bad_deletes)
  //    {
  //      for (void *q = free_list; q != NULL; q = *(reinterpret_cast<void **>(q)))
  //	if (q == p)
  //	  assert(0);
  //    }

  n_inuse--;
  n_frees++;
  if (n_per_slab == 0)
    {
      //      deleted_amount += element_size;
      char *q = reinterpret_cast<char *>(p);
      deletearray(q);
      return;
    }
  bzero(p,element_size);	// we clear this element because it's free, and it may contain words that look like pointers to gc
  *(reinterpret_cast<void **>(p)) = free_list;
  free_list = p;
}

class doubling_stash : public our_new_delete
{
  doubling_stash(const doubling_stash&){assert(0);}
  void operator=(const doubling_stash&){assert(0);}
public:
  doubling_stash();
  ~doubling_stash();

  void *new_elem(size_t size);
  void delete_elem(void *p);
  size_t allocated_size(void *p);
private:
  stash *doubles[NDOUBLES];
  size_t double_size[NDOUBLES];
};

extern doubling_stash *doubles;

inline void engine_alloc(unsigned int n)
{
  engine_allocated += n;
  if (engine_allocated > engine_highwater)
    engine_highwater = engine_allocated;
}

inline void engine_dealloc(unsigned int n)
{
  engine_allocated -= n;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
