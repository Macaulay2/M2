// (c) 1995  Michael E. Stillman
#ifndef _mem_hh_
#define _mem_hh_

#include "style.hpp"

const int NDOUBLES = 25;
const int slab_size = 2040;
const char bad_pattern = '\245';
const int word_size = sizeof(void *);

extern unsigned int engine_allocated;
extern unsigned int engine_highwater;

// Each type should include something like the following:
#if 0
// in .hpp file:
friend void i_stashes();
static stash *mystash;
void *operator new(size_t size) { return mystash->new_elem(); }
void operator delete(void *p) { mystash->delete_elem(p); }
// in .cc file
stash *matrix_rec::mystash;
// in object.cc in i_stashes:
matrix_rec::mystash = new stash("matrix", sizeof(matrix_rec));
#endif

class slab
{
  friend class stash;
  static int n_slabs;
  slab *next;
  char s[slab_size];
  
  slab() : next(NULL) { n_slabs++; }
  ~slab() { n_slabs--; }
};

class stash
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

  // private routines
  void chop_slab();
};

class doubling_stash
{
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
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
