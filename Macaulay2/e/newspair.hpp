// Copyright 1996  Michael E. Stillman
#ifndef _newspair_hpp_
#define _newspair_hpp_

#include "freemod.hpp"
#include "polyring.hpp"

struct GB_elem
{
  GB_elem *next;
  vec f;
  vec fsyz;
  int *lead_exp;
  int is_min;			// TY_MINIMAL, TY_SMALL_GB, TY_LARGE_GB, TY_REMOVED
  int me;
  
  GB_elem()
    : next(NULL),
      f(NULL), fsyz(NULL), lead_exp(NULL), is_min(0), me(0) {}
  GB_elem(vec f, vec fsyz, int is_min) 
    : next(NULL),
      f(f), fsyz(fsyz), lead_exp(NULL), is_min(is_min), me(0) {}

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct S_pair
{
  S_pair *next;
  char is_gen;			// 0 means spair, 1 means gen.  spairs get sorted
				// by degree, then fsyz, gens get sorted by degree, then f.
  vec f;			// A vector in F, NULL iff the s-pair has not yet been
				// computed, OR at the end of a reduction, if the element
				// reduced to 0.
  vec fsyz;			// A vector in G: each index corresponds to a GB elem
				// G should use a Schreyer order

  S_pair(int is_gen, vec f, vec fsyz);
  S_pair();

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct s_pair_bunch
{
  s_pair_bunch *next;
  int mydeg;

  S_pair *pairs;
  S_pair *gens;
  S_pair *unsorted_pairs;
  S_pair *unsorted_gens;

  int nelems;			// Number remaining
  int ngens;			// Number remaining

  s_pair_bunch(int d) : next(NULL), mydeg(d), 
    pairs(NULL), gens(NULL),
    unsorted_pairs(NULL), unsorted_gens(NULL),
    nelems(0), ngens(0) {}

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class s_pair_set
{
  const FreeModule *F, *G;

  s_pair_bunch *heap;		// Sorted by increasing degree
  s_pair_bunch *this_deg;	// Points to current degree (which should be the first)

  int nelems;
  int ngens;
  int ncomputed;

  intarray pairs_done;

  void flush_degree(s_pair_bunch *&p);

  int compare(S_pair *f, S_pair *g) const;
  void sort(S_pair *&p) const;
  S_pair *merge(S_pair *p, S_pair *q) const;

  s_pair_bunch *get_degree(int d);

  void debug_out(S_pair *s) const;
public:
  s_pair_set(const FreeModule *F, const FreeModule *G);
  ~s_pair_set();
  void remove_pair(S_pair *&s);
  void remove_pair_list(S_pair *&p);

  void insert(S_pair *&p);	// Insert an s-pair or generator

  int next_degree(int &nextdeg);	// Returns number to be done in nextdeg.
  S_pair *next_pair();		// Returns NULL if no more
  S_pair *next_gen();		// Returns NULL if no more
  void flush_degree();

  int n_elems_left() const { return nelems; } // The number currently contained in this set
  int n_gens_left() const { return ngens; }
  int n_computed() const { return ncomputed; }

  void stats();			// Displays some statistics about this set.

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif
