// Copyright 1996  Michael E. Stillman
#ifndef _newspair_hh_
#define _newspair_hh_

#include "freemod.hh"
#include "polyring.hh"

struct s_pair;

struct gb_elem
{
  gb_elem *next;
  vec f;
  vec fsyz;
  int *lead_exp;
  int is_min;			// TY_MINIMAL, TY_SMALL_GB, TY_LARGE_GB, TY_REMOVED
  int me;
  
  gb_elem()
    : next(NULL),
      f(NULL), fsyz(NULL), lead_exp(NULL), is_min(0), me(0) {}
  gb_elem(vec f, vec fsyz, int is_min) 
    : next(NULL),
      f(f), fsyz(fsyz), lead_exp(NULL), is_min(is_min), me(0) {}

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct s_pair
{
  s_pair *next;
  int status;			// whether this pair needs to be done or not.
  int deg;			// Primary degree of this pair.
  gb_elem *first;		// Never NULL
  gb_elem *second;		// NULL iff the second element would be in the base ring
  vec f;			// A vector in F, NULL iff the s-pair has not yet been
				// computed, OR at the end of a reduction, if the element
				// reduced to 0.
  vec fsyz;			// A vector in Fsyz
  int *lcm;			// An exponent vector

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

struct s_pair_bunch
{
  s_pair_bunch *next;
  int mydeg;

  s_pair *elems;
  s_pair *unsorted;
  int nelems;

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class s_pair_set
{
  int offset_degree;
  array<s_pair_bunch *> heap;
  int thisdeg;
  int lodeg;			// If lodeg < thisdeg, then need to change degree.

  int compare(s_pair *f, s_pair *g) const;
  void sort_list(s_pair *&p) const;
public:
  s_pair_set(const monoid *M);
  ~s_pair_set();

  int n_elems();		// The number currently contained in this set

  int next_degree(&nextdeg);	// Returns number to be done in nextdeg.
  s_pair *remove();		// Returns NULL if no more in this degree.
				// OR if an element of lower degree has appeared.

  void insert(s_pair *&p);	// Insert a set of s pairs.

  void stats();			// Displays some statistics about this set.
};

#endif
