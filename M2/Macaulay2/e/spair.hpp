// Copyright 1996  Michael E. Stillman
#ifndef _spair_hh_
#define _spair_hh_

#include "freemod.hpp"
#include "polyring.hpp"
#include "gbring.hpp"

struct s_pair;

struct gb_elem : public our_new_delete
{
  gb_elem *next;
  gb_elem *next_min;
  s_pair *pair_list;  // List of pairs with this as 'first' element
  gbvector *f;
  gbvector *fsyz;
  int *lead_exp;
  int is_min;
  int me;

  gb_elem()
      : next(NULL),
        next_min(NULL),
        pair_list(NULL),
        f(NULL),
        fsyz(NULL),
        lead_exp(NULL),
        is_min(0),
        me(0)
  {
  }
  gb_elem(gbvector *f0, gbvector *fsyz0, int is_min0)
      : next(NULL),
        next_min(NULL),
        pair_list(NULL),
        f(f0),
        fsyz(fsyz0),
        lead_exp(NULL),
        is_min(is_min0),
        me(0)
  {
  }
};

struct s_pair : public our_new_delete
{
  s_pair *next;
  s_pair *next_same;  // Next one with the same 'first'
  int syz_type;
  int compare_num;  // <0 means 'deleted': don't compute the corresp s-pair
  int degree;
  int *lcm;  // A packed monomial (should it be an expvector?)
  gb_elem *first;
  gb_elem *second;
  gbvector *f;     // A vector in NGB_comp::gens.rows()
  gbvector *fsyz;  // A vector in NGB_comp::syz.rows()
};

const int NHEAP = 12;

class s_pair_heap : public our_new_delete
{
  const Monoid *M;

  s_pair *heap[NHEAP];
  int n_in_heap[NHEAP];
  int top_of_heap;
  int nelems;

  int compare(s_pair *f, s_pair *g) const;
  s_pair *merge(s_pair *f, s_pair *g) const;

 public:
  s_pair_heap(const Monoid *M);

  s_pair *grab_remaining_pairs();  // designed so the user can then remove the
                                   // data associated with them
  ~s_pair_heap();

  void insert(s_pair *&p);
  void insert(s_pair *p, int len);
  s_pair *remove();
  void put_back(s_pair *&p);

  void sort_list(s_pair *&p) const;

  int n_elems() { return nelems; }
  void stats() const;
  void text_out(buffer &o) const;

  s_pair *debug_list(int i)
  {
    return heap[i];
  }  // DO NOT USE, except for debugging purposes!
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
