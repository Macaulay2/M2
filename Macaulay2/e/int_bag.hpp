// (c) 1994  Michael E. Stillman
#ifndef _int_bag_hh_
#define _int_bag_hh_

#include "style.hpp"
#include "varpower.hpp"

class int_bag
{
  union {
    int b_elem;
    void *b_ptr;
  } val;
  intarray mon;			// varpower representation

public:
  int_bag() : mon() { val.b_ptr = 0; }
  int_bag(int b, const intarray &m) : mon(m) { val.b_elem = b; }
  int_bag(void *b, const intarray &m) : mon(m) { val.b_ptr = b; }
  int_bag(int b): mon() { val.b_elem = b; }
  int_bag(void *b): mon() { val.b_ptr = b; }
  int_bag(const int_bag &gcb)
    : val(gcb.val), mon(gcb.mon) {}
  int_bag(const int_bag *gcb)
    : val(gcb->val), mon(gcb->mon) {}

  const intarray &monom() const { return mon; }
        intarray &monom()       { return mon; }

  int basis_elem() const { return val.b_elem; }
  void *basis_ptr() const { return val.b_ptr; }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};
#endif
