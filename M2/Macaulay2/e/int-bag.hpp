// (c) 1994  Michael E. Stillman
#ifndef _int_bag_hh_
#define _int_bag_hh_

#include "newdelete.hpp"

class int_bag : public our_new_delete
{
  union
  {
    int b_elem;
    void* b_ptr;
  } val;
  gc_vector<int> mon;  // varpower representation

 public:
  int_bag() : mon() { memset(&val, 0, sizeof(val)); }
  int_bag(int b) : mon() { val.b_elem = b; }
  int_bag(void* b) : mon() { val.b_ptr = b; }
  int_bag(int b, const gc_vector<int>& m) : mon(m) { val.b_elem = b; }
  int_bag(void* b, const gc_vector<int>& m) : mon(m) { val.b_ptr = b; }
  int_bag(const int_bag& gcb) : val(gcb.val), mon(gcb.mon) {}
  int_bag(const int_bag* gcb) : val(gcb->val), mon(gcb->mon) {}

  gc_vector<int>& monom() { return mon; }
  const gc_vector<int>& monom() const { return mon; }

  gc_vector<int>& vector() { return mon; }
  const gc_vector<int>& vector() const { return mon; }

  int basis_elem() const { return val.b_elem; }
  void* basis_ptr() const { return val.b_ptr; }
};

typedef int_bag Bag;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
