// Copyright 2002  Michael E. Stillman

#ifndef _schorder_hpp_
#define _schorder_hpp_

#include "buffer.hpp"
#include "intarray.hpp"
#include "monoid.hpp"

class SchreyerOrder
{
  const Monoid *M;

  intarray _order; // Each 'entry' is an array of ints of length _nslots:
		   // compare_num, followed by the (encoded) monomial.
  int _nslots;
  int _rank;

  void symm1(int lastn,	     // can use lastn..rank()-1 in product
	     int pow) const;   // remaining power to take

public:
  SchreyerOrder(const Monoid *m) : M(m), _nslots(m->monomial_size() + 1), _rank(0) {}

  static SchreyerOrder *create(const Matrix *m);

  int rank() const { return _rank; }
  int compare_num(int i) const { return _order[i * _nslots]; }
  const int *base_monom(int i) const { return _order.raw() + i*_nslots + 1; }

  bool is_equal(const SchreyerOrder *G) const;
  SchreyerOrder *copy() const;
  SchreyerOrder *sub_space(int n) const;
  SchreyerOrder *sub_space(const M2_arrayint a) const;
  void append_order(const SchreyerOrder *G);
  SchreyerOrder *direct_sum(const SchreyerOrder *G) const;
  SchreyerOrder *tensor(const SchreyerOrder *G) const;
  SchreyerOrder *exterior(int p) const;

  SchreyerOrder *symm(int n) const;

  void append(int compare_num, const int *base_monom);
  // Copies the monomial

  void schreyer_up(int *m, int comp, int *result);// 'result' is allowed to be 'm'.

  void schreyer_down(int *m, int comp, int *result); // 'result' is allowed to be 'm'.

  int schreyer_compare(int *m1, int comp1, int *m2, int comp2);
  // Assumes that m1, m2 are 'schreyer up'.

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
