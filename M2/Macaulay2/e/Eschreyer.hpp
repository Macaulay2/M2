// Copyright 1999  Michael E. Stillman
#ifndef _Eschreyer_hpp_
#define _Eschreyer_hpp

#include "polyring.hpp"
#include "matrix.hpp"
#include "geovec.hpp"

class GBKernelComputation : public object
{
  const PolynomialRing *R;
  const Ring *K;
  const Monoid *M;
  const FreeModule *F;  // This is where the action is...
  const FreeModule *G;  // This is where the resulting syzygies live.
        // This MUST be a Schreyer free module compatible with the input!

  array<MonomialIdeal *> mi;  // Used in reduction.
  array<vec> gb;            // This is the "stripped" GB.
  array<vec> syzygies;     // This is basically the result.

  ring_elem one;
  int *PAIRS_mon;   // A monomial in M, used only in new_pairs.
  int *REDUCE_mon;  // A monomial in M, used only in reduce.
  int *REDUCE_exp;  // An exponent vector, used only in reduce.
  int n_ones;
  int n_unique;
  int n_others;
  int total_reduce_count;

  void new_pairs(int i);
  void strip_gb(const Matrix *m);  // Fills in 'gb' with stripped GB.

  vec make_syz_term(ring_elem c, const int *monom, int comp) const;
  // This routine grabs 'c', and 'monom' should be the total monomial.

  bool find_ring_divisor(const int *exponents, ring_elem &result);
  int find_divisor(const MonomialIdeal * mi, const int *exponents, int &result);
  // Returns the index of the least element in the monomial order which divides.

  vec s_pair(vec syz) const;
  void reduce(vec &g, vec &gsyz);  // Reduces g to zero.  gsyz is real result.
public:
  GBKernelComputation(const Matrix *m);

  virtual ~GBKernelComputation();

  int calc();

  Matrix *get_syzygies();

public:
  GBKernelComputation * cast_to_GBKernelComputation() { return this; }
  const GBKernelComputation * cast_to_GBKernelComputation() const { return this; }
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
