// Copyright 2014 Michael E. Stillman.

#ifndef _res__gausser_hpp_
#define _res__gausser_hpp_

#include "res-f4-mem.hpp"

#include "../ring.hpp"
#include "../ZZp.hpp"
#include "../coeffrings.hpp"

class ResF4Mem;
typedef int FieldElement;
typedef int ComponentIndex;

class ResGausser
{
  enum {ZZp} typ;
  const Ring *K;

  CoefficientRingZZp *Kp;
  mutable ResF4Mem Mem;

  ResGausser(const Z_mod *K0);
public:
  typedef FieldElement* CoefficientArray;
  //  typedef void *CoefficientArray;

  struct dense_row {
    ComponentIndex len; // coeffs is an array 0..len-1
    CoefficientArray coeffs;
  };

  ~ResGausser() {}

  static ResGausser *newResGausser(const Ring *K);

  const Ring * get_ring() const { return K; }

  const CoefficientRingZZp* get_coeff_ring() const { return Kp; }


  void set_one(FieldElement& one) const { one = 0; } // exponent for 1

  void negate(FieldElement a, FieldElement& result) const
  {
    Kp->negate(result, a);
  }
  
  CoefficientArray from_ringelem_array(ComponentIndex len, ring_elem *elems) const;

  void to_ringelem_array(ComponentIndex len, CoefficientArray, ring_elem *result) const;

  // leading coefficient
  FieldElement lead_coeff(CoefficientArray coeffs) const
  {
    return coeffs[0];
  }

  // other routines:
  void deallocate_F4CCoefficientArray(CoefficientArray &F, ComponentIndex len) const;

  // reduce mod p. (QQ --> ZZ/p) (place in double's??)

  // other reductions

  // combine them (ZZ/p1, ..., ZZ/pn --> QQ)

  // Hensel lift routine?

  // evaluation of multivariate poly's or fcn's.

  void dense_row_allocate(dense_row &r, ComponentIndex nelems) const;
  // create a row of 0's (over K).

  void dense_row_clear(dense_row &r, ComponentIndex first, ComponentIndex last) const;

  void dense_row_deallocate(dense_row &r) const;

  ComponentIndex dense_row_next_nonzero(dense_row &r, ComponentIndex first, ComponentIndex last) const;

  void dense_row_fill_from_sparse(dense_row& r,
                                  ComponentIndex len,
                                  CoefficientArray sparse,
                                  ComponentIndex* comps) const;
  // Fills 'r' from 'sparse' (and 'comps')

  void dense_row_cancel_sparse_monic(dense_row& r,
                               ComponentIndex len,
                               CoefficientArray sparse,
                               ComponentIndex* comps) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1.

  void dense_row_cancel_sparse(dense_row& r,
                               ComponentIndex len,
                               CoefficientArray sparse,
                               ComponentIndex* comps,
                               FieldElement& result_c) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is recorded in result_c.
  
  int coeff_to_int(FieldElement f) const //anton
  {
    return Kp->to_int(f);
  }

  mutable long n_dense_row_cancel;
  mutable long n_subtract_multiple;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
