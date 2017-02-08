// Copyright 2014 Michael E. Stillman.

#ifndef _res__gausser_hpp_
#define _res__gausser_hpp_

#include "../ring.hpp"
#include "../ZZp.hpp"
#include "../coeffrings.hpp"

typedef int FieldElement;
typedef int ComponentIndex;

class CoefficientVector {
  // disallow copy...
  friend class ResGausser;
  CoefficientVector() : mValue(nullptr) {}
private:
  void * mValue;
};

typedef struct { void * mValue; } NewCoefficientArray;

class ResGausser
{
  enum {ZZp} typ;
  const Z_mod *K; // only used to construct Kp.  Will be removed MES.

  CoefficientRingZZp *Kp;

  ResGausser(int p);

  FieldElement* coefficientArray(NewCoefficientArray f) const { return reinterpret_cast<FieldElement*>(f.mValue); }
  std::vector<FieldElement>& coefficientVector(CoefficientVector f) const { return * reinterpret_cast<std::vector<FieldElement>* >(f.mValue); }
public:
  typedef FieldElement* CoefficientArray;
  //  typedef void *CoefficientArray;

  struct dense_row {
    ComponentIndex len; // coeffs is an array 0..len-1
    CoefficientArray coeffs;
  };

  ~ResGausser() {}

  static ResGausser *newResGausser(int p);

  const Ring * get_ring() const { return K; }

  const CoefficientRingZZp* get_coeff_ring() const { return Kp; }


  void set_one(FieldElement& one) const { one = 0; } // exponent for 1

  void negate(FieldElement a, FieldElement& result) const
  {
    Kp->negate(result, a);
  }
  
  CoefficientArray from_ints(ComponentIndex len, const int* elems) const;

  void to_ints(ComponentIndex len, CoefficientArray, int *result) const;

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

  void dense_row_cancel_sparse(dense_row& r,
                               ComponentIndex len,
                               CoefficientArray sparse,
                               ComponentIndex* comps,
                               std::vector<FieldElement>& result_loc
                               ) const;

  int coeff_to_int(FieldElement f) const
  // Returns an integer in the range -a..a, (or 0..1) where a = floor(p/2),  p is the characteristic.
  {
    return Kp->to_int(f);
  }

  mutable long n_dense_row_cancel;
  mutable long n_subtract_multiple;

private:
  CoefficientVector coefficientVector(std::vector<FieldElement>* vals) const {
    CoefficientVector result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

public:
  void pushBackOne(std::vector<FieldElement>& coeffs) const;
  void pushBackMinusOne(std::vector<FieldElement>& coeffs) const;
  void pushBackElement(std::vector<FieldElement>& coeffs, const FieldElement* take_from_here, size_t loc) const;
  void pushBackNegatedElement(std::vector<FieldElement>& coeffs, const FieldElement* take_from_here, size_t loc) const;
  
  size_t size(CoefficientVector r) const;
  
  CoefficientVector allocateCoefficientVector(ComponentIndex nelems) const;
  // create a row of 0's (over K).

  void clear(CoefficientVector r, ComponentIndex first, ComponentIndex last) const;
  // set the elements in the range first..last to 0.

  void deallocate(CoefficientVector r) const;

  ComponentIndex nextNonzero(CoefficientVector r, ComponentIndex first, ComponentIndex last) const;
  // returns last+1 in the case when there are no non-zero elements left.

  void fillFromSparse(CoefficientVector r,
                      ComponentIndex len,
                      CoefficientVector sparse,
                      ComponentIndex* comps) const;
  // Fills 'r' from 'sparse' (and 'comps')

  void sparseCancelGivenMonic(CoefficientVector r,
                              ComponentIndex len,
                              CoefficientVector sparse,
                              ComponentIndex* comps) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1.

  void sparseCancel(CoefficientVector r,
                    ComponentIndex len,
                    CoefficientVector sparse,
                    ComponentIndex* comps,
                    CoefficientVector result_loc
                    ) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is recorded in result_c.
  
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
