// Copyright 2014 Michael E. Stillman.

#ifndef _res__gausser_hpp_
#define _res__gausser_hpp_

#include "../ring.hpp"
#include "../ZZp.hpp"
#include "../coeffrings.hpp"

typedef int ComponentIndex;

class CoefficientVector {
  // disallow copy...
  friend class ResGausser;
public:
  CoefficientVector() : mValue(nullptr) {}

  bool isNull() const { return mValue == nullptr; }
  void swap(CoefficientVector& b)
  {
    std::swap(mValue, b.mValue);
  }
private:
  void * mValue;
};

class ResGausser
{
  typedef int FieldElement;

  enum {ZZp} typ;
  const Ring *K;
  CoefficientRingZZp *Kp;

  ResGausser(const Ring* K);

  std::vector<FieldElement>& coefficientVector(CoefficientVector f) const {
    return * reinterpret_cast<std::vector<FieldElement>* >(f.mValue);
  }
public:
  ~ResGausser() {}

  static ResGausser *newResGausser(const Ring* K1);

  const Ring * get_ring() const { return K; }

  const CoefficientRingZZp* get_coeff_ring() const { return Kp; }

  void set_one(FieldElement& one) const { one = 0; } // exponent for 1

  void negate(FieldElement a, FieldElement& result) const
  {
    Kp->negate(result, a);
  }
  
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
  void pushBackOne(CoefficientVector& coeffs) const;
  void pushBackMinusOne(CoefficientVector& coeffs) const;
  void pushBackElement(CoefficientVector& coeffs, const CoefficientVector& take_from_here, size_t loc) const;
  void pushBackNegatedElement(CoefficientVector& coeffs, const CoefficientVector& take_from_here, size_t loc) const;

  ring_elem to_ring_elem(const CoefficientVector& coeffs, size_t loc) const; // in res-f4-m2-interface.cpp
  void from_ring_elem(CoefficientVector& result, ring_elem a) const; // appends to result.
  
  size_t size(CoefficientVector r) const {
    return coefficientVector(r).size();
  }
  
  CoefficientVector allocateCoefficientVector(ComponentIndex nelems) const;
  // create a row of 0's (over K).

  CoefficientVector allocateCoefficientVector() const;
  // create an empty array

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

  void sparseCancel(CoefficientVector r,
                    CoefficientVector sparse,
                    ComponentIndex* comps,
                    CoefficientVector result_loc
                    ) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is recorded in result_c.

  void debugDisplay(CoefficientVector r) const;
  void debugDisplayRow(int ncolumns, const std::vector<int>& comps, CoefficientVector coeffs) const;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
