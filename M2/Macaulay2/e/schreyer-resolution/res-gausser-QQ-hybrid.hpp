// Copyright 2017 Michael E. Stillman.

#ifndef _res_gausser_qq_hybrid_hpp_
#define _res_gausser_qq_hybrid_hpp_

#include "aring-RRR.hpp"                        // for ARingRRR
#include "aring-qq-gmp.hpp"                     // for ARingQQGMP
#include "aring-zzp-flint.hpp"                  // for ARingZZpFlint, ARingZ...
#include "ringelem.hpp"                         // for ring_elem
#include "schreyer-resolution/res-gausser.hpp"  // for ComponentIndex, Coeff...

#include <gmp.h>                                // for mpq_t
#include <iosfwd>                               // for ostream
#include <vector>                               // for vector

class Ring;

class ResGausserQQHybrid : public ResGausser
{
  using RRRType = M2::ARingRRR;
  struct FieldElement
  {
    double mDouble;
    M2::ARingZZpFlint::ElementType mMod1;
    M2::ARingZZpFlint::ElementType mMod2;
    int mDenominatorSize;
    int mAccuracy;
    bool mIsPresent;
    RRRType::ElementType mLongDouble;
  };

  const M2::ARingQQGMP mQQRing;
  const M2::ARingZZpFlint Kp1;
  const M2::ARingZZpFlint Kp2;
  const M2::ARingRRR mRRing;
  FieldElement mZero;
  FieldElement mMinusOne;
  FieldElement mOne;

  mutable int mMaxDenominatorSize;

 public:
  ResGausserQQHybrid(const Ring* K,
                     unsigned long precision1,
                     size_t p1,
                     size_t p2);

  virtual ~ResGausserQQHybrid() {}
 private:
  CoefficientVector coefficientVector(std::vector<FieldElement>* vals) const
  {
    CoefficientVector result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

  std::vector<FieldElement>& coefficientVector(CoefficientVector f) const
  {
    return *reinterpret_cast<std::vector<FieldElement>*>(f.mValue);
  }

  void init_element(FieldElement& a) const;
  void clear_element(FieldElement& a) const;
  void from_long_element(FieldElement& a, long val) const;
  void from_mpq_element(FieldElement& a, mpq_t val, int denomPower) const;
  void copy_element(FieldElement& result, const FieldElement& a) const;
  void negate_element(FieldElement& a) const;
  bool is_zero_element(FieldElement& a) const;

  void subtract_multiple_to_element(FieldElement& result,
                                    const FieldElement& a,
                                    const FieldElement& b) const;
  // result = result - a*b

 public:
  virtual void pushBackOne(CoefficientVector& coeffs) const;
  virtual void pushBackMinusOne(CoefficientVector& coeffs) const;
  virtual void pushBackElement(CoefficientVector& coeffs,
                               const CoefficientVector& take_from_here,
                               size_t loc) const;
  virtual void pushBackNegatedElement(CoefficientVector& coeffs,
                                      const CoefficientVector& take_from_here,
                                      size_t loc) const;

  virtual bool isAllowedCoefficientRing(const Ring* K) const;
  virtual ring_elem to_ring_elem(
      const Ring* K,
      const CoefficientVector& coeffs,
      size_t loc) const;  // in res-f4-m2-interface.cpp
  virtual void from_ring_elem(CoefficientVector& result,
                              ring_elem numer,
                              ring_elem denom)
      const;  // appends to result. bit numer, denom are in ARingZZGMP.
  virtual long to_modp_long(CoefficientVector& coeffs, size_t loc) const;

  virtual size_t size(CoefficientVector r) const
  {
    return coefficientVector(r).size();
  }

  virtual CoefficientVector allocateCoefficientVector(
      ComponentIndex nelems) const;
  // create a row of 0's (over K).

  virtual CoefficientVector allocateCoefficientVector() const;
  // create an empty array

  virtual void clear(CoefficientVector r,
                     ComponentIndex first,
                     ComponentIndex last) const;
  // set the elements in the range first..last to 0.

  virtual void deallocate(CoefficientVector r) const;

  virtual ComponentIndex nextNonzero(CoefficientVector r,
                                     ComponentIndex first,
                                     ComponentIndex last) const;
  // returns last+1 in the case when there are no non-zero elements left.

  virtual void fillFromSparse(CoefficientVector r,
                              ComponentIndex len,
                              CoefficientVector sparse,
                              ComponentIndex* comps) const;
  // Fills 'r' from 'sparse' (and 'comps')

  virtual void sparseCancel(CoefficientVector r,
                            CoefficientVector sparse,
                            ComponentIndex* comps,
                            CoefficientVector result_loc) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is recorded in result_c.

  virtual void sparseCancel(CoefficientVector r,
                            CoefficientVector sparse,
                            ComponentIndex* comps) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
  // The value of c is not recorded in this version.

  std::ostream& out(std::ostream& o, FieldElement& f) const;
  virtual std::ostream& out(std::ostream& o,
                            CoefficientVector f,
                            int loc) const;
  virtual std::ostream& debugDisplay(std::ostream& o,
                                     CoefficientVector r) const;
  virtual std::ostream& debugDisplayRow(std::ostream& o,
                                        int ncolumns,
                                        const std::vector<int>& comps,
                                        CoefficientVector coeffs) const;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
