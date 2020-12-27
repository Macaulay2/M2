// Copyright 2014-2017 Michael E. Stillman.

#ifndef _res__gausser_zzp_hpp_
#define _res__gausser_zzp_hpp_

#include <iosfwd>                               // for ostream
#include <vector>                               // for vector
#include "coeffrings.hpp"                       // for CoefficientRingZZp
#include "ringelem.hpp"                         // for ring_elem
#include "schreyer-resolution/res-gausser.hpp"  // for ComponentIndex, Coeff...
class Ring;

class ResGausserZZp : public ResGausser
{
  typedef int FieldElement;

 private:
  CoefficientRingZZp* Kp;

  void set_one(FieldElement& one) const { one = 0; }  // exponent for 1
  void negate(FieldElement a, FieldElement& result) const
  {
    Kp->negate(result, a);
  }

  int coeff_to_int(FieldElement f) const
  // Returns an integer in the range -a..a, (or 0..1) where a = floor(p/2),  p
  // is the characteristic.
  {
    return Kp->to_int(f);
  }

  std::vector<FieldElement>& coefficientVector(CoefficientVector f) const
  {
    return *reinterpret_cast<std::vector<FieldElement>*>(f.mValue);
  }

  CoefficientVector coefficientVector(std::vector<FieldElement>* vals) const
  {
    CoefficientVector result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

 public:
  ResGausserZZp(const Ring* K);
  virtual ~ResGausserZZp() {}
  const CoefficientRingZZp* get_coeff_ring() const { return Kp; }
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
                              ring_elem a,
                              ring_elem unused) const;  // appends to result.
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
  // The value of c is not recorded.

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
