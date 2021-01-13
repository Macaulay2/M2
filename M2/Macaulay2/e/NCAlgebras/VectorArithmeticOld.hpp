#ifndef __vector_arithmetic__
#define __vector_arithmetic__

#include "Range.hpp"  // for Range

class Ring;
union ring_elem;

class VectorArithmetic
{
private:
  const Ring* mRing;
  
public:
  VectorArithmetic(const Ring* R) : mRing(R) {}

  virtual void sparseRowToDenseRow(Range<ring_elem> dense,
                           const Range<ring_elem>& coeffs,
                           const Range<int>& comps) const;

  virtual void denseRowCancelFromSparse(Range<ring_elem> dense,
                                const Range<ring_elem>& coeffs,
                                const Range<int>& comps) const;

  virtual int denseRowNextNonzero(const Range<ring_elem>& dense,
                          int first,
                          int last) const;

  virtual void denseRowToSparseRow(Range<ring_elem> dense,
                           Range<ring_elem>& coeffs,
                           Range<int>& comps,
                           int first,
                           int last) const;

  virtual void sparseRowMakeMonic(Range<ring_elem>& coeffs);
  
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:


