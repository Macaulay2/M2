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

  void sparseRowToDenseRow(Range<ring_elem> dense,
                           const Range<ring_elem>& coeffs,
                           const Range<int>& comps) const;

  void denseRowCancelFromSparse(Range<ring_elem> dense,
                                const Range<ring_elem>& coeffs,
                                const Range<int>& comps) const;

  int denseRowNextNonzero(const Range<ring_elem>& dense,
                          int first,
                          int last) const;

  void denseRowToSparseRow(Range<ring_elem> dense,
                           Range<ring_elem>& coeffs,
                           Range<int>& comps,
                           int first,
                           int last) const;

  void sparseRowMakeMonic(Range<ring_elem>& coeffs);
  
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:


