#include "VectorArithmetic.hpp"

void VectorArithmetic::sparseRowToDenseRow(Range<ring_elem> dense,
                                           const Range<ring_elem>& coeffs,
                                           const Range<int>& comps) const
{
}

void VectorArithmetic::denseRowCancelFromSparse(Range<ring_elem> dense,
                                                const Range<ring_elem>& coeffs,
                                                const Range<int>& comps) const
{
}

int VectorArithmetic::denseRowNextNonzero(const Range<ring_elem>& dense,
                                          int first,
                                          int last) const
{
  return -1;
}

void VectorArithmetic::denseRowToSparseRow(Range<ring_elem> dense,
                                           Range<ring_elem>& coeffs,
                                           Range<int>& comps,
                                           int first,
                                           int last) const
{
  
}

void VectorArithmetic::sparseRowMakeMonic(Range<ring_elem>& coeffs)
{
  // divide all entries in coeffs by ring_elem in first position
  // done in place
  ring_elem leadCoeff = *coeffs.begin();
  
  for (auto c : coeffs) { *c = mRing->divide(*c, leadCoeff); }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
