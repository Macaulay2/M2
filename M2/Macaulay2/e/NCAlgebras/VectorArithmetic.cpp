#include "VectorArithmetic.hpp"

#include "NCAlgebras/Range.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

void VectorArithmetic::sparseRowToDenseRow(Range<ring_elem> dense,
                                           const Range<ring_elem>& coeffs,
                                           const Range<int>& comps) const
{
  for (int i = 0; i < comps.size(); i++) dense[comps[i]] = coeffs[i];
}

void VectorArithmetic::denseRowCancelFromSparse(Range<ring_elem> dense,
                                                const Range<ring_elem>& coeffs,
                                                const Range<int>& comps) const
{
  ring_elem a = dense[comps[0]];
  for (int i=0; i < comps.size(); i++)
    {
      ring_elem tmp = mRing->mult(a, coeffs[i]);
      dense[comps[i]] = mRing->subtract(dense[comps[i]], tmp);
    }
}

int VectorArithmetic::denseRowNextNonzero(const Range<ring_elem>& dense,
                                          int first,
                                          int last) const
{
  for (int i = first; i <= last; i++)
    if (!mRing->is_zero(dense[i])) return i;
  return last + 1;
}

void VectorArithmetic::denseRowToSparseRow(Range<ring_elem> dense,
                                           Range<ring_elem>& coeffs,
                                           Range<int>& comps,
                                           int first,
                                           int last) const
{
  int len = 0;
  
  // first can be -1 if the row is zero.  in this case, we should
  // not be accessing dense[i] for i negative.
  for (int i = first; i >= 0 and i <= last; i++)
    if (!mRing->is_zero(dense[i])) len++;

  ring_elem* ptr = newarray(ring_elem, len);
  coeffs = Range<ring_elem>(ptr, ptr + len);

  int* ptr2 = newarray_atomic(int, len);
  comps = Range<int>(ptr2, ptr2 + len);

  int next = 0;
  for (int i = first; i >= 0 and i <= last; i++)
    if (!mRing->is_zero(dense[i]))
      {
        coeffs[next] = dense[i];
        comps[next] = i;
        ++next;
        dense[i] = mRing->zero();
      }
}

void VectorArithmetic::sparseRowMakeMonic(Range<ring_elem>& coeffs)
{
  // divide all entries in coeffs by ring_elem in first position
  // done in place
  ring_elem leadCoeff = coeffs[0];
  
  for (auto& c : coeffs) { c = mRing->divide(c, leadCoeff); }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
