#include "NCAlgebras/VectorArithmetic.hpp"
#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for newarray, newarray_atomic
#include "ring.hpp"              // for Ring
#include "ringelem.hpp"          // for ring_elem

#include <iostream>
#include <execution>

void VectorArithmetic::sparseRowToDenseRow(Range<ring_elem> dense,
                                           const Range<ring_elem>& coeffs,
                                           const Range<int>& comps) const
{
  // FM: Do we have to set these to zero before filling?  If not, why not?
  //std::fill(dense.begin(),dense.end(),mRing->zero());
  // dense is zeroed out in denseRowToSparseRow after moving it out.
  for (int i = 0; i < comps.size(); i++) dense[comps[i]] = coeffs[i];
}

void VectorArithmetic::denseRowCancelFromSparse(Range<ring_elem> dense,
                                                const Range<ring_elem>& coeffs,
                                                const Range<int>& comps) const
{
  // this is the fastest of the three implementations below :(
  ring_elem a = dense[comps[0]];
  for (int i=0; i < comps.size(); ++i)
   {
     ring_elem tmp = mRing->mult(a, coeffs[i]);
     dense[comps[i]] = mRing->subtract(dense[comps[i]], tmp);
   }

  // added a pair-range iterator type in Range.hpp which make the next two work
  // however, both are about 5-8% slower

  // range-based for loop
  // PairRange<ring_elem,int> pairRange(coeffs,comps);
  // ring_elem a = dense[comps[0]];
  // for (auto p : pairRange) {
  //   dense[p.second] = mRing->subtract(dense[p.second], 
  //                                     mRing->mult(a,p.first));
  // }

  // std::for_each with parallel?
  // ring_elem a = dense[comps[0]];
  // PairRange<ring_elem,int> pairRange(coeffs,comps);
  // std::for_each( // std::execution::par,  // not available yet...
  //               pairRange.begin(),
  //               pairRange.end(),
  //               [&dense,a,this](std::pair<ring_elem,int> p) {
  //                 dense[p.second] = this->mRing->subtract(dense[p.second], 
  //                                                         this->mRing->mult(a,p.first));
  //               });
}

int VectorArithmetic::denseRowNextNonzero(const Range<ring_elem>& dense,
                                          int first,
                                          int last) const
{
  // implementation using find_if_not
  //auto nextNonzero = std::find_if_not(dense.cbegin() + first,
  //                                    dense.cbegin() + last + 1,
  //                                    [this](ring_elem c) { return this->mRing->is_zero(c); });
  //return (nextNonzero - dense.cbegin());
  
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

#ifndef NDEBUG
  bool any = false;
  for (int i=0; i < first; ++i)
    if (!mRing->is_zero(dense[i]))
      {
        if (!any)
          {
            std::cout << "--*** VectorArithmetic: [first,last] = [" << first << ", " << last << "]" << std::endl;
            std::cout << "  dense array is non-zero, indices: ";
            any = true;
          }
        std::cout << i << " ";
      }
  for (int i=last+1; i < dense.size(); ++i)
    if (!mRing->is_zero(dense[i]))
      {
        if (!any)
          {
            std::cout << "--*** VectorArithmetic: [first,last] = [" << first << ", " << last << "]" << std::endl;
            std::cout << "  dense array is non-zero, indices: ";
            any = true;
          }
        std::cout << i << " ";
      }
  if (any) std::cout << std::endl;
#endif
  
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

#ifndef NDEBUG
  any = false;
  for (int i=0; i < dense.size(); ++i)
    if (!mRing->is_zero(dense[i]))
      {
        if (!any)
          {
            std::cout << "error ***: dense array is non-zero, indices: ";
            any = true;
          }
        std::cout << i << " ";
      }
  if (any) std::cout << std::endl;
#endif
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
