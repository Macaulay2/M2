/* Copyright 2015, Michael E. Stillman */

#include "res-poly-ring.hpp"

long poly::npoly_destructor = 0;
long poly_constructor::ncalls = 0;
long poly_constructor::ncalls_fromarray = 0;

void ResPolyRing::memUsage(const poly& f, long& nterms, long& bytes_used, long& bytes_alloc) const
{
  long sz = 0;
  sz = f.len * sizeof(FieldElement);
  sz += f.len * monoid().max_monomial_size() * sizeof(res_monomial_word);
  nterms += f.len;
  bytes_used += sz;
  bytes_alloc += sz;
}

bool check_poly(const ResPolyRing& R, const poly&f, const ResSchreyerOrder& ord)
{
  // We loop through each monomial, checking it against the one before
  // The order used is the Schreyer order given.
  auto& M = R.monoid();
  poly_iter i(R, f);
  poly_iter end(R,f,1);
  res_packed_monomial prev = nullptr;
  for (; i != end; ++i)
    {
      if (prev == nullptr)
        prev = i.monomial();
      else
        {
          // Now compare to previous monomial
          long comp1 = M.get_component(prev);
          long comp2 = M.get_component(i.monomial());
          int result = M.compare_schreyer(prev, i.monomial(),
                                          ord.mTotalMonom[comp1], ord.mTotalMonom[comp2],
                                          ord.mTieBreaker[comp1], ord.mTieBreaker[comp2]);
          if (result == EQ or result == GT)
            {
              return false;
            }
          prev = i.monomial();
        }
    }
  return true;
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
