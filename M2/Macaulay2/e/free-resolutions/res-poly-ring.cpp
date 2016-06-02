/* Copyright 2015, Michael E. Stillman */

#include "stdinc.hpp"
#include "res-poly-ring.hpp"

void ResPolyRing::memUsage(const poly& f, long& nterms, long& bytes_used, long& bytes_alloc) const
{
  long sz = 0;
  sz = f.len * sizeof(FieldElement);
  sz += f.len * monoid().max_monomial_size() * sizeof(monomial_word);
  nterms += f.len;
  bytes_used += sz;
  bytes_alloc += sz;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
