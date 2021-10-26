// Copyright 2011 Michael E. Stillman

#include "schurSn.hpp"

SchurSnRing::SchurSnRing(const Ring *A, int n) : SchurRing2(A, n) {}
SchurSnRing *SchurSnRing::create(const Ring *A, int n)
{
  SchurSnRing *R = new SchurSnRing(A, n);
  R->initialize_SchurRing2();
  return R;
}

ring_elem SchurSnRing::mult(const ring_elem f, const ring_elem g) const
{
  return SchurRing2::mult(f, g);
}

ring_elem SchurSnRing::tensor_mult(const ring_elem f, const ring_elem g) const
{
  return SchurRing2::mult(f, g);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
