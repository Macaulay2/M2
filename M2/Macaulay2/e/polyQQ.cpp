// Copyright 2005 Michael E. Stillman

#include "polyQQ.hpp"

PolyQQ::~PolyQQ()
{
}

PolyQQ *PolyQQ::create(const PolyRing *P)
{
#if 0
  const PolyQQ *R = new PolyQQ;
  R->initialize_ring(P->charac(),
		     P->n_vars(),
		     P->get_degree_ring());
  R->ambientR = P;

  zeroV = R->from_int(0);
  oneV = R->from_int(1);
  minus_oneV = R->from_int(-1);

  R->_gb_ring = GBRing::create_PolynomialRing(globalZZ,P->getMonoid());

  return R;
#endif
}

PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const Matrix *I)
  // I should be be a one row matrix, and a GB in P.
  // These facts are NOT checked.
  // Any quotient elements of P are ignored?
{
}

PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const PolyQQ *B)
  // B should be a logical coeff ring of P.
  // All quotient elements of B are extended up to P.
  // and a new ring is made.
{
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
