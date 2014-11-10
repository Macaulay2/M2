/* Copyright 2014, Michael E. Stillman */

#include "res-f4-computation.hpp"
#include "matrix.hpp"

ResolutionComputation* createF4Res(const Matrix *m,
                                   int max_level,
                                   int strategy
                                   )
{
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const Ring *K = R->getCoefficients();

  return new F4ResComputation(*R,
                              * m->rows(),
                              max_level);

  // Actually, now fill it the first two parts
}

F4ResComputation::F4ResComputation(const PolynomialRing& R,
                                   const FreeModule& F,
                                   int max_level)

  : mRing(R),
    mFreeModule(F)
{
  // Create a Gausser
}

F4ResComputation::~F4ResComputation()
{
  remove_res();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
