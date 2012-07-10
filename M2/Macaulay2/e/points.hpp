// Copyright 2005  Michael E. Stillman

#ifndef _points_hpp_
#define _points_hpp_

#include "engine.h"
#include "dmat-LU.hpp"
class M2_Matrix;
class PolynomialRing;

template <typename CoeffRing>
class PointsComputation
{
public:
  static M2_Matrix *points(const PolynomialRing *R,
                        const typename CoeffRing::ring_type *K,
                        const DMat<CoeffRing> *Pts,
                        M2_Matrix * & result_std_monoms);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
