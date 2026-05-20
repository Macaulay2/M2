#warning \
    "Remove points.hpp, points.cpp once this branch is merged into the trunk"

// Copyright 2005  Michael E. Stillman

#ifndef _points_hpp_
#define _points_hpp_

class Matrix;
class PolynomialRing;

template <typename CoeffRing>
class PointsComputation
{
 public:
  static Matrix *points(const PolynomialRing *R,
                        const typename CoeffRing::ring_type *K,
                        const DMat<CoeffRing> *Pts,
                        Matrix *&result_std_monoms);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
