#warning \
    "Remove points.hpp, points.cpp once this branch is merged into the trunk"

// Copyright 2005  Michael E. Stillman

#ifndef _points_hpp_
#define _points_hpp_

/**
 * @file points.hpp
 * @brief `PointsComputation<CoeffRing>::points` --- ideal and standard-monomial basis of a finite point set.
 *
 * Templated entry point that takes a polynomial ring `R`, its
 * coefficient ring `K`, and a `DMat<CoeffRing>` whose columns are
 * points in `K^n`, and returns generators of the vanishing ideal
 * `I(Pts) subset R` plus a one-row matrix of standard monomials
 * forming a `K`-basis of the quotient `R / I(Pts)`. The
 * generators are produced under the engine's graded-reverse-lex
 * order; the standard-monomial output is the natural Hilbert-
 * function complement.
 *
 * The `#warning` at the very top of the file flags it as a branch
 * leftover marked for removal once the host work merges. Engine
 * callers reach this code through the M2 ideal-of-points
 * interfaces in `Points`-family packages; nothing inside the
 * engine consumes it directly yet.
 *
 * @see matrix.hpp
 * @see dmat.hpp
 */

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
