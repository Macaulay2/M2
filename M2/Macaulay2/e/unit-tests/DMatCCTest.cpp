// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-mutable-matrices/mat-arith.hpp"
#include "basic-rings/aring-CC.hpp"
#include "unit-tests/ARingMatrixTest.hpp"
#include "unit-tests/MatrixShape.hpp"

// Use the deterministic-prefix-then-random generator contract.
template <>
void getElement<M2::ARingCC>(const M2::ARingCC& R,
                             int index,
                             M2::ARingCC::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

TEST(DMatCC, addition)
{
  M2::ARingCC R;
  testMatrixAdd<DMat<M2::ARingCC>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingCC>>(R, ntrials, 2, 3);
}

TEST(DMatCC, normSquared)
{
  M2::ARingCC C;
  ARingMatrixGenerator<DMat<M2::ARingCC>> matgen(C);
  DMat<M2::ARingCC> M(C, 5, 5);
  matgen.nextMatrix(M, MatrixShape::Identity);

  const auto& R = C.real_ring();
  M2::ARingCC::RealRingType::Element result(R), expected(R);
  R.set(expected, 5);
  normSquared(submatrix(M), result);
  EXPECT_TRUE(R.is_equal(result, expected));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
