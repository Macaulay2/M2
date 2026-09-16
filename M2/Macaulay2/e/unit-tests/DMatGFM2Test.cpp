// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-m2-GF.hpp"
#include "unit-tests/ARingMatrixTest.hpp"
#include "unit-tests/util-polyring-creation.hpp"

// Use the deterministic-prefix-then-random generator contract.
template <>
void getElement<M2::ARingGFM2>(const M2::ARingGFM2& R,
                               int index,
                               M2::ARingGFM2::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

// x is primitive in GF(3)[x]/(x^2 + x + 2), a field with nine elements.
TEST(DMatGFM2, addition)
{
  const PolynomialRing* P = simplePolynomialRing(3, {"x"});
  ASSERT_NE(P, nullptr);
  const auto* Q = dynamic_cast<const PolynomialRing*>(
      simpleQuotientRing(P, {"x^2+x+2"}));
  ASSERT_NE(Q, nullptr);
  M2::ARingGFM2 R(*Q, Q->var(0));
  testMatrixAdd<DMat<M2::ARingGFM2>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingGFM2>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
