// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-GF-flint-big.hpp"
#include "unit-tests/ARingMatrixTest.hpp"
#include "unit-tests/util-polyring-creation.hpp"

// Use the deterministic-prefix-then-random generator contract.
template <>
void getElement<M2::ARingGFFlintBig>(const M2::ARingGFFlintBig& R,
                                     int index,
                                     M2::ARingGFFlintBig::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

// x is primitive in GF(3)[x]/(x^2 + x + 2), a field with nine elements.
TEST(DMatGFFlintBig, addition)
{
  const PolynomialRing* P = simplePolynomialRing(3, {"x"});
  ASSERT_NE(P, nullptr);
  const auto* Q = dynamic_cast<const PolynomialRing*>(
      simpleQuotientRing(P, {"x^2+x+2"}));
  ASSERT_NE(Q, nullptr);
  M2::ARingGFFlintBig R(*Q, Q->var(0));
  testMatrixAdd<DMat<M2::ARingGFFlintBig>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingGFFlintBig>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
