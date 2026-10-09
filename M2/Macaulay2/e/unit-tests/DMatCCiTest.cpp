// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-CCi.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Use the deterministic-prefix-then-random generator contract.
template <>
void getElement<M2::ARingCCi>(const M2::ARingCCi& R,
                              int index,
                              M2::ARingCCi::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

TEST(DMatCCi, addition)
{
  M2::ARingCCi R(100);
  testMatrixAdd<DMat<M2::ARingCCi>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingCCi>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
