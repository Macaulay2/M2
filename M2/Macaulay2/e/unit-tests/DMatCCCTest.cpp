// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-CCC.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Use the deterministic-prefix-then-random generator contract.
template <>
void getElement<M2::ARingCCC>(const M2::ARingCCC& R,
                              int index,
                              M2::ARingCCC::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

TEST(DMatCCC, addition)
{
  M2::ARingCCC R(100);
  testMatrixAdd<DMat<M2::ARingCCC>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingCCC>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
