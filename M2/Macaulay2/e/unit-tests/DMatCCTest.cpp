// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-CC.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
