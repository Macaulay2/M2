// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-RR.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingRR>(const M2::ARingRR& R,
                             int index,
                             M2::ARingRR::ElementType& result);

TEST(DMatRR, addition)
{
  M2::ARingRR R;
  testMatrixAdd<DMat<M2::ARingRR>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingRR>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
