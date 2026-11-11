// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-RRR.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingRRR>(const M2::ARingRRR& R,
                              int index,
                              M2::ARingRRR::ElementType& result);

TEST(DMatRRR, addition)
{
  M2::ARingRRR R(100);
  testMatrixAdd<DMat<M2::ARingRRR>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingRRR>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
