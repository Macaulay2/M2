// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-ZZ-flint.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingZZ>(const M2::ARingZZ& R,
                             int index,
                             M2::ARingZZ::ElementType& result);

TEST(DMatZZ, addition)
{
  M2::ARingZZ R;
  testMatrixAdd<DMat<M2::ARingZZ>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingZZ>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
