// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-RRi.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingRRi>(const M2::ARingRRi& R,
                              int index,
                              M2::ARingRRi::ElementType& result);

TEST(DMatRRi, addition)
{
  M2::ARingRRi R(100);
  testMatrixAdd<DMat<M2::ARingRRi>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingRRi>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
