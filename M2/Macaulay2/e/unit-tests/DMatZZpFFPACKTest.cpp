// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-ZZp-ffpack.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingZZpFFPACK>(const M2::ARingZZpFFPACK& R,
                                    int index,
                                    M2::ARingZZpFFPACK::ElementType& result);

TEST(DMatZZpFFPACK, addition)
{
  M2::ARingZZpFFPACK R(101);
  testMatrixAdd<DMat<M2::ARingZZpFFPACK>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingZZpFFPACK>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
