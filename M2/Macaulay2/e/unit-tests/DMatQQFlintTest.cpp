// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-QQ-flint.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingQQFlint>(const M2::ARingQQFlint& R,
                                  int index,
                                  M2::ARingQQFlint::ElementType& result);

TEST(DMatQQFlint, addition)
{
  M2::ARingQQFlint R;
  testMatrixAdd<DMat<M2::ARingQQFlint>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingQQFlint>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
