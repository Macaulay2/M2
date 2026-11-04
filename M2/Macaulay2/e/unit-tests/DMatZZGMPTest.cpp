// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-ZZ-gmp.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingZZGMP>(const M2::ARingZZGMP& R,
                                int index,
                                M2::ARingZZGMP::ElementType& result);

TEST(DMatZZGMP, addition)
{
  M2::ARingZZGMP R;
  testMatrixAdd<DMat<M2::ARingZZGMP>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingZZGMP>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
