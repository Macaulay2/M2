// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-ZZp-flint.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingZZpFlint>(const M2::ARingZZpFlint& R,
                                   int index,
                                   M2::ARingZZpFlint::ElementType& result);

TEST(DMatZZpFlint, addition)
{
  M2::ARingZZpFlint R(101);
  testMatrixAdd<DMat<M2::ARingZZpFlint>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingZZpFlint>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
