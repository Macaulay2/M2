// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include "basic-rings/aring-QQ-gmp.hpp"
#include "unit-tests/ARingMatrixTest.hpp"

// Generator defined in the corresponding ARing test file.
template <>
void getElement<M2::ARingQQGMP>(const M2::ARingQQGMP& R,
                                int index,
                                M2::ARingQQGMP::ElementType& result);

TEST(DMatQQGMP, addition)
{
  M2::ARingQQGMP R;
  testMatrixAdd<DMat<M2::ARingQQGMP>>(R, ntrials, 2, 2);
  testMatrixAdd<DMat<M2::ARingQQGMP>>(R, ntrials, 2, 3);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
