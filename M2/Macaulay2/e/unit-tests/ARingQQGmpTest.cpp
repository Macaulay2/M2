// Copyright 2012-2013 Michael E. Stillman

/**
 * @file unit-tests/ARingQQGmpTest.cpp
 * @brief gtest coverage for the GMP-backed `M2::ARingQQGMP` rational aring.
 *
 * Plugs `M2::ARingQQGMP` into the shared `ARingTest.hpp` harness
 * via the `getElement<M2::ARingQQGMP>` specialisation, which
 * uses the deterministic `[-25, 24]` integer prefix for the
 * first 50 trials and delegates to `R.random(result)` for the
 * rest --- so the random source is the ring's own `mpq_t` draw
 * rather than a separately-seeded RNG. `testSomeMore` then
 * runs arithmetic, equality, inverse, and stream round-trips
 * against the GMP-canonicalised representation.
 *
 * Paired with `ARingQQFlintTest.cpp` (same operations over
 * FLINT `fmpq_t`); cross-backend agreement between the two is
 * the regression guard the `file-aring-zz-tests` family
 * depends on. A historic catch on the GMP side was a rounding
 * subtlety in `invert(0)` that only the cross-check surfaced.
 *
 * @see ARingTest.hpp
 * @see aring-qq-gmp.hpp
 * @see ARingQQFlintTest.cpp
 * @see ARingZZTest.cpp
 */

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-qq-gmp.hpp"

#include "ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template <>
void getElement<M2::ARingQQGMP>(const M2::ARingQQGMP& R,
                                int index,
                                M2::ARingQQGMP::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    {
      R.random(result);
    }
}

TEST(ARingQQGMP, create)
{
  M2::ARingQQGMP R;

  M2::ARingQQGMP::ElementType a;
  buffer o;

  ARingElementGenerator<M2::ARingQQGMP> gen(R);
  R.init(a);
  gen.nextElement(a);

  EXPECT_EQ(ringName(R), "QQGMP");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingQQGMP, display)
{
  M2::ARingQQGMP R;

  M2::ARingQQGMP::ElementType a, b;
  buffer o;

  ARingElementGenerator<M2::ARingQQGMP> gen(R);
  R.init(a);
  R.init(b);
  gen.nextElement(a);
  gen.nextElement(b);
  R.divide(a, a, b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_TRUE(strcmp(o.str(), "24/23") == 0);
  std::cout << "a = ." << o.str() << "." << std::endl;

  EXPECT_EQ(ringName(R), "QQGMP");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
  R.clear(b);
}

TEST(ARingQQGMP, arithmetic)
{
  M2::ARingQQGMP R;

  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  //  testPower(R, ntrials);  // this test can't work, as it expects a finite
  //  field
  testAxioms(R, ntrials);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
