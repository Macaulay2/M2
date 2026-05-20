// Copyright 2012-2013 Michael E. Stillman

/**
 * @file unit-tests/ARingQQFlintTest.cpp
 * @brief gtest coverage for the FLINT-backed `M2::ARingQQFlint` rational aring.
 *
 * Plugs `M2::ARingQQFlint` into the `ARingTest.hpp` harness via
 * the `getElement<M2::ARingQQFlint>` specialisation: indices
 * `< 50` produce the deterministic `[-25, 24]` integer prefix
 * (covering zero, plus / minus one, and small integer
 * canonicalisations of `fmpq_t`), and the rest come from
 * `R.random(result)` so the random source is the ring's own
 * FLINT-backed draw. The standard `testSomeMore` battery then
 * runs arithmetic, equality, inverse, and stream round-trips
 * against the FLINT canonicalised representation.
 *
 * Cross-checks against `ARingQQGmpTest.cpp` (same operations
 * over GMP `mpq_t`) are the regression net the
 * `file-aring-zz-tests` family relies on; a divergence between
 * the two reveals a FLINT regression, a GMP edge case, or a
 * wrapper bug. Historic catches include a `fmpq_canonicalise`
 * sign bug on `0/-1`.
 *
 * @see ARingTest.hpp
 * @see aring-qq-flint.hpp
 * @see ARingQQGmpTest.cpp
 * @see ARingZZTest.cpp
 */

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "aring-qq-flint.hpp"

#include "ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template <>
void getElement<M2::ARingQQFlint>(const M2::ARingQQFlint& R,
                                  int index,
                                  M2::ARingQQFlint::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    {
      R.random(result);
    }
}

TEST(ARingQQFlint, create)
{
  M2::ARingQQFlint R;

  M2::ARingQQFlint::ElementType a;
  buffer o;

  ARingElementGenerator<M2::ARingQQFlint> gen(R);
  R.init(a);
  gen.nextElement(a);

  EXPECT_EQ(ringName(R), "QQFlint");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingQQFlint, display)
{
  M2::ARingQQFlint R;

  M2::ARingQQFlint::ElementType a, b;
  buffer o;

  ARingElementGenerator<M2::ARingQQFlint> gen(R);
  R.init(a);
  R.init(b);
  gen.nextElement(a);
  gen.nextElement(b);
  R.divide(a, a, b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_TRUE(strcmp(o.str(), "24/23") == 0);
  std::cout << "a = ." << o.str() << "." << std::endl;

  EXPECT_EQ(ringName(R), "QQFlint");
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), 0);
  R.clear(a);
}

TEST(ARingQQFlint, arithmetic)
{
  M2::ARingQQFlint R;

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
