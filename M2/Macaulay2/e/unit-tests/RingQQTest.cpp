// Copyright 2013 Michael E. Stillman

/**
 * @file unit-tests/RingQQTest.cpp
 * @brief gtest coverage for the legacy `RingQQ` rational ring (`globalQQ`).
 *
 * Specialises `RingTest.hpp`'s `getElement<RingQQ>` with the
 * deterministic `[-25, 24]` integer prefix followed by random
 * rationals drawn from `rawRandomQQ(maxH)` with `maxH = 10^11`;
 * the `gmp_QQ` result feeds through `R.from_rational` so any
 * canonicalisation bugs surface in the test loop. Standard
 * arithmetic and identity assertions run against `globalQQ`
 * --- the engine's shared `RingQQ` instance --- and double as a
 * regression check on `aring-glue.hpp`, which threads the
 * modern `ARingQQ*` backends back into the legacy `Ring`
 * surface the interpreter still uses.
 *
 * Companion to `RingZZTest.cpp`, `RingZZpTest.cpp`,
 * `RingRRRTest.cpp`, `RingCCCTest.cpp`, and `RingTowerTest.cpp`
 * (all under the `file-ring-tests` markdown). Cross-checks
 * against the `aring`-side `ARingQQGmpTest.cpp` /
 * `ARingQQFlintTest.cpp` keep the two interfaces in sync.
 *
 * @see RingTest.hpp
 * @see aring-glue.hpp
 * @see ARingQQGmpTest.cpp
 * @see interface/random.h
 */

#include "RingTest.hpp"

static bool maxH_initialized = false;
static mpz_t maxH;

#include "aring-glue.hpp"
#include "ZZp.hpp"

template <>
ring_elem getElement<RingQQ>(const RingQQ &R, int index)
{
  if (index < 50) return R.from_long(index - 25);
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  gmp_QQ a1 = rawRandomQQ(maxH);
  ring_elem result;
  EXPECT_TRUE(R.from_rational(a1, result));
  return result;
}
////////////////////////////////////////////////////////
TEST(RingQQ, create)
{
  const Ring *R = globalQQ;
  EXPECT_TRUE(R != nullptr);

  EXPECT_TRUE(dynamic_cast<const Z_mod *>(R) == nullptr);
  EXPECT_TRUE(dynamic_cast<const RingQQ *>(R) != nullptr);
  EXPECT_EQ(R->coefficient_type(), Ring::COEFF_QQ);
  EXPECT_FALSE(R->is_ZZ());
  EXPECT_EQ(ringName(*R), "QQGMP");
}
TEST(RingQQ, ones)
{
  const Ring *R = globalQQ;
  EXPECT_TRUE(R->is_equal(R->one(), R->from_long(1)));
  EXPECT_TRUE(R->is_equal(R->minus_one(), R->from_long(-1)));
  EXPECT_TRUE(R->is_equal(R->zero(), R->from_long(0)));
  EXPECT_TRUE(R->is_zero(R->from_long(0)));
}
TEST(RingQQ, negate) { testRingNegate(globalQQ, ntrials); }
TEST(RingQQ, add) { testRingAdd(globalQQ, ntrials); }
TEST(RingQQ, subtract) { testRingSubtract(globalQQ, ntrials); }
TEST(RingQQ, multDivide) { testRingDivide(globalQQ, ntrials); }
TEST(RingQQ, axioms) { testRingAxioms(globalQQ, ntrials); }
TEST(RingQQ, power) { testRingPower(globalQQ, ntrials); }
TEST(RingQQ, syzygy) { testRingSyzygy(globalQQ, ntrials); }
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
