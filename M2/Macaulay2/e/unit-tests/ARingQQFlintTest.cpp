// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-QQ-flint.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <string>

#include "unit-tests/ARingTest.hpp"

typedef M2::ARingQQFlint RingType;

template <>
void getElement<RingType>(const RingType& R,
                          int index,
                          RingType::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      R.random(result);
    }
}

#include "unit-tests/ARingQQTest.hpp"

INSTANTIATE_TYPED_TEST_SUITE_P(ARingQQFlint,
                               ARingQQ,
                               ::testing::Types<RingType>);

namespace {

// Cases specific to the flint implementation.
class ARingQQFlint : public ::testing::Test
{
 protected:
  RingType R;
};

TEST_F(ARingQQFlint, Construction)
{
  // Backend identity must agree with the printed name.
  // static_cast avoids odr-using ringID, which has no out-of-line definition.
  EXPECT_EQ(static_cast<int>(RingType::ringID),
            static_cast<int>(M2::ring_QQFlint));
  EXPECT_EQ(ringName(R), "QQFlint");
}

TEST_F(ARingQQFlint, Formatting)
{
  // A worked fraction checks numerator/denominator order without generator
  // assumptions.

  RingType::Element a(R), b(R);
  buffer o;

  R.set(a, 24);
  R.set(b, 23);
  R.divide(a, a, b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_EQ(std::string(o.str()), "24/23");
}

TEST_F(ARingQQFlint, Conversions)
{
  // Promotion accepts ZZ, and unsupported source rings and lifts report
  // failure.

  RingType::Element a(R), expected(R);
  ring_elem f = globalZZ->from_long(-12);

  {
    // A negative integer exposes sign loss in promotion.
    SCOPED_TRACE("promote: ZZ --> QQ");
    EXPECT_TRUE(R.promote(globalZZ, f, a));
    R.set(expected, -12);
    EXPECT_TRUE(R.is_equal(a, expected));
  }

  {
    // Use a genuine source element even when the source ring is unsupported.
    SCOPED_TRACE("promote: unsupported source; lift unsupported");
    f = globalQQ->from_long(-12);
    R.set(a, -12);
    EXPECT_FALSE(R.promote(globalQQ, f, a));
    EXPECT_FALSE(R.lift(globalZZ, a, f));
  }
}

TEST_F(ARingQQFlint, DISABLED_fromRingElemConst)
{
  // The const accessor returns a noncanonical FLINT representation of this
  // integer. Disabled until it agrees with the copying accessor on the original
  // value. https://github.com/Macaulay2/M2/issues/4696
  RingType::Element a(R), copied(R);
  R.set(a, -24);
  ring_elem stored;
  R.to_ring_elem(stored, a);

  R.from_ring_elem(copied, stored);

  EXPECT_TRUE(R.is_equal(a, copied));
  EXPECT_TRUE(R.is_equal(a, R.from_ring_elem_const(stored)));
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
