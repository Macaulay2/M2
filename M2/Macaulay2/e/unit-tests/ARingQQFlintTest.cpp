// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "basic-rings/aring-QQ-flint.hpp"

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
  // static_cast avoids odr-using ringID, which has no out-of-line definition.
  EXPECT_EQ(static_cast<int>(RingType::ringID),
            static_cast<int>(M2::ring_QQFlint));
  EXPECT_EQ(ringName(R), "QQFlint");
}

TEST_F(ARingQQFlint, Formatting)
{
  RingType::Element a(R), b(R);
  buffer o;

  ARingElementGenerator<RingType> gen(R);
  gen.nextElement(a);
  gen.nextElement(b);
  R.divide(a, a, b);
  R.elem_text_out(o, a, true, false, false);
  EXPECT_EQ(std::string(o.str()), "24/23");
}

TEST_F(ARingQQFlint, Conversions)
{
  RingType::Element a(R), expected(R);
  ring_elem f = globalZZ->from_long(-12);

  {
    SCOPED_TRACE("promote: ZZ --> QQ");
    EXPECT_TRUE(R.promote(globalZZ, f, a));
    R.set(expected, -12);
    EXPECT_TRUE(R.is_equal(a, expected));
  }

  {
    SCOPED_TRACE("promote: nothing else promotes, and nothing lifts");
    EXPECT_FALSE(R.promote(globalQQ, f, a));
    EXPECT_FALSE(R.lift(globalZZ, a, f));
  }
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
