#include <gtest/gtest.h>

#include "basic-rings/aring-CCi.hpp"
#include "unit-tests/ARingTest.hpp"

TEST(ARingCCi, create)
{
  M2::ARingCCi C1, C2(100);
  EXPECT_EQ(C1.get_precision(), 53);
  EXPECT_EQ(C2.get_precision(), 100);
  EXPECT_EQ(C1.characteristic(), 0);
  EXPECT_EQ(C2.characteristic(), 0);
  EXPECT_EQ(ringName(C1), "ACCi_53");
  EXPECT_EQ(ringName(C2), "ACCi_100");
}

TEST(ARingCCi, isZero)
{
  M2::ARingCCi C;
  M2::ARingCCi::ElementType a;
  C.init(a);

  // a = 0
  C.set_from_long(a, 0);
  EXPECT_TRUE(C.is_zero(a));

  // a = [0,1]
  C.set_real_part_from_doubles(a, 0, 1);
  EXPECT_FALSE(C.is_zero(a));

  // a = 1
  C.set_real_part_from_doubles(a, 1, 1);
  EXPECT_FALSE(C.is_zero(a));

  // a = 1 + [0,1]*ii
  C.set_imaginary_part_from_doubles(a, 0, 1);
  EXPECT_FALSE(C.is_zero(a));

  // a = 1 + ii
  C.set_imaginary_part_from_doubles(a, 1, 1);
  EXPECT_FALSE(C.is_zero(a));
}
