#include <gtest/gtest.h>
#include <cstdint>
#include <initializer_list>
#include "exceptions.hpp"
#include "monomials/overflow.hpp"


volatile int x = 200;

TEST(OverflowTest, Throw)
{
    EXPECT_THROW(
        safe::ov("throw overflow exception"),
        exc::overflow_exception
    );
}

TEST(OverflowTest, SubOverflow)
{
    EXPECT_THROW(
        safe::sub(INT32_MIN, 1 - x + x),
        exc::overflow_exception
    );
}

TEST(OverflowTest, AddOverflow)
{
    EXPECT_THROW(
        safe::add(INT32_MAX, 1 - x + x),
        exc::overflow_exception
    );
}

TEST(OverflowTest, MultOverflow)
{
    EXPECT_THROW(
        safe::mult(0x8000, 0x10000 - x + x),
        exc::overflow_exception
    );
}

TEST(OverflowTest, DivOverflow)
{
    EXPECT_THROW(
        safe::div(INT32_MIN, -1 - x + x),
        exc::overflow_exception
    );
}

TEST(OverflowTest, MinusOverflow)
{
    EXPECT_THROW(
        safe::minus(INT32_MIN - x + x),
        exc::overflow_exception
    );
}

namespace {
TEST(OverflowTest, signedBoundariesAndAssignments)
{
  // Legal boundary results stay exact; failed assignments leave the destination
  // intact.
  EXPECT_EQ(safe::add(INT32_MAX, -1), INT32_MAX - 1);
  EXPECT_EQ(safe::add(INT32_MIN, 1), INT32_MIN + 1);
  EXPECT_THROW(safe::add(INT32_MIN, -1), exc::overflow_exception);
  EXPECT_EQ(safe::sub(INT32_MIN, -1), INT32_MIN + 1);
  EXPECT_THROW(safe::sub(INT32_MAX, -1), exc::overflow_exception);
  EXPECT_EQ(safe::sub_pos(2, 3), 0);
  EXPECT_EQ(safe::sub_pos(3, 3), 0);
  EXPECT_EQ(safe::sub_pos(7, 2), 5);
  EXPECT_THROW(safe::sub_pos(INT32_MAX, -1), exc::overflow_exception);
  EXPECT_EQ(safe::minus(0), 0);
  EXPECT_EQ(safe::minus(-7), 7);
  EXPECT_EQ(safe::minus(INT32_MAX), -INT32_MAX);
  EXPECT_EQ(safe::mult(INT32_MIN, 1), INT32_MIN);
  EXPECT_EQ(safe::mult(INT32_MIN, 0), 0);
  EXPECT_EQ(safe::mult(-7, 3), -21);
  EXPECT_THROW(safe::mult(INT32_MIN, -1), exc::overflow_exception);
  // A nonzero divisor is a precondition, not an overflow error path.
  EXPECT_EQ(safe::div(-7, 3), -2);
  EXPECT_EQ(safe::div(INT32_MIN, 1), INT32_MIN);
  EXPECT_EQ(safe::div(0, -1), 0);
  int32_t value = 10;
  EXPECT_EQ(safe::add_to(value, 3), 13);
  EXPECT_EQ(value, 13);
  EXPECT_EQ(safe::sub_from(value, 5), 8);
  EXPECT_EQ(value, 8);
  EXPECT_EQ(safe::mult_by(value, -3), -24);
  EXPECT_EQ(value, -24);
  EXPECT_EQ(safe::div_by(value, 4), -6);
  EXPECT_EQ(value, -6);
  value = INT32_MAX;
  EXPECT_THROW(safe::add_to(value, 1), exc::overflow_exception);
  EXPECT_EQ(value, INT32_MAX);
  EXPECT_THROW(safe::sub_from(value, -1), exc::overflow_exception);
  EXPECT_EQ(value, INT32_MAX);
  EXPECT_THROW(safe::mult_by(value, 2), exc::overflow_exception);
  EXPECT_EQ(value, INT32_MAX);
  value = INT32_MIN;
  EXPECT_THROW(safe::div_by(value, -1), exc::overflow_exception);
  EXPECT_EQ(value, INT32_MIN);
}

TEST(OverflowTest, packingLimits)
{
  // Every packed lane reserves its high bit; overflow in any lane must be
  // detected.
  EXPECT_EQ(safe::fits_7(127), 127);
  EXPECT_EQ(safe::fits_15(32767), 32767);
  EXPECT_EQ(safe::fits_31(INT32_MAX), INT32_MAX);
  EXPECT_THROW(safe::fits_7(128), exc::overflow_exception);
  EXPECT_THROW(safe::fits_7(-1), exc::overflow_exception);
  EXPECT_THROW(safe::fits_15(32768), exc::overflow_exception);
  EXPECT_THROW(safe::fits_15(-1), exc::overflow_exception);
  EXPECT_THROW(safe::fits_31(-1), exc::overflow_exception);
  EXPECT_FALSE(safe::over_1(INT32_MAX));
  EXPECT_TRUE(safe::over_1(INT32_MIN));
  EXPECT_FALSE(safe::over_2(0x7fff7fff));
  EXPECT_FALSE(safe::over_4(0x7f7f7f7f));
  EXPECT_EQ(safe::pos_add(9, 8), 17);
  EXPECT_THROW(safe::pos_add(INT32_MAX, 1), exc::overflow_exception);
  EXPECT_EQ(safe::pos_add_2(0x00020003, 0x00040005), 0x00060008);
  EXPECT_EQ(safe::pos_add_4(0x01020304, 0x04030201), 0x05050505);
  for (int shift : {0, 16})
    {
      SCOPED_TRACE(shift);
      EXPECT_TRUE(
          safe::over_2(static_cast<int32_t>(uint32_t {0x8000} << shift)));
      EXPECT_THROW(
          safe::pos_add_2(static_cast<int32_t>(uint32_t {0x7fff} << shift),
                          int32_t {1} << shift),
          exc::overflow_exception);
    }
  for (int shift : {0, 8, 16, 24})
    {
      SCOPED_TRACE(shift);
      EXPECT_TRUE(safe::over_4(static_cast<int32_t>(uint32_t {0x80} << shift)));
      EXPECT_THROW(
          safe::pos_add_4(int32_t {0x7f} << shift, int32_t {1} << shift),
          exc::overflow_exception);
    }
}
}  // namespace
