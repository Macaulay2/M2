#include <gtest/gtest.h>
#include <cstdint>
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

