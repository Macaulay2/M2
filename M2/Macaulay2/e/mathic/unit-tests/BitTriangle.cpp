#include "mathic/BitTriangle.h"
#include <gtest/gtest.h>

TEST(BitTriangle, NoOp) {
  mathic::BitTriangle tri;
};

TEST(BitTriangle, emptyAndColumnCount) {
  mathic::BitTriangle tri;
  ASSERT_TRUE(tri.empty());
  ASSERT_EQ(0u, tri.columnCount());

  tri.addColumn();
  ASSERT_FALSE(tri.empty());
  ASSERT_EQ(1u, tri.columnCount());

  tri.addColumn();
  ASSERT_FALSE(tri.empty());
  ASSERT_EQ(2u, tri.columnCount());
}

TEST(BitTriangle, getSetBit) {
  size_t const colCount = 20; // consider colCount columns
  size_t const modPattern = 11;
  mathic::BitTriangle tri;
  for (size_t col = 0; col < colCount; ++col) {
	tri.addColumn();
	for (size_t row = 0; row < col; ++row) {
	  ASSERT_FALSE(tri.bit(col, row));
	  ASSERT_FALSE(tri.bitUnordered(col, row));
	  ASSERT_FALSE(tri.bitUnordered(row, col));

	  tri.setBit(col, row, true);
	  ASSERT_TRUE(tri.bit(col, row));
	  ASSERT_TRUE(tri.bitUnordered(col, row));
	  ASSERT_TRUE(tri.bitUnordered(row, col));

	  tri.setBitUnordered(col, row, false);
	  ASSERT_FALSE(tri.bit(col, row));
	  ASSERT_FALSE(tri.bitUnordered(col, row));
	  ASSERT_FALSE(tri.bitUnordered(row, col));

	  tri.setBitUnordered(row, col, true);
	  ASSERT_TRUE(tri.bit(col, row));
	  ASSERT_TRUE(tri.bitUnordered(col, row));
	  ASSERT_TRUE(tri.bitUnordered(row, col));

	  // set bits mod 11 since 3 is relatively prime to 2
	  // so any mod 2^k pattern is avoided and 11>8 so we do
	  // get bytes that are all ones.
	  bool const value = ((row + col) % modPattern != 0);

	  tri.setBit(col, row, value);
	  ASSERT_EQ(value, tri.bit(col, row));
	  ASSERT_EQ(value, tri.bitUnordered(col, row));
	  ASSERT_EQ(value, tri.bitUnordered(row, col));
	}
  }
  ASSERT_EQ(colCount, tri.columnCount());

  // check that the pattern of bits is preserved
  for (size_t col = 0; col < colCount; ++col) {
	for (size_t row = 0; row < col; ++row) {
	  bool const value = ((row + col) % 11 != 0);
	  ASSERT_EQ(value, tri.bit(col, row));
	  ASSERT_EQ(value, tri.bitUnordered(col, row));
	  ASSERT_EQ(value, tri.bitUnordered(row, col));
	}
  }
}
