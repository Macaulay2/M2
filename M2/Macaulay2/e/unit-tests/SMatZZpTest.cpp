#include <gtest/gtest.h>

#include "basic-rings/aring-ZZp.hpp"
#include "basic-rings/aring-glue.hpp"
#include "basic-mutable-matrices/smat.hpp"

namespace {
using ZZpRing = M2::ARingZZp;
using SparseMat = SMat<ZZpRing>;

void setEntry(SparseMat& matrix, const ZZpRing& ring, size_t row, long value)
{
  ZZpRing::ElementType entry;
  ring.set_from_long(entry, value);
  matrix.set_entry(row, 0, entry);
}
}

TEST(SMatZZp, subtractsEveryCoefficient)
{
  ZZpRing ring(101);
  SparseMat left(ring, 2, 1), right(ring, 2, 1), expected(ring, 2, 1);
  setEntry(left, ring, 0, 7);
  setEntry(left, ring, 1, 11);
  setEntry(right, ring, 0, 2);
  setEntry(right, ring, 1, 3);
  setEntry(expected, ring, 0, 5);
  setEntry(expected, ring, 1, 8);

  left.subtractInPlace(right);
  EXPECT_TRUE(left.is_equal(expected));
}

TEST(SMatZZp, subtractsEmptyColumn)
{
  ZZpRing ring(101);
  SparseMat left(ring, 2, 1), right(ring, 2, 1);
  setEntry(left, ring, 0, 7);
  setEntry(left, ring, 1, 11);
  SparseMat expected(left);

  left.subtractInPlace(right);
  EXPECT_TRUE(left.is_equal(expected));
}
