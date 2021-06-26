// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "mathicgb/QuadMatrixBuilder.hpp"

#include "mathicgb/Poly.hpp"
#include "mathicgb/PolyRing.hpp"
#include "mathicgb/io-util.hpp"
#include "mathicgb/Basis.hpp"
#include "mathicgb/QuadMatrix.hpp"
#include "mathicgb/MathicIO.hpp"
#include <gtest/gtest.h>

using namespace mgb;

namespace {
  std::string monToStr(const PolyRing& ring, ConstMonomial a) {
    std::ostringstream out;
    ring.monomialDisplay(out, a, false, true);
    return out.str();
  }

  template<class Monoid>
  std::string monoToStr(
    const Monoid& monoid,
    typename Monoid::ConstMonoRef a
  ) {
    std::ostringstream out;
    MathicIO<Monoid>().writeMonomial(monoid, false, a, out);
    return out.str();
  }

  void createColumns(const char* left, const char* right, QuadMatrixBuilder& b)
  {
    const PolyRing& ring = b.ring();
    {
      std::istringstream in(left);
      Scanner scanner(in);
      auto p = MathicIO<>().readPolyDoNotOrder(ring, true, scanner);
      size_t colCount = 0;
      for (auto it = p.begin(); it != p.end(); ++it) {
        QuadMatrixBuilder::LeftRightColIndex lrCol =
          b.createColumnLeft(it.mono()).first;
        ASSERT_TRUE(lrCol.left());
        ASSERT_FALSE(lrCol.right());
        auto col = lrCol.leftIndex();
        ASSERT_EQ(col, lrCol.index());
        ASSERT_EQ(colCount, col);
        ++colCount;
      }
    }
    {
      std::istringstream in(right);
      Scanner scanner(in);
      auto p = MathicIO<>().readPolyDoNotOrder(ring, true, scanner);
      size_t colCount = 0;
      for (auto it = p.begin(); it != p.end(); ++it) {
        QuadMatrixBuilder::LeftRightColIndex lrCol =
          b.createColumnRight(it.mono()).first;
        ASSERT_TRUE(lrCol.right());
        ASSERT_FALSE(lrCol.left());
        auto col = lrCol.rightIndex();
        ASSERT_EQ(col, lrCol.index());
        ASSERT_EQ(colCount, col);
        ++colCount;
      }
    }
  }
}

TEST(QuadMatrixBuilder, Empty) {
  // test a builder with no rows and no columns
  PolyRing ring(2, PolyRing::Monoid(0));
  QuadMatrixBuilder::Map map(ring);
  QuadMatrixBuilder::Monomials monoLeft;
  QuadMatrixBuilder::Monomials monoRight;
  QuadMatrixBuilder b(ring, map, monoLeft, monoRight);
  const char* matrixStr = 
    "Left columns:\n"
    "Right columns:\n"
    "matrix with no rows | matrix with no rows\n"
    "                    |                    \n"
    "matrix with no rows | matrix with no rows\n";
  auto matrix = b.buildMatrixAndClear();
  matrix.leftColumnMonomials = monoLeft;
  matrix.rightColumnMonomials = monoRight;
  ASSERT_EQ(matrixStr, matrix.toString());
}

TEST(QuadMatrixBuilder, Construction) {
  std::unique_ptr<PolyRing> ring(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  QuadMatrixBuilder::Map map(*ring);
  QuadMatrixBuilder::Monomials monoLeft;
  QuadMatrixBuilder::Monomials monoRight;
  QuadMatrixBuilder b(*ring, map, monoLeft, monoRight);
  createColumns("a<1>+<0>", "bc<0>+b<0>+c<0>", b);

  // top row: nothing, nothing
  b.rowDoneTopLeftAndRight();

  // top row: 0#1 1#2, 2#3
  b.appendEntryTopLeft(0, 1);
  b.appendEntryTopLeft(1, 2);
  b.appendEntryTopRight(2, 3);
  b.rowDoneTopLeftAndRight();

  // bottom row: 1#4, nothing
  b.appendEntryBottomLeft(1,4);
  b.rowDoneBottomLeftAndRight();

  // bottom row: nothing, 0#5
  b.appendEntryBottomRight(0,5);
  b.rowDoneBottomLeftAndRight();

  // bottom row: nothing, nothing
  b.rowDoneBottomLeftAndRight();

  const char* matrixStr =
    "Left columns: a 1\n"
    "Right columns: bc b c\n"
    "0:         | 0:    \n"
    "1: 0#1 1#2 | 1: 2#3\n"
    "           |       \n"
    "0: 1#4     | 0:    \n"
    "1:         | 1: 0#5\n"
    "2:         | 2:    \n";
  auto matrix = b.buildMatrixAndClear();
  matrix.leftColumnMonomials = monoLeft;
  matrix.rightColumnMonomials = monoRight;
  ASSERT_EQ(matrixStr, matrix.toString());
}

TEST(QuadMatrixBuilder, ColumnQuery) {
  std::unique_ptr<PolyRing> ring(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  QuadMatrixBuilder::Map map(*ring);
  QuadMatrixBuilder::Monomials monoLeft;
  QuadMatrixBuilder::Monomials monoRight;
  QuadMatrixBuilder b(*ring, map, monoLeft, monoRight);
  createColumns("a<1>+<0>", "b<0>+c<0>+bc<0>", b);

  // coefficient 1X=left, 2X=right, 30=not there, % 10 = column index
  std::istringstream in
    ("10a<1>+11<0>+20b<0>+21c<0>+22bc<0>+30ab<0>+30e<0>+10a<1>");
  Scanner scanner(in);
  auto p = MathicIO<>().readPolyDoNotOrder(b.ring(), true, scanner);
  for (auto it = p.begin(); it != p.end(); ++it) {
    const QuadMatrixBuilder::LeftRightColIndex* col =
      MonomialMap<QuadMatrixBuilder::LeftRightColIndex>::Reader(map).
        find(it.mono()).first;
    if (it.coef() / 10 == 3)
      ASSERT_EQ(col, static_cast<void*>(0));
    else {
      ASSERT_TRUE(col != static_cast<void*>(0));
      ASSERT_EQ(it.coef() % 10, col->index());
      if (it.coef() / 10 == 2)
        ASSERT_TRUE(col->right());
      else
        ASSERT_TRUE(col->left());
    }
  }
}

TEST(QuadMatrixBuilder, SortColumns) {
  // construct builder and reverse lex order
  std::unique_ptr<PolyRing> ring(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  Basis basis(*ring);
  
  // one row top, no rows bottom, no columns
  {
    QuadMatrixBuilder::Map map(*ring);
    QuadMatrixBuilder::Monomials monoLeft;
    QuadMatrixBuilder::Monomials monoRight;
    QuadMatrixBuilder b(*ring, map, monoLeft, monoRight);
    b.rowDoneTopLeftAndRight();
    auto matrix = b.buildMatrixAndClear();
    matrix.sortColumnsLeftRightParallel();
    const char* matrixStr = 
      "Left columns:\n"
      "Right columns:\n"
      "0:                  | 0:                 \n"
      "                    |                    \n"
      "matrix with no rows | matrix with no rows\n";
    ASSERT_EQ(matrixStr, matrix.toString());
  }

  {
    QuadMatrixBuilder::Map map(*ring);
    QuadMatrixBuilder::Monomials monoLeft;
    QuadMatrixBuilder::Monomials monoRight;
    QuadMatrixBuilder b(*ring, map, monoLeft, monoRight);
    createColumns("<0>+a<0>", "b<0>+bcd<0>+bc<0>", b);
    b.appendEntryTopLeft(0,1);
    b.appendEntryTopLeft(1,2);
    b.appendEntryTopRight(0,3);
    b.appendEntryTopRight(1,4);
    b.appendEntryTopRight(2,5);
    b.rowDoneTopLeftAndRight();

    b.appendEntryBottomLeft(0,6);
    b.appendEntryBottomLeft(1,7);
    b.appendEntryBottomRight(0,8);
    b.appendEntryBottomRight(1,9);
    b.appendEntryBottomRight(2,10);
    b.rowDoneBottomLeftAndRight();
    auto matrix = b.buildMatrixAndClear();
    matrix.leftColumnMonomials = monoLeft;
    matrix.rightColumnMonomials = monoRight;

    const char* matrixStr1 =
      "Left columns: 1 a\n"
      "Right columns: b bcd bc\n"
      "0: 0#1 1#2 | 0: 0#3 1#4 2#5 \n"
      "           |                \n"
      "0: 0#6 1#7 | 0: 0#8 1#9 2#10\n";
    ASSERT_EQ(matrixStr1, matrix.toString());

    const char* matrixStr2 =
      "Left columns: a 1\n"
      "Right columns: bcd bc b\n"
      "0: 1#1 0#2 | 0: 2#3 0#4 1#5 \n"
      "           |                \n"
      "0: 1#6 0#7 | 0: 2#8 0#9 1#10\n";
    matrix.sortColumnsLeftRightParallel();
    ASSERT_EQ(matrixStr2, matrix.toString());

    matrix.sortColumnsLeftRightParallel();
    ASSERT_EQ(matrixStr2, matrix.toString());
  }
}

TEST(QuadMatrixBuilder, BuildAndClear) {
  std::unique_ptr<PolyRing> ring(ringFromString("32003 6 1\n1 1 1 1 1 1"));
  QuadMatrixBuilder::Map map(*ring);
  QuadMatrixBuilder::Monomials monoLeft;
  QuadMatrixBuilder::Monomials monoRight;
  QuadMatrixBuilder b(*ring, map, monoLeft, monoRight);
  createColumns("a<1>+<0>", "b<0>+c<0>+bc<0>", b);

  b.appendEntryTopLeft(1, 1);
  b.appendEntryTopRight(2, 2);
  b.rowDoneTopLeftAndRight();

  b.appendEntryBottomLeft(1, 3);
  b.appendEntryBottomRight(2, 4);
  b.rowDoneBottomLeftAndRight();

  QuadMatrix qm(b.buildMatrixAndClear());
  qm.leftColumnMonomials = monoLeft;
  qm.rightColumnMonomials = monoRight;

  // test that the quad matrix is right
  ASSERT_EQ("0: 1#1\n", qm.topLeft.toString());
  ASSERT_EQ("0: 2#2\n", qm.topRight.toString());
  ASSERT_EQ("0: 1#3\n", qm.bottomLeft.toString());
  ASSERT_EQ("0: 2#4\n", qm.bottomRight.toString());
  ASSERT_EQ(2, qm.leftColumnMonomials.size());
  ASSERT_EQ(3, qm.rightColumnMonomials.size());

  const auto& monoid = ring->monoid();
  ASSERT_EQ("a", monoToStr(monoid, *qm.leftColumnMonomials[0]));
  ASSERT_EQ("b", monoToStr(monoid, *qm.rightColumnMonomials[0]));
}
