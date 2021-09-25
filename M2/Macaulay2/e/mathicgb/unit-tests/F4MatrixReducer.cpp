// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"

#include "mathicgb/F4MatrixReducer.hpp"
#include "mathicgb/SparseMatrix.hpp"
#include "mathicgb/QuadMatrix.hpp"
#include "mathicgb/io-util.hpp"
#include "mathicgb/Poly.hpp"
#include "mathicgb/MathicIO.hpp"
#include <gtest/gtest.h>
#include <sstream>

using namespace mgb;

TEST(F4MatrixReducer, Reduce) {
  auto ring = ringFromString("101 6 1\n10 1 1 1 1 1");
  QuadMatrix m(*ring);

  std::istringstream in("a4+a3+a2+a1+b5+b4+b3+b2+b1");
  Scanner scanner(in);
  auto p = MathicIO<>().readPoly(*ring, false, scanner);
  size_t count = 0;
  for (auto it = p.begin(); it != p.end(); ++it) {
    if (count < 4)
      m.leftColumnMonomials.push_back(it.mono());
    else
      m.rightColumnMonomials.push_back(it.mono());
    ++count;
  }

  // top left
  m.topLeft.clear();
  m.topLeft.appendEntry(0, 1);
  m.topLeft.appendEntry(1, 2);
  m.topLeft.appendEntry(3, 3);
  m.topLeft.rowDone();
  m.topLeft.appendEntry(1, 1);
  m.topLeft.appendEntry(2, 3);
  m.topLeft.rowDone();
  m.topLeft.appendEntry(2, 1);
  m.topLeft.appendEntry(3, 7);
  m.topLeft.rowDone();
  m.topLeft.appendEntry(3, 1);
  m.topLeft.rowDone();

  // top right
  m.topRight.clear();
  m.topRight.appendEntry(2,8);
  m.topRight.rowDone();
  m.topRight.appendEntry(3,9);
  m.topRight.rowDone();
  m.topRight.appendEntry(4,10);
  m.topRight.rowDone();
  m.topRight.rowDone();

  // bottom left
  m.bottomLeft.clear();
  m.bottomLeft.rowDone();
  m.bottomLeft.appendEntry(1, 9);
  m.bottomLeft.rowDone();
  m.bottomLeft.appendEntry(0, 2);
  m.bottomLeft.appendEntry(1, 99); // 100 = -2 mod 101
  m.bottomLeft.appendEntry(2, 83); // 83 = -18 mod 101
  m.bottomLeft.appendEntry(3, 6);
  m.bottomLeft.rowDone();
  m.bottomLeft.appendEntry(0, 1);
  m.bottomLeft.appendEntry(1, 1);
  m.bottomLeft.appendEntry(3, 24);
  m.bottomLeft.rowDone();
  m.bottomLeft.rowDone();
  m.bottomLeft.appendEntry(3, 100);
  m.bottomLeft.rowDone();

  // bottom right
  m.bottomRight.clear();
  m.bottomRight.rowDone();
  m.bottomRight.appendEntry(1, 2);
  m.bottomRight.appendEntry(3, 11);
  m.bottomRight.rowDone();
  m.bottomRight.appendEntry(2, 16);
  m.bottomRight.appendEntry(3, 47);
  m.bottomRight.rowDone();
  m.bottomRight.appendEntry(0, 1);
  m.bottomRight.appendEntry(2, 12);
  m.bottomRight.appendEntry(3, 13);
  m.bottomRight.appendEntry(4, 41);
  m.bottomRight.rowDone();
  m.bottomRight.appendEntry(0, 2);
  m.bottomRight.appendEntry(1, 2);
  m.bottomRight.appendEntry(2, 8);
  m.bottomRight.appendEntry(3, 75);
  m.bottomRight.appendEntry(4, 90);
  m.bottomRight.rowDone();
  m.bottomRight.appendEntry(0, 1);
  m.bottomRight.appendEntry(1, 1);
  m.bottomRight.appendEntry(2, 4);
  m.bottomRight.appendEntry(3, 88);
  m.bottomRight.appendEntry(4, 45);
  m.bottomRight.rowDone();

  MATHICGB_ASSERT(m.debugAssertValid());
  const char* origStr = 
    "Left columns: a4 a3 a2 a\n"
    "Right columns: b5 b4 b3 b2 b\n"
    "0: 0#1 1#2 3#3       | 0: 2#8                  \n"
    "1: 1#1 2#3           | 1: 3#9                  \n"
    "2: 2#1 3#7           | 2: 4#10                 \n"
    "3: 3#1               | 3:                      \n"
    "                     |                         \n"
    "0:                   | 0:                      \n" // zero row
    "1: 1#9               | 1: 1#2 3#11             \n" // becomes second row
    "2: 0#2 1#99 2#83 3#6 | 2: 2#16 3#47            \n" // zero on left red.  
    "3: 0#1 1#1 3#24      | 3: 0#1 2#12 3#13 4#41   \n" // becomes first row
    "4:                   | 4: 0#2 1#2 2#8 3#75 4#90\n" // zero on right red.
    "5: 3#100             | 5: 0#1 1#1 2#4 3#88 4#45\n"; // zero on right red.

  ASSERT_EQ(origStr, m.toString()) << "Printed m:\n" << m;

  SparseMatrix reduced
    (F4MatrixReducer(ring->charac()).reducedRowEchelonFormBottomRight(m));

  const char* redStr =
    "0: 0#1 2#4 3#22 4#11\n"
    "1: 1#1 3#66 4#34\n";
  reduced.sortRowsByIncreasingPivots();
  ASSERT_EQ(redStr, reduced.toString()) << "Printed reduced:\n" << reduced;
}
