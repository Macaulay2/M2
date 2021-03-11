// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"

#include "mathicgb/Poly.hpp"
#include "mathicgb/PolyRing.hpp"
#include "mathicgb/F4MatrixBuilder.hpp"
#include "mathicgb/Basis.hpp"
#include "mathicgb/PolyBasis.hpp"
#include "mathicgb/io-util.hpp"
#include "mathicgb/mtbb.hpp"
#include "mathicgb/MathicIO.hpp"
#include <gtest/gtest.h>
#include <memory>

using namespace mgb;

namespace {
  // We need a struct to keep the ring and so on alive after
  // construction - we cannot just return an object.
  //
  // @todo: This whole thing is fairly ridiculous - some kind of more
  // general dependency injection mechanism might be nice here.
  struct BuilderMaker {
    BuilderMaker():
      mRing(ringFromString("101 6 1\n1 1 1 1 1 1")),
      mIdeal(*mRing),
      mBasis(
        *mRing,
        MonoLookup::makeFactory(mRing->monoid(), 1)->make(true, true)
      )
    {}

    const Poly& addBasisElement(const std::string& str) {
      std::istringstream in(str);
      Scanner scanner(in);
      auto p = make_unique<Poly>
        (MathicIO<>().readPoly(*mRing, false, scanner));
      mBasis.insert(std::move(p));
      return mBasis.poly(mBasis.size() - 1);
    }

    F4MatrixBuilder& create() {
      MATHICGB_ASSERT(mBuilder.get() == 0);
      mBuilder.reset(new F4MatrixBuilder(mBasis));
      return *mBuilder;
    }

    const PolyRing& ring() const {return *mRing;}
     
  private:
    std::unique_ptr<PolyRing> mRing;
    Basis mIdeal;
    PolyBasis mBasis;
    std::unique_ptr<F4MatrixBuilder> mBuilder;
  };
}

TEST(F4MatrixBuilder, Empty) {
  for (int threadCount = 1; threadCount < 4; ++threadCount) {
    mgb::mtbb::task_scheduler_init scheduler(threadCount);
    BuilderMaker maker;
    F4MatrixBuilder& builder = maker.create();

    QuadMatrix matrix(maker.ring());
    builder.buildMatrixAndClear(matrix);
    ASSERT_EQ(0, matrix.topLeft.rowCount());
    ASSERT_EQ(0, matrix.bottomLeft.rowCount());
    ASSERT_EQ(0, matrix.topLeft.computeColCount());
    ASSERT_EQ(0, matrix.topRight.computeColCount());
    ASSERT_EQ(0, matrix.leftColumnMonomials.size());
    ASSERT_EQ(0, matrix.rightColumnMonomials.size());
  }
}

TEST(F4MatrixBuilder, SPair) {
  for (int threadCount = 1; threadCount < 4; ++threadCount) {
    mgb::mtbb::task_scheduler_init scheduler(threadCount);
    BuilderMaker maker;
    const Poly& p1 = maker.addBasisElement("a4c2-d");
    const Poly& p2 = maker.addBasisElement("a4b+d");
    // S-pair of p1 and p2 is -c2d-bd
    const Poly& p3 = maker.addBasisElement("c2d+3");
    F4MatrixBuilder& builder = maker.create();
    builder.addSPolynomialToMatrix(p1, p2);
    QuadMatrix qm(builder.ring());
    builder.buildMatrixAndClear(qm);
    const char* const str1 = 
      "Left columns: c2d\n"
      "Right columns: bd 1\n"
      "0: 0#1   | 0: 1#3  \n"
      "         |         \n"
      "0: 0#100 | 0: 0#100\n";
    const char* const str2 = 
      "Left columns: c2d\n"
      "Right columns: bd 1\n"
      "0: 0#1 | 0: 0#1\n"
      "       |       \n"
      "0: 0#1 | 0: 1#3\n";
    std::string qmStr = qm.toString();
    ASSERT_TRUE(str1 == qmStr || str2 == qmStr) <<
      "\n** str1: " << str1 << "\n** qm: " << qmStr;
  }
}

TEST(F4MatrixBuilder, OneByOne) {
  for (int threadCount = 1; threadCount < 4; ++threadCount) {
    mgb::mtbb::task_scheduler_init scheduler(threadCount);
    BuilderMaker maker;
    const Poly& p = maker.addBasisElement("a");
    F4MatrixBuilder& builder = maker.create();
    builder.addPolynomialToMatrix(p.leadMono(), p);
    QuadMatrix qm(builder.ring());
    builder.buildMatrixAndClear(qm);
    const char* str = 
      "Left columns: a2\n"
      "Right columns:\n"
      "0: 0#1 | 0:\n"
      "       |   \n"
      "0: 0#1 | 0:\n";
    ASSERT_EQ(str, qm.toString()) << "** qm:\n" << qm;
  }
}

TEST(F4MatrixBuilder, DirectReducers) {
  for (int threadCount = 1; threadCount < 4; ++threadCount) {
    BuilderMaker maker;
    maker.addBasisElement("a6"); // reducer == to lead term
    maker.addBasisElement("a3b2+a3c"); // reducer == to lower order term
    maker.addBasisElement("c"); // reducer divides
    maker.addBasisElement("d2"); // does not divide
    F4MatrixBuilder& builder = maker.create();

    Poly p1(builder.ring());
    { 
      std::istringstream in("a3+b2+c+d");
      Scanner scanner(in);
      p1 = MathicIO<>().readPoly(builder.ring(), false, scanner);
      builder.addPolynomialToMatrix(p1.leadMono(), p1);
    }

    Poly p2(builder.ring());
    {
      std::istringstream in("a3+2b2+3c+4d");
      Scanner scanner(in);
      p2 = MathicIO<>().readPoly(builder.ring(), false, scanner);
      builder.addPolynomialToMatrix(p2.leadMono(), p2);
    }

    QuadMatrix qm(builder.ring());
    builder.buildMatrixAndClear(qm);

    const char* str =
      "Left columns: a6 a3b2 a3c\n"
      "Right columns: a3d\n"
      "0: 2#1         | 0:    \n"
      "1: 1#1 2#1     | 1:    \n"
      "2: 0#1         | 2:    \n"
      "               |       \n"
      "0: 0#1 1#1 2#1 | 0: 0#1\n"
      "1: 0#1 1#2 2#3 | 1: 0#4\n";
    qm = qm.toCanonical();
    ASSERT_EQ(str, qm.toString()) << "** qm:\n" << qm;
  }
}

TEST(F4MatrixBuilder, IteratedReducer) {
  for (int threadCount = 1; threadCount < 4; ++threadCount) {
    BuilderMaker maker;
    const Poly& p1 = maker.addBasisElement("a4-a3");
    const Poly& p2 = maker.addBasisElement("a-1");
    F4MatrixBuilder& builder = maker.create();
    builder.addPolynomialToMatrix(p1.leadMono(), p2);
    QuadMatrix qm(builder.ring());
    builder.buildMatrixAndClear(qm);
    const char* str = 
      "Left columns: a5 a4 a3 a2 a\n"
      "Right columns: 1\n"
      "0: 4#1       | 0: 0#100\n"
      "1: 3#1 4#100 | 1:      \n"
      "2: 2#1 3#100 | 2:      \n"
      "3: 1#1 2#100 | 3:      \n"
      "4: 0#1 1#100 | 4:      \n"
      "             |         \n"
      "0: 0#1 1#100 | 0:      \n";
    ASSERT_EQ(str, qm.toCanonical().toString()) << "** qm:\n" << qm;
  }
}
