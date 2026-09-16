#include <string>

#include <vector>
#include <memory>
#include <gtest/gtest.h>

#include "error.h"
#include "buffer.hpp"
#include "util.hpp"
#include "rings/ring.hpp"
#include "rings/polyring.hpp"
#include "interface/monomial-ordering.h"
#include "interface/monoid.h"
#include "interface/ring.h"
#include "interface/aring.h"

#include "unit-tests/util-polyring-creation.hpp"

TEST(PolyRing, createDegreesRing)
{
  // Degree rings preserve the requested grading variables over ZZ.
  const PolynomialRing* DR = degreeRing(1);
  EXPECT_FALSE(error());
  ASSERT_NE(DR, nullptr);

  EXPECT_EQ(DR->getCoefficients(), globalZZ);
  EXPECT_EQ(DR->n_vars(), 1);
}

TEST(PolyRing, createDegreesRing2)
{
  // Degree rings preserve the requested grading variables over ZZ.
  const PolynomialRing* DR = degreeRing({"t1", "t2"});
  EXPECT_FALSE(error());
  ASSERT_NE(DR, nullptr);

  EXPECT_EQ(DR->getCoefficients(), globalZZ);
  EXPECT_EQ(DR->getMonoid()->variableNames(),
            (std::vector<std::string> {"t1", "t2"}));
}

TEST(PolyRing, create1)
{
  // Construction retains the four variable names and the coefficient field.

  // Create coefficient ring
  const Ring* kk = rawARingZZpFlint(101);
  ASSERT_NE(kk, nullptr);

  // Equal degrees select the standard graded reverse lexicographic order.
  std::vector<std::string> varnames {"a", "b", "c", "d"};
  std::vector<int> degs {1, 1, 1, 1};
  std::vector<int> heft {1};

  MonomialOrdering* mo =
      rawGRevLexMonomialOrdering(stdvector_to_M2_arrayint(degs), 32);
  const Monoid* M = Monoid::create(mo, degreeRing(1), varnames, degs, heft);
  ASSERT_NE(M, nullptr);

  const PolynomialRing* R = PolyRing::create(kk, M);

  ASSERT_NE(R, nullptr);
  EXPECT_EQ(R->getMonoid()->variableNames(),
            (std::vector<std::string> {"a", "b", "c", "d"}));
  EXPECT_EQ(R->characteristic(), 101);
}

TEST(PolyRing, createSimple)
{
  // Construction retains the four variable names and the coefficient field.

  // Create coefficient ring
  const PolynomialRing* R = simplePolynomialRing(101, {"a", "b", "c", "d"});

  ASSERT_NE(R, nullptr);
  EXPECT_EQ(R->getMonoid()->variableNames(),
            (std::vector<std::string> {"a", "b", "c", "d"}));
  EXPECT_EQ(R->characteristic(), 101);
}
