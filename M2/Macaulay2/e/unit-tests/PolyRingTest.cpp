// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <vector>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "../error.h"
#include "../buffer.hpp"
#include "../util.hpp"
#include "../ring.hpp"
#include "../polyring.hpp"
#include "../interface/monomial-ordering.h"
#include "../interface/monoid.h"
#include "../interface/ring.h"
#include "../interface/aring.h"

#include "util-polyring-creation.hpp"

TEST(PolyRing, createDegreesRing)
{
  const Ring* DR = degreeRing(1);
  EXPECT_FALSE(error());
  EXPECT_TRUE(DR != nullptr);

  buffer o;
  DR->text_out(o);
  std::cout << "ring is " << o.str() << std::endl;
}

TEST(PolyRing, createDegreesRing2)
{
  const Ring* DR = degreeRing({"t1", "t2"});
  EXPECT_FALSE(error());
  EXPECT_TRUE(DR != nullptr);

  buffer o;
  DR->text_out(o);
  std::cout << "ring is " << o.str() << std::endl;
}

TEST(PolyRing, create1)
{
  // Creaating a polynomial ring from C++.
  // Plan: this should be a simple constructor call!

  // Create coefficient ring
  const Ring* kk = rawARingZZpFlint(101); // or IM2_Ring_ZZ(), IM2_Ring_QQ(), and others...
  EXPECT_TRUE(kk != nullptr);

  // Now create the monomial order.  This one is a pain in the butt!
  std::vector<std::string> varnames { "a", "b", "c", "d" };
  std::vector<int> degs {1,1,1,1};
  std::vector<int> heft {1};

  MonomialOrdering* mo = rawGRevLexMonomialOrdering(stdvector_to_M2_arrayint(degs), 32);
  const Monoid* M = Monoid::create(
                             mo,
                             varnames,
                             degreeRing(1),
                             degs,
                             heft
                             );
  EXPECT_TRUE(M != nullptr);

  const Ring* R = PolyRing::create(kk, M);

  EXPECT_TRUE(R != nullptr);
  buffer o;
  R->text_out(o);
  std::cout << "ring is " << o.str() << std::endl;
}

TEST(PolyRing, createSimple)
{
  // Creaating a polynomial ring from C++.
  // Plan: this should be a simple constructor call!

  // Create coefficient ring
  const Ring* R = simplePolynomialRing(101, { "a", "b", "c", "d" });

  EXPECT_TRUE(R != nullptr);
  buffer o;
  R->text_out(o);
  std::cout << "ring is " << o.str() << std::endl;
}
