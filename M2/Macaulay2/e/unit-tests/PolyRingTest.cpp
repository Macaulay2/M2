// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <vector>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "error.h"
#include "buffer.hpp"
#include "util.hpp"
#include "rings/ring.hpp"
#include "rings/polyring.hpp"
#include "rings/skew.hpp"
#include "interface/monomial-ordering.h"
#include "interface/monoid.h"
#include "interface/ring.h"
#include "interface/aring.h"

#include "unit-tests/util-polyring-creation.hpp"
#include "interface/ringelement.h"
#include "monomials/monomial.hpp"
#include "ring-elements/ring-element.hpp"
#include "exceptions.hpp"

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
                             degreeRing(1),
                             varnames,
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

// makeTerm builds a*monom directly, without going through ring arithmetic.
TEST(PolyRing, makeTerm)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y", "z"});
  const Ring* kk = R->getCoefficients();

  // x^2*z^3; a varpower lists variables in decreasing order
  std::vector<int> vp = varpowerOf({{2, 3}, {0, 2}});
  ring_elem t = R->makeTerm(kk, kk->from_long(1), vp.data());
  EXPECT_TRUE(R->is_equal(t, monomialOf(R, {{0, 2}, {2, 3}})));

  // the coefficient is carried, not dropped
  ring_elem u = R->makeTerm(kk, kk->from_long(7), vp.data());
  EXPECT_TRUE(R->is_equal(u, R->mult(R->from_long(7), t)));

  // the empty monomial gives back the coefficient
  std::vector<int> one = varpowerOf({});
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, kk->from_long(5), one.data()),
                          R->from_long(5)));
}

// The square of a skew commutative variable is zero, so a monomial with any
// such exponent >= 2 makes the whole term zero.  Note that the engine has no
// way to represent e^2, so failing to check this yields a bogus ring element
// rather than an error.
TEST(PolyRing, makeTermSkew)
{
  const PolynomialRing* E =
      simpleSkewPolynomialRing(101, {"e0", "e1", "e2"}, {0, 1, 2});
  ASSERT_NE(E, nullptr);
  const Ring* kk = E->getCoefficients();
  ring_elem one = kk->from_long(1);

  // a squarefree monomial is untouched
  std::vector<int> squarefree = varpowerOf({{2, 1}, {0, 1}});
  EXPECT_TRUE(E->is_equal(E->makeTerm(kk, one, squarefree.data()),
                          monomialOf(E, {{0, 1}, {2, 1}})));

  // e2^2 is zero, and so is any monomial divisible by it
  std::vector<int> square = varpowerOf({{2, 2}});
  EXPECT_TRUE(E->is_zero(E->makeTerm(kk, one, square.data())));

  std::vector<int> mixed = varpowerOf({{2, 2}, {0, 1}});
  EXPECT_TRUE(E->is_zero(E->makeTerm(kk, one, mixed.data())));

  // a nonzero coefficient does not rescue it
  EXPECT_TRUE(E->is_zero(E->makeTerm(kk, kk->from_long(7), square.data())));
}

// Only the skew variables square to zero; the commuting ones are unaffected.
TEST(PolyRing, makeTermPartiallySkew)
{
  const PolynomialRing* R =
      simpleSkewPolynomialRing(101, {"x", "y", "a", "b"}, {2, 3});
  ASSERT_NE(R, nullptr);
  const Ring* kk = R->getCoefficients();
  ring_elem one = kk->from_long(1);

  std::vector<int> xSquared = varpowerOf({{0, 2}});
  EXPECT_TRUE(R->is_equal(R->makeTerm(kk, one, xSquared.data()),
                          monomialOf(R, {{0, 2}})));

  std::vector<int> aSquared = varpowerOf({{2, 2}});
  EXPECT_TRUE(R->is_zero(R->makeTerm(kk, one, aSquared.data())));
}

// exp_is_zero, in both forms.  The 1-argument form covers every variable of
// the ring; the 2-argument form is for an exponent vector with only n entries.
TEST(PolyRing, skewExpIsZero)
{
  std::vector<int> skewvars {1, 3};
  SkewMultiplication skew(4, static_cast<int>(skewvars.size()), skewvars.data());

  int squarefree[4] {2, 1, 5, 1};  // skew variables appear at most once
  EXPECT_FALSE(skew.exp_is_zero(squarefree));

  int skewSquared[4] {1, 2, 0, 0};
  EXPECT_TRUE(skew.exp_is_zero(skewSquared));

  int lastVarCubed[4] {0, 0, 0, 3};
  EXPECT_TRUE(skew.exp_is_zero(lastVarCubed));
}

TEST(PolyRing, skewExpIsZeroBounded)
{
  std::vector<int> skewvars {1, 3};
  SkewMultiplication skew(4, static_cast<int>(skewvars.size()), skewvars.data());

  int exp[4] {1, 2, 0, 2};  // both e1 and e3 are squared

  EXPECT_TRUE(skew.exp_is_zero(exp, 4));
  EXPECT_TRUE(skew.exp_is_zero(exp, 2));   // e1 is still in range
  EXPECT_FALSE(skew.exp_is_zero(exp, 1));  // neither skew variable is in range
  EXPECT_FALSE(skew.exp_is_zero(exp, 0));

  // the unbounded form agrees with n = number of variables
  EXPECT_EQ(skew.exp_is_zero(exp), skew.exp_is_zero(exp, 4));

  // a vector with only 2 entries must not be read past the end; under a
  // sanitizer this catches the bound being ignored
  std::vector<int> shortExp {0, 1};
  EXPECT_FALSE(skew.exp_is_zero(shortExp.data(), 2));
}

TEST(PolyRing, makeTermRejectsBadInput)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y", "z"});
  const Ring* kk = R->getCoefficients();
  ring_elem one = kk->from_long(1);

  // a variable the ring does not have
  std::vector<int> tooMany = varpowerOf({{3, 1}});
  EXPECT_THROW(R->makeTerm(kk, one, tooMany.data()), exc::engine_error);

  // a negative exponent, in a ring with no Laurent variables
  std::vector<int> negative = varpowerOf({{1, -1}, {0, 2}});
  EXPECT_THROW(R->makeTerm(kk, one, negative.data()), exc::engine_error);

  // a coefficient from an unrelated ring
  const Ring* wrong = IM2_Ring_ZZp(2);
  std::vector<int> vp = varpowerOf({{0, 1}});
  EXPECT_THROW(R->makeTerm(wrong, wrong->from_long(1), vp.data()),
               exc::engine_error);
}

// An interface function that fails must return null *and* set the error flag.
// Returning a value with the flag set leaves a pending error that surfaces
// later, at an unrelated call.
TEST(PolyRing, termInterfaceReportsFailure)
{
  const PolynomialRing* R = simplePolynomialRing(0, {"x", "y"});
  const Ring* wrong = IM2_Ring_ZZp(2);
  (void)error_message();  // clears the flag

  const RingElement* a = RingElement::make_raw(wrong, wrong->from_long(1));
  std::vector<int> vp = varpowerOf({{0, 1}});
  EngineMonomial* m = EngineMonomial::make(vp.data());

  EXPECT_TRUE(IM2_RingElement_term(R, a, m) == nullptr);
  EXPECT_TRUE(error());
  (void)error_message();

  const Ring* kk = R->getCoefficients();
  const RingElement* b = RingElement::make_raw(kk, kk->from_long(3));
  const RingElement* ok = IM2_RingElement_term(R, b, m);
  EXPECT_TRUE(ok != nullptr);
  EXPECT_FALSE(error());
}
