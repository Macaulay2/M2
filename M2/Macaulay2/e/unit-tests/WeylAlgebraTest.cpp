// Copyright 2026, The Macaulay2 Authors.

#include <gtest/gtest.h>

#include <string>

#include "interface/ring.h"
#include "ring-elements/ring-element.hpp"
#include "util.hpp"
#include "rings/weylalg.hpp"
#include "unit-tests/RingTest.hpp"
#include "unit-tests/RingElem.hpp"
#include "unit-tests/util-polyring-creation.hpp"

class WeylAlgebraTestAccessor
{
 public:
  static ring_elem binomial(const WeylAlgebra* W, int top, int bottom)
  {
    return W->binomial(top, bottom);
  }
  static ring_elem multinomial(const WeylAlgebra* W,
                               const ring_elem a,
                               const_exponents exptop,
                               const_exponents expbottom)
  {
    return W->multinomial(a, exptop, expbottom);
  }
  static const Ring* coefficientRing(const WeylAlgebra* W)
  {
    return W->getCoefficients();
  }
  static int nderivatives(const WeylAlgebra* W) { return W->_nderivatives; }
};

namespace {

// Each test gets QQ[x,y,Dx,Dy] with the standard two derivative pairs.
class WeylAlgebraTest : public ::testing::Test
{
 protected:
  const WeylAlgebra* W = nullptr;
  const Ring* K = nullptr;  // coefficient ring (QQ)

  void SetUp() override
  {
    W = simpleWeylAlgebra(0, {"x", "y", "Dx", "Dy"}, {0, 1}, {2, 3});
    ASSERT_NE(W, nullptr);
    K = WeylAlgebraTestAccessor::coefficientRing(W);
  }

  // Helper: check that a ring_elem in K equals a given long value
  void expectEqualLong(ring_elem actual, long expected)
  {
    EXPECT_EQ(RingElem(K, actual), RingElem::fromInt(K, expected));
  }
};

TEST_F(WeylAlgebraTest, create)
{
  // The constructor preserves derivative pairs, grading, and printed ring
  // settings.
  EXPECT_FALSE(W->is_commutative_ring());
  EXPECT_TRUE(W->is_weyl_algebra());
  EXPECT_EQ(4, W->n_vars());
  EXPECT_EQ(2, WeylAlgebraTestAccessor::nderivatives(W));

  std::string ans {
      "WeylAlgebra(QQGMP[x,y,Dx,Dy,\n"
      "  DegreeLength => 1,\n"
      "  Degrees => {1, 1, -1, -1},\n"
      "  Heft => {1},\n"
      "  MonomialOrder => {\n"
      "    GRevLex => {1,1,1,1},\n"
      "    Position => Up\n"
      "    }\n"
      "  ])"};
  buffer o;
  W->text_out(o);
  EXPECT_EQ(ans, std::string(o.str()));
}

TEST_F(WeylAlgebraTest, commutator)
{
  // Each derivative has commutator one with its variable and zero with the
  // other.
  auto x = RingElem::var(W, 0);
  auto y = RingElem::var(W, 1);
  auto Dx = RingElem::var(W, 2);
  auto Dy = RingElem::var(W, 3);
  auto one = RingElem::fromInt(W, 1);
  auto zero = RingElem::fromInt(W, 0);

  // Test Dx*x - x*Dx == 1 (the defining Weyl relation)
  EXPECT_EQ(Dx * x - x * Dx, one);

  // Test Dy*y - y*Dy == 1
  EXPECT_EQ(Dy * y - y * Dy, one);

  // Test Dx*y - y*Dx == 0 (cross terms commute)
  EXPECT_EQ(Dx * y - y * Dx, zero);
}

TEST_F(WeylAlgebraTest, binomial)
{
  // Known coefficients cover endpoints, symmetry, and values beyond the small
  // cache.
  struct Case
  {
    const char* name;
    int top;
    int bottom;
    long expected;
  };
  const Case cases[] = {{"empty choice", 0, 0, 1},
                        {"choose none", 5, 0, 1},
                        {"choose one", 5, 1, 5},
                        {"choose two", 5, 2, 10},
                        {"symmetric choice", 5, 3, 10},
                        {"choose all", 5, 5, 1},
                        {"central six", 6, 3, 20},
                        {"central ten", 10, 5, 252},
                        {"beyond cache", 20, 10, 184756},
                        {"larger coefficient", 30, 15, 155117520}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      expectEqualLong(
          WeylAlgebraTestAccessor::binomial(W, sample.top, sample.bottom),
          sample.expected);
    }
}

TEST_F(WeylAlgebraTest, multinomial)
{
  // multinomial(c, top, bottom) = c * product_i binomial(top[i], bottom[i])
  // where the product is over the _nderivatives derivative pairs.
  // Here _nderivatives = 2 (for Dx, Dy).

  ring_elem one = K->from_long(1);

  // All bottom entries zero: result should be c * 1 = c
  {
    SCOPED_TRACE("multinomial: no derivatives");
    int top[] = {5, 3};
    int bottom[] = {0, 0};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom),
                    1);
  }

  // Single nonzero bottom entry: should give binomial(5,2) = 10
  {
    SCOPED_TRACE("multinomial: one derivative pair");
    int top[] = {5, 3};
    int bottom[] = {2, 0};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom),
                    10);
  }

  // Both entries nonzero: binomial(5,2) * binomial(3,1) = 10 * 3 = 30
  {
    SCOPED_TRACE("multinomial: both derivative pairs");
    int top[] = {5, 3};
    int bottom[] = {2, 1};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom),
                    30);
  }

  // With a scalar coefficient: 7 * binomial(4,2) * binomial(6,3) = 7 * 6 * 20 =
  // 840
  {
    SCOPED_TRACE("multinomial: nonunit coefficient");
    ring_elem seven = K->from_long(7);
    int top[] = {4, 6};
    int bottom[] = {2, 3};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, seven, top, bottom),
                    840);
  }
}

TEST_F(WeylAlgebraTest, fromString)
{
  // Parsed Weyl expressions preserve coefficient signs and variable order.
  const auto x = RingElem::var(W, 0);
  const auto y = RingElem::var(W, 1);
  const auto Dx = RingElem::var(W, 2);
  struct Case
  {
    const char* name;
    const char* input;
    RingElem expected;
  };
  // Cases isolate a variable, ordered monomial, sum, constants and sign.
  for (const auto& sample :
       {Case {"variable", "x", x},
        Case {"ordered monomial", "3*x^2*Dx", x * x * Dx * 3},
        Case {"mixed signs",
              "x^2+3*y-1",
              x * x + y * 3 - RingElem::fromInt(W, 1)},
        Case {"constant", "5", RingElem::fromInt(W, 5)},
        Case {"zero", "0", RingElem::fromInt(W, 0)},
        Case {"negative coefficient", "-x", -x}})
    {
      SCOPED_TRACE(sample.name);
      EXPECT_EQ(RingElem::fromString(W, sample.input), sample.expected);
    }
}

TEST(PolyRingFromString, basic)
{
  // Parsed expressions agree with independent arithmetic over the prime field.
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y", "z"});
  ASSERT_NE(R, nullptr);

  auto x = RingElem::var(R, 0);
  auto y = RingElem::var(R, 1);
  auto z = RingElem::var(R, 2);
  auto one = RingElem::fromInt(R, 1);

  // Parser syntax uses explicit ^ and *; toString() uses compact notation,
  // so its output cannot yet supply a round-trip input.
  struct Case
  {
    const char* name;
    const char* input;
    RingElem expected;
  };
  for (const auto& sample :
       {Case {"first variable", "x", x},
        Case {"last variable", "z", z},
        Case {"monomial", "3*x^2*y", x * x * y * 3},
        Case {"mixed signs", "x^2+3*y-1", x * x + y * 3 - one},
        Case {"coefficient reduction", "102*x", x},
        Case {"constant", "7", RingElem::fromInt(R, 7)},
        Case {"zero", "0", RingElem::fromInt(R, 0)},
        Case {"multiple terms",
              "x^3+2*x*y*z-y^2+z",
              x.power(3) + x * y * z * 2 - y * y + z}})
    {
      SCOPED_TRACE(sample.name);
      EXPECT_EQ(RingElem::fromString(R, sample.input), sample.expected);
    }
}

}  // namespace
