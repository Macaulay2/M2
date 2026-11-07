// Copyright 2026, The Macaulay2 Authors.

#include "interface/ring.h"
#include "ring-elements/ring-element.hpp"
#include "util.hpp"
#include "rings/weylalg.hpp"
#include "unit-tests/RingTest.hpp"
#include "unit-tests/RingElem.hpp"
#include "unit-tests/util-polyring-creation.hpp"

class WeylAlgebraTestAccessor {
 public:
  static ring_elem binomial(const WeylAlgebra* W, int top, int bottom) {
    return W->binomial(top, bottom);
  }
  static ring_elem multinomial(const WeylAlgebra* W,
                               const ring_elem a,
                               const_exponents exptop,
                               const_exponents expbottom) {
    return W->multinomial(a, exptop, expbottom);
  }
  static const Ring* coefficientRing(const WeylAlgebra* W) {
    return W->getCoefficients();
  }
  static int nderivatives(const WeylAlgebra* W) {
    return W->_nderivatives;
  }
};

// Test fixture: creates a WeylAlgebra QQ[x,y,Dx,Dy]
class WeylAlgebraTest : public ::testing::Test {
 protected:
  const WeylAlgebra* W = nullptr;
  const Ring* K = nullptr;  // coefficient ring (QQ)

  void SetUp() override {
    W = simpleWeylAlgebra(0, {"x", "y", "Dx", "Dy"}, {0, 1}, {2, 3});
    K = WeylAlgebraTestAccessor::coefficientRing(W);
  }

  // Helper: check that a ring_elem in K equals a given long value
  void expectEqualLong(ring_elem actual, long expected) {
    ring_elem exp = K->from_long(expected);
    EXPECT_TRUE(K->is_equal(actual, exp));
  }
};

TEST_F(WeylAlgebraTest, create)
{
  EXPECT_FALSE(W->is_commutative_ring());
  EXPECT_TRUE(W->is_weyl_algebra());
  EXPECT_EQ(4, W->n_vars());
  EXPECT_EQ(2, WeylAlgebraTestAccessor::nderivatives(W));

  std::string ans {"WeylAlgebra(QQGMP[x,y,Dx,Dy,\n"
                   "  DegreeLength => 1,\n"
                   "  Degrees => {1, 1, -1, -1},\n"
                   "  Heft => {1},\n"
                   "  MonomialOrder => {\n"
                   "    GRevLex => {1,1,1,1},\n"
                   "    Position => Up\n"
                   "    }\n"
                   "  ])" };
  buffer o;
  W->text_out(o);
  EXPECT_EQ(ans, std::string(o.str()));
}

TEST_F(WeylAlgebraTest, commutator)
{
  auto x  = RingElem::var(W, 0);
  auto y  = RingElem::var(W, 1);
  auto Dx = RingElem::var(W, 2);
  auto Dy = RingElem::var(W, 3);
  auto one  = RingElem::fromInt(W, 1);
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
  // binomial(n, k) = n! / (k! * (n-k)!)
  // Results are ring elements in the coefficient ring K (QQ).

  // Base cases
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 0, 0), 1);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 5, 0), 1);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 5, 1), 5);

  // Standard values
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 5, 2), 10);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 5, 3), 10);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 5, 5), 1);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 6, 3), 20);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 10, 5), 252);

  // Larger values (beyond the cached binomtable)
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 20, 10), 184756);
  expectEqualLong(WeylAlgebraTestAccessor::binomial(W, 30, 15), 155117520);
}

TEST_F(WeylAlgebraTest, multinomial)
{
  // multinomial(c, top, bottom) = c * product_i binomial(top[i], bottom[i])
  // where the product is over the _nderivatives derivative pairs.
  // Here _nderivatives = 2 (for Dx, Dy).

  ring_elem one = K->from_long(1);

  // All bottom entries zero: result should be c * 1 = c
  {
    int top[] = {5, 3};
    int bottom[] = {0, 0};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom), 1);
  }

  // Single nonzero bottom entry: should give binomial(5,2) = 10
  {
    int top[] = {5, 3};
    int bottom[] = {2, 0};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom), 10);
  }

  // Both entries nonzero: binomial(5,2) * binomial(3,1) = 10 * 3 = 30
  {
    int top[] = {5, 3};
    int bottom[] = {2, 1};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, one, top, bottom), 30);
  }

  // With a scalar coefficient: 7 * binomial(4,2) * binomial(6,3) = 7 * 6 * 20 = 840
  {
    ring_elem seven = K->from_long(7);
    int top[] = {4, 6};
    int bottom[] = {2, 3};
    expectEqualLong(WeylAlgebraTestAccessor::multinomial(W, seven, top, bottom), 840);
  }
}

TEST_F(WeylAlgebraTest, fromString)
{
  // Single variable
  auto x = RingElem::var(W, 0);
  EXPECT_EQ(RingElem::fromString(W, "x"), x);

  // Monomial with coefficient
  auto Dx = RingElem::var(W, 2);
  EXPECT_EQ(RingElem::fromString(W, "3*x^2*Dx"), x * x * Dx * 3);

  // Polynomial with multiple terms
  auto y = RingElem::var(W, 1);
  auto expected = x * x + y * 3 - RingElem::fromInt(W, 1);
  EXPECT_EQ(RingElem::fromString(W, "x^2+3*y-1"), expected);

  // Constant
  EXPECT_EQ(RingElem::fromString(W, "5"), RingElem::fromInt(W, 5));

  // Zero
  EXPECT_TRUE(RingElem::fromString(W, "0").isZero());

  // Negative coefficient
  EXPECT_EQ(RingElem::fromString(W, "-x"), -x);
}

TEST(PolyRingFromString, basic)
{
  const PolynomialRing *R = simplePolynomialRing(101, {"x", "y", "z"});

  auto x = RingElem::var(R, 0);
  auto y = RingElem::var(R, 1);
  auto z = RingElem::var(R, 2);
  auto one = RingElem::fromInt(R, 1);

  // Single variable
  EXPECT_EQ(RingElem::fromString(R, "x"), x);
  EXPECT_EQ(RingElem::fromString(R, "z"), z);

  // Monomial with coefficient
  EXPECT_EQ(RingElem::fromString(R, "3*x^2*y"), x * x * y * 3);

  // Polynomial
  auto f = x * x + y * 3 - one;
  EXPECT_EQ(RingElem::fromString(R, "x^2+3*y-1"), f);

  // Coefficient reduction mod 101
  EXPECT_EQ(RingElem::fromString(R, "102*x"), x);

  // Constant
  EXPECT_EQ(RingElem::fromString(R, "7"), RingElem::fromInt(R, 7));

  // Zero
  EXPECT_TRUE(RingElem::fromString(R, "0").isZero());

  // Multi-term polynomial
  // NOTE: can't round-trip via toString() yet — it outputs "x3+2xyz-y2+z"
  // (no ^ or *) which the parser doesn't accept. TODO: make these compatible.
  auto g = RingElem::fromString(R, "x^3+2*x*y*z-y^2+z");
  auto g_expected = x.power(3) + x * y * z * 2 - y * y + z;
  EXPECT_EQ(g, g_expected);
}
