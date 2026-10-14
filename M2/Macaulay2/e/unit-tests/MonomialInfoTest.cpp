// Tests for the MonomialInfo packed-monomial format used by the F4 GB algorithm.
//
// Run with: ./M2-unit-tests --gtest_filter="MonomialInfo*"
//
// These tests verify the core arithmetic and comparison invariants of
// MonomialInfo.  They are particularly important on 32-bit platforms where
// the previous 'long'-based monomial_word was only 32 bits wide.

#include <gtest/gtest.h>

#include <cstdlib>
#include <memory>
#include <vector>

#include "f4/moninfo.hpp"
#include "monomials/monordering.hpp"

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Build a MonomialInfo for an n-variable ring with the given monomial order.
// heft degrees are all 1 (standard grading), no module component (rank-1).
static std::unique_ptr<MonomialInfo> makeMonomialInfo(int nvars,
                                                      MonomialOrdering* mo)
{
  std::vector<int> heft(nvars, 1);
  return std::make_unique<MonomialInfo>(nvars, mo, heft, std::vector<int>{0});
}

static std::unique_ptr<MonomialInfo> makeGRevLex(int nvars)
{
  return makeMonomialInfo(nvars, MonomialOrderings::join({
    MonomialOrderings::GRevLex(nvars),
    MonomialOrderings::PositionUp()
  }));
}

// GRevLex4 corresponds to MonomialSize=>8 in M2 (4 exponents per 32-bit word).
static std::unique_ptr<MonomialInfo> makeGRevLex4(int nvars)
{
  return makeMonomialInfo(nvars, MonomialOrderings::join({
    MonomialOrderings::GRevLex4(nvars),
    MonomialOrderings::PositionUp()
  }));
}

// GRevLex2 corresponds to MonomialSize=>16.
static std::unique_ptr<MonomialInfo> makeGRevLex2(int nvars)
{
  return makeMonomialInfo(nvars, MonomialOrderings::join({
    MonomialOrderings::GRevLex2(nvars),
    MonomialOrderings::PositionUp()
  }));
}

static std::unique_ptr<MonomialInfo> makeLex(int nvars)
{
  return makeMonomialInfo(nvars, MonomialOrderings::join({
    MonomialOrderings::Lex(nvars),
    MonomialOrderings::PositionUp()
  }));
}

// Pack an exponent vector into a fresh monomial buffer.
static std::vector<monomial_word> packExpvector(const MonomialInfo& MI,
                                                const std::vector<int64_t>& e,
                                                int comp = 0)
{
  std::vector<monomial_word> m(MI.max_monomial_size());
  MI.from_expvector(e.data(), comp, m.data());
  return m;
}

// ---------------------------------------------------------------------------
// Fixture for 4-variable grevlex tests
// ---------------------------------------------------------------------------

class MonomialInfoGRevLex4Var : public ::testing::Test
{
 protected:
  void SetUp() override
  {
    srand(12345);
    MI = makeGRevLex(4);
  }
  std::unique_ptr<MonomialInfo> MI;
};

// ---------------------------------------------------------------------------
// Round-trip: from_expvector / to_expvector
// ---------------------------------------------------------------------------

TEST_F(MonomialInfoGRevLex4Var, RoundTripNonzero)
{
  std::vector<int64_t> e = {3, 1, 4, 1};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(4);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  EXPECT_EQ(comp, 0);
  for (int i = 0; i < 4; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

TEST_F(MonomialInfoGRevLex4Var, RoundTripZeroExponents)
{
  std::vector<int64_t> e = {0, 0, 0, 0};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(4);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  EXPECT_EQ(comp, 0);
  for (int i = 0; i < 4; i++) EXPECT_EQ(result[i], 0) << "var " << i;
}

TEST_F(MonomialInfoGRevLex4Var, RoundTripComponent)
{
  std::vector<int64_t> e = {2, 0, 1, 3};
  auto m = packExpvector(*MI, e, 7);
  std::vector<int64_t> result(4);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  EXPECT_EQ(comp, 7);
  for (int i = 0; i < 4; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

// ---------------------------------------------------------------------------
// Multiplication: unchecked_mult
// ---------------------------------------------------------------------------

TEST_F(MonomialInfoGRevLex4Var, MultiplyExponentsAdd)
{
  std::vector<int64_t> e1 = {1, 2, 0, 3};
  std::vector<int64_t> e2 = {2, 0, 4, 1};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());

  std::vector<int64_t> result(4);
  monomial_word comp;
  MI->to_expvector(prod.data(), result.data(), comp);
  for (int i = 0; i < 4; i++) EXPECT_EQ(result[i], e1[i] + e2[i]) << "var " << i;
}

TEST_F(MonomialInfoGRevLex4Var, MultiplyMatchesDirectPack)
{
  // unchecked_mult(m(e1), m(e2)) == m(e1+e2) for all fields including hash
  std::vector<int64_t> e1 = {3, 0, 2, 1};
  std::vector<int64_t> e2 = {1, 4, 0, 2};
  std::vector<int64_t> esum = {4, 4, 2, 3};
  auto m1   = packExpvector(*MI, e1);
  auto m2   = packExpvector(*MI, e2);
  auto msum = packExpvector(*MI, esum);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_TRUE(MI->is_equal(prod.data(), msum.data()));
}

TEST_F(MonomialInfoGRevLex4Var, HashIsAdditive)
{
  // The core property of the additive hash: hash(m*n) == hash(m) + hash(n).
  // This is the property that would fail if monomial_word were too narrow to
  // hold the hash without truncation.
  std::vector<int64_t> e1 = {2, 1, 3, 0};
  std::vector<int64_t> e2 = {0, 3, 1, 2};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_EQ(MI->hash_value(prod.data()),
            MI->hash_value(m1.data()) + MI->hash_value(m2.data()));
}

// ---------------------------------------------------------------------------
// Division: unchecked_divide
// ---------------------------------------------------------------------------

TEST_F(MonomialInfoGRevLex4Var, DivideExponentsSubtract)
{
  std::vector<int64_t> e1 = {4, 3, 2, 5};
  std::vector<int64_t> e2 = {1, 1, 2, 3};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> quot(MI->max_monomial_size());
  MI->unchecked_divide(m1.data(), m2.data(), quot.data());

  std::vector<int64_t> result(4);
  monomial_word comp;
  MI->to_expvector(quot.data(), result.data(), comp);
  for (int i = 0; i < 4; i++) EXPECT_EQ(result[i], e1[i] - e2[i]) << "var " << i;
}

TEST_F(MonomialInfoGRevLex4Var, MultThenDivideIsIdentity)
{
  std::vector<int64_t> e1 = {2, 0, 3, 1};
  std::vector<int64_t> e2 = {1, 2, 0, 1};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  std::vector<monomial_word> quot(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  MI->unchecked_divide(prod.data(), m2.data(), quot.data());
  EXPECT_TRUE(MI->is_equal(quot.data(), m1.data()));
}

// ---------------------------------------------------------------------------
// Equality
// ---------------------------------------------------------------------------

TEST_F(MonomialInfoGRevLex4Var, EqualSameMonomial)
{
  auto m1 = packExpvector(*MI, {1, 2, 3, 4});
  auto m2 = packExpvector(*MI, {1, 2, 3, 4});
  EXPECT_TRUE(MI->is_equal(m1.data(), m2.data()));
}

TEST_F(MonomialInfoGRevLex4Var, NotEqualDifferentExponent)
{
  auto m1 = packExpvector(*MI, {1, 2, 3, 4});
  auto m2 = packExpvector(*MI, {1, 2, 3, 5});
  EXPECT_FALSE(MI->is_equal(m1.data(), m2.data()));
}

TEST_F(MonomialInfoGRevLex4Var, NotEqualDifferentComponent)
{
  auto m1 = packExpvector(*MI, {1, 2, 0, 0}, 0);
  auto m2 = packExpvector(*MI, {1, 2, 0, 0}, 1);
  EXPECT_FALSE(MI->is_equal(m1.data(), m2.data()));
}

// ---------------------------------------------------------------------------
// Comparison: grevlex order
// ---------------------------------------------------------------------------

TEST_F(MonomialInfoGRevLex4Var, CompareHigherDegreeIsGreater)
{
  auto hi = packExpvector(*MI, {2, 1, 0, 0}); // degree 3
  auto lo = packExpvector(*MI, {1, 0, 0, 0}); // degree 1
  EXPECT_EQ(MI->compare(hi.data(), lo.data()), GT);
  EXPECT_EQ(MI->compare(lo.data(), hi.data()), LT);
}

TEST_F(MonomialInfoGRevLex4Var, CompareEqualMonomials)
{
  auto m1 = packExpvector(*MI, {1, 2, 0, 3});
  auto m2 = packExpvector(*MI, {1, 2, 0, 3});
  EXPECT_EQ(MI->compare(m1.data(), m2.data()), EQ);
}

TEST_F(MonomialInfoGRevLex4Var, CompareGRevLexTieBreakLastVar)
{
  // Equal degree; grevlex prefers the monomial with smaller exponent for the
  // last (highest-index) variable: x0^2 > x1^2 > x2^2 > x3^2.
  auto m0 = packExpvector(*MI, {2, 0, 0, 0}); // degree 2
  auto m1 = packExpvector(*MI, {0, 2, 0, 0});
  auto m2 = packExpvector(*MI, {0, 0, 2, 0});
  auto m3 = packExpvector(*MI, {0, 0, 0, 2});
  EXPECT_EQ(MI->compare(m0.data(), m1.data()), GT);
  EXPECT_EQ(MI->compare(m1.data(), m2.data()), GT);
  EXPECT_EQ(MI->compare(m2.data(), m3.data()), GT);
}

TEST_F(MonomialInfoGRevLex4Var, CompareAntisymmetric)
{
  auto m1 = packExpvector(*MI, {3, 0, 1, 0});
  auto m2 = packExpvector(*MI, {0, 2, 0, 2});
  int r12 = MI->compare(m1.data(), m2.data());
  int r21 = MI->compare(m2.data(), m1.data());
  if      (r12 == GT) EXPECT_EQ(r21, LT);
  else if (r12 == LT) EXPECT_EQ(r21, GT);
  else                EXPECT_EQ(r21, EQ);
}

TEST_F(MonomialInfoGRevLex4Var, CompareTransitive)
{
  auto a = packExpvector(*MI, {3, 0, 0, 0}); // degree 3
  auto b = packExpvector(*MI, {1, 1, 0, 0}); // degree 2
  auto c = packExpvector(*MI, {0, 0, 0, 1}); // degree 1
  EXPECT_EQ(MI->compare(a.data(), b.data()), GT);
  EXPECT_EQ(MI->compare(b.data(), c.data()), GT);
  EXPECT_EQ(MI->compare(a.data(), c.data()), GT);
}

// ---------------------------------------------------------------------------
// Lex order
// ---------------------------------------------------------------------------

TEST(MonomialInfoLex, CompareFirstVarDominates)
{
  srand(77777);
  auto MI = makeLex(3);
  // Lex: x0^2 > x0*x1 > x1^2 regardless of total degree
  auto x0sq = packExpvector(*MI, {2, 0, 0});
  auto x0x1 = packExpvector(*MI, {1, 1, 0});
  auto x1sq = packExpvector(*MI, {0, 2, 0});
  EXPECT_EQ(MI->compare(x0sq.data(), x0x1.data()), GT);
  EXPECT_EQ(MI->compare(x0x1.data(), x1sq.data()), GT);
  EXPECT_EQ(MI->compare(x0sq.data(), x1sq.data()), GT);
}

TEST(MonomialInfoLex, RoundTrip)
{
  srand(88888);
  auto MI = makeLex(3);
  std::vector<int64_t> e = {5, 0, 3};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(3);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  for (int i = 0; i < 3; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

// ---------------------------------------------------------------------------
// GRevLex4 (MonomialSize=>8): the order used in the originally-failing test
// ---------------------------------------------------------------------------

TEST(MonomialInfoGRevLex4, RoundTrip7Vars)
{
  srand(42);
  auto MI = makeGRevLex4(7);
  std::vector<int64_t> e = {3, 0, 1, 4, 0, 2, 1};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(7);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  for (int i = 0; i < 7; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

TEST(MonomialInfoGRevLex4, HashAdditive7Vars)
{
  srand(42);
  auto MI = makeGRevLex4(7);
  std::vector<int64_t> e1 = {1, 2, 0, 3, 1, 0, 2};
  std::vector<int64_t> e2 = {2, 0, 3, 0, 1, 2, 1};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_EQ(MI->hash_value(prod.data()),
            MI->hash_value(m1.data()) + MI->hash_value(m2.data()));
}

TEST(MonomialInfoGRevLex4, CompareCorrect7Vars)
{
  srand(42);
  auto MI = makeGRevLex4(7);
  // x0^2 (degree 2) > x6^2 (degree 2) in grevlex: x0^2 has smaller x6 exponent
  auto x0sq = packExpvector(*MI, {2, 0, 0, 0, 0, 0, 0});
  auto x6sq = packExpvector(*MI, {0, 0, 0, 0, 0, 0, 2});
  EXPECT_EQ(MI->compare(x0sq.data(), x6sq.data()), GT);
  EXPECT_EQ(MI->compare(x6sq.data(), x0sq.data()), LT);
}

// ---------------------------------------------------------------------------
// GRevLex2 (MonomialSize=>16)
// ---------------------------------------------------------------------------

TEST(MonomialInfoGRevLex2, RoundTrip5Vars)
{
  srand(55555);
  auto MI = makeGRevLex2(5);
  std::vector<int64_t> e = {0, 5, 2, 0, 3};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(5);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  for (int i = 0; i < 5; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

TEST(MonomialInfoGRevLex2, HashAdditive5Vars)
{
  srand(55555);
  auto MI = makeGRevLex2(5);
  std::vector<int64_t> e1 = {1, 0, 3, 2, 0};
  std::vector<int64_t> e2 = {0, 2, 1, 0, 4};
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_EQ(MI->hash_value(prod.data()),
            MI->hash_value(m1.data()) + MI->hash_value(m2.data()));
}

// ---------------------------------------------------------------------------
// Many variables: stress all monomial slots and the hash accumulator
// ---------------------------------------------------------------------------

TEST(MonomialInfoManyVars, RoundTrip10Vars)
{
  srand(33333);
  auto MI = makeGRevLex(10);
  std::vector<int64_t> e = {1, 2, 3, 4, 5, 0, 1, 0, 2, 1};
  auto m = packExpvector(*MI, e);
  std::vector<int64_t> result(10);
  monomial_word comp;
  MI->to_expvector(m.data(), result.data(), comp);
  for (int i = 0; i < 10; i++) EXPECT_EQ(result[i], e[i]) << "var " << i;
}

TEST(MonomialInfoManyVars, MultiplyMatchesDirectPack10Vars)
{
  srand(33333);
  const int n = 10;
  auto MI = makeGRevLex(n);
  std::vector<int64_t> e1(n), e2(n), esum(n);
  for (int i = 0; i < n; i++) { e1[i] = i; e2[i] = n - i; esum[i] = n; }
  auto m1   = packExpvector(*MI, e1);
  auto m2   = packExpvector(*MI, e2);
  auto msum = packExpvector(*MI, esum);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_TRUE(MI->is_equal(prod.data(), msum.data()));
}

TEST(MonomialInfoManyVars, HashAdditive10Vars)
{
  srand(33333);
  const int n = 10;
  auto MI = makeGRevLex(n);
  std::vector<int64_t> e1(n), e2(n);
  for (int i = 0; i < n; i++) { e1[i] = i % 4; e2[i] = (i + 2) % 3; }
  auto m1 = packExpvector(*MI, e1);
  auto m2 = packExpvector(*MI, e2);
  std::vector<monomial_word> prod(MI->max_monomial_size());
  MI->unchecked_mult(m1.data(), m2.data(), prod.data());
  EXPECT_EQ(MI->hash_value(prod.data()),
            MI->hash_value(m1.data()) + MI->hash_value(m2.data()));
}
