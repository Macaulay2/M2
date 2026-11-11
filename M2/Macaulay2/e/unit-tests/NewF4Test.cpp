#include <string>

#include <vector>
#include <gtest/gtest.h>

#include "rings/polyring.hpp"
#include "unit-tests/util-polyring-creation.hpp"
#include "gb-f4/MonomialHashTable.hpp"
#include "gb-f4/MonomialLookupTable.hpp"
#include "basic-rings/vector-arithmetic.hpp"
#include "BasicPolyListParser.hpp"
#include "gb-f4/PolynomialList.hpp"

TEST(NewF4, hashstats)
{
  // Fresh statistics start at zero before any table operations.
  newf4::HashTableStats stats;
  EXPECT_EQ(stats.n_calls_find, 0);
  EXPECT_EQ(stats.n_clashes, 0);
  EXPECT_EQ(stats.max_run_length, 0);
  EXPECT_EQ(stats.monequal_count, 0);
  EXPECT_EQ(stats.monequal_fails, 0);
}

TEST(NewF4, hashtable)
{
  // A deliberately constant hash exercises collision handling and table growth;
  // repeated lookups must retain both indices and stored monomials.
  newf4::MonomialHashTable table(5);
  MemoryBlock storage;
  std::vector<newf4::MonomialIndex> indices;
  for (int exponent = 1; exponent <= 256; ++exponent)
    {
      SCOPED_TRACE(::testing::Message() << "insert exponent " << exponent);
      newf4::MonomialView monomial({5, 0, 2, 1, exponent}, storage);
      const auto index = table.find(monomial, 7);
      indices.push_back(index);
      EXPECT_EQ(table.monomialAt(index), monomial);
    }
  EXPECT_EQ(table.size(), 256);
  for (int exponent = 1; exponent <= 256; ++exponent)
    {
      SCOPED_TRACE(::testing::Message() << "repeat exponent " << exponent);
      newf4::MonomialView monomial({5, 0, 2, 1, exponent}, storage);
      EXPECT_EQ(table.find(monomial, 7), indices[exponent - 1]);
    }
  EXPECT_EQ(table.size(), 256);
}

TEST(NewF4, matrixstream)
{
  // Streaming preserves every coefficient and monomial in the four input
  // polynomials.
  const PolynomialRing* R = simplePolynomialRing(1235952427, {"x", "y", "z"});
  const std::string polys = R"(1*x^1+2*y^1+2*z^1+1235952426
y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1
y^2+988761941*z^2+741571456*y^1+494380971*z^1
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1
)";

  ASSERT_NE(R, nullptr);
  const Ring* K = R->getCoefficients();
  VectorArithmetic VA(K);

  BasicPolyList B = parseBasicPolyListFromString(polys, {"x", "y", "z"});
  newf4::MonomialHashTable monHashTable;
  newf4::PolynomialList L(VA, monHashTable);
  newf4::PolynomialListStreamCollector S(1235952427, 3, 1, L);
  toStream(B, S);
  ASSERT_EQ(L.size(), 4);
  EXPECT_EQ(monHashTable.size(), 8);
  for (size_t index = 0; index < B.size(); ++index)
    {
      SCOPED_TRACE(::testing::Message() << "polynomial " << index);
      ASSERT_EQ(L[index].mMonomials.size(), B[index].mCoefficients.size());
      std::vector<int> monomials;
      for (size_t term = 0; term < L[index].mMonomials.size(); ++term)
        {
          SCOPED_TRACE(::testing::Message() << "term " << term);
          // The vector API uses signed representatives of residues modulo p.
          const long coefficient =
              VA.to_modp_long(L[index].mCoefficients, term);
          EXPECT_EQ((coefficient % 1235952427 + 1235952427) % 1235952427,
                    mpz_fdiv_ui(B[index].mCoefficients[term].get_mpz_t(),
                                1235952427));
          const auto monomial =
              monHashTable.monomialAt(L[index].mMonomials[term]);
          monomials.push_back(monomial.size());
          for (const auto& variable : monomial)
            {
              monomials.push_back(variable.first);
              monomials.push_back(variable.second);
            }
        }
      EXPECT_EQ(monomials, B[index].mMonomials);
    }
}

TEST(NewF4MonomialLookupTable, mask_creation)
{
  // The mask records variable support; increasing a power retains divisibility.
  MemoryBlock B;
  for (int i = 1; i < 10; ++i)
    {
      newf4::MonomialView m({5, 0, 2, i, 3}, B);
      auto mask = newf4::MonomialLookupTable::createMask(m);
      SCOPED_TRACE(::testing::Message() << "variable " << i);
      EXPECT_EQ(mask, (1UL << 0) | (1UL << i));
    }

  newf4::MonomialView ab({5, 0, 1, 1, 1}, B);
  newf4::MonomialView ac({5, 0, 1, 2, 1}, B);
  newf4::MonomialView ab2({5, 0, 1, 1, 2}, B);
  auto abMask = newf4::MonomialLookupTable::createMask(ab);
  auto ab2Mask = newf4::MonomialLookupTable::createMask(ab2);
  auto acMask = newf4::MonomialLookupTable::createMask(ac);
  EXPECT_FALSE(newf4::MonomialLookupTable::maskDivides(abMask, acMask));
  EXPECT_TRUE(newf4::MonomialLookupTable::maskDivides(abMask, ab2Mask));
}

TEST(NewF4MonomialLookupTable, monomialDivides)
{
  // Divisibility requires sufficient exponents for every variable in the
  // divisor.
  MemoryBlock B;
  newf4::MonomialView ab({5, 0, 1, 1, 1}, B);
  newf4::MonomialView ac({5, 0, 1, 2, 1}, B);
  newf4::MonomialView ab2({5, 0, 1, 1, 2}, B);
  newf4::MonomialView unit({1}, B);
  newf4::MonomialView ab2f({7, 0, 1, 1, 2, 5, 1}, B);
  newf4::MonomialView bd({5, 1, 1, 3, 1}, B);
  newf4::MonomialView abdf({9, 0, 1, 1, 1, 3, 1, 5, 1}, B);
  EXPECT_FALSE(newf4::MonomialView::monomialDivides(ab, ac));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(ab, ab2));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(unit, unit));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(unit, ab2));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(bd, abdf));
  EXPECT_FALSE(newf4::MonomialView::monomialDivides(ab2, abdf));
}

TEST(NewF4MonomialLookupTable, monomialOperations)
{
  // Shared-variable inputs distinguish maximum, sum, and truncated-difference
  // exponents.
  MemoryBlock B;
  newf4::MonomialView ab({5, 0, 1, 1, 1}, B);
  newf4::MonomialView ac({5, 0, 1, 2, 1}, B);
  newf4::MonomialView abc({7, 0, 1, 1, 1, 2, 1}, B);
  newf4::MonomialView a2bc({7, 0, 2, 1, 1, 2, 1}, B);
  newf4::MonomialView b({3, 1, 1}, B);
  newf4::MonomialView lcm = newf4::MonomialView::lcm(ab, ac, B);
  newf4::MonomialView product = newf4::MonomialView::product(ab, ac, B);
  newf4::MonomialView quotient = newf4::MonomialView::quotient(ab, ac, B);

  EXPECT_TRUE(newf4::MonomialView::monomialDivides(lcm, abc));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(abc, lcm));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(product, a2bc));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(a2bc, product));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(quotient, b));
  EXPECT_TRUE(newf4::MonomialView::monomialDivides(b, quotient));
}
