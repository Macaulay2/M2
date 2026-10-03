#include "monomials/montable.hpp"
#include "monomials/montableZZ.hpp"
#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>
#include <gmp.h>

namespace {
std::string readFile(FILE* file)
{
  std::rewind(file);
  std::string text;
  char chunk[256];
  while (std::fgets(chunk, sizeof(chunk), file)) text += chunk;
  return text;
}

TEST(MonomialTable, lookupComponentsAndCache)
{
  // Duplicate entries remain distinct, while cached divisors must respect the
  // component and query.
  int x[] = {2, 0, 0}, y[] = {0, 3, 0}, large[] = {3, 3, 0},
      small[] = {1, 1, 0}, z[] = {0, 0, 1};
  std::unique_ptr<MonomialTable> table(MonomialTable::make(3));
  EXPECT_EQ(table->find_divisor(x, 4), -1);
  EXPECT_EQ(table->find_divisors(-1, x, 4), 0);
  EXPECT_EQ(table->find_exact(x, 4), nullptr);
  table->insert(x, 1, 11);
  table->insert(y, 1, 22);
  table->insert(x, 1, 33);
  table->insert(z, 3, 44);
  EXPECT_EQ(table->find_divisor(y, 1), 22);
  EXPECT_EQ(table->find_divisor(large, 1), 22);
  EXPECT_EQ(table->find_divisor(large, 2), -1);
  EXPECT_EQ(table->find_divisor(small, 1), -1);
  EXPECT_EQ(table->find_divisor(z, 1), -1);
  EXPECT_EQ(table->find_divisor(z, 3), 44);
  VECTOR(MonomialTable::mon_term*) matches;
  EXPECT_EQ(table->find_divisors(-1, large, 1, &matches), 3);
  std::vector<int> ids;
  for (const auto* term : matches) ids.push_back(term->_val);
  std::sort(ids.begin(), ids.end());
  EXPECT_EQ(ids, (std::vector<int> {11, 22, 33}));
  matches.clear();
  EXPECT_EQ(table->find_divisors(1, large, 1, &matches), 1);
  ASSERT_EQ(matches.size(), 1);
  EXPECT_EQ(table->find_divisors(1, large, 1), 1);
  EXPECT_EQ(table->find_divisors(2, large, 1), 2);
  EXPECT_EQ(table->find_divisors(-1, small, 1), 0);
  EXPECT_EQ(table->find_exact(large, 1), nullptr);
  EXPECT_EQ(table->find_exact(z, 1), nullptr);
  auto* exact = table->find_exact(x, 1);
  ASSERT_NE(exact, nullptr);
  EXPECT_EQ(exact->_lead, x);
  exact->_val = 55;
  EXPECT_EQ(table->find_exact(x, 1)->_val, 55);
  std::unique_ptr<FILE, decltype(&std::fclose)> file(std::tmpfile(),
                                                     std::fclose);
  ASSERT_NE(file, nullptr);
  table->show(file.get());
  const auto text = readFile(file.get());
  EXPECT_NE(text.find("3 vars, 4 components, 4 elements"), std::string::npos);
  EXPECT_NE(text.find("[2 0 0 ] (55)"), std::string::npos);
}

class MonomialTableZZTest : public testing::Test
{
 protected:
  // Coefficients and exponent arrays outlive the table, which borrows them.
  mpz_t six, ten, thirty, two, seven, minusFour;
  int x[3] {1, 0, 0}, y[3] {0, 1, 0}, xy[3] {1, 1, 0}, x2[3] {2, 0, 0},
      z[3] {0, 0, 1}, one[3] {};
  std::unique_ptr<MonomialTableZZ> table;
  void SetUp() override
  {
    mpz_init_set_si(six, 6);
    mpz_init_set_si(ten, 10);
    mpz_init_set_si(thirty, 30);
    mpz_init_set_si(two, 2);
    mpz_init_set_si(seven, 7);
    mpz_init_set_si(minusFour, -4);
    table.reset(MonomialTableZZ::make(3));
    table->insert(six, x, 1, 10);
    table->insert(ten, y, 1, 20);
    table->insert(thirty, xy, 1, 30);
    table->insert(seven, z, 3, 40);
  }
  void TearDown() override
  {
    table.reset();
    mpz_clears(six, ten, thirty, two, seven, minusFour, nullptr);
  }
};

TEST_F(MonomialTableZZTest, membershipAndDivisors)
{
  // 2xy belongs to (6x,10y) through a coefficient gcd, but neither term divides
  // it.
  EXPECT_TRUE(table->is_weak_member(two, xy, 1));
  EXPECT_FALSE(table->is_strong_member(two, xy, 1));
  EXPECT_TRUE(table->is_strong_member(thirty, xy, 1));
  EXPECT_TRUE(table->is_weak_member(six, x2, 1));
  EXPECT_FALSE(table->is_weak_member(seven, xy, 1));
  EXPECT_FALSE(table->is_weak_member(two, x, 1));
  EXPECT_FALSE(table->is_weak_member(two, z, 1));
  EXPECT_FALSE(table->is_weak_member(two, one, 1));
  EXPECT_FALSE(table->is_weak_member(two, xy, 2));
  EXPECT_FALSE(table->is_weak_member(two, xy, 9));
  EXPECT_EQ(table->find_smallest_coeff_divisor(xy, 1), 10);
  EXPECT_EQ(table->find_smallest_coeff_divisor(one, 1), -1);
  EXPECT_EQ(table->find_smallest_coeff_divisor(x, 9), -1);
  EXPECT_EQ(table->find_term_divisors(-1, thirty, xy, 9), 0);
  EXPECT_EQ(table->find_monomial_divisors(-1, xy, 9), 0);
  EXPECT_EQ(table->find_monomial_divisors(-1, x, 1), 1);
  EXPECT_EQ(table->find_term_divisors(-1, two, x, 1), 0);
  EXPECT_EQ(table->find_term_divisors(-1, thirty, one, 1), 0);
  EXPECT_EQ(table->find_monomial_divisors(-1, one, 1), 0);
  VECTOR(MonomialTableZZ::mon_term*) terms;
  EXPECT_EQ(table->find_term_divisors(-1, thirty, xy, 1, &terms), 3);
  EXPECT_EQ(terms.size(), 3);
  terms.clear();
  EXPECT_EQ(table->find_monomial_divisors(2, xy, 1, &terms), 2);
  EXPECT_EQ(terms.size(), 2);
  EXPECT_EQ(table->find_monomial_divisors(-1, xy, 1), 3);
  table->insert(minusFour, xy, 1, 50);
  EXPECT_EQ(table->find_smallest_coeff_divisor(xy, 1), 50);
}

TEST_F(MonomialTableZZTest, exactLookupMutationAndPrinting)
{
  // Exact lookup distinguishes coefficients, powers, components, and the
  // minimum id.
  EXPECT_EQ(table->find_exact(six, x, 9), nullptr);
  EXPECT_EQ(table->find_exact(ten, x, 1), nullptr);
  EXPECT_EQ(table->find_exact(six, x2, 1), nullptr);
  EXPECT_EQ(table->find_exact(six, z, 1), nullptr);
  EXPECT_EQ(table->find_exact_monomial(x, 9, 0), nullptr);
  EXPECT_EQ(table->find_exact_monomial(x2, 1, 0), nullptr);
  EXPECT_EQ(table->find_exact_monomial(z, 1, 0), nullptr);
  EXPECT_EQ(table->find_exact_monomial(x, 1, 11), nullptr);
  auto* term = table->find_exact_monomial(x, 1, 10);
  ASSERT_NE(term, nullptr);
  EXPECT_EQ(term, table->find_exact(six, x, 1));
  table->change_coefficient(term, two, 60);
  EXPECT_EQ(table->find_exact(six, x, 1), nullptr);
  EXPECT_EQ(table->find_exact(two, x, 1), term);
  EXPECT_EQ(term->_val, 60);
  EXPECT_TRUE(table->is_strong_member(two, x, 1));
  buffer out, bare;
  table->show_mon_term(out, term);
  table->show_mon_term(bare, nullptr, x, 3);
  EXPECT_STREQ(out.str(), "2[1,0,0] (60)\n");
  EXPECT_STREQ(bare.str(), "[1,0,0] (3)\n");
  std::unique_ptr<FILE, decltype(&std::fclose)> file(std::tmpfile(),
                                                     std::fclose);
  ASSERT_NE(file, nullptr);
  table->show(file.get());
  table->show_weak(file.get(), two, x, 1, 60);
  const auto text = readFile(file.get());
  EXPECT_NE(text.find("3 vars, 4 components, 4 elements"), std::string::npos);
  EXPECT_NE(text.find("2[1,0,0] (60)"), std::string::npos);
  EXPECT_NE(text.find("coeff=2 exp=[1 0 0 ] comp=1 val=60"), std::string::npos);
  std::unique_ptr<MonomialTableZZ> constants(MonomialTableZZ::make(0));
  buffer scalar;
  constants->show_mon_term(scalar, two, one, 1);
  EXPECT_STREQ(scalar.str(), "2[] (1)\n");
}

TEST_F(MonomialTableZZTest, weakAndStrongGeneratingSets)
{
  // Positive coefficients are required by the generator sorter. 14xy is weakly
  // redundant only.
  mpz_t fourteen;
  mpz_init_set_ui(fourteen, 14);
  VECTOR(mpz_srcptr) coefficients {thirty, ten, six, fourteen, six, seven};
  VECTOR(exponents_t) powers {xy, y, x, xy, x, z};
  VECTOR(int) components {1, 1, 1, 1, 1, 3};
  for (bool stable : {true, false})
    {
      SCOPED_TRACE(stable);
      VECTOR(int) result;
      MonomialTableZZ::find_weak_generators(
          3, coefficients, powers, components, result, stable);
      std::vector<int> selected(result.begin(), result.end());
      // An unstable sort may choose either identical occurrence of 6x.
      if (!stable)
        for (int& index : selected)
          if (index == 4) index = 2;
      std::sort(selected.begin(), selected.end());
      EXPECT_EQ(selected, (std::vector<int> {1, 2, 5}));
    }
  VECTOR(int) result;
  MonomialTableZZ::find_strong_generators(
      3, coefficients, powers, components, result);
  std::vector<int> selected(result.begin(), result.end());
  std::sort(selected.begin(), selected.end());
  EXPECT_EQ(selected, (std::vector<int> {1, 2, 3, 5}));
  mpz_clear(fourteen);
}

TEST_F(MonomialTableZZTest, rejectsLargerPowersWithSameSupport)
{
  // A support mask alone cannot decide divisibility when powers differ.
  table->insert(six, x2, 2, 80);
  EXPECT_FALSE(table->is_weak_member(six, x, 2));
  EXPECT_EQ(table->find_term_divisors(-1, six, x, 2), 0);
  EXPECT_EQ(table->find_monomial_divisors(-1, x, 2), 0);
  EXPECT_EQ(table->find_smallest_coeff_divisor(x, 2), -1);
  EXPECT_EQ(table->find_exact(six, x, 2), nullptr);
  EXPECT_EQ(table->find_exact_monomial(x, 2, 0), nullptr);
}
}  // namespace
