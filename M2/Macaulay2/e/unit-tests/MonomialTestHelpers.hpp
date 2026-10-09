#ifndef M2_UNIT_TESTS_MONOMIAL_TEST_HELPERS_HPP
#define M2_UNIT_TESTS_MONOMIAL_TEST_HELPERS_HPP

#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <initializer_list>
#include <memory>
#include <vector>
#include "monomials/ExponentList.hpp"
#include "monomials/monideal.hpp"

namespace monomialTest {
using Dense = std::array<int, 3>;
using Ideal = std::unique_ptr<MonomialIdeal>;

inline Bag* bag(Dense exponents, int id = 0)
{
  auto* result = new Bag(id);
  // Encode independently of the conversion routine under test.
  auto& sparse = result->monom();
  sparse.push_back(1);
  for (int v = 2; v >= 0; --v)
    if (exponents[v] != 0)
      {
        sparse.push_back(v);
        sparse.push_back(exponents[v]);
      }
  sparse[0] = sparse.size();
  return result;
}

inline Ideal ideal(const PolynomialRing* ring,
                   std::initializer_list<Dense> gens)
{
  VECTOR(Bag*) entries;
  for (const auto& gen : gens) entries.push_back(bag(gen));
  return Ideal(new MonomialIdeal(ring, entries));
}

inline std::vector<Dense> generators(const MonomialIdeal& ideal)
{
  std::vector<Dense> result;
  for (const auto& entry : ideal)
    {
      Dense dense {};
      const auto& sparse = entry.monom();
      for (int i = 1; i < sparse[0]; i += 2) dense[sparse[i]] = sparse[i + 1];
      result.push_back(dense);
    }
  std::sort(result.begin(), result.end());
  return result;
}

inline void expectGenerators(const MonomialIdeal& actual,
                             std::initializer_list<Dense> expected)
{
  std::vector<Dense> sorted(expected);
  std::sort(sorted.begin(), sorted.end());
  EXPECT_EQ(generators(actual), sorted);
  EXPECT_EQ(actual.size(), sorted.size());
  actual.debug_check();
}

template <class Sparse>
std::vector<typename Sparse::Exponent> sparseValues(
    typename Sparse::ConstExponents a)
{ return {a, a + Sparse::length(a)}; }

template <class DenseOps>
void checkDenseArithmetic()
{
  using E = typename DenseOps::Exponent;
  using V = std::array<E, 4>;
  V a {2, 0, 5, 1}, b {1, 3, 5, 0}, out {}, other {};
  DenseOps::copy(4, a.data(), out.data());
  EXPECT_EQ(out, a);
  EXPECT_TRUE(DenseOps::equal(4, a.data(), out.data()));
  EXPECT_FALSE(DenseOps::equal(4, a.data(), b.data()));
  DenseOps::mult(4, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {3, 3, 10, 1}));
  DenseOps::power(4, a.data(), 3, out.data());
  EXPECT_EQ(out, (V {6, 0, 15, 3}));
  DenseOps::multpower(4, a.data(), b.data(), 2, out.data());
  EXPECT_EQ(out, (V {4, 6, 15, 1}));
  DenseOps::divide(4, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {1, -3, 0, 1}));
  DenseOps::quotient(4, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {1, 0, 0, 1}));
  EXPECT_TRUE(DenseOps::divides(4, out.data(), a.data()));
  EXPECT_FALSE(DenseOps::divides(4, a.data(), b.data()));
  DenseOps::gcd(4, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {1, 0, 5, 0}));
  DenseOps::lcm(4, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {2, 3, 5, 1}));
  DenseOps::syz(4, a.data(), b.data(), out.data(), other.data());
  EXPECT_EQ(out, (V {0, 3, 0, 0}));
  EXPECT_EQ(other, (V {1, 0, 0, 1}));
  EXPECT_EQ(DenseOps::lex_compare(4, a.data(), b.data()), GT);
  EXPECT_EQ(DenseOps::lex_compare(4, b.data(), a.data()), LT);
  EXPECT_EQ(DenseOps::lex_compare(4, a.data(), a.data()), EQ);
  EXPECT_EQ(DenseOps::simple_degree(4, a.data()), 8);
  EXPECT_EQ(DenseOps::weight(4, a.data(), std::vector<E> {3, 7}), 6);
  EXPECT_EQ(DenseOps::weight(4, a.data(), std::vector<E> {3, 7, -1, 2, 99}), 3);
  EXPECT_EQ(DenseOps::weight(4, a.data(), std::vector<E> {}), 0);
  DenseOps::mult(4, a.data(), b.data(), a.data());
  EXPECT_EQ(a, (V {3, 3, 10, 1}));
  DenseOps::one(4, out.data());
  EXPECT_TRUE(DenseOps::is_one(4, out.data()));
  EXPECT_FALSE(DenseOps::is_one(4, b.data()));
  DenseOps::power(4, b.data(), 0, out.data());
  EXPECT_EQ(out, (V {0, 0, 0, 0}));
  // Zero variables must leave the caller's output storage alone.
  out.fill(17);
  DenseOps::mult(0, a.data(), b.data(), out.data());
  EXPECT_EQ(out, (V {17, 17, 17, 17}));
  EXPECT_TRUE(DenseOps::is_one(0, a.data()));
  EXPECT_EQ(DenseOps::simple_degree(0, a.data()), 0);
}

template <class Sparse, bool Legacy>
void checkSparseQueries()
{
  using E = typename Sparse::Exponent;
  using V = typename Sparse::Vector;
  V a {Legacy ? 5 : 2, 2, 3, 0, 2};
  V b {Legacy ? 3 : 1, 2, 3};
  V one {Legacy ? 1 : 0};
  EXPECT_EQ(Sparse::length(a.data()), 5);
  EXPECT_EQ(Sparse::npairs(a.data()), 2);
  EXPECT_EQ(Sparse::topvar(a.data()), 2);
  EXPECT_TRUE(Sparse::is_one(one.data()));
  EXPECT_FALSE(Sparse::is_one(a.data()));
  EXPECT_TRUE(Sparse::is_equal(a.data(), a.data()));
  EXPECT_FALSE(Sparse::is_equal(a.data(), b.data()));
  EXPECT_EQ(Sparse::compare(a.data(), b.data()), GT);
  EXPECT_EQ(Sparse::compare(b.data(), a.data()), LT);
  EXPECT_EQ(Sparse::compare(a.data(), a.data()), EQ);
  V bigger = a;
  bigger[2] = 4;
  EXPECT_EQ(Sparse::compare(a.data(), bigger.data()), LT);
  EXPECT_EQ(Sparse::compare(bigger.data(), a.data()), GT);
  EXPECT_EQ(Sparse::simple_degree(a.data()), 5);
  EXPECT_EQ(Sparse::weight(a.data(), std::vector<int> {4}), 11);
  EXPECT_EQ(Sparse::weight(a.data(), std::vector<int> {4, 0, -2}), 2);
  E var = -1, power = -1;
  EXPECT_FALSE(Sparse::is_pure_power(a.data(), var, power));
  EXPECT_TRUE(Sparse::is_pure_power(b.data(), var, power));
  EXPECT_EQ(var, 2);
  EXPECT_EQ(power, 3);
  V copied;
  Sparse::copy(a.data(), copied);
  EXPECT_EQ(copied, a);
  EXPECT_EQ(Sparse::computeHashValue(copied.data()),
            Sparse::computeHashValue(a.data()));
  ExponentListIterator<E, Legacy> iter(a.data());
  ExponentListIterator<E, Legacy> copy(iter);
  ASSERT_TRUE(copy.valid());
  EXPECT_EQ(copy.var(), 2);
  EXPECT_EQ(copy.exponent(), 3);
  ++copy;
  ASSERT_TRUE(copy.valid());
  EXPECT_EQ(copy.var(), 0);
  ++copy;
  EXPECT_FALSE(copy.valid());
  EXPECT_EQ(iter.var(), 2);
  buffer text;
  Sparse::elem_text_out(text, a.data());
  EXPECT_STREQ(text.str(), "c3a2");
}
}  // namespace monomialTest
#endif
