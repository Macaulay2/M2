#include "monomials/monideal.hpp"
#include "monomials/monideal-minprimes.hpp"
#include <gtest/gtest.h>
#include <algorithm>
#include <memory>
#include <string>
#include <vector>
#include "MonomialTestHelpers.hpp"
#include "util-polyring-creation.hpp"

namespace {
using namespace monomialTest;
class MonomialIdealTest : public testing::Test
{
 protected:
  const PolynomialRing* ring = simplePolynomialRing(101, {"x", "y", "z"});
};

TEST_F(MonomialIdealTest, constructionIterationAndRemoval)
{
  // Unsorted redundant generators minimize on construction and retain their
  // baggage.
  VECTOR(Bag*)
  entries {bag({2, 1, 0}, 1),
           bag({0, 0, 2}, 2),
           bag({2, 0, 0}, 3),
           bag({0, 2, 0}, 4),
           bag({2, 0, 0}, 5)};
  VECTOR(Bag*) rejects;
  MonomialIdeal result(ring, entries, rejects);
  expectGenerators(result, {{2, 0, 0}, {0, 2, 0}, {0, 0, 2}});
  ASSERT_EQ(rejects.size(), 2);
  std::vector<int> rejected;
  for (auto* entry : rejects)
    {
      rejected.push_back(entry->basis_elem());
      delete entry;
    }
  std::sort(rejected.begin(), rejected.end());
  EXPECT_EQ(rejected, (std::vector<int> {1, 5}));
  EXPECT_EQ(result.get_ring(), ring);
  EXPECT_EQ(result.topvar(), 2);
  EXPECT_EQ(result.n_pure_powers(), 3);
  EXPECT_FALSE(result.is_one());
  auto first = result.begin();
  EXPECT_EQ(first->basis_elem(), 2);
  auto previous = first++;
  EXPECT_EQ(previous->basis_elem(), 2);
  EXPECT_EQ(first->basis_elem(), 4);
  EXPECT_EQ(sparseValues<varpower>(result.first_elem()),
            (std::vector<int> {3, 2, 2}));
  EXPECT_EQ(sparseValues<varpower>(result.second_elem()),
            (std::vector<int> {3, 1, 2}));
  auto reverse = result.beginAtLast();
  EXPECT_EQ(reverse->basis_elem(), 3);
  auto last = reverse--;
  EXPECT_EQ(last->basis_elem(), 3);
  EXPECT_EQ(reverse->basis_elem(), 4);
  --reverse;
  EXPECT_EQ(reverse->basis_elem(), 2);
  --reverse;
  EXPECT_EQ(reverse, result.end());
  Ideal copy(result.copy());
  EXPECT_TRUE(result.is_equal(result));
  EXPECT_TRUE(result.is_equal(*copy));
  EXPECT_EQ(result.hash(), copy->hash());
  auto different = ideal(ring, {{1, 0, 0}, {0, 2, 0}, {0, 0, 2}});
  EXPECT_FALSE(result.is_equal(*different));
  auto smaller = ideal(ring, {{1, 0, 0}});
  EXPECT_FALSE(result.is_equal(*smaller));
  Bag* removed = nullptr;
  for (int id : {2, 4, 3})
    {
      ASSERT_EQ(copy->remove(removed), 1);
      EXPECT_EQ(removed->basis_elem(), id);
      delete removed;
      copy->debug_check();
    }
  EXPECT_EQ(copy->remove(removed), 0);
  EXPECT_EQ(copy->size(), 0);
  EXPECT_EQ(copy->topvar(), -1);
  EXPECT_EQ(result.size(), 3);
}

TEST_F(MonomialIdealTest, membershipAndAllDivisors)
{
  // Exhaust a small exponent box; membership and returned generators agree with
  // direct inequalities.
  auto source = ideal(ring, {{2, 0, 0}, {1, 1, 0}, {0, 2, 1}, {0, 0, 3}});
  const std::vector<Dense> known {{2, 0, 0}, {1, 1, 0}, {0, 2, 1}, {0, 0, 3}};
  for (int x = 0; x <= 3; ++x)
    for (int y = 0; y <= 3; ++y)
      for (int z = 0; z <= 3; ++z)
        {
          Dense query {x, y, z};
          SCOPED_TRACE(testing::PrintToString(query));
          std::vector<Dense> expected;
          for (const auto& gen : known)
            if (gen[0] <= x && gen[1] <= y && gen[2] <= z)
              expected.push_back(gen);
          std::sort(expected.begin(), expected.end());
          Bag* found = nullptr;
          EXPECT_EQ(source->search_expvector(query.data(), found),
                    !expected.empty());
          std::unique_ptr<Bag> sparse(bag(query));
          EXPECT_EQ(source->search(sparse->monom().data(), found),
                    !expected.empty());
          VECTOR(Bag*) divisors;
          source->find_all_divisors(query.data(), divisors);
          std::vector<Dense> actual;
          for (const auto* divisor : divisors)
            {
              Dense d {};
              const auto& m = divisor->monom();
              for (int i = 1; i < m[0]; i += 2) d[m[i]] = m[i + 1];
              actual.push_back(d);
            }
          std::sort(actual.begin(), actual.end());
          EXPECT_EQ(actual, expected);
        }
  EXPECT_EQ(source->insert(bag({2, 1, 1})), 0);
  EXPECT_EQ(source->insert(bag({0, 3, 0})), 1);
  expectGenerators(*source,
                   {{2, 0, 0}, {1, 1, 0}, {0, 2, 1}, {0, 0, 3}, {0, 3, 0}});
}

TEST_F(MonomialIdealTest, sumProductIntersectionAndDifference)
{
  // I=(x^2,y), J=(x,z) have small independently computed minimal generators.
  auto i = ideal(ring, {{2, 0, 0}, {0, 1, 0}}),
       j = ideal(ring, {{1, 0, 0}, {0, 0, 1}});
  Ideal sum(*i + *j), product(*i * *j), intersection(i->intersect(*j)),
      difference(*i - *j);
  expectGenerators(*sum, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}});
  expectGenerators(*product, {{3, 0, 0}, {2, 0, 1}, {1, 1, 0}, {0, 1, 1}});
  expectGenerators(*intersection, {{2, 0, 0}, {1, 1, 0}, {0, 1, 1}});
  expectGenerators(*difference, {{0, 1, 0}});
  int xy[] = {5, 1, 1, 0, 1};
  Ideal principal(i->intersect(xy));
  expectGenerators(*principal, {{1, 1, 0}});
  buffer text;
  i->text_out(text);
  EXPECT_STREQ(text.str(), "y x2 ");
}

TEST_F(MonomialIdealTest, quotientsSaturationRadicalAndDual)
{
  // Powers distinguish finite quotients from saturation; duality uses the
  // stated bounding vector.
  auto i = ideal(ring, {{2, 1, 0}, {0, 2, 0}, {0, 0, 3}});
  auto j = ideal(ring, {{1, 0, 0}, {0, 1, 0}});
  int x[] = {3, 0, 1};
  Ideal quotient(i->quotient(x)), byIdeal(i->quotient(*j)), erased(i->erase(x)),
      saturated(i->sat(*j)), radical(i->radical());
  expectGenerators(*quotient, {{1, 1, 0}, {0, 2, 0}, {0, 0, 3}});
  expectGenerators(*byIdeal, {{1, 1, 0}, {0, 2, 0}, {0, 0, 3}});
  expectGenerators(*erased, {{0, 1, 0}, {0, 0, 3}});
  expectGenerators(*saturated, {{0, 1, 0}, {0, 0, 3}});
  expectGenerators(*radical, {{0, 1, 0}, {0, 0, 1}});
  auto bounds = i->lcm();
  EXPECT_EQ((std::vector<int>(bounds->array, bounds->array + bounds->len)),
            (std::vector<int> {2, 2, 3}));
  Ideal dual(i->alexander_dual(bounds));
  expectGenerators(*dual, {{1, 1, 1}, {0, 2, 1}});
  Ideal twice(dual->alexander_dual(bounds));
  expectGenerators(*twice, {{2, 1, 0}, {0, 2, 0}, {0, 0, 3}});
}

TEST_F(MonomialIdealTest, borelClosure)
{
  // All downward variable moves of z^2 give the six degree-two monomials.
  auto source = ideal(ring, {{0, 0, 2}});
  EXPECT_FALSE(source->is_borel());
  Ideal closure(source->borel());
  expectGenerators(
      *closure,
      {{2, 0, 0}, {1, 1, 0}, {1, 0, 1}, {0, 2, 0}, {0, 1, 1}, {0, 0, 2}});
  EXPECT_TRUE(closure->is_borel());
  Ideal twice(closure->borel());
  EXPECT_TRUE(closure->is_equal(*twice));
  EXPECT_EQ(closure->n_pure_powers(), 3);
  EXPECT_EQ(closure->hash(), twice->hash());
}

TEST_F(MonomialIdealTest, zeroAndUnitIdeals)
{
  // The empty and unit ideals satisfy the absorbing and identity cases of ideal
  // operations.
  auto zero = ideal(ring, {}), unit = ideal(ring, {{0, 0, 0}}),
       i = ideal(ring, {{2, 0, 0}, {0, 1, 0}});
  EXPECT_EQ(zero->begin(), zero->end());
  EXPECT_EQ(zero->beginAtLast(), zero->end());
  EXPECT_FALSE(zero->is_one());
  EXPECT_TRUE(unit->is_one());
  EXPECT_EQ(unit->n_pure_powers(), 0);
  buffer z, u;
  zero->text_out(z);
  unit->text_out(u);
  EXPECT_STREQ(z.str(), "0");
  EXPECT_STREQ(u.str(), "1");
  int query[] = {1, 2, 3};
  Bag* found = nullptr;
  EXPECT_EQ(zero->search_expvector(query, found), 0);
  EXPECT_EQ(unit->search_expvector(query, found), 1);
  VECTOR(Bag*) divisors;
  zero->find_all_divisors(query, divisors);
  EXPECT_TRUE(divisors.empty());
  Ideal product(*i * *zero), intersection(i->intersect(*zero)),
      quotient(i->quotient(*zero)), sat(i->sat(*zero)),
      same(i->quotient(*unit));
  expectGenerators(*product, {});
  expectGenerators(*intersection, {});
  expectGenerators(*quotient, {{0, 0, 0}});
  expectGenerators(*sat, {{0, 0, 0}});
  expectGenerators(*same, {{2, 0, 0}, {0, 1, 0}});
  Ideal radical(zero->radical()), borel(zero->borel());
  expectGenerators(*radical, {});
  expectGenerators(*borel, {});
  EXPECT_TRUE(zero->is_borel());
  EXPECT_TRUE(unit->is_borel());
  EXPECT_EQ(zero->next(static_cast<void*>(nullptr)), nullptr);
  EXPECT_EQ(zero->prev(static_cast<void*>(nullptr)), nullptr);
  EXPECT_FALSE(zero->valid(nullptr));
  stash shared("monomial test nodes", sizeof(Nmi_node));
  monideal_pair pair(ring, &shared);
  pair.mi->insert_minimal(bag({1, 0, 0}));
  pair.mi_search->insert_minimal(bag({0, 1, 0}));
  expectGenerators(*pair.mi, {{1, 0, 0}});
  expectGenerators(*pair.mi_search, {{0, 1, 0}});
  delete pair.mi;
  delete pair.mi_search;
  monideal_pair separate(ring);
  EXPECT_EQ(separate.mi->size(), 0);
  EXPECT_EQ(separate.mi_search->size(), 0);
  delete separate.mi;
  delete separate.mi_search;
}

TEST_F(MonomialIdealTest, minimalPrimesAndCodimension)
{
  // (xy,xz) has primes (x) and (y,z); powers do not change those supports.
  auto source = ideal(ring, {{2, 3, 0}, {1, 0, 4}});
  const MonomialIdeal* pointer = source.get();
  MinimalPrimes codim(pointer);
  EXPECT_EQ(codim.codimension(), 1);
  MinimalPrimes all(pointer);
  Ideal primes(all.min_primes(3, -1));
  expectGenerators(*primes, {{1, 0, 0}, {0, 1, 1}});
  MinimalPrimes bounded(pointer);
  Ideal onlyHeightOne(bounded.min_primes(1, -1));
  expectGenerators(*onlyHeightOne, {{1, 0, 0}});
  MinimalPrimes alternate(pointer);
  Ideal other(alternate.alg1_min_primes(3, -1));
  expectGenerators(*other, {{1, 0, 0}, {0, 1, 1}});
  auto triangle = ideal(ring, {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}});
  pointer = triangle.get();
  MinimalPrimes triangleCodim(pointer);
  EXPECT_EQ(triangleCodim.codimension(), 2);
  MinimalPrimes trianglePrimes(pointer);
  Ideal three(trianglePrimes.min_primes(3, -1));
  expectGenerators(*three, {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}});
  MinimalPrimes limited(pointer);
  Ideal first(limited.min_primes(2, 1));
  EXPECT_EQ(first->size(), 1);
  const auto selected = generators(*first);
  const auto expected = generators(*three);
  EXPECT_NE(std::find(expected.begin(), expected.end(), selected.front()),
            expected.end());
  auto zero = ideal(ring, {}), unit = ideal(ring, {{0, 0, 0}});
  pointer = zero.get();
  MinimalPrimes zeroCodim(pointer);
  EXPECT_EQ(zeroCodim.codimension(), 0);
  MinimalPrimes zeroPrimes(pointer);
  Ideal zp(zeroPrimes.min_primes(3, -1));
  expectGenerators(*zp, {{0, 0, 0}});
  pointer = unit.get();
  MinimalPrimes unitCodim(pointer);
  EXPECT_EQ(unitCodim.codimension(), 4);
  MinimalPrimes unitPrimes(pointer);
  Ideal up(unitPrimes.min_primes(3, -1));
  expectGenerators(*up, {});
}

TEST_F(MonomialIdealTest, unimplementedEntryPoints)
{
  // These declarations have no linked definitions and cannot be called in an
  // engine test.
  GTEST_SKIP()
      << "insert_w_deletions; MinimalPrimes associated_primes, max_indep_sets, "
         "and static wrappers are declared but not defined.";
}

TEST_F(MonomialIdealTest, removalCollapsesTrieLayers)
{
  // Removing mixed-support generators must preserve every remaining path and
  // its divisibility lookup.
  auto source = ideal(ring,
                      {{3, 0, 0},
                       {2, 1, 0},
                       {1, 2, 0},
                       {0, 3, 0},
                       {2, 0, 1},
                       {1, 1, 1},
                       {0, 2, 1},
                       {1, 0, 2},
                       {0, 1, 2},
                       {0, 0, 3}});
  auto remaining = generators(*source);
  while (!remaining.empty())
    {
      SCOPED_TRACE(testing::PrintToString(remaining));
      Bag* raw = nullptr;
      ASSERT_EQ(source->remove(raw), 1);
      std::unique_ptr<Bag> removed(raw);
      Dense actual {};
      const auto& sparse = removed->monom();
      for (int i = 1; i < sparse[0]; i += 2) actual[sparse[i]] = sparse[i + 1];
      auto found = std::find(remaining.begin(), remaining.end(), actual);
      ASSERT_NE(found, remaining.end());
      remaining.erase(found);
      EXPECT_EQ(generators(*source), remaining);
      source->debug_check();
      for (const auto& gen : remaining)
        {
          Bag* divisor = nullptr;
          EXPECT_EQ(source->search_expvector(gen.data(), divisor), 1);
        }
    }
  EXPECT_EQ(source->size(), 0);
  EXPECT_EQ(source->topvar(), -1);
}

TEST_F(MonomialIdealTest, diagnosticTree)
{
  // The tree display's leaf count must match mixed-support generators,
  // including an empty tree.
  auto source = ideal(ring, {{2, 0, 0}, {1, 1, 0}, {0, 0, 3}});
  testing::internal::CaptureStdout();
  source->debug_out();
  const auto text = testing::internal::GetCapturedStdout();
  EXPECT_NE(text.find("monomials      = 3"), std::string::npos);
  EXPECT_NE(text.find("c3"), std::string::npos);
  testing::internal::CaptureStdout();
  source->debug_out(0);
  const auto counts = testing::internal::GetCapturedStdout();
  EXPECT_NE(counts.find("monomials      = 3"), std::string::npos);
  auto empty = ideal(ring, {});
  testing::internal::CaptureStdout();
  empty->debug_out();
  const auto zero = testing::internal::GetCapturedStdout();
  EXPECT_NE(zero.find("monomials      = 0"), std::string::npos);
}

// The outer codimension loop continues after the requested count is reached.
// Disabled until the linked defect is fixed and the intended result is
// returned. https://github.com/Macaulay2/M2/issues/4716
TEST_F(MonomialIdealTest, DISABLED_minimalPrimeCountAcrossCodimensions)
{
  // A global count limit applies even when minimal primes have different
  // heights.
  auto source = ideal(ring, {{1, 1, 0}, {1, 0, 1}});
  const MonomialIdeal* pointer = source.get();
  MinimalPrimes finder(pointer);
  Ideal result(finder.min_primes(3, 1));
  EXPECT_EQ(result->size(), 1);
  expectGenerators(*result, {{1, 0, 0}});
}
}  // namespace
