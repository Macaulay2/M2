// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "gb-f4/MonomialHashTable.hpp"

TEST(NewF4, hashstats)
{
  newf4::HashTableStats stats;

  stats.nfind_or_insert++;
  stats.dump();
}

TEST(NewF4, hashtable)
{
  newf4::MonomialHashTable hashtab;
  std::vector<int32_t> mdata{5, 1, 2, 2, 3};
  newf4::Monomial m(mdata);
  newf4::MonomialIndex m1 = hashtab.find(m, 7342643);
  newf4::MonomialIndex m2 = hashtab.find(m, 7342643);
  std::cout << "m1 = " << m1 << std::endl;
  EXPECT_EQ(m1, m2);
}
