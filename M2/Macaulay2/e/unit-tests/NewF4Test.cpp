// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "gb-f4/MonomialHashTable.hpp"

TEST(NewF4, hashstats)
{
  newf4::HashTableStats stats;

  stats.dump();
}

newf4::HashInt hashFunction(const newf4::MonomialView& m)
{
  newf4::HashInt hash = 0;
  for (auto a : m) { hash += a; }
  return hash;
}

TEST(NewF4, hashtable)
{
  newf4::MonomialHashTable hashtab(5);
  // std::vector<int32_t> mdata{5, 1, 2, 2, 5};
  // newf4::Monomial m(mdata);
  // newf4::MonomialIndex m1 = hashtab.find(m, 7342643);
  // newf4::MonomialIndex m2 = hashtab.find(m, 7342643);
  // std::cout << "m1 = " << m1 << std::endl;
  // EXPECT_EQ(m1, m2);

  MemoryBlock B;
  for (auto i=0; i<100000; ++i)
    {
      newf4::MonomialView m({5, 1, 2, i, 3}, B);
      newf4::MonomialIndex m1 = hashtab.find(m, hashFunction(m));
    }
  for (auto i=0; i<10000; ++i)
    {
      newf4::MonomialView m({5, 1, 2, i, 3}, B);
      newf4::MonomialIndex m1 = hashtab.find(m, hashFunction(m));
    }
  std::cout << std::endl;
  hashtab.dump();

}
