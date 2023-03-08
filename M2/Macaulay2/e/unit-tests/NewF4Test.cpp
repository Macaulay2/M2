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

class TestMonoid
{
};

TEST(NewF4, hashtable)
{
  TestMonoid M;
  newf4::MonomialHashTable<TestMonoid> hashtab(M);
}
