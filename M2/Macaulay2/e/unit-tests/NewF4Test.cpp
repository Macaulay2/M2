// In BUILD tree in Macaulay2/e:
// ./M2-unit-tests --gtest_filter="*F4*"

#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "f4/new-f4-types.hpp"

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
