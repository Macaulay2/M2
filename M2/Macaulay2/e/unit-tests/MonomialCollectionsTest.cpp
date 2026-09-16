#include "monomials/monomial-collection.hpp"
#include "monomials/monomial-sets.hpp"
#include <gtest/gtest.h>
#include <algorithm>
#include <array>
#include <sstream>
#include <string>
#include <vector>
#include <utility>

namespace {
TEST(MonomialCollections, borrowedSets)
{
  // Sets retain caller storage and return the first pointer for equal contents,
  // even under collisions.
  int a[] = {3, 1, 2}, same[] = {3, 1, 2}, different[] = {3, 1, 3},
      shorter[] = {2, 1};
  MonomialSetFixedSize fixed(3);
  EXPECT_EQ(fixed.elementSize(), 3);
  EXPECT_EQ(fixed.numElements(), 0);
  EXPECT_EQ(fixed.find(a), nullptr);
  EXPECT_EQ(fixed.findOrInsert(a),
            std::make_pair(static_cast<const int*>(a), true));
  EXPECT_EQ(fixed.findOrInsert(same),
            std::make_pair(static_cast<const int*>(a), false));
  EXPECT_TRUE(fixed.findOrInsert(different).second);
  EXPECT_EQ(fixed.find(same), a);
  EXPECT_EQ(fixed.numElements(), 2);
  MonomialSetVarSize variable;
  EXPECT_EQ(variable.find(a), nullptr);
  EXPECT_EQ(variable.findOrInsert(a),
            std::make_pair(static_cast<const int*>(a), true));
  EXPECT_FALSE(variable.findOrInsert(same).second);
  EXPECT_TRUE(variable.findOrInsert(shorter).second);
  EXPECT_EQ(variable.find(different), nullptr);
  EXPECT_EQ(variable.find(same), a);
  EXPECT_EQ(variable.numElements(), 2);
  MonomialHashAndEqFixedSize fixedEq(3);
  MonomialHashAndEqVarSize varEq;
  EXPECT_EQ(fixedEq(a), fixedEq(same));
  EXPECT_EQ(varEq(a), varEq(same));
  EXPECT_FALSE(fixedEq(a, different));
  EXPECT_FALSE(varEq(a, different));
  EXPECT_FALSE(varEq(a, shorter));
}

TEST(MonomialCollections, ownedFixedStorage)
{
  // Collections copy external data and release only the top duplicate
  // allocation.
  int input[] = {2, 3, 4}, same[] = {2, 3, 4}, other[] = {4, 3, 2};
  MonomialCollectionFixedSize collection(3);
  EXPECT_EQ(collection.elementSize(), 3);
  EXPECT_EQ(collection.size(), 0);
  EXPECT_EQ(collection.find(input), nullptr);
  auto inserted = collection.findOrInsert(input);
  ASSERT_TRUE(inserted.second);
  EXPECT_NE(inserted.first, input);
  input[0] = 99;
  EXPECT_EQ(collection.find(same), inserted.first);
  EXPECT_EQ(collection.find(input), nullptr);
  EXPECT_FALSE(collection.findOrInsert(same).second);
  EXPECT_EQ(collection.size(), 1);
  auto& arena = collection.monomialMemorySpace();
  int* interned = arena.allocate(3);
  std::copy(other, other + 3, interned);
  auto added = collection.findOrInsertTopInternedMonomial(interned);
  EXPECT_TRUE(added.second);
  EXPECT_EQ(added.first, interned);
  EXPECT_EQ(collection.size(), 2);
  interned = arena.allocate(3);
  std::copy(same, same + 3, interned);
  EXPECT_EQ(collection.findOrInsertTopInternedMonomial(interned),
            std::make_pair(inserted.first, false));
  EXPECT_EQ(collection.size(), 2);
}

TEST(MonomialCollections, ownedVariableStorage)
{
  // Length participates in interning, and canonical pointers survive later
  // allocations.
  int shortMon[] = {2, 8}, longMon[] = {4, 8, 2, 7};
  MonomialCollectionVarSize collection;
  EXPECT_EQ(collection.size(), 0);
  EXPECT_EQ(collection.find(shortMon), nullptr);
  auto first = collection.findOrInsert(shortMon);
  auto second = collection.findOrInsert(longMon);
  EXPECT_TRUE(first.second);
  EXPECT_TRUE(second.second);
  EXPECT_EQ(collection.find(shortMon), first.first);
  EXPECT_EQ(collection.find(longMon), second.first);
  EXPECT_EQ(collection.findOrInsert(shortMon),
            std::make_pair(first.first, false));
  auto& arena = collection.monomialMemorySpace();
  int* interned = arena.allocate(4);
  std::copy(longMon, longMon + 4, interned);
  EXPECT_EQ(collection.findOrInsertTopInternedMonomial(interned),
            std::make_pair(second.first, false));
  EXPECT_EQ(collection.size(), 2);
}

TEST(MonomialCollections, arenaStack)
{
  // Popping the most recent allocation preserves earlier monomials and updates
  // the live count.
  MonomialMemorySpace arena;
  EXPECT_EQ(arena.size(), 0);
  auto first = arena.alloc(3);
  ASSERT_GE(first.second - first.first, 3);
  first.first[0] = 42;
  auto* second = arena.allocate(7);
  EXPECT_EQ(arena.size(), 2);
  arena.popLastAlloc(second);
  EXPECT_EQ(arena.size(), 1);
  EXPECT_EQ(first.first[0], 42);
  arena.popLastAlloc(first.first);
  EXPECT_EQ(arena.size(), 0);
  arena.freeAllAllocs();
  arena.freeAllAllocsAndBackingMemory();
  EXPECT_EQ(arena.size(), 0);
}

TEST(MonomialCollections, moduleMonomialSet)
{
  // Component is part of the key; sorting must update lookup indices without
  // losing stored words.
  int xy[] = {4, 2, 0, 1}, x[] = {3, 1, 0}, y[] = {3, 1, 1};
  ModuleMonomDefaultConfig config(2), copied(config);
  ModuleMonomialSet set(copied);
  EXPECT_EQ(set.size(), 0);
  EXPECT_EQ(set.find(Monom(x), 1), std::make_pair(-1, false));
  EXPECT_TRUE(set.insert(Monom(xy), 2));
  EXPECT_TRUE(set.insert(Monom(x), 1));
  EXPECT_TRUE(set.insert(Monom(y), 1));
  EXPECT_TRUE(set.insert(Monom(xy), 1));
  EXPECT_FALSE(set.insert(Monom(xy), 2));
  EXPECT_EQ(set.size(), 4);
  EXPECT_EQ(set.find(Monom(xy), 2), std::make_pair(0, true));
  EXPECT_EQ(set.find(Monom(xy), 1), std::make_pair(3, true));
  EXPECT_EQ(set.find(Monom(x), 3), std::make_pair(-1, false));
  set.sort();
  EXPECT_EQ(set.set().size(), 4);
  EXPECT_EQ(set.uniqueMonoms().size(), 4);
  int index = 0;
  for (const auto& mon : set)
    {
      EXPECT_EQ(mon.index(), index++);
      auto conf = set.configuration();
      EXPECT_TRUE(conf.Eq(mon, mon));
      EXPECT_EQ(conf.Hash(mon), mon.hash());
    }
  for (auto entry : std::vector<std::pair<const int*, int>> {
           {xy, 2}, {x, 1}, {y, 1}, {xy, 1}})
    {
      auto found = set.find(Monom(entry.first), entry.second);
      ASSERT_TRUE(found.second);
      ASSERT_GE(found.first, 0);
      ASSERT_LT(found.first, set.size());
      const auto& stored = set.uniqueMonoms()[found.first];
      EXPECT_EQ(stored.component(), entry.second);
      EXPECT_EQ(stored.size(), entry.first[0] + 3);
      EXPECT_TRUE(std::equal(
          entry.first + 1, entry.first + entry.first[0], stored.begin() + 4));
    }
  std::ostringstream out;
  set.display(out);
  EXPECT_FALSE(out.str().empty());
  std::ostringstream pair;
  pair << std::make_pair(3, 7);
  EXPECT_EQ(pair.str(), "[3,7]");
}

// Releasing all arena storage leaves the live allocation count unchanged.
// Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4712
TEST(MonomialCollections, DISABLED_releaseAllResetsLiveCount)
{
  // Both release operations must let subsequent allocations start counting from
  // zero.
  for (bool releaseBacking : {false, true})
    {
      SCOPED_TRACE(releaseBacking);
      MonomialMemorySpace arena;
      arena.allocate(3);
      arena.allocate(5);
      if (releaseBacking)
        arena.freeAllAllocsAndBackingMemory();
      else
        arena.freeAllAllocs();
      EXPECT_EQ(arena.size(), 0);
      auto* next = arena.allocate(2);
      EXPECT_EQ(arena.size(), 1);
      arena.popLastAlloc(next);
      EXPECT_EQ(arena.size(), 0);
    }
}
// The comparator accepts equality and violates std::sort's strict-order
// contract. Disabled until the linked defect is fixed and these intended-result
// assertions pass. https://github.com/Macaulay2/M2/issues/4713
TEST(MonomialCollections, DISABLED_comparatorIsStrict)
{
  // std::sort requires irreflexivity, including distinct objects with equal
  // keys.
  int raw[] = {3, 1, 0};
  int first[6], second[6];
  auto a = monomToModuleMonom(Monom(raw), 1, {first, first + 6});
  auto b = monomToModuleMonom(Monom(raw), 1, {second, second + 6});
  ModuleMonomLessThan less;
  EXPECT_FALSE(less(a, a));
  EXPECT_FALSE(less(a, b));
  EXPECT_FALSE(less(b, a));
}

}  // namespace
