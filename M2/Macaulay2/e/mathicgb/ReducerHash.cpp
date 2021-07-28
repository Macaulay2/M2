// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerHash.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "PolyHashTable.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

void reducerHashDependency() {}

/// The simplest reducer using a hashtable. All terms are in the hash table
/// and in the queue. There are no duplicates as new terms are looked up in
/// the hash table before insertion into the queue. The coefficient is
/// stored in the hash table.
template<template<typename> class Queue>
class ReducerHash : public TypicalReducer {
public:
  ReducerHash(const PolyRing &ring);

  virtual std::string description() const { 
    return mQueue.getName() + "-hashed";
  }

  void insertTail(NewConstTerm multiplier, const Poly& f);
  void insert(ConstMonoRef multiplier, const Poly& f);

  virtual bool leadTerm(NewConstTerm& result);
  void removeLeadTerm();

  size_t getMemoryUse() const;

  void resetReducer();

public:
  class Configuration : public ReducerHelper::PlainConfiguration {
  public:
    typedef PolyHashTable::Node* Entry;

    Configuration(const PolyRing& ring): PlainConfiguration(ring) {}

    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().lessThan(a->mono(), b->mono());
    }
  };
  
private:
  mutable std::vector<PolyHashTable::Node*> mNodesTmp;
  const PolyRing &mRing;
  PolyHashTable mHashTable;
  Queue<Configuration> mQueue;
};

template<template<typename> class Q>
ReducerHash<Q>::ReducerHash(const PolyRing &ring):
  mRing(ring),
  mHashTable(ring),
  mQueue(Configuration(ring))
{}

template<template<typename> class Q>
void ReducerHash<Q>::insertTail(NewConstTerm multiplier, const Poly& f) {
  if (f.termCount() <= 1)
    return;

  mNodesTmp.clear();
  auto it = f.begin();
  const auto end = f.end();
  for (++it; it != end; ++it) {
    auto p = mHashTable.insertProduct(*it, multiplier);
    if (p.second)
      mNodesTmp.emplace_back(p.first);
  }
  if (!mNodesTmp.empty())
    mQueue.push(mNodesTmp.begin(), mNodesTmp.end());
}

template<template<typename> class Q>
void ReducerHash<Q>::insert(ConstMonoRef multiplier, const Poly& f) {
  mNodesTmp.clear();
  const auto end = f.end();
  for (auto it = f.begin(); it != end; ++it) {
    auto p = mHashTable.insertProduct(it.mono(), multiplier, it.coef());
    if (p.second)
      mNodesTmp.emplace_back(p.first);
  }
  if (!mNodesTmp.empty())
    mQueue.push(mNodesTmp.begin(), mNodesTmp.end());
}

template<template<typename> class Q>
bool ReducerHash<Q>::leadTerm(NewConstTerm& result) {
  while (!mQueue.empty()) {
    const auto top = mQueue.top();
    if (!mRing.coefficientIsZero(top->value())) {
      result.coef = top->value();
      result.mono = top->mono().ptr();
      return true;
    }
    mQueue.pop();
    mHashTable.remove(top);
  }
  return false;
}

template<template<typename> class Q>
void ReducerHash<Q>::removeLeadTerm() {
  const auto top = mQueue.top();
  mQueue.pop();
  mHashTable.remove(top);
}

template<template<typename> class Q>
void ReducerHash<Q>::resetReducer() {
  while (!mQueue.empty()) {
    const auto top = mQueue.top();
    mQueue.pop();
    mHashTable.remove(top);
  }
  mHashTable.clear();
}

template<template<typename> class Q>
size_t ReducerHash<Q>::getMemoryUse() const {
  size_t result = TypicalReducer::getMemoryUse();
  result += mHashTable.getMemoryUse();
  result += mQueue.getMemoryUse();
  return result;
}

MATHICGB_REGISTER_REDUCER(
  "TourHash",
  Reducer_TourTree_Hashed,
  make_unique<ReducerHash<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapHash",
  Reducer_Heap_Hashed,
  make_unique<ReducerHash<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoHash",
  Reducer_Geobucket_Hashed,
  make_unique<ReducerHash<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
