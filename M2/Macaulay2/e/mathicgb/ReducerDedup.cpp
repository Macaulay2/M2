#include "memtailor/memtailor.h"
#include "memtailor/memtailor.h"

// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerDedup.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

void reducerDedupDependency() {}

/// As ReducerNoDedup, except that if two like terms are compared, then
/// they are removed and replaced by their sum.
template<template<typename> class Queue>
class ReducerDedup : public TypicalReducer {
public:
  ReducerDedup(const PolyRing& R);
  virtual ~ReducerDedup();

  virtual std::string description() const { 
    return mQueue.getName() + "-dedup"; 
  }

  virtual void insertTail(NewConstTerm multiplier, const Poly& f);
  virtual void insert(ConstMonoRef multiplier, const Poly& f);

  virtual bool leadTerm(NewConstTerm& result);
  virtual void removeLeadTerm();

  virtual size_t getMemoryUse() const;

  virtual void resetReducer();

public:
  // This Configuration is designed to work with
  // mathic::TourTree, mathic::Heap, and mathic::Geobucket
  class Configuration : public ReducerHelper::DedupConfiguration {
  public:
    typedef NewTerm Entry;
    Configuration(const PolyRing& ring): DedupConfiguration(ring) {}
    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().compare(*a.mono, *b.mono);
    }
    Entry deduplicate(Entry a, Entry b) const {
      // change a.coeff, and free b.monom
      ring().coefficientAddTo(a.coef, b.coef);
      ring().monoid().freeRaw(*b.mono);
      return a;
    }
  };

private:
  const PolyRing& mRing;
  NewTerm mLeadTerm;
  bool mLeadTermKnown;
  Queue<Configuration> mQueue;
};

template<template<typename> class Q>
ReducerDedup<Q>::ReducerDedup(const PolyRing& ring):
  mRing(ring),
  mLeadTermKnown(false),
  mQueue(Configuration(ring))
{
  mLeadTerm.mono = mRing.monoid().alloc().release();
}

template<template<typename> class Q>
ReducerDedup<Q>::~ReducerDedup() {
  resetReducer();
  mRing.monoid().freeRaw(*mLeadTerm.mono);
}

template<template<typename> class Q>
void ReducerDedup<Q>::insertTail(NewConstTerm multiple, const Poly& poly) {
  if (poly.termCount() <= 1)
    return;
  mLeadTermKnown = false;

  auto it = poly.begin();
  const auto end = poly.end();
  for (++it; it != end; ++it) {
    NewTerm t;
    t.mono = mRing.monoid().alloc().release();
    mRing.monoid().multiply(*multiple.mono, it.mono(), *t.mono);
    mRing.coefficientMult(multiple.coef, it.coef(), t.coef);
    mQueue.push(t);
  }
}

template<template<typename> class Q>
void ReducerDedup<Q>::insert(ConstMonoRef multiple, const Poly& poly) {
  if (poly.isZero())
    return;
  mLeadTermKnown = false;

  const auto end = poly.end();
  for (auto it = poly.begin(); it != end; ++it) {
    NewTerm t = {it.coef(), mRing.monoid().alloc().release()};
    mRing.monoid().multiply(multiple, it.mono(), *t.mono);
    mQueue.push(t);
  }
}

template<template<typename> class Q>
bool ReducerDedup<Q>::leadTerm(NewConstTerm& result) {
  if (!mLeadTermKnown) {
    do {
      if (mQueue.empty())
        return false;
      mLeadTerm = mQueue.top();
      mQueue.pop();
    
      while (!mQueue.empty()) {
        auto entry = mQueue.top();
        if (!mRing.monoid().equal(*entry.mono, *mLeadTerm.mono))
          break;
        mRing.coefficientAddTo(mLeadTerm.coef, entry.coef);
        mRing.monoid().freeRaw(*entry.mono);
        mQueue.pop();
      }
    } while (mRing.coefficientIsZero(mLeadTerm.coef));
    mLeadTermKnown = true;
  }

  result = mLeadTerm;
  return true;
}

template<template<typename> class Q>
void ReducerDedup<Q>::removeLeadTerm() {
  if (!mLeadTermKnown) {
    NewConstTerm dummy;
    leadTerm(dummy);
  }
  mLeadTermKnown = false;
}

template<template<typename> class Q>
void ReducerDedup<Q>::resetReducer() {
  class MonomialFree {
  public:
    MonomialFree(const PolyRing& ring): mRing(ring) {}

    bool proceed(NewTerm entry)
    {
      mRing.monoid().freeRaw(*entry.mono);
      return true;
    }
  private:
    const PolyRing& mRing;
  };

  MonomialFree freeer(mRing);
  mQueue.forAll(freeer);
  mQueue.clear();
}

template<template<typename> class Q>
size_t ReducerDedup<Q>::getMemoryUse() const {
  return TypicalReducer::getMemoryUse() + mQueue.getMemoryUse();
}

MATHICGB_REGISTER_REDUCER(
  "TourDedup",
  Reducer_TourTree_Dedup,
  make_unique<ReducerDedup<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapDedup",
  Reducer_Heap_Dedup,
  make_unique<ReducerDedup<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoDedup",
  Reducer_Geobucket_Dedup,
  make_unique<ReducerDedup<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
