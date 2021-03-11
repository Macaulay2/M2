// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerNoDedup.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

void reducerNoDedupDependency() {}

/// The most straight forward of the reducers. Simply keep a priority queue
/// with all the pending terms. If two like monomials are compared in the
/// queue, they are not combined - both stay in the queue. The sum of like
/// terms is only taken at the end, when the leading term and its
/// coefficient is determined.
template<template<typename> class Queue>
class ReducerNoDedup : public TypicalReducer {
public:
  ReducerNoDedup(const PolyRing& R);
  virtual ~ReducerNoDedup();

  virtual std::string description() const { 
    return mQueue.getName() + "-nodedup"; 
  }

  virtual void insertTail(NewConstTerm multiplier, const Poly& f);
  virtual void insert(ConstMonoRef multiplier, const Poly& f);

  virtual bool leadTerm(NewConstTerm& result);
  virtual void removeLeadTerm();

  virtual size_t getMemoryUse() const;

  virtual void resetReducer();

private:
  // This Configuration is designed to work with
  // mathic::TourTree, mathic::Heap, and mathic::Geobucket
  class Configuration : public ReducerHelper::PlainConfiguration {
  public:
    typedef NewTerm Entry;
    Configuration(const PolyRing& ring): PlainConfiguration(ring) {}
    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().lessThan(*a.mono, *b.mono);
    }
  };

  const PolyRing& mRing;
  NewTerm mLeadTerm;
  bool mLeadTermKnown;
  Queue<Configuration> mQueue;
};

template<template<typename> class Q>
ReducerNoDedup<Q>::ReducerNoDedup(const PolyRing& ring):
  mRing(ring),
  mLeadTermKnown(false),
  mQueue(Configuration(ring))
{
  mLeadTerm.mono = mRing.monoid().alloc().release();
}

template<template<typename> class Q>
ReducerNoDedup<Q>::~ReducerNoDedup() {
  resetReducer();
  mRing.monoid().freeRaw(*mLeadTerm.mono);
}

template<template<typename> class Q>
void ReducerNoDedup<Q>::insertTail(NewConstTerm multiple, const Poly& poly) {
  if (poly.termCount() <= 1)
    return;
  mLeadTermKnown = false;

  auto it = poly.begin();
  const auto end = poly.end();
  for (++it; it != end; ++it) {
    NewTerm t;
    t.mono = mRing.allocMonomial();
    mRing.monoid().multiply(*multiple.mono, it.mono(), *t.mono);
    mRing.coefficientMult(multiple.coef, it.coef(), t.coef);
    mQueue.push(t);
  }
}

template<template<typename> class Q>
void ReducerNoDedup<Q>::insert(ConstMonoRef multiple, const Poly& poly) {
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
bool ReducerNoDedup<Q>::leadTerm(NewConstTerm& result) {
  if (!mLeadTermKnown) {
    do {
      if (mQueue.empty())
        return false;
      mLeadTerm = mQueue.top();
      mQueue.pop();
    
      while (true) {
        if (mQueue.empty())
          break;
      
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
void ReducerNoDedup<Q>::removeLeadTerm() {
  if (!mLeadTermKnown) {
    NewConstTerm dummy;
    leadTerm(dummy);
  }
  mLeadTermKnown = false;
}

template<template<typename> class Q>
void ReducerNoDedup<Q>::resetReducer() {
  class MonomialFree {
  public:
    MonomialFree(const PolyRing& ring): mRing(ring) {}

    bool proceed(NewTerm entry) {
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
size_t ReducerNoDedup<Q>::getMemoryUse() const {
  return TypicalReducer::getMemoryUse() + mQueue.getMemoryUse();
}

MATHICGB_REGISTER_REDUCER(
  "TourNoDedup",
  Reducer_TourTree_NoDedup,
  make_unique<ReducerNoDedup<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapNoDedup",
  Reducer_Heap_NoDedup,
  make_unique<ReducerNoDedup<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoNoDedup",
  Reducer_Geobucket_NoDedup,
  make_unique<ReducerNoDedup<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
