// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerPack.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

void reducerPackDependency() {}

/// Keep a priority queue with entries that represent a term of a polynomial
/// times a multiplier. When an entry is popped, move on to the next
/// term of the polynomial, if any, and push that back into the queue. The
/// idea is that this reduces the number of elements in the queue, leading
/// to faster queue operations. Memory is also saved compared to expanding
/// each new polynomial with n terms into n entries in the queue.
template<template<typename> class Queue>
class ReducerPack : public TypicalReducer {
public:
  ReducerPack(const PolyRing& ring):
    mRing(ring),
    mLeadTermKnown(false),
    mQueue(Configuration(ring)),
    mPool(sizeof(MultipleWithPos))
  {
    mLeadTerm.mono = mRing.monoid().alloc().release();
  }

  virtual ~ReducerPack() {
    resetReducer();
    mRing.monoid().freeRaw(*mLeadTerm.mono);
  }

  virtual std::string description() const {
    return mQueue.getName() + "-packed";
  }

  virtual void insertTail(NewConstTerm multiplier, const Poly& f);
  virtual void insert(ConstMonoRef multiplier, const Poly& f);

  virtual bool leadTerm(NewConstTerm& result);
  virtual void removeLeadTerm();

  virtual size_t getMemoryUse() const;

private:
  virtual void resetReducer();

  // Represents a term multiple of a polynomial, 
  // together with a current term of the multiple.
  struct MultipleWithPos {
    MultipleWithPos(const Poly& poly, NewConstTerm multiple);

    Poly::ConstTermIterator pos;
    const Poly::ConstTermIterator end;
    NewTerm multiple;

    // invariant: current is the monomial product of multiple.monom 
    // and pos.getMonomial().
    MonoPtr current;

    // Ensures the invariant, so sets current to the product of
    // multiple.monom and pos.getMonomial().
    void computeCurrent(const PolyRing& ring);
    void currentCoefficient(const PolyRing& ring, coefficient& coeff);
    void destroy(const PolyRing& ring);
  };

  class Configuration : public ReducerHelper::PlainConfiguration {
  public:
    typedef MultipleWithPos* Entry;
    Configuration(const PolyRing& ring) : PlainConfiguration(ring) {}
    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().lessThan(*a->current, *b->current);
    }
  };

private:
  const PolyRing& mRing;
  NewTerm mLeadTerm;
  bool mLeadTermKnown;
  Queue<Configuration> mQueue;
  memt::BufferPool mPool;
};

template<template<typename> class Q>
void ReducerPack<Q>::insertTail(NewConstTerm multiple, const Poly& poly)
{
  if (poly.termCount() <= 1)
    return;
  mLeadTermKnown = false;

  MultipleWithPos* entry =
    new (mPool.alloc()) MultipleWithPos(poly, multiple);
  ++entry->pos;
  entry->computeCurrent(poly.ring());
  mQueue.push(entry);
}

template<template<typename> class Q>
void ReducerPack<Q>::insert(ConstMonoRef multiple, const Poly& poly)
{
  if (poly.isZero())
    return;
  mLeadTermKnown = false;

  NewConstTerm termMultiple = {1, multiple.ptr()};
  auto entry = new (mPool.alloc()) MultipleWithPos(poly, termMultiple);
  entry->computeCurrent(poly.ring());
  mQueue.push(entry);
}

template<template<typename> class Q>
ReducerPack<Q>::MultipleWithPos::MultipleWithPos(
  const Poly& poly,
  NewConstTerm multipleParam
):
  pos(poly.begin()),
  end(poly.end()),
  current(poly.ring().allocMonomial())
{
  multiple.mono = poly.ring().monoid().alloc().release();
  poly.ring().monoid().copy(*multipleParam.mono, *multiple.mono);
  multiple.coef = multipleParam.coef;
}

template<template<typename> class Q>
void ReducerPack<Q>::MultipleWithPos::computeCurrent(const PolyRing& ring) {
  ring.monoid().multiply(*multiple.mono, pos.mono(), *current);  
}

template<template<typename> class Q>
void ReducerPack<Q>::MultipleWithPos::currentCoefficient
(const PolyRing& ring, coefficient& coeff) {
  ring.coefficientMult(multiple.coef, pos.coef(), coeff);
}

template<template<typename> class Q>
void ReducerPack<Q>::MultipleWithPos::destroy(const PolyRing& ring) {
  ring.monoid().freeRaw(*current);
  ring.monoid().freeRaw(*multiple.mono);

  // Call the destructor to destruct the iterators into std::vector.
  // In debug mode MSVC puts those in a linked list and the destructor
  // has to be called since it takes an iterator off the list. There were
  // memory corruption problems in debug mode before doing this on MSVC.
  this->~MultipleWithPos();
}

template<template<typename> class Q>
bool ReducerPack<Q>::leadTerm(NewConstTerm& result)
{
  if (!mLeadTermKnown) {
    do {
      if (mQueue.empty())
        return false;
      MultipleWithPos* entry = mQueue.top();
      std::swap(mLeadTerm.mono, entry->current);
      entry->currentCoefficient(mRing, mLeadTerm.coef);
    
      while (true) {
        ++entry->pos;
        if (entry->pos == entry->end) {
          mQueue.pop();
          entry->destroy(mRing);
          mPool.free(entry);
        } else {
          entry->computeCurrent(mRing);
          mQueue.decreaseTop(entry);
        }
      
        if (mQueue.empty())
          break;
      
        entry = mQueue.top();
        if (!mRing.monoid().equal(*entry->current, *mLeadTerm.mono))
          break;
        coefficient coef;
        entry->currentCoefficient(mRing, coef);
        mRing.coefficientAddTo
          (mLeadTerm.coef, const_cast<const coefficient&>(coef));
      }
    } while (mRing.coefficientIsZero(mLeadTerm.coef));
  }

  result = mLeadTerm;
  mLeadTermKnown = true;
  return true;
}

template<template<typename> class Q>
void ReducerPack<Q>::removeLeadTerm()
{
  if (!mLeadTermKnown) {
    NewConstTerm dummy;
    leadTerm(dummy);
  }
  mLeadTermKnown = false;
}

template<template<typename> class Q>
void ReducerPack<Q>::resetReducer()
{
  class MonomialFree {
  public:
    MonomialFree(const PolyRing& ring): mRing(ring) {}

    bool proceed(MultipleWithPos* entry) {
      entry->destroy(mRing);
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
size_t ReducerPack<Q>::getMemoryUse() const
{
  return
    TypicalReducer::getMemoryUse() +
    mQueue.getMemoryUse() +
    mPool.getMemoryUse();
}

MATHICGB_REGISTER_REDUCER(
  "TourNoDedupPack",
  Reducer_TourTree_NoDedup_Packed,
  make_unique<ReducerPack<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapNoDedupPack",
  Reducer_Heap_NoDedup_Packed,
  make_unique<ReducerPack<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoNoDedupPack",
  Reducer_Geobucket_NoDedup_Packed,
  make_unique<ReducerPack<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
