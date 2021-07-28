// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerPackDedup.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

void reducerPackDedupDependency() {}

/// As ReducerPack, with an extra feature: if two items with the same current
/// monomial in the queue are compared by the queue, then combine the first
/// into the second. That way, the second can be removed from the queue.
/// This requires keeping a linked list with each entry of the other entries
/// that have been combined into it, since we need to move on to the next term
/// of all of those entries. This technique further reduces the number of
/// entries in the queue.
///
/// Note that the linked lists are circular to make it fast to append two
/// linked lists. The trick is that you can combine two distinct circular
/// lists by swapping the next pointers of any one node in the first list and
/// of any one node in the second list.
template<template<typename> class Queue>
class ReducerPackDedup : public TypicalReducer {
public:
  ReducerPackDedup(const PolyRing& ring);
  virtual ~ReducerPackDedup();

  virtual std::string description() const {
    return mQueue.getName() + "-packed";
  }

  virtual void insertTail(NewConstTerm multiplier, const Poly& f);
  virtual void insert(ConstMonoRef multiplier, const Poly& f);

  virtual bool leadTerm(NewConstTerm& result);
  virtual void removeLeadTerm();

  virtual size_t getMemoryUse() const;

  virtual void resetReducer();

private:
  // Represents a term multiple of a polynomial, 
  // together with a current term of the multiple.
  struct MultipleWithPos {
    MultipleWithPos(const Poly& poly, NewConstTerm multiple);

    Poly::ConstTermIterator pos;
    const Poly::ConstTermIterator end;
    NewTerm multiple;

    // invariant: current is the monomial product of multiple.monom 
    // and pos.mono().
    MonoPtr current;

    // Ensures the invariant, so sets current to the product of
    // multiple.monom and pos.mono().
    void computeCurrent(const PolyRing& ring);
    void currentCoefficient(const PolyRing& ring, Coefficient& coeff);
    void addCurrentCoefficient(const PolyRing& ring, Coefficient& coeff);
    void destroy(const PolyRing& ring);

    // Points to a circular list of entries that have the same current
    // monomial. If no other such entries have been discovered, then
    // chain points to this object itself. We use a circular linked list
    // as those allow merging in O(1) time.
    MultipleWithPos* chain;
    void mergeChains(MultipleWithPos& entry) {
      // This only works if *this and entry are not already in the
      // same chain!
      std::swap(chain, entry.chain);
    }
  };

  class Configuration : public ReducerHelper::DedupConfiguration {
  public:
    typedef MultipleWithPos* Entry;
    Configuration(const PolyRing& ring): DedupConfiguration(ring) {}
    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().compare(*a->current, *b->current);
    }
    Entry deduplicate(Entry a, Entry b) const {
      a->mergeChains(*b);
      return a;
    }
  };
private:
  class MonomialFree;
  
  const PolyRing& mRing;
  NewTerm mLeadTerm;
  bool mLeadTermKnown;
  Queue<Configuration> mQueue;
  memt::BufferPool mPool;
};

template<template<typename> class Q>
ReducerPackDedup<Q>::ReducerPackDedup(const PolyRing& ring):
  mRing(ring),
  mLeadTermKnown(false),
  mQueue(Configuration(ring)),
  mPool(sizeof(MultipleWithPos))
{
  mLeadTerm.mono = mRing.monoid().alloc().release();
}

template<template<typename> class Q>
ReducerPackDedup<Q>::~ReducerPackDedup() {
  resetReducer();
  mRing.monoid().freeRaw(*mLeadTerm.mono);
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::insertTail(NewConstTerm multiple, const Poly& poly) {
  if (poly.termCount() <= 1)
    return;
  mLeadTermKnown = false;

  auto entry = new (mPool.alloc()) MultipleWithPos(poly, multiple);
  ++entry->pos;
  entry->computeCurrent(poly.ring());
  mQueue.push(entry);
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::insert(ConstMonoRef multiple, const Poly& poly) {
  if (poly.isZero())
    return;
  mLeadTermKnown = false;

  NewConstTerm termMultiple = {1, multiple.ptr()};
  auto entry = new (mPool.alloc()) MultipleWithPos(poly, termMultiple);
  entry->computeCurrent(poly.ring());
  mQueue.push(entry);
}

template<template<typename> class Q>
ReducerPackDedup<Q>::MultipleWithPos::MultipleWithPos(
  const Poly& poly,
  NewConstTerm multipleParam
):
  pos(poly.begin()),
  end(poly.end()),
  current(poly.ring().monoid().alloc().release()),
  chain(this)
{
  multiple.mono = poly.ring().monoid().alloc().release();
  poly.ring().monoid().copy(*multipleParam.mono, *multiple.mono);
  multiple.coef = multipleParam.coef;
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::MultipleWithPos::computeCurrent(
  const PolyRing& ring
) {
  ring.monoid().multiply(*multiple.mono, pos.mono(), *current);
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::MultipleWithPos::currentCoefficient(
  const PolyRing& ring,
  Coefficient& coef
) {
  ring.coefficientMult(multiple.coef, pos.coef(), coef);
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::MultipleWithPos::addCurrentCoefficient(
  const PolyRing& ring,
  Coefficient& coeff
) {
  Coefficient tmp;
  ring.coefficientMult(multiple.coef, pos.coef(), tmp);
  ring.coefficientAddTo(coeff, tmp);
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::MultipleWithPos::destroy(const PolyRing& ring) {
  MultipleWithPos* entry = this;
  do {
    ring.monoid().freeRaw(*entry->current);
    ring.monoid().freeRaw(*entry->multiple.mono);
    MultipleWithPos* next = entry->chain;
    MATHICGB_ASSERT(next != 0);

    // Call the destructor to destruct the iterators into std::vector.
    // In debug mode MSVC puts those in a linked list and the destructor
    // has to be called since it takes an iterator off the list. There were
    // memory corruption problems in debug mode on MSVC before doing this.
    entry->~MultipleWithPos();

    entry = next;
  } while (entry != this);
}

template<template<typename> class Q>
bool ReducerPackDedup<Q>::leadTerm(NewConstTerm& result) {
  if (!mLeadTermKnown) {
    do {
      if (mQueue.empty())
        return false;
      auto entry = mQueue.top();
      entry->currentCoefficient(mRing, mLeadTerm.coef);
      while (true) {
        // store the chained elements
        const auto chainBegin = entry->chain;
        const auto chainEnd = entry; // the list is circular
        entry->chain = entry; // detach any chained elements

        // handle the entry itself
        std::swap(mLeadTerm.mono, entry->current);
        ++entry->pos;
        if (entry->pos == entry->end) {
          mQueue.pop();
          entry->destroy(mRing);
          mPool.free(entry);
        } else {
          entry->computeCurrent(mRing);
          // Inserted spans must be in descending order
          MATHICGB_ASSERT(mQueue.getConfiguration().ring().
            monoid().lessThan(*entry->current, *mLeadTerm.mono));
          mQueue.decreaseTop(entry);
        }

        // handle any chained elements
        auto chain = chainBegin;
        while (chain != chainEnd) {
          MATHICGB_ASSERT(chain != 0);
          MATHICGB_ASSERT(mRing.monoid().equal(*chain->current, *mLeadTerm.mono));

          const auto next = chain->chain;
          chain->chain = chain; // detach from remaining chained elements

          chain->addCurrentCoefficient(mRing, mLeadTerm.coef);
          ++chain->pos;
          if (chain->pos == chain->end) {
            chain->destroy(mRing);
            mPool.free(chain);
          } else {
            chain->computeCurrent(mRing);
            // Inserted spans must be in descending order
            MATHICGB_ASSERT(mQueue.getConfiguration().ring().
              monoid().lessThan(*chain->current, *mLeadTerm.mono));
            mQueue.push(chain);
          }
          chain = next;
        }
      
        if (mQueue.empty())
          break;
      
        entry = mQueue.top();
        if (!mRing.monoid().equal(*entry->current, *mLeadTerm.mono))
          break;
        entry->addCurrentCoefficient(mRing, mLeadTerm.coef);
      }
    } while (mRing.coefficientIsZero(mLeadTerm.coef));
    mLeadTermKnown = true;
  }

  result = mLeadTerm;
  return true;
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::removeLeadTerm() {
  if (!mLeadTermKnown) {
    NewConstTerm dummy;
    leadTerm(dummy);
  }
  mLeadTermKnown = false;
}

template<template<typename> class Q>
void ReducerPackDedup<Q>::resetReducer() {
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
size_t ReducerPackDedup<Q>::getMemoryUse() const {
  return
    TypicalReducer::getMemoryUse() +
    mQueue.getMemoryUse() +
    mPool.getMemoryUse();
}

MATHICGB_REGISTER_REDUCER(
  "TourDedupPack",
  Reducer_TourTree_Dedup_Packed,
  make_unique<ReducerPackDedup<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapDedupPack",
  Reducer_Heap_Dedup_Packed,
  make_unique<ReducerPackDedup<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoDedupPack",
  Reducer_Geobucket_Dedup_Packed,
  make_unique<ReducerPackDedup<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
