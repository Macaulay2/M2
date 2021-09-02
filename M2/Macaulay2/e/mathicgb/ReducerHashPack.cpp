// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "ReducerHashPack.hpp"

#include "TypicalReducer.hpp"
#include "ReducerHelper.hpp"
#include "PolyHashTable.hpp"
#include "mathic/mathic.h"
#include "memtailor/memtailor.h"

MATHICGB_NAMESPACE_BEGIN

void reducerHashPackDependency() {}

/// A combination of ReducerHash and ReducerPack. Each entry in the queue
/// corresponds to a term (times a multiplier) of a polynomial. When such a
/// term is taken off the queue, we advance it to the next term of the
/// corresponding polynomial and push that back into the queue. Each entry
/// in the queue is also associated to a hash table entry, where the
/// data is stored - the entries in the queue are just pointers into
/// hash table nodes. There are no like terms to add up in the queue, since
/// the hash table identifies such before insertion into the queue.
///
/// There is no need for the complexity of chaining as in ReducerPackDedup,
/// since we simply keep advancing through the terms in a polynomial until
/// there are no more terms or we get a term that is unlike any other term
/// in the queue. We could have stored a list of like terms for each
/// entry in the queue as in ReducerPackDedup, which might reduce the size of
/// the queue, but this is a good compromise that leads to simpler code.
template<template<typename> class Queue>
class ReducerHashPack : public TypicalReducer {
public:
  ReducerHashPack(const PolyRing& R);
  virtual ~ReducerHashPack();

  virtual std::string description() const { 
    return mQueue.getName() + "-hashed-packed";
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
    PolyHashTable::Node* node;

    void destroy(const PolyRing& ring);
  };

  class Configuration : public ReducerHelper::PlainConfiguration {
  public:
    typedef MultipleWithPos* Entry;
    Configuration(const PolyRing& ring) : PlainConfiguration(ring) {}
    CompareResult compare(const Entry& a, const Entry& b) const {
      return ring().monoid().lessThan(a->node->mono(), b->node->mono());
    }
  };

  void insertEntry(MultipleWithPos* entry);

  const PolyRing& mRing;
  Queue<Configuration> mQueue;
  PolyHashTable mHashTable;
  memt::BufferPool mPool;
};

template<template<typename> class Q>
ReducerHashPack<Q>::ReducerHashPack(const PolyRing& ring):
  mRing(ring),
  mQueue(Configuration(ring)),
  mHashTable(ring),
  mPool(sizeof(MultipleWithPos))
{}

template<template<typename> class Q>
ReducerHashPack<Q>::~ReducerHashPack() {
  resetReducer();
}

template<template<typename> class Q>
void ReducerHashPack<Q>::insertTail(NewConstTerm multiple, const Poly& poly) {
  MATHICGB_ASSERT(&poly.ring() == &mRing);
  if (poly.termCount() <= 1)
    return;
  auto entry = new (mPool.alloc()) MultipleWithPos(poly, multiple);
  ++entry->pos;
  insertEntry(entry);
}

template<template<typename> class Q>
void ReducerHashPack<Q>::insert(ConstMonoRef multiple, const Poly& poly) {
  MATHICGB_ASSERT(&poly.ring() == &mRing);
  if (poly.isZero())
    return;
  NewConstTerm termMultiple = {1, multiple.ptr()};
  insertEntry(new (mPool.alloc()) MultipleWithPos(poly, termMultiple));
}

template<template<typename> class Q>
ReducerHashPack<Q>::MultipleWithPos::MultipleWithPos(
  const Poly& poly,
  NewConstTerm multipleParam
):
  pos(poly.begin()),
  end(poly.end()),
  node(0)
{
  multiple.mono = poly.ring().monoid().alloc().release();
  poly.ring().monoid().copy(*multipleParam.mono, *multiple.mono);
  multiple.coef = multipleParam.coef;
}

template<template<typename> class Q>
void ReducerHashPack<Q>::MultipleWithPos::destroy(const PolyRing& ring) {
  ring.monoid().freeRaw(*multiple.mono);

  // Call the destructor to destruct the iterators into std::vector.
  // In debug mode MSVC puts those in a linked list and the destructor
  // has to be called since it takes an iterator off the list. There were
  // memory corruption problems in debug mode on MSVC before doing this.
  this->~MultipleWithPos();
}

template<template<typename> class Q>
bool ReducerHashPack<Q>::leadTerm(NewConstTerm& result) {
  while (!mQueue.empty()) {
    auto entry = mQueue.top();
    MATHICGB_ASSERT(entry != nullptr);

    if (!mRing.coefficientIsZero(entry->node->value())) {
      result.coef = entry->node->value();
      result.mono = entry->node->mono().ptr();
      return true;
    }
    removeLeadTerm();
  }
  return false;
}

template<template<typename> class Q>
void ReducerHashPack<Q>::removeLeadTerm() {
  MATHICGB_ASSERT(!mQueue.empty());

  auto entry = mQueue.top();
  MATHICGB_ASSERT(entry != 0);

  // remove node from hash table first since we are going to be changing
  // the monomial after this, and if we do that before the hash value will
  // change. That might prompt an assert inside the hash table.
  mHashTable.remove(entry->node);

  MATHICGB_ASSERT(entry->pos != entry->end);
  while (true) {
    ++entry->pos;
    if (entry->pos == entry->end) {
      mQueue.pop();
      entry->destroy(mRing);
      mPool.free(entry);
      break;
    }

    const auto p = mHashTable.insertProduct(entry->multiple, *entry->pos);
    if (p.second) {
      entry->node = p.first;
      mQueue.decreaseTop(entry);
      break;
    }
  }
}

template<template<typename> class Q>
void ReducerHashPack<Q>::insertEntry(MultipleWithPos* entry) {
  MATHICGB_ASSERT(entry != 0);
  for (; entry->pos != entry->end; ++entry->pos) {
    const auto p = mHashTable.insertProduct(entry->multiple, *entry->pos);
    if (p.second) {
      entry->node = p.first;
      mQueue.push(entry);
      return;
    }
  }
  entry->destroy(mRing);
  mPool.free(entry);
}

template<template<typename> class Q>
void ReducerHashPack<Q>::resetReducer() {
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
  mHashTable.clear();
}

template<template<typename> class Q>
size_t ReducerHashPack<Q>::getMemoryUse() const {
  return mQueue.getMemoryUse() +
    mPool.getMemoryUse() +
    mHashTable.getMemoryUse();
}


MATHICGB_REGISTER_REDUCER(
  "TourHashPack",
  Reducer_TourTree_Hashed_Packed,
  make_unique<ReducerHashPack<mic::TourTree>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "HeapHashPack",
  Reducer_Heap_Hashed_Packed,
  make_unique<ReducerHashPack<mic::Heap>>(ring)
);

MATHICGB_REGISTER_REDUCER(
  "GeoHashPack",
  Reducer_Geobucket_Hashed_Packed,
  make_unique<ReducerHashPack<mic::Geobucket>>(ring)
);

MATHICGB_NAMESPACE_END
